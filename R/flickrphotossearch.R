#' flickr.photos.search
#'
#' Returns data.frame of metadata for geotagged images inside the bounding box. Essentially a pared down R/Python version of the flickr \href{https://www.flickr.com/services/api/explore/flickr.photos.search}{tool}
#'
#' @param bbox a character string describing the bounding box
#' @param api_key your personal API key from \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#' @param secret your secret code from \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#'
#' @return data.frame
#' @export
#'
#' @examples
#' api <- '1aaaaa11a1aa11aaa1a11a111a1aaaaa'   # insert your own API key here
#' sec <- '11a1a111a111a1a1'                   # insert your own secret code here
#' bb <- '-65,44.5,-64.5,45'
#' photos <- flickr.photos.search(api_key = api,secret = sec,bbox=bb)

flickr.photos.search <- function(api_key,secret,bbox){

    # turn on python
    if(PythonInR::pyIsConnected()==FALSE){
        PythonInR::pyConnect()
    }
    # load python module
    pyImport(import = c("flickrapi"))

    # authentication
    FlickrAPI <- pyFunction("flickrapi.FlickrAPI")
    pySet('flickr',FlickrAPI(api_key,secret))

    # define function
    pyfps <- pyFunction("flickr.photos.search")

    # define 30 year timestamp
    tstamp <- as.integer(as.numeric(Sys.time())-(60*60*24*365*30))

    # query number of pages
    query_photos <- pyfps(bbox=bbox,
                                      has_geo=1,
                                      extras='geo,tags',
                                      last_update=tstamp,
                                      format='parsed-json',
                                      per_page=100,
                                      page='1')
    tp <- query_photos$photos$pages

    # loop over all the pages, tried to use dplyr for this but can't figure out how to handle errors in 'do'
    photos <- NULL
    for(p in 1:tp){
        print(p)
        # try to download the page, convert json to dataframe and add page number
        test_photos <- try(do.call(rbind.data.frame,
                                   c(pyfps(bbox=as.character(bbox),
                                           has_geo=1,
                                           extras='geo,tags',
                                           last_update=tstamp,
                                           format='parsed-json',
                                           per_page=100,
                                           page=p)$photos$photo,
                                     stringsAsFactors=F)
        ) %>%
            mutate(page=p),
        silent=TRUE
        )

        # if page is bad, do nothing; if it's good, append to photos
        if(class(test_photos)=="data.frame"){
            photos <- rbind(photos,test_photos)
        }
    }



    PythonInR::pyExit()
    return(photos)
}

