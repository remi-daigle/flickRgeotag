#' Search flickr for photos by bounding box
#'
#' Returns data.frame of metadata for geotagged images inside the bounding box for a single page. Essentially a pared down R version of the flickr \href{https://www.flickr.com/services/api/explore/flickr.photos.search}{tool}
#'
#' @param bbox the spatial bounding box from spatial data; output from sp::bbox() (e.g. bbox=bbox(shp)) or a character string (e.g. bbox='-65,44.5,-64.5,45')
#' @param api_key your personal API key from \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#' @param secret your secret code from \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#' @param extras character string of potential extra fields returned. By default ('geo,tags') returns the geotag information (latitude, longitude, etc) and the photo's tags (keywords). Currently supported fields are: description, license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o
#' @param pagenum LOGICAL will return the number of pages if TRUE, defaults to return a data.frame of metadata
#' @param page page number
#'
#' @return data.frame
#' @export
#'
#' @examples
#' api <- '1aaaaa11a1aa11aaa1a11a111a1aaaaa'   # insert your own API key here
#' sec <- '11a1a111a111a1a1'                   # insert your own secret code here
#' bb <- '-65,44.5,-64.5,45'
#' photos <- flickr.photos.search(api_key = api,secret = sec,bbox=bb)

flickr.photos.search <- function(api_key,secret,bbox,extras='geo,tags',page=1,pagenum=FALSE){
    # make bbox a character string if necessary
    if(class(bbox)=="matrix"){
        bbox=paste0(as.character(bbox(EastCoast)),collapse=",")
    }

    # edit bbox and extra to work in url
    bbox <- gsub(',','%2C',bbox)
    extras <- gsub(',','%2C',extras)

    # define 30 year timestamp
    tstamp <- as.integer(as.numeric(Sys.time())-(60*60*24*365*30))

    # query number of pages and metadata
    raw <- NULL
    count=0
    while(class(raw$photos$photo)!="data.frame"&count<=10){
        count=count+1

        raw <- fromJSON(getURL(paste0('https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=',
                                      api_key,
                                      '&bbox='
                                      ,bbox,
                                      '&has_geo=1&extras=',
                                      extras,
                                      '&per_page=100&page=',
                                      page,
                                      '&format=json&nojsoncallback=1'),
                               encoding= 'UTF-8'))
        if(class(raw$photos$photo)=="list") Sys.sleep(1)
    }

    if(class(raw$photos$photo)=="list") raw$photos$photo <- data.frame("context"=NA,"accuracy"=NA,"server"=NA,"isfriend"=NA,"title"=NA,"longitude"=NA,"latitude"=NA,"secret"=NA,"geo_is_public"=NA,"id"=NA,"geo_is_family"=NA,"owner"=NA,"isfamily"=NA,"farm"=NA,"geo_is_friend"=NA,"place_id"=NA,"woeid"=NA,"geo_is_contact"=NA,"ispublic"=NA)


    if(pagenum==TRUE){
        return(raw$photos$pages)
    } else{
        return(raw$photos$photo)
    }
}

