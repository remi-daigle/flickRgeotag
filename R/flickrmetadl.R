#' Download all metadata related to flickr.photos.search
#'
#' Returns data.frame of metadata for geotagged images inside the bounding box for all available pages. Unavailable pages will be filled with NA
#'
#' @inheritParams flickr.photos.search
#'
#'
#' @return data.frame
#' @export
#'
#' @examples
#' api <- '1aaaaa11a1aa11aaa1a11a111a1aaaaa'   # insert your own API key here
#' sec <- '11a1a111a111a1a1'                   # insert your own secret code here
#' bb <- '-65,44.5,-64.5,45'
#' photos <- flickr.photos.search(api_key = api,secret = sec,bbox=bb)
flickr.meta.dl <- function(api_key,secret,bbox,extras='geo,tags,date_taken,url_m'){

    pages <- flickr.photos.search(api_key = api_key,secret = secret,bbox=bbox,extras = extras,pagenum=TRUE)

    df <- data.frame(p=1:pages)


    photos <- df %>%
        group_by(p) %>%
        do(flickr.photos.search(api_key = api_key,secret = secret,bbox=bbox,extras = extras,page=.$p)) %>%
        ungroup()

    # remove duplicate records
    photos <- photos[!duplicated(photos),]

    return(photos)
}

