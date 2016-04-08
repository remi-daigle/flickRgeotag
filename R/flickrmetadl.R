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
flickr.meta.dl <- function(api_key,secret,bbox,extras){

    pages <- flickr.photos.search(api_key = api_key,secret = secret,bbox=bbox,extras = extras,pagenum=TRUE)

    df <- data.frame(p=1:pages)


    photos <- df %>%
        group_by(p) %>%
        do(flickr.photos.search(api_key = api_key,secret = secret,bbox=bbox,extras = extras,page=.$p))
}

