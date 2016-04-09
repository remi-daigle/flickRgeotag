#' Retrieves owner profile information
#'
#' @param photos metadata data.frame from flickr.meta.dl() or flickr.photos.search()
#'
#' @return data.frame
#' @export
#'
#' @examples
#' api <- '1aaaaa11a1aa11aaa1a11a111a1aaaaa'   # insert your own API key here
#' sec <- '11a1a111a111a1a1'                   # insert your own secret code here
#' bb <- '-65,44.5,-64.5,45'
#' photos <- flickr.photos.search(api_key = api,secret = sec,bbox=bb)
#' photosplusinfo <- flickr.people.dl(photos)
flickr.people.dl <- function(photos){

    # get info on owners
    locations <- photos %>%
        select(owner) %>%
        unique() %>%
        group_by(owner) %>%
        do(flickr.people.getInfo(.$owner)) %>%
        ungroup()

    # join to photos data.frame
    photosplus <- left_join(photos,locations,by='owner')

    return(photosplus)
}
