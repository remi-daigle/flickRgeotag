#' plots flickr metadata using leaflet
#'
#' @inheritParams flickr.people.dl
#'
#' @return leaflet plot
#' @export
#'
#' @examples
#' api <- '1aaaaa11a1aa11aaa1a11a111a1aaaaa'   # insert your own API key here
#' sec <- '11a1a111a111a1a1'                   # insert your own secret code here
#' bb <- '-65,44.5,-64.5,45'
#' photos <- flickr.photos.search(api_key = api,secret = sec,bbox=bb)
#' photosplusinfo <- qflickr.plot(photos)
qflickr.plot <- function(photos){

    # remove NAs
    photos <- photos[!is.na(photos$latitude),]
    photos <- photos[!is.na(photos$longitude),]

    # make lat/long spatial points
    spoints <- sp::SpatialPoints(cbind(as.numeric(photos$longitude),as.numeric(photos$latitude)))
    sp::proj4string(spoints) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    # define html link
    photos$htmlformat <- paste0("<img src='",photos$url_m,"'>")

    # extract bbox from photos
    lng1=min(photos$longitude)
    lat1=min(photos$latitude)
    lng2=max(photos$longitude)
    lat2=max(photos$latitude)

    # plot with leaflet
    leaflet::leaflet(photos) %>%
        leaflet:: fitBounds(lng1, lat1, lng2, lat2)%>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(lng=~longitude,
                            lat=~latitude,
                            clusterOptions = leaflet::markerClusterOptions(),
                            popup = ~htmlformat
        )

}

