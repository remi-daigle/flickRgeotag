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
#' photos <- flickr.photos.search(api_key = api,secret = sec,bb=bb)
flickr.meta.dl <- function(api_key,bb,extras='geo,tags,date_taken,url_m',deg=0.01){

    # make bbox a character string if necessary
    if(class(bbox)=="matrix"){
        bbox=paste0(as.character(bbox),collapse=",")
    }

    # extract bbox info
    if(class(bb)=="character"){
        lat1 <- as.numeric(strsplit(bb,",")[[1]][2])
        lat2 <- as.numeric(strsplit(bb,",")[[1]][4])
        lng1 <- as.numeric(strsplit(bb,",")[[1]][1])
        lng2 <- as.numeric(strsplit(bb,",")[[1]][3])
    }

    # loop over each `deg` cell
    for(i in 1:ceiling((lng2-lng1)*(1/deg))){
        for(j in 1:ceiling((lat2-lat1)*(1/deg))){
            lat <- lat1+(j-1)*deg
            lng <- lng1+(i-1)*deg
            bb_temp <- paste(lng,
                               lat,
                               lng+deg,
                               lat+deg,
                               sep=",")

            assign(paste0("temp_photo_",bb_temp),
                       flickr.photos.search(api_key = api_key,
                                            bbox=bb_temp,
                                            extras = extras,
                                            .allpages=TRUE),
                   envir=environment()
            )

        }
    }

    # list all temp_photo_'s assigned in the loops
    df_list <- lapply(ls(pattern="temp_photo_*"),get,envir=environment())
    full_names <- unique(unlist(lapply(df_list,names)))
    index <- lapply(lapply(df_list,names),all.equal.character,full_names)=="TRUE"

    # rbind all the temp_photo_'s that have all the desired columns and have data
    photos <- do.call("rbind",df_list[index])


    # remove duplicate records
    photos <- photos[!duplicated(photos),]

    return(photos)
}


