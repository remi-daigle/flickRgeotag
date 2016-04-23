#' Download all metadata related to flickr.photos.search
#'
#' Returns data.frame of metadata for geotagged images inside the bounding box for all available pages. Unavailable pages will be filled with NA
#'
#' @inheritParams flickr.photos.search
#' @param sub_bbox a list of spatial bounding box from spatial data; output from
#'  \code{splitbbox()}
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
flickr.meta.dl <- function(api_key,sub_bbox,extras='geo,tags,date_taken,url_m'){

    for(i in 1:length(sub_bbox)){
        # get data
        assign(paste0('temp_photo_',i),flickr.photos.search(api_key, sub_bbox[[i]],.allpages = T),envir=environment())
    }

    # make a list of output
    photo_list <- lapply(ls(pattern="temp_photo_*"),get,envir=environment())

    # remove empties
    photo_list <- photo_list[unlist(lapply(photo_list,nrow))>0]

    # fill in empty columns
    nam <- unique(unlist(lapply(photo_list,names)))
    for(i in 1:length(photo_list)){
        missing <- NULL
        df <- photo_list[[i]]
        missing <- nam[!nam %in% names(df)]
        df[missing] <- NA
        photo_list[[i]] <- df[nam]
    }

    photos <- unique(do.call('rbind',photo_list))
    # rm(list=ls(pattern="temp_photo_*"))

    return(photos)
}


