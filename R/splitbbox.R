#' Title
#'
#' @inheritParams flickr.photos.search
#'
#' @return list
#' @export
#'
#' @examples
#' splitbbox(api_key, bbox)
splitbbox <- function(api_key, bbox=NULL, extras=c("geo","tags","date_taken","url_m"),
                       .allpages=FALSE, .usecache=TRUE, .oldn2=NULL, ...){
    if(missing(api_key)) {
        api_key <- "9050c4b4efcc5fa378dc51233c098422" # paleolimbot's API key...feel free to leave in here
        message("Using default api_key...please get your own at https://www.flickr.com/services/apps/create/apply/")
    }

    # make bbox a character string if necessary
    if(any(class(bbox)=="matrix")){
        bbox=paste0(as.character(bbox),collapse=",")
    }

    if(length(extras) > 1) { # more R-like to pass this in as a vector
        extras <- paste(extras, collapse=",")
    }

    # use of ... allows caller to pass an arbitrary number of other params
    # to further constrain search (e.g. tags, text, any other param)
    queryparams <- list(...)
    queryparams$.usecache <- .usecache

    # check to see if any search criteria were entered
    if(is.null(bbox) && length(queryparams) == 0) {
        stop("No search criteria passed to flickr.photos.search()")
    }

    # check to see if 'page' and '.allpages=TRUE' (will generate cryptic error later)
    if(.allpages && "page" %in% names(queryparams)) {
        stop("Ambiguous call: cannot pass 'page' and '.allpages=TRUE'")
    }

    queryparams$method = "flickr.photos.search"
    queryparams$api_key <- api_key
    queryparams$bbox <- bbox
    queryparams$extras <- extras

    # initial query to server
    raw <- do.call(flickr.restquery, queryparams)

    # if there was an error, raise an error
    if(raw$stat != "ok") {
        stop("Error occured in call to flickr.photos.search: ", raw$message, " (code: ", raw$code, ")")
    }

    n1 <- as.numeric(raw$photos$total)
    print(paste("n1 = ",n1))
    n2 <- NULL


    if(n1 > 1500){

        print(paste("original bbox",bbox,"returning too many results; now splitting..."))

        lat1 <- as.numeric(strsplit(bbox,",")[[1]][2])
        lat2 <- as.numeric(strsplit(bbox,",")[[1]][4])
        lng1 <- as.numeric(strsplit(bbox,",")[[1]][1])
        lng2 <- as.numeric(strsplit(bbox,",")[[1]][3])
        dlat <- (lat2-lat1)/10
        dlng <- (lng2-lng1)/10

        grid <- expand.grid(lat=0:9,lng=0:9)
        grid$lat1 <- lat1+dlat*grid$lat
        grid$lat2 <- lat2-dlat*(9-grid$lat)
        grid$lng1 <- lng1+dlng*grid$lng
        grid$lng2 <- lng2-dlng*(9-grid$lng)

        sub_bbox <- paste(grid$lng1,grid$lat1,grid$lng2,grid$lat2,sep=",")

        for(i in 1:length(sub_bbox)){

            queryparams$bbox <- sub_bbox[i]
            queryparams$extras <- extras

            # initial query to server
            raw <- do.call(flickr.restquery, queryparams)

            # if there was an error, raise an error
            if(raw$stat != "ok") {
                stop("Error occured in call to flickr.photos.search: ", raw$message, " (code: ", raw$code, ")")
            }

            n2 <- c(n2,as.numeric(raw$photos$total))
            print(paste("n2 = ",n2[i]))
        }

        # abort the split if bbox is unsplittable
        if(!is.null(.oldn2)&&.oldn2%in%n2){
            return(bbox)
        }


    } else {
        return(bbox)
    }

    # recursively call splitbbox if sub_bboxes should be split further
    subsub <- NULL
    sub_bbox_keep <- rep(TRUE,length(sub_bbox))
    if(any(n2 > 1500)){
        print(paste("too many returns in ",sum(n2 > 1500),"sub bboxes; now splitting further..."))

        for(i in (1:length(n2))[n2 > 1500]){

            new <- splitbbox(api_key, sub_bbox[i], extras,.allpages, .usecache, .oldn2 = n2[i], ...)

            subsub <- c(subsub,new)
            sub_bbox_keep[i] <- FALSE
        }
    }
    sub_bbox <- c(sub_bbox[sub_bbox_keep],subsub)

    return(sub_bbox)

}
