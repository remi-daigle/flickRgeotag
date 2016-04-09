
#' Gets available user profile info
#'
#' Gets available user profile info, currently only retrieve user location
#'
#' @param owner an owner value in the metadata data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' flickr.people.getInfo('85701104@N03')
flickr.people.getInfo <- function(owner){
    baseurl <- 'https://api.flickr.com/services/rest/?method=flickr.people.getInfo&api_key='

    userid <- gsub('@','%40',owner)

    raw <- getURL(paste0(baseurl,api_key,'&user_id=',userid,'&format=json&nojsoncallback=1'),encoding= 'UTF-8')

    info <- fromJSON(raw)

    if(is.null(info$person$location$`_content`)){
        info=data.frame(location=NA,stringsAsFactors = FALSE)
    } else{
        info=data.frame(location=info$person$location$`_content`,stringsAsFactors = FALSE)
    }

    return(info)
}

# flickr.people.getInfo(owner)
