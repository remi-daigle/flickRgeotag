
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

    raw <- flickr.restquery(method="flickr.people.getInfo",
                            api_key=api_key,
                            user_id=owner
    )

    if(is.null(raw$person$location$`_content`)){
        info=data.frame(location="",stringsAsFactors = FALSE)
    } else{
        info=data.frame(location=raw$person$location$`_content`,stringsAsFactors = FALSE)
    }

    return(info)
}
