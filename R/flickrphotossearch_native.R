
#' Call the Flickr REST API
#'
#' Calls the \href{Flickr REST API}{https://www.flickr.com/services/api/}, returning the parsed
#' JSON as a \code{list}. This is a low-level function designed to serve as the base to all
#' flickr method calls. Results are cached and the cache is used instead of querying the REST
#' API to prevent unnecessary calls (useful for repetetive scripts). To pass a \code{list} of
#' key/value pairs, use \code{do.call(flickr.restquery, key_value_list)}
#'
#' @param ... Arguments are treated as key/value pairs to be passed to the URL. The caller is responsible
#'              for properly escaping any strings to be used in a URL (e.g. by xml2::url_escape).
#' @param rest_api The base URL for the REST API.
#' @param .usecache \code{TRUE} if the cache should be used, use \code{FALSE} to force a call to the
#'                  REST API.
#'
#' @return A \code{list} of the result. This list is guaranteed to have a \code{stat} and \code{result}
#'          element describing the result.
#' @export
#'
#' @examples
#' library(flickRgeotag)
#' flickr.restquery(method="flickr.test.echo", name="value", api_key="610ccba3a846af469e1894da33514ea1")
#'
flickr.restquery <- function(..., rest_api="https://api.flickr.com/services/rest/", .usecache=TRUE) {
    # make URL
    searchparams <- list(...)
    # may override some search params: https://www.flickr.com/services/api/response.json.html
    searchparams$format <- "json"
    searchparams$nojsoncallback <- 1

    # sorting ensures consistent url_hash with identical parameters
    params <- sapply(sort(names(searchparams)),
                     function(item) {paste(item, searchparams[[item]], sep="=")})
    url_string <- sprintf("%s?%s", rest_api, paste(params, collapse="&"))
    url_hash <- digest::digest(url_string)

    lines <- NULL

    # check for cached result
    if(exists(".flickr_api_result", envir=.GlobalEnv)) {
        lines <- get(".flickr_api_result", envir=.GlobalEnv)[[url_hash]]
    }

    # if there is no cached result, query the URL and parse using rjson
    if(is.null(lines) || !.usecache) {
        .flickr_api_result <- NULL; rm(.flickr_api_result) # trick CMD check
        message("Retreiving information from ", url_string)
        connect <- url(url_string)
        lines <- try(paste(readLines(connect, warn = FALSE), collapse=""), silent = TRUE)
        close(connect)

        # store geocoded information in users global environment
        if(!exists(".flickr_api_result", envir=.GlobalEnv)) {
            db <<- list()
        } else {
            db <- get(".flickr_api_result", envir=.GlobalEnv)
        }
        db[[url_hash]] <- lines
        .flickr_api_result <<- db
    }

    if(class(lines) == "try-error") {
        return(list(stat="url_request_error", code=-1))
    } else {
        return(rjson::fromJSON(lines))
    }
}
