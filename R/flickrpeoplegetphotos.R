#' Search flickr for particular user's photos
#'
#' An R implementation of
#' \href{https://www.flickr.com/services/api/explore/flickr.people.getPhotos}{flickr.people.getPhotos}
#' from the \href{https://www.flickr.com/services/api/}{Flickr API}. Searches for photos based on
#' user-defined criteria. Some parameters (\code{extras}) allow passing of R objects
#' that are converted to the text input needed by the API.
#'
#' @param api_key your personal API key from
#'   \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#' @param user_id The ID of the user who's photos to return. A value of "me" will return the calling user's photos.
#' @param extras character vector of potential extra fields returned. By default
#'   ('geo,tags') returns the geotag information (latitude, longitude, etc) and
#'   the photo's tags (keywords). Currently supported fields are: description,
#'   license, date_upload, date_taken, owner_name, icon_server, original_format,
#'   last_update, geo, tags, machine_tags, o_dims, views, media, path_alias,
#'   url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o
#' @param .allpages Pass \code{TRUE} to return all pages of results (by default
#'   only 100 ish results are returned).
#' @param .usecache Pass \code{FALSE} to not use cached results.
#' @param ... Key/value pairs as defined in
#'   \href{https://www.flickr.com/services/api/flickr.photos.search.html}{the
#'   API documentation}. \code{text="my search query"} is particularly useful.
#'
#' @return A \code{data.frame} of photo information.
#' @export
#'
#' @examples
#' user_id = "141233693@N02"
#' flickr.people.getPhotos(api_key,user_id = user_id)

flickr.people.getPhotos <- function(api_key, user_id = NULL, extras=c("geo","tags","date_taken","url_m"),
                                 output="metadata",.allpages=FALSE, .usecache=TRUE, ...) {
    if(missing(api_key)) {
        api_key <- "9050c4b4efcc5fa378dc51233c098422" # paleolimbot's API key...feel free to leave in here
        message("Using default api_key...please get your own at https://www.flickr.com/services/apps/create/apply/")
    }

    if(length(extras) > 1) { # more R-like to pass this in as a vector
        extras <- paste(extras, collapse=",")
    }

    # use of ... allows caller to pass an arbitrary number of other params
    # to further constrain search (e.g. tags, text, any other param)
    queryparams <- list(...)
    queryparams$.usecache <- .usecache

    # check to see if any search criteria were entered
    if(is.null(user_id) && length(queryparams) == 0) {
        stop("No search criteria passed to flickr.photos.search()")
    }

    # check to see if 'page' and '.allpages=TRUE' (will generate cryptic error later)
    if(.allpages && "page" %in% names(queryparams)) {
        stop("Ambiguous call: cannot pass 'page' and '.allpages=TRUE'")
    }

    queryparams$method = "flickr.people.getPhotos"
    queryparams$api_key <- api_key
    queryparams$user_id <- user_id
    queryparams$extras <- extras

    # initial query to server
    raw <- do.call(flickr.restquery, queryparams)

    # if there was an error, raise an error
    if(raw$stat != "ok") {
        stop("Error occured in call to flickr.people.getPhotos: ", raw$message, " (code: ", raw$code, ")")
    }
    # if there are no photos, return empty data.frame
    if(is.null(raw$photos) || length(raw$photos$photo) == 0 || raw$photos$total == 0) {
        if(is.null(queryparams$page)) warning("No photos found, returning emtpy data frame")
        return(data.frame()) # makes more sense than data frame with one row
    }

    df <- raw$photos$photo

    if(.allpages) {
        if(raw$photos$pages > 1) {
            message("Downloading ", raw$photos$pages, " pages (estimated ", raw$photos$total, " photos)")
            pb <- utils::txtProgressBar(min=0, max=raw$photos$pages, width=20, file=stderr())
            for(page in 2:raw$photos$pages) {
                newdf <- suppressMessages(flickr.photos.search(api_key=api_key, bbox=bbox, extras=extras,
                                                               .allpages=FALSE, .usecache=.usecache, page=page, ...))

                # if there are no rows, quit the loop
                if(nrow(newdf) == 0) {
                    break
                }
                # fill in empty columns
                missing <- NULL
                missing <- names(df)[!names(df) %in% names(newdf)]
                newdf[missing] <- NA

                missing <- NULL
                missing <- names(newdf)[!names(newdf) %in% names(df)]
                df[missing] <- NA

                df <- rbind(df, newdf)
                utils::setTxtProgressBar(pb, page)
            }
            message("...done!")

            # remove duplicate entries (often included in flickr output)
            return(unique(df))
        } else {
            return(df)
        }
    } else {
        if(raw$photos$pages > 1) {
            message(sprintf("Returning %s photos of %s. Use .allpages=TRUE to query all photos (may take a long time).",
                            nrow(df), raw$photos$total))
        }
        return(df)
    }

}
