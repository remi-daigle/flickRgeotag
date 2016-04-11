#' Search flickr for photos by bounding box
#'
#' Returns data.frame of metadata for geotagged images inside the bounding box for a single page. Essentially a pared down R version of the flickr \href{https://www.flickr.com/services/api/explore/flickr.photos.search}{tool}
#'
#' @param bbox the spatial bounding box from spatial data; output from \code{prettymapr::searchbbox()}, \code{sp::bbox()} (e.g. \code{bbox=bbox(shp)}) or a character string (e.g. \code{bbox='-65,44.5,-64.5,45'})
#' @param api_key your personal API key from \href{https://www.flickr.com/services/apps/create/apply/}{flickr}
#' @param extras character vector of potential extra fields returned. By default ('geo,tags') returns the geotag information (latitude, longitude, etc) and the photo's tags (keywords). Currently supported fields are: description, license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o
#' @param .allpages Pass \code{TRUE} to return all pages of results (by default only 100 ish results are returned).
#' @param ... Key/value pairs as defined in \href{https://www.flickr.com/services/api/flickr.photos.search.html}{the API documentation}. \code{text="my search query"} is particularly useful.
#'
#' @return A \code{data.frame} of photo information.
#' @export
#'
#' @examples
#' library(prettymapr)
#' bb <- searchbbox("halifax, NS")
#' photos <- flickr.photos.search(bbox=bb, text="water", min_taken_date="2016-01-01")
#' photos2 <- flickr.photos.search(bbox=bb, text="water", .allpages=TRUE)
#' photos3 <- flickr.photos.search(bbox=searchbbox("wolfville, NS"), min_taken_date = "2016-01-01")

flickr.photos.search <- function(api_key, bbox=NULL, extras=c("geo","tags","date_taken","url_m"),
                                 .allpages=FALSE, .usecache=TRUE, ...) {

    if(missing(api_key)) {
        api_key <- "9050c4b4efcc5fa378dc51233c098422" # paleolimbot's API key...feel free to leave in here
        message("Using default api_key...please get your own at https://www.flickr.com/services/apps/create/apply/")
    }

    # make bbox a character string if necessary
    if(class(bbox)=="matrix"){
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
        stop("Error occured in call to flickr.photos.search: ", raw$stat, " (code: ", raw$code, ")")
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

