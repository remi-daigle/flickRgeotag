% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flickrphotossearch.R
\name{flickr.photos.search}
\alias{flickr.photos.search}
\title{Search flickr for photos}
\usage{
flickr.photos.search(api_key, bbox = NULL, extras = c("geo", "tags",
  "date_taken", "url_m"), output = "metadata", .allpages = FALSE,
  .usecache = TRUE, ...)
}
\arguments{
\item{api_key}{your personal API key from
\href{https://www.flickr.com/services/apps/create/apply/}{flickr}}

\item{bbox}{the spatial bounding box from spatial data; output from
\code{prettymapr::searchbbox()}, \code{sp::bbox()} (e.g.
\code{bbox=bbox(shp)}) or a character string (e.g.
\code{bbox='-65,44.5,-64.5,45'})}

\item{extras}{character vector of potential extra fields returned. By default
('geo,tags') returns the geotag information (latitude, longitude, etc) and
the photo's tags (keywords). Currently supported fields are: description,
license, date_upload, date_taken, owner_name, icon_server, original_format,
last_update, geo, tags, machine_tags, o_dims, views, media, path_alias,
url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o}

\item{output}{character string indicating output type. Either "metadata" (default) for geotagged images, or "total" indicating the total number of records}

\item{.allpages}{Pass \code{TRUE} to return all pages of results (by default
only 100 ish results are returned).}

\item{.usecache}{Pass \code{FALSE} to not use cached results.}

\item{...}{Key/value pairs as defined in
\href{https://www.flickr.com/services/api/flickr.photos.search.html}{the
API documentation}. \code{text="my search query"} is particularly useful.}
}
\value{
A \code{data.frame} of photo information.
}
\description{
An R implementation of
\href{https://www.flickr.com/services/api/explore/flickr.photos.search}{flickr.photos.search}
from the \href{https://www.flickr.com/services/api/}{Flickr API}. Searches for photos based on
user-defined criteria. Some parameters (\code{bbox} and \code{extras}) allow passing of R objects
that are converted to the text input needed by the API.
}
\examples{
library(prettymapr)
bb <- searchbbox("halifax, NS")
photos <- flickr.photos.search(bbox=bb, text="water", min_taken_date="2016-01-01")
photos2 <- flickr.photos.search(bbox=bb, text="water", .allpages=TRUE)
photos3 <- flickr.photos.search(bbox=searchbbox("wolfville, NS"), min_taken_date = "2016-01-01")
}
