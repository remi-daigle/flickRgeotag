# flickRgeotag
A 'work in progress' R package that queries flickr for geotagged data. Works as an R wrapper for the flickr API.

## Installation

This package is not on CRAN, please install directly from GitHub:

```
devtools::install_github("https://github.com/remi-daigle/flickRgeotag")
```

## Example usage:

Getting photos from Halifax, Nova Scotia, Canada taken after January 1, 2016 which contains the tag "water":

```
library(prettymapr)
library(flickRgeotag)
bb <- searchbbox("halifax, NS")
photos <- flickr.photos.search(bbox=bb, text="water", min_taken_date="2016-01-01")
```

