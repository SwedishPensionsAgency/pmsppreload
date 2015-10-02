#' Load Cached Data
#'
#' Loads the cached data for a specific report
#'
#' @param reportConfig the config for the report
#' @param cacheDir directory, where to look after the data file
#' @param ... parameters passed to \code{\link{load}}
#'
#' @export
load_cached_data <- function (reportConfig, cacheDirectory = "cache", envir = parent.frame(), verbose = FALSE) {
  sqlQuery <- readLines(file.path(reportConfig$path, "data", "query.sql")) %>% paste(collapse = "\n")

  if (length(sqlQuery) < 1) {
    stop("SQL Query missing! Tried to read from location:\n", file.path(reportConfig$path, "data", "query.sql"))
  }

  # Hash SQL query in order to get the cache name
  sqlQueryHash <- digest::digest(sqlQuery)
  cacheFile <- file.path(getwd(), cacheDirectory, paste0(sqlQueryHash, ".RData"))
  if (file.exists(cacheFile)) {
    if (verbose) {
      message("Loading: ", cacheFile, " ...")
    }
    load(cacheFile, envir = envir, verbose = verbose)
    if (verbose) {
      message("Done")
    }
  } else {
    warning("No cache file found for this SQL query. File name (", cacheFile, ")")
  }
}
