#' Get Double Multi Column Deliminiter
#'
#' The names of double multi columns have two parts, which must be seprated in order to display the right header in the table. This is a helper function needed in the preloading and the server.R of the app.
#'
#' @param asRegex logical, Should the return value be a regex for use in e.g. \code{\link{str_split}}?
#' @export
get_double_multi_column_deliminiter = function(asRegex = FALSE) {
  if (asRegex) {
    return("<%>")
  } else {
    return("<%>")
  }
}