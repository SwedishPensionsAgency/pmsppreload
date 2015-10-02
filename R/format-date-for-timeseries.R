#' Format year and month for timeseries
#'
#' @param year numeric vector for year
#' @param month numeric vector for month
#' @param day numeric vecotr for day
#' @param format character string used with \code{\link{format.Date}}
#'
#' @details \code{year}, \code{month}, and \code{day} must be of same length, otherwise they will be recycled to match the longest of them. See the details section of \code{\link{sprintf}}.
#'
#' @export
format_date_for_timeseries = function (year, month, day = 1, format = "%Y-%m") {
  date = as.Date(sprintf("%.f-%.f-%.f", year, month, day))
  return(format(date, format = format))
}
