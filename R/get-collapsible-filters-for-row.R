#' Get collapsible filters aplicable to a row
#'
#' @description Filters collapsible filters in a config for a given row selection.
#'
#' @param config The configuration of the report
#' @param row The row for which we want the available collapsible filters
#'
#' @return Returns a list of filters applicable to a specific row as it is not possible to filter by the row itself or its childrows.
#' This filters are expressed as column names to filter by.
#'
#' @export


get_collapsible_filters_for_row <- function(config, row) {
  thing <- config$rows[row]
  thingVars <- unlist(lapply(thing, function(x) c(x$parent, x$child)))
  names(thingVars) <- NULL

  blacklist <- unlist(lapply(config$filters$collapsible, function(x) x$vars))
  names(thingVars) <- NULL

  blacklist <- blacklist[ blacklist %in% thingVars]



  filteredFilters <- config$filters$collapsible[lapply(config$filters$collapsible, function(x) sum(x$vars %in% blacklist)) == 0]
  return(filteredFilters)
}

