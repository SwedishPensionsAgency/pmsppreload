#' Get collapsible filters aplicable to a row
#'
#' @description Filters collapsible filters in a config for a given row selection.
#'
#' @param increment A list containing the start and end parameters of the interval
#' @param config The config of the whole report

#' @return Returns a list of filters applicable to a specific row as it is not possible to filter by the row itself or its childrows.
#' This filters are expressed as column names to filter by.
#' @export

create_query_from_config <- function (increment, config) {

  if(length(increment) > 0 ){

  queryTemplateTable = "SELECT {{variables}} FROM {{sqlTable}} WHERE {{{where}}}"

  sqlTable <- config$metadata$sqlTable
  where <- get_min_max_string(increment,get_time_variables_alias(config))

  variables <- "*"


  query = whisker.render(queryTemplateTable,
                         data = list(variables = variables,sqlTable = sqlTable,where = where))
  }
  else{
    query = paste("SELECT * FROM ",config$metadata$sqlTable)
  }

  query
}


#' Get min and max string
#'
#' @description Filters collapsible filters in a config for a given row selection.
#'
#' @param increment A list containing the start and end parameters of the interval
#' @param timeVariablesString A string containing the value of the time variable to be evaluated
#'
#' @return Returns a list of filters applicable to a specific row as it is not possible to filter by the row itself or its childrows.
#' This filters are expressed as column names to filter by.
#' @export

get_min_max_string <- function (increment, timeVariablesString) {

  from <- increment$from
  to <- increment$to

  fromValue <- get_time_value(from)
  toValue <- get_time_value(to)

  paste0(" (", timeVariablesString,") >= ", fromValue, " and (", timeVariablesString, ") <= ", toValue)

}



get_time_value <- function (from) {

  fromVariable <- from[[1]]
  for(variable in from[-1]){
    fromVariable <- fromVariable * 100 + variable
  }
  fromVariable
}

