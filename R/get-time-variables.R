#' Get Time Variables
#'
#' Get time variables form config
#'
#' @param config
#'
#' @export
#'
get_time_variables = function(config) {
  return(config$filters$static$time$vars)
}



#' Get Time Variables query
#'
#' Get time variables form config and return compound variable
#'
#' @param config
#'
#' @export
#'
get_time_variables_alias <- function(config){
  variables <- get_time_variables(config)
  if(length(variables) == 1 ){
    result <- variables
  }else{
    result <- variables[1]
    for(variable in variables[-1]){
      result  <- paste0(result  , " * 100 + ", variable)

    }
  }
  result
}