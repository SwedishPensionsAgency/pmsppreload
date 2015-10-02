#' Extract all Variable Names from Config
#'
#' Extract all variable names from config and return a character vector. Can be used to create a SQL query.
#'
#' @param config named list, config list to extract all variable names from
#' @param name character, name of the list item that holds varibales, default is \code{var}
#'
#' @export
extract_variables_names <- function(config, storageNames = c("var", "vars", "parent", "child")) {
  # browser()
  varibleNames = c()
  for (partName in names(config)) {
    if (partName %in% storageNames) {
      varibleNames = c(varibleNames, config[[ partName ]])
    } else if (!is.atomic(config[[ partName ]])) {
      varibleNames = c(varibleNames, extract_variables_names(config = config[[ partName ]], storageNames = storageNames))
    }
  }
  return(unique(varibleNames))
}
