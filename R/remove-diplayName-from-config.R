#' Remove Display Name from Config Parts
#'
#' @param reportConfig
#'
#' @export
#'
remove_display_name_from_config = function(config){
  for (partName in names(config)) {
    if (class(config[[ partName ]]) == "list") {
      if (!is.null(config[[ partName ]]$displayName)) {
        config[[ partName ]]$displayName = NULL
      }
      if (!is.null(config[[ partName ]]$displayNameShort)) {
        config[[ partName ]]$displayNameShort = NULL
      }
    }
  }
  return(config)
}