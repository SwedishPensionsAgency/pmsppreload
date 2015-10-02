#' Get a parseable list of all reports in application
#'
#' @param root The root directory to search for config files
#'
#' @return A list with the contents of all config files, sorted by folder structure
#' @export

load_all_report_configs <- function(root) {

  # Run
  listOfTabFolders = list.dirs(root, full.names = TRUE, recursive = FALSE)
  listOfReportConfigs = list()

  # Get all configs and generate a unique ID for each report
  for (tabFolder in listOfTabFolders) {


    tabIndex = which(tabFolder == listOfTabFolders)
    tabSerial = paste("tab", tabIndex, sep = "-")

    filePath <- file.path(tabFolder, "config.json")
    if(file.exists(filePath)){

      tabConfig = jsonlite::fromJSON(filePath, simplifyDataFrame = FALSE)
      tabConfig$path = tabFolder

      listOfReportConfigs[[ tabSerial ]] = list(meta = tabConfig, reports = list())


      listOfReportFolders = list.dirs(tabFolder, full.names = TRUE, recursive = FALSE)

      for (reportFolder in listOfReportFolders) {

        reportIndex = which(reportFolder == listOfReportFolders)
        reportSerial = paste('report', tabIndex, reportIndex, sep = "-")

        reportConfigFile = file.path(reportFolder, "config.json")
        if (file.exists(reportConfigFile)) {
          reportConfig = jsonlite::fromJSON(reportConfigFile, simplifyDataFrame = FALSE)
          reportConfig$path = reportFolder

          listOfReportConfigs[[ tabSerial ]]$reports[[ reportSerial ]] = reportConfig
        }
      }
    }

  }

  return(listOfReportConfigs)
}
