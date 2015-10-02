#' Get list of desired reports
#'
#' @description filters the listOfReports returning only those that fill the criteria of desiredTabs
#' @param desiredTabs A vector of tab names the scrip will be run for. If left empty the script will be run for all tabs.
#' @param listOfReports A list of reports to be filtered
#'
#' @export

get_list_of_desired_reports <- function (desiredTabs, listOfReports) {

  if(  is.null(desiredTabs)){
    listOfDesiredReports <- listOfReports
    amountOfReports <- length(listOfReports$reports)
    amountOfDesiredReports <- length(listOfReports$reports)
  }else if (length(listOfReports$reports) > 0) {

#     listOfDesiredReports <- listOfReports[sapply(listOfReports$reports, function(x) basename(dirname(x$path)) %in% desiredTabs )]
    listOfDesiredReports <- list(reports = listOfReports$reports[sapply(listOfReports$reports, function(x) is_desired_path(path = x$path, desiredTabs = desiredTabs) )])
    amountOfDesiredReports <- length(listOfDesiredReports$reports)
    amountOfReports <- length(listOfReports$reports)

  }else{
    listOfDesiredReports <- list(reports = c())
    amountOfDesiredReports <- 0
    amountOfReports <- 0
  }



  message(" Found ", amountOfReports, " reports: but filtered it down to: ", amountOfDesiredReports )
  listOfDesiredReports
}


#' Get list of desired reports
#'
#' @description filters the listOfReports returning only those that fill the criteria of desiredTabs
#' @param path A path to analyze
#' @param listOfReports A list of reports to be filtered
#'
#' @export

is_desired_path <- function (path, desiredTabs) {

  desired <- FALSE
  for(desiredTab in desiredTabs){
    if(grepl(pattern = "/", x = desiredTab)){
      if(grepl(pattern = paste0(desiredTab, "$"), x = path)){
        desired = TRUE
      }
    }else{
      if (basename(dirname(path)) == desiredTab){
        desired = TRUE
      }
    }
  }

  desired

}
