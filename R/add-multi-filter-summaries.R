#' Add multi filter Summaries
#'
#' @description Summarises data grouped by rowGroupDots while at the same time excluding the collapsibleFilters from the summary generating all permutations.
#'
#' @param collapsibleFilters A list of collapsible filters for which summaries should be generated
#' @param rowGroupDots The parameter to be used as .dots in the group_by_ function.
#' @param data The raw data subject of summarisation.
#' @param summaryDots The parameter to be used as .dots in the summarise_ function.
#'
#' @return Returns a data set with several summarisations of the original data plus the original data.
#' A filter being ignored means that the data will not be grouped by that filter thus generating subtotals not previously generated.
#' Every combination of ignored and not ignored filters is once processed.
#'
#' @export

add_multi_filter_summaries <- function (collapsibleFilters, rowGroupDots, data, summaryDots) {
  tableResult <- NULL
  allVariables <- unlist(sapply(collapsibleFilters, function(static) { static$vars }) %>% c())
  names(allVariables) <- NULL

  for(collapsibleFilterGroup in collapsibleFilters){

    filterVars <- collapsibleFilterGroup$vars

    otherFilters <- allVariables[- which(allVariables %in% filterVars)]

    names(filterVars) <- NULL
    tableResult2 <- add_filter_summaries(rowGroupDots, filterVars, data, summaryDots, otherFilters)
    if (is.null(tableResult)){
      tableResult <- tableResult2
    }else{
      tableResult <- bind_rows(tableResult, tableResult2)
    }
  }

  if (length(collapsibleFilters) > 1){
    tableResultAllAll <- summarise_removing_filters(rowGroupDots, allVariables, data, summaryDots)
    tableResult <- bind_rows(tableResult, tableResultAllAll)
  }

  tableResult <- unique(tableResult)
  tableResult
}



#' Add filter Summaries
#'
#' @description Summarises data grouped by rowGroupDots while at the same time excluding the filterVars from the summary.
#'
#' @param rowGroupDots The parameter to be used as .dots in the group_by_ function.
#' @param filterVars A vector of the column names we want to generate summaries for. This column names are the vars of the filter we are generating the summaries for.
#' @param data The raw data subject of summarisation.
#' @param summaryDots The parameter to be used as .dots in the summarise_ function.
#' @param otherFilters A vector of the column names of every other collapsible filter.
#'
#' @return Returns a data set with several summarisations of the original data plus the original data.
#' Every filter is once ignored, not grouping by that filter thus generating totals
#' for that column which can then be retrieved by searching the label value "Samtliga".
#'
#' @export


add_filter_summaries <- function (rowGroupDots, filterVars, data, summaryDots, otherFilters) {

  removedFilters <- c()
  tableData <- NULL
  if(length(filterVars) > 1){
    tableData2 <- NULL
    filterVar <- filterVars[2]


    tableData2 <- summarise_all_combinations_for_columns(data, filterVar, otherFilters, rowGroupDots, summaryDots)


    tableData <- tableData2

    filterVar <- filterVars

    tableData2 <- summarise_all_combinations_for_columns(data, filterVar, otherFilters, rowGroupDots, summaryDots)


  }else{
    filterVar <- filterVars[1]
    tableData2 <- summarise_all_combinations_for_columns(data, filterVar, otherFilters, rowGroupDots, summaryDots)


  }

    if (is.null(tableData) ){
      tableData <- tableData2
    }else {

      tableData <- bind_rows(tableData, tableData2)

    }

  tableData
}



summarise_all_combinations_for_columns <- function (data, filterVar, otherFilters, rowGroupDots, summaryDots) {

  tableData2 <- summarise_removing_filters(rowGroupDots, filterVar, data, summaryDots)
  if(length(otherFilters) > 0){

    for(amountOfCombinations in 1:length(otherFilters)){
      combinations <- combn(otherFilters, amountOfCombinations)
      for(matrixEntry in 1:dim(combinations)[2]){
        filterCombination <- combinations[, matrixEntry]

        allRemoved <- c(filterVar, filterCombination)

        tableData3 <- summarise_removing_filters(rowGroupDots, allRemoved, data, summaryDots)

        tableData2 <- bind_rows(tableData2, tableData3)
      }
    }
  }
  tableData2
}



#' Summarise removing filters
#'
#' @description Summarises data grouped by rowGroupDots while at the same time excluding the removedFilters from the summary
#'
#' @param rowGroupDots The parameter to be used as .dots in the group_by_ function.
#' @param removedFilterVars A vector of names of filtered colums we want to avoid grouping by.
#' @param data The raw data subject of summarisation.
#' @param summaryDots The parameter to be used as .dots in the summarise_ function.
#'
#' @return Returns a data set with several summarisations of the original data.
#' Every removedFilter is once ignored, not grouping by that filter thus generating totals
#' for that column which can then be retrieved by searching the label value "Samtliga".
#'

summarise_removing_filters <- function (rowGroupDots, removedFilterVars, data, summaryDots) {

  rowGroupDotsAlt <- rowGroupDots[- which(rowGroupDots %in% removedFilterVars)]

  groupedData <- data %>% group_by_(.dots = rowGroupDotsAlt)

  tableData2 <- groupedData %>% summarise_(.dots = summaryDots)

  for(removedFilterVar in removedFilterVars){

    tableData2 <- mutate(tableData2, filter = "Samtliga")
    names(tableData2)[which(names(tableData2) == "filter")] <- removedFilterVar

  }
  tableData2
}



