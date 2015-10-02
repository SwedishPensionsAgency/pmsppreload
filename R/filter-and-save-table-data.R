#' Filter and save table data single column
#'
#' @description Generates a single column summary of the baseData acording to the grouping and filtering parameters recieved.
#'
#'  @param metric A string with the name of the metric for which the summary will be created
#'  @param column A string with the name of the column for which the summary will be created
#'  @param sex A string with the name of the sex for which the summary will be created
#'  @param rowgroup A string with the name of the row for which the summary will be created
#'  @param config The config of the whole report
#'  @param verbose Verbose mode, i.e. tell the user what the program is doing
#'  @param filteredBySexData Data frame containing the raw data only for the sex passed in the sex parameter
#'  @param rowGroupDots Vector of column names to group by in order to summarise.
#'  @param columnDefinitions A list of column definitions (?)
#'  @param sexFilterDots Message related. Remove?
#'  @param fileType Defines which kind of file is to be generated. Options: base, child, subtotal
#'
#' @return The corresponding data sets is stored to disk. The data set is named
#'  "tableData" for immediate use in the Shiny frontend.
#'



filter_and_save_table_data_single_column <- function (metric, column, sex, rowgroup, config, verbose, filteredBySexData, rowGroupDots, columnDefinitions, sexFilterDots, fileType = "base") {

  fileTypeString <- getFileTypeString(fileType)

  dataName <- paste0(metric, "_", column, "_", sex, "_", rowgroup, fileTypeString)
  # ".RData")
#   dataPath <- file.path(config$target, "data", dataName)

  if (verbose) {
    message("Creating data set ", dataName, "...", appendLF = FALSE)
  }

  summaryDots <- columnDefinitions[ paste(metric, column, sep = "_") ]
  names(summaryDots) <- column

#   groupedData <- filteredBySexData %>% group_by_(.dots = rowGroupDots)
#
#
#
#   tableData <- groupedData %>% summarise_(.dots = summaryDots)

  functionToEvaluate <- parse(text = paste0("list( ", names(summaryDots), " = ", summaryDots, ")"))


  tableData <- filteredBySexData[,eval(functionToEvaluate),  by = rowGroupDots]


  filteredFilters <- get_collapsible_filters_for_row(config, rowgroup)


  if(length(filteredFilters) > 0){
    multiFilterSummariesData <- add_multi_filter_summaries_alt(filteredFilters, rowGroupDots, filteredBySexData, summaryDots)

    tableData <- rbind(tableData, multiFilterSummariesData )
  }


  # Error handling
  if (is.null(tableData)) {
    warning("A table that is equal to NULL was created during munge. Please see the application log for further details.")
    message("Table is NULL for the following parameters:\n",
            "\tmetric: ", metric, "\n",
            "\tcolumn: ", column, "\n",
            "\tsex: ", sex, "\n\n",
            "dots:\n",
            "\tsexFilterDots: ", sexFilterDots, "\n",
            "\trowGroupDots: ", rowGroupDots, "\n",
            "\tsummaryDots: ", summaryDots
    )
  }


  write_single_column_data_to_disk(tableDataBig = tbl_df(tableData), dataName, verbose, config)

  rm(tableData); gc()
}



#' Filter and save table data multi column
#'
#' @description Generates a multi column summary merging preexisting files acording to the grouping and filtering parameters recieved.
#'
#'  @param metric A string with the name of the metric for which the summary will be created
#'  @param column A string with the name of the column for which the summary will be created
#'  @param sex A string with the name of the sex for which the summary will be created
#'  @param rowgroup A string with the name of the row for which the summary will be created
#'  @param config The config of the whole report
#'  @param verbose Verbose mode, i.e. tell the user what the program is doing
#'  @param menuGroup The name of the group that is multi column and summaries will be merged by. i.e "metrics"
#'  @param groupConfig The configuration of the menuGroup
#'  @param multipleColumnGroups Name of the multi column group to merge by
#'  @param fileType Defines which kind of file is to be generated. Options: base, child, subtotal
#'  @param isDoubleMultiColumn A boolean that determines whether the summary to be created is multi column or double multi column.
#'
#' @return The corresponding data sets is stored to disk. The data set is named
#'  "tableData" for immediate use in the Shiny frontend.


filter_and_save_table_data_multicolumn <- function (metric, column, sex, row, config, verbose, menuGroup, groupConfig, multipleColumnGroups,  fileType = "base", isDoubleMultiColumn = FALSE, uniqueTimeValues) {


  fileTypeString <- getFileTypeString(fileType)

  for (i in 1:nrow(uniqueTimeValues)) {


    filterStrings <- c()
    varValues <- c()
    for(timeVariable in get_time_variables(config)) {
      varValue <- select_(uniqueTimeValues, .dots=timeVariable)[i,]
      varValues <- c(varValues, varValue)
    }

    fileDateString <- paste0("_", paste(varValues, collapse = "_"))



    fileName <- paste0(metric, "_", column, "_", sex, "_", row, fileTypeString, fileDateString, ".RData")
    filePath <- file.path(config$target, "data", fileName)

    if (verbose) {
      message("Creating data set ", fileName, "...", appendLF = FALSE)
    }

    placeholderSourceFileName <- switch(
      menuGroup,
      "metrics" = paste0("%s", "_", column, "_", sex, "_", row, fileTypeString, fileDateString, ".RData"),
      "columns" = paste0(metric, "_", "%s", "_", sex, "_", row, fileTypeString, fileDateString, ".RData"),
      "sex" = paste0(metric, "_", column, "_", "%s", "_", row, fileTypeString, fileDateString,".RData")
    )


    sourceDataSets <- NULL

    for (set in groupConfig$multipleColumn[[ multipleColumnGroups ]]$singleColumn) {
      dataFileName <- sprintf(placeholderSourceFileName, set)
      dataFilePath <- file.path(config$target, "data", dataFileName)
      load(dataFilePath)

      if(isDoubleMultiColumn){
        tableData <- tableData %>%
          #         group_by_(.dots = names(tableData)[1:(ncol(tableData)-3)]) %>%
          rename_(.dots = setNames(names(tableData)[(ncol(tableData) - 2):ncol(tableData)], paste(set, names(tableData)[(ncol(tableData) - 2):ncol(tableData)], sep = get_double_multi_column_deliminiter())))
      }else{
        tableData <- tableData %>%
          #       group_by_(.dots = names(tableData)[1:ncol(tableData)-1]) %>%
          rename_(.dots = setNames(names(tableData)[ncol(tableData)], set))
      }

      if (is.null(sourceDataSets)){
        sourceDataSets <- tableData
      } else {

        sourceDataSets <- data.table(sourceDataSets)
        tableData <- data.table(tableData)
        if(isDoubleMultiColumn){

          size <- length(names(tableData)) -3


        } else{
          size <- length(names(tableData)) -1
        }

        #allow.cartesian is needed when the data set has NULLs in the columns we are merging by.
        if (size > 0) {
          by <- names(tableData)[1:size]
          sourceDataSets <- merge(sourceDataSets,tableData, all=TRUE, by = by, allow.cartesian = TRUE)
        }else{
          sourceDataSets <- cbind(sourceDataSets, tableData)
        }

      }

    }

    tableData <- tbl_df(sourceDataSets)

    # Error handling
    if (is.null(tableData)) {
      warning("A table that is equal to NULL was created during munge. Please see the application log for further details.")
      message("Table is NULL for the following multicolumn parameters:\n",
              "\tmetric: ", metric, "\n",
              "\tcolumn: ", column, "\n",
              "\tsex: ", sex, "\n",
              "\trow: ", row, "\n",
              "\tmenuGroup: ", menuGroup, "\n",
              "\tmultipleColumnGroups: ", multipleColumnGroups, "\n"
      )
    }

    save(tableData, file = filePath)

  }
  if (verbose) {
    message(" Done.")
  }
  rm(tableData); gc()
}





getFileTypeString <- function (fileType) {
  switch(fileType,
         base = "",
         child = "_childrows",
         subtotal = "_subtotals"
  )

}



# Write data to disk
write_single_column_data_to_disk <- function (tableDataBig, dataName, verbose, config) {



  if(class(tableDataBig[[1]]) != "character"){
    tableDataBig[[1]] <- as.character(tableDataBig[[1]])
  }

  timeVariables <- get_time_variables(config)

  uniqueTimeValues <- get_unique_time_values(tableDataBig, timeVariables)

  for (i in 1:nrow(uniqueTimeValues)) {


    filterStrings <- c()
    varValues <- c()
    for(timeVariable in timeVariables) {
      varValue <- select_(uniqueTimeValues, .dots=timeVariable)[i,]
      filterStrings <- c(filterStrings, paste(timeVariable, varValue, sep= " == "))
      varValues <- c(varValues, varValue)
    }

    filterDots <- paste(filterStrings, collapse = " & ")


    removeVars <- paste("-", timeVariables)

    tableData <- filter_(tableDataBig, .dots = filterDots) %>% select_(.dots = removeVars)



    dataNameTime <- paste0(dataName, "_", paste(varValues, collapse = "_"),".RData")

    dataPath <- file.path(config$target, "data", dataNameTime)

    if(grepl("integer",dataPath) || grepl("NA", dataPath)){
      message("Something went wrong")
    }

    save(tableData, file = dataPath)

  }
  if (verbose) {
    message(" Done.")
  }
}



get_unique_time_values <- function (tableData, timeVariables) {

  uniqueTimeValues <- tableData %>% select_(.dots = timeVariables)  %>% distinct_(.dots = timeVariables)
}

