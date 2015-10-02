#' Preloading script for reports
#'
#' @description A script for preloading data and storing it in a pre-aggregated fashion for reports in the STA Shiny interface
#'
#' @param sourceReportDir The source directory in which to search for report config files
#' @param targetAppDir The target app directory to which to write the pre-processed data and config files
#' @param verbose Verbose mode, i.e. tell the user what the program is doing
#' @param cache Store downloaded data locally; boolean. Note that activating local caching could take up a LOT of hard disk space!
#' @param cacheDirectory This is the location to store cached data. Required if \code{cache = TRUE}
#' @param desiredTabs A vector of tab names the scrip will be run for. If left empty the script will be run for all tabs.
#' @param increment A list that consists of two elements, from and to in order to set limits to the data to process.
#' @param scope Determines which parts of the scripts the user wants to run. Posible values 'logs', 'all'
#'
#' @examples
#' \dontrun{
#' preload_reports(cache = TRUE, logToFile = TRUE, desiredTabs = c("1-bt"))
#' }
#'
#' @export

preload_reports <- function(
  sourceReportDir = file.path(getwd(), "modules/reports/"),
  targetAppDir = file.path(getwd(), "modules/reports/"),
  verbose = TRUE,
  logToFile = FALSE,
  cache = FALSE,
  cacheDirectory = "cache",
  desiredTabs = c(),
  increment = list(),
  scope = "all"
) {
  # Log file management
  if (logToFile == TRUE) {
    con <- file("preload_reports.log.txt", open = "w")
    open(con)
    sink(con, type = "message")
    on.exit({
      sink(type = "message")
      closeAllConnections()
    })
  }

  # Cache error handling
  if (cache == TRUE && is.null(cacheDirectory)) {
    stop("cache option set to TRUE but no cacheDirectory specified.")
  }

  # Due warning
  if (cache == TRUE) {
    message("WARNING! The cache parameter is set to TRUE. This might create datasets several gigabytes large on your local hard drive!")
    warning("cache is set to TRUE. This might create datasets several gigabytes large on your local hard drive!")
  }


  if (verbose) {
    message("Initialising data preloading.\nSource Report Directory:\n", sourceReportDir, "\nTarget App Dir:\n", targetAppDir)
    message("Loading list of report config files...", appendLF = FALSE)
  }

  listOfAllReportTabs <- load_all_report_configs(sourceReportDir)

  if (verbose) {
    message(" Done. Found ", length(listOfAllReportTabs), " tabs with the following number of reports: ",
            paste(sapply(listOfAllReportTabs, function(tab) { length(tab$reports) }), collapse = ", ")
    )
  }


  for (listOfReports in listOfAllReportTabs) {

    listOfDesiredReports <-   get_list_of_desired_reports(desiredTabs, listOfReports)



     for (config in listOfDesiredReports$reports) {

       # Read SQL query
       if (verbose) {
         message("Reading SQL query...", appendLF = FALSE)
       }

       sqlQuery <- create_query_from_config(increment, config)



       if (length(sqlQuery) < 1) {
         stop("SQL Query missing! Tried to read from location:\n", file.path(config$path, "data/query.sql"))
       } else if (verbose) {
         message(" Done. Using the following SQL query:")
         message(sqlQuery)
       }

       # Hash SQL query in order to get the cache name
       sqlQueryHash <- digest::digest(sqlQuery)


      # Does the cached file already exist?
      if (cache == TRUE) {
        cacheFileName <- file.path(cacheDirectory, paste0(sqlQueryHash, ".RData"))

        if (file.exists(cacheFileName)) {
          fetchDataFromCache <- TRUE
        } else {
          fetchDataFromCache <- FALSE
        }
      } else {
        fetchDataFromCache <- FALSE
      }

      if (verbose) {
        message("\n\nCREATING DATA FOR REPORT: '", config$metadata$reportname, "'.")
        message("Config file location:\n", config$path)
      }

      # Fetch data if there is no cached file
      if (fetchDataFromCache == FALSE) {



        if (verbose) {
          message("Fetching data from SQL Server...", appendLF = FALSE)
        }
        baseData <- pmtools::pedal_query(sqlQuery, data.table = FALSE) %>% tbl_df()

        if (verbose) {
          message(" Done.")

          message("Attempting to convert numeric columns stored as character to proper numeric values...", appendLF = FALSE)
        }

        baseData <- convert_numerics_stored_as_char(baseData)

        if (verbose) {
          message(" Done.")
        }

        # Store data in cache if cache == TRUE
        if (cache == TRUE) {
          if (!dir.exists(cacheDirectory)) {
            if (verbose) {
              message("The specified cache directory doesn't exist. Creating it at location\n", cacheDirectory, "...", appendLF = FALSE)
            }
            dir.create(cacheDirectory, recursive = TRUE)
            if (verbose) {
              message(" Done.")
            }
          }

          if (verbose) {
            message("Saving data to cache...", appendLF = FALSE)
          }

          save(baseData, file = cacheFileName)

        }

      } else {
        # Fetch data from cache instead

        if (verbose) {
          message("Cached data found at location:\n", cacheFileName)
          message("Fetching data from cache...", appendLF = FALSE)
        }

        load(cacheFileName)


      }

      if (verbose) {

        message(" Done.")
        message("Attaching the following target path to config file:\n", str_replace(config$path, sourceReportDir, targetAppDir))
      }
      config$target <- str_replace(config$path, sourceReportDir, targetAppDir)

      ## TODO: Implement handling of "target" file structure.
      # Currently we don't do anything to verify or check that the target file structure exists,
      # and we also just write data (i.e. not the config file) to the "target" directory. This
      # means that for the preloading script to work we need to set targetAppDir == sourceReportDir.
      if (verbose) {
        message("Checking if there's a file structure in place at the target path...", appendLF = FALSE)
      }

      copy_all_files(from = sourceReportDir, to = targetAppDir)


      if (verbose) {
        message(" Done.")
        message("Running script to split and store data on disk.")
      }

      # copy the whole config
      fullConfig = config
      # remove diplayNames of config main parts
      config = remove_display_name_from_config(config)

      if(scope != "logs"){
        split_and_save_report_data(config, baseData, verbose)
      }

      save_all_lookups(config, verbose)


      if (verbose) {
        message("Finished data munging for report '", config$metadata$reportname, "'.")
        message("Writing update to log file ..")
      }

      # restore config
      config = fullConfig

      reportPath <- paste0(basename(dirname(config$path)),"/", basename(config$path))
      updated <- Sys.time()
      newLog  <- data.frame(reportPath, updated)
      write.table(newLog, paste0(targetAppDir, "/log.csv"), append = TRUE, sep = ";", col.names = FALSE, row.names = FALSE)

      if (verbose) {
        message("Done")
      }
    }
  }

  # Close log connection
  if (logToFile == TRUE) {
    sink()
    closeAllConnections()
  }
}



#' Convert all columns in a data frame that are convertible from character to numeric
#' @param data The data frame to be checked and modified accordingly
#'
#' @export
convert_numerics_stored_as_char <- function(data) {

  tempData <- data

  for (col in names(tempData)) {
     if (class(tempData[[ col ]]) == "character") {
       tempData[[col]] <- type.convert(tempData[[col]])
       if (is.factor(tempData[[col]]) && !grepl("UTF-8", Sys.getlocale(category = "LC_COLLATE"))) {
         levels(tempData[[col]]) <- enc2utf8(levels(tempData[[col]]))
       }
     }
     if (class(tempData[[ col ]]) == "integer"){
       tempData[[col]] <- as.numeric(tempData[[col]])
     }
  }

  return(tempData)
}


#Copy all files and directories (an the contents of the directories) in directory "from" inside the directory "to"
copy_all_files <- function(from, to){
  if (to != from){

    for(file in list.files(from, full.names = TRUE)){
      file.copy(file, paste(to, sep = ""), recursive=TRUE, overwrite = TRUE)
    }
  }
}


save_all_lookups <- function(reportConfig, verbose){
  for(rowName in names(reportConfig$rows)){

    row <- reportConfig$rows[[rowName]]
    if(!is.null(row$lookup)){

      query = paste("SELECT * FROM ",row$lookup)

      if(verbose){
        message(paste0("Calling: ", query))
      }

      lookupData <- pmtools::pedal_query(query, data.table = FALSE) %>% tbl_df()
      fileName <- paste0(reportConfig$path, "/", rowName, ".RData")
      save(lookupData, file = fileName)
    }
  }
}


