# Copyright 2026 European Medicines Agency
#
# This software is developed by the DARWIN EU Coordination Centre
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This study code contains unmodified open source dependencies.
#' Log and display a message with timestamp
#'
#' This function logs a message to a file and displays it in the console using `cli` alerts.
#' If a logger does not already exist in the global environment, it initializes one and creates
#' the necessary folders and log file. The log file is saved under a timestamped results directory.
#'
#' @param message A character string containing the message to log and display.
#' @param type A character string indicating the type of message to display. Options are:
#'   `"info"` (default), `"alert"`, `"success"`, `"warning"`, or `"danger"`.
#'
#' @details
#' This function depends on the following global variables:
#' - `dbName`: used to name the results and log folders.
#'
#' It also creates or uses the following global variables:
#' - `resultsFolder`: the path to the results directory.
#' - `logger`: a `log4r` logger object used to write messages to a log file.
#'
#' @return No return value. Used for side effects (logging and displaying messages).
#' @export
msgLog <- function(message, type = "info") {
  timestamp <- format(Sys.time(), "%Y%m%d")
  
  # Check if logger exists and is valid
  if (!exists("logger", envir = .GlobalEnv) || !inherits(get("logger", envir = .GlobalEnv), "logger")) {
    
    # Create results folder if not already defined
    if (!exists("resultsFolder", envir = .GlobalEnv)) {
      resultsFolder <- here::here(paste0("results/study-results/", dbName, "_", timestamp))
      assign("resultsFolder", resultsFolder, envir = .GlobalEnv)
    } else {
      resultsFolder <- get("resultsFolder", envir = .GlobalEnv)
    }
    
    # Create log folder
    logFolder <- file.path(resultsFolder, "log")
    if (!dir.exists(resultsFolder)) {
      cli::cli_alert("Creating results folder: {resultsFolder}")
      fs::dir_create(resultsFolder, recurse = TRUE)
    }
    if (!dir.exists(logFolder)) {
      cli::cli_alert("Creating log folder: {logFolder}")
      fs::dir_create(logFolder, recurse = TRUE)
    }
    
    # Create logger
    log_file <- file.path(logFolder, paste0("log_", dbName, "_", timestamp, ".txt"))
    logger <- log4r::create.logger()
    log4r::logfile(logger) <- log_file
    log4r::level(logger) <- "INFO"
    assign("logger", logger, envir = .GlobalEnv)
  } else {
    logger <- get("logger", envir = .GlobalEnv)
  }
  
  # Log and display message
  log4r::info(logger, glue::glue(message))
  full_message <- paste(message, Sys.time())
  
  switch(type,
         alert = cli::cli_alert(full_message),
         info = cli::cli_alert_info(full_message),
         success = cli::cli_alert_success(full_message),
         warning = cli::cli_alert_warning(full_message),
         danger = cli::cli_alert_danger(full_message),
         cli::cli_alert_info(full_message) # default
  )
}


setupResultsFolder <- function(resultsFolder,dbName){
  
  
  if(!dir.exists(resultsFolder)) {
    cli::cli_alert("Creating results folder: {resultsFolder}")
    fs::dir_create(resultsFolder, recurse = TRUE)
  }
  
  logFolder <- paste(resultsFolder,"log",sep = "/")
  
  timestamp <- format(Sys.time(), "%Y-%m-%d")
  log_file <- paste(logFolder, paste0("log_", dbName, "_", timestamp, ".txt"), sep = "/")
  
  if(!dir.exists(logFolder)) {
    cli::cli_alert("Creating log folder: {logFolder}")
    fs::dir_create(logFolder, recurse = TRUE)
  }
  
  logger <- log4r::create.logger()
  log4r::logfile(logger) <- log_file
  log4r::level(logger) <- "INFO"
  
  return(logger)
}



# Launch Shiny App -----
launchResultsExplorer <- function(dataFolder, useCachedData = TRUE) {
  if (missing(dataFolder) || is.null(dataFolder) || !nzchar(dataFolder)) {
    stop("`dataFolder` must be a non-empty path to your results directory.")
  }
  
  # Normalize inputs
  dataFolder <- normalizePath(dataFolder, winslash = "/", mustWork = TRUE)
  
  # Resolve the app directory in the source tree
  app_dir <- normalizePath(file.path("inst", "shiny", "StudyResultsExplorer"),
                           winslash = "/", mustWork = TRUE)
  
  # Ensure data/ exists under the app directory and set the path to data.rds
  data_dir <- file.path(app_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  data_rds <- file.path(data_dir, "data.rds")
  
  # Provide settings to the app (consumed by preprocessing.R)
  .GlobalEnv$shinySettings <- list(
    dataFolder    = dataFolder,
    useCachedData = isTRUE(useCachedData)
  )
  assign("APP_DIR",      app_dir,  envir = .GlobalEnv)
  assign("PKG_DATA_DIR", data_dir, envir = .GlobalEnv)   # where settings CSVs live & where data.rds is stored
  assign("DATA_RDS",     data_rds, envir = .GlobalEnv)   # exact data.rds path
  
  # Helpful diagnostics
  message("Launching StudyResultsExplorer with:")
  message("  app_dir:   ", app_dir)
  message("  data_dir:  ", data_dir)
  message("  data_rds:  ", data_rds)
  message("  dataFolder:", dataFolder)
  message("  useCached: ", isTRUE(useCachedData))
  
  invisible(
    shiny::runApp(app_dir)
  )
}



# Helper function to ensure cohort names are under 63 character limit for Postgresql ----
# Ensure codelist names are under 63 charatcer limit for Postgresql 
shortenNames <- function(x, max_len = 45) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- tolower(gsub("^_+|_+$", "", x))
  substr(x, 1, max_len)
}
