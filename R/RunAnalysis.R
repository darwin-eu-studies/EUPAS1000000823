# Copyright 2025 European Medicines Agency
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
# Setting up folders ------------------------------------------------------
tag <- "R3_v0.2.1" #"R1_v0.1.0" # Name files with study run number (first run) and GitHub package version used.
resultsFolder <- here::here(glue::glue("results/{dbName}"))
logFolder <- glue::glue("{resultsFolder}/log")
logger <- setupResultsFolder(resultsFolder, dbName) # see utils.R
msgLog("P4-C1-020: DARWIN EU® - Assessment of immunoglobulin use in clinical practice", type = "header")
export_date <- gsub("-", "", Sys.Date())

# Check dbName ----
allowed <- c("CDW Bordeaux", "IQVIA DA Germany", "CPRD GOLD")

if (!exists("dbName", inherits = FALSE) || !is.character(dbName) || length(dbName) != 1L || is.na(dbName) ||
    !(dbName %in% allowed)) {
  cli::cli_abort("dbName - Please specify one of: {paste(allowed, collapse = ', ')}")
}

# Check refresh date ----
if(!exists("refresh_date")){
  cli::cli_abort("refresh_date is missing. Please specify the latest database refresh date (YYYY-MM-DD)")
}

# Run study ------------------------------------------------------

#  Save session info ------------------------------------------------------
msgLog("Saving session information:")
dbms <- CDMConnector::dbms(cdm)
session_file <- glue::glue("{logFolder}/session_info_{tag}_{dbName}_{export_date}.txt")
capture.output(sessionInfo(), file = session_file)
write(c("DBMS: ", dbms), file = session_file, append = TRUE)

# Save CDM snapshot --------------------------------------------------
msgLog("Saving CDM snapshot information:")
cdm_snapshot <- OmopSketch::summariseOmopSnapshot(cdm)
omopgenerics::exportSummarisedResult(cdm_snapshot, fileName = glue::glue("{resultsFolder}/cdm_snapshot_{tag}_{dbName}_{export_date}.csv"))
runStudy <- function(
    runCohortGeneration,
    runObj1_missing_adr,
    runObj1,
    runObj2,
    runObj3,
    runObj4
){
  
  # Assign parameters to env.
  list2env(list(runCohortGeneration = runCohortGeneration,runObj1_missing_adr = runObj1_missing_adr, runObj1 = runObj1, runObj2 = runObj2, runObj3 = runObj3, runObj4 = runObj4),envir = .GlobalEnv)

  if(runCohortGeneration){  
  # Generate cohorts --------------------------------------------------------
  msgLog("Starting 0_CohortGeneration.R ======", type = "subheader")
  source(here::here("R/0_CohortGeneration.R"))
  msgLog("Cohort Generation is Complete",type = "success")
  }
  
  if(isTRUE(runObj1)|isTRUE(runObj1_missing_adr)){
    # Obj.1 - Annual Prevalence of Immunoglobulin use --------
    tryCatch(
      {
        msgLog("Starting 1_Prevalence.R ======", type = "subheader")
        source(here::here("R/1_Prevalence.R"))
        msgLog("Obj. 1 is Complete",type = "success")
      },
      error = function(e) {
        writeLines(as.character(e), glue::glue("{logFolder}/error_prevalence_{tag}_{dbName}_{export_date}.txt"))
      }
    )
  }
  
  if(runObj2){
    # Obj.2 - Distribution and quarterly trends of prespecified immunoglobulin brands among prevalent immunoglobulin users --------------------------
    msgLog("Starting Obj. 2 ======", type = "subheader")
    source(here::here("R/2_PrevalenceIgBrands.R"))
    msgLog("Obj. 2 is Complete",type = "success")
  }
  
  if(runObj3){
    # Obj.3 - Estimate the number of immunoglobulin prescriptions, treatment duration, and dose  --------------------------
    msgLog("Starting Obj. 3 ======", type = "subheader")
    source(here::here("R/3_DrugUtilisation.R"))
    msgLog("Obj. 3 is Complete",type = "success")
  }
  
  if(runObj4){
  # Obj.4 - Characterise patients initiating treatment with prespecified immunoglobulins  --------------------------
  msgLog("Starting Obj. 4 ======", type = "subheader")
  source(here::here("R/4_Characterisation.R"))
  msgLog("Obj. 4 is Complete",type = "success")
  }
  # Export results ----------------------------------------------------------
  msgLog("Exporting ZIP file with results", type = "subheader")
  output_zip <- file.path(resultsFolder, glue::glue("study-results_{tag}_{dbName}_{export_date}.zip"))
  zip::zipr(zipfile = output_zip, files = list.files(resultsFolder, full.names = TRUE))
  msgLog(glue::glue("Results saved to: {resultsFolder} \n -- Thank you for running the study!"), type = "success")
}
