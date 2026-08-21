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
# Install Renv
install.packages("renv")
install.packages("CirceR")

# Activate renv, if not already activated.
renv::activate()

# Restore the packages.
renv::restore()

# Load Libraries
library(OmopSketch)
library(omopgenerics)
library(CDMConnector)
library(CodelistGenerator)
library(CohortConstructor)
library(DrugUtilisation)
library(CohortCharacteristics)
library(PatientProfiles)
library(IncidencePrevalence)
library(odbc)
library(dplyr)
library(here)
library(log4r)
source("R/support/utils.R")



# [*] EDIT BELOW ==============================================================

# connection details for DatabaseConnector
options(sqlRenderTempEmulationSchema = "") # Needed for Snowflake rendering

dbName <- "" # Options: "CDW Bordeaux", "IQVIA DA Germany", "CPRD GOLD"
refresh_date <- # IMPORTANT:  Please specify the latest database refresh date (YYYY-MM-DD)
cdmSchema <- ""
writeSchema <- ""
tablePrefix <- "p20_"
minCellCount <- 5

con <- DBI::dbConnect(...)


cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema,
                                writeSchema = writeSchema, 
                                cdmName = dbName,
                                writePrefix = tablePrefix
)

# [*] RUN STUDY ===============================================================

source(here::here("R/RunAnalysis.R"))
runStudy(
    runCohortGeneration = TRUE,
    runObj1 = TRUE,
    runObj2 = TRUE,
    runObj3 = FALSE,
    runObj4 = FALSE,
    runObj1_missing_adr = FALSE
)

# [*] VIEW RESULTS ============================================================

# To view the shiny app run the following code
# Set path to the folder with the results

source("R/support/utils.R")
resultsFolder <- here::here("results")
launchResultsExplorer(dataFolder = resultsFolder, useCachedData = FALSE)
