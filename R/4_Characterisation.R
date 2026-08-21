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
# ____________________________________________________________________________________________________________________________________________________________________

# Objectives --------
# ____________________________________________________________________________________________________________________________________________________________________

# To characterise patients initiating treatment of prespecified immunoglobulins in terms of 
# i) demographics at index date,
# ii) indication for use, 
# iii) comorbidities, 
# iv) common infections post-index
# v) antibiotics post-index.
# ____________________________________________________________________________________________________________________________________________________________________

# Demographics --------
# ____________________________________________________________________________________________________________________________________________________________________
# sex, age and Age group: 1 to 18 years, 19 to 65 years, >65 years and older at index pg16/60 protocol

msgLog("Characterising Demographics")

charDemographics <- cdm$ig_init %>%
  PatientProfiles::addCohortName() %>% 
  PatientProfiles::addDemographics(age = TRUE,
                                   ageGroup = list(c(1, 18),c(19, 65),c(66, 150)),
                                   sex = TRUE) %>%
  PatientProfiles::summariseResult(group = list("cohort_name")) %>%
  omopgenerics::suppress(minCellCount = minCellCount)

# ____________________________________________________________________________________________________________________________________________________________________

# Indications for Use --------
# ____________________________________________________________________________________________________________________________________________________________________
#prespecified indications for use will be assessed 
#any time prior to 7 days after the index date,
#30 days prior to 7 days after the index date, 
#and 7 days prior to 7 days after the index date
# Indication for use include the following prespecified indication groups 1) primary immunodeficiency syndrome, 2)
# secondary immunodeficiencies, 3) neurology, 4) haematology, 5) infectious diseases, 6) solid organ transplantation, 7)
# internal medicine, 8) hepatology, and 9) other indications.
#* = Prespecified indications of use classified as 
#* ‘Other indications’ will be assessed overall and separately for each indication, where available, as specified in section 8.6.3
#* page 17/59

json_folder_path <- here::here("inst","concept_sets","prespecified_indication_of_use")
indication_of_use_codelist <- omopgenerics::importConceptSetExpression(json_folder_path, type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(indication_of_use_codelist) <- gsub("p4_c1_020_|_\\d+","",names(indication_of_use_codelist))

json_folder_path <- here::here("inst","concept_sets","prespecified_grouped_indications")
grouped_indications_codelist <- omopgenerics::importConceptSetExpression(json_folder_path, type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(grouped_indications_codelist) <- gsub("p4_c1_020_|_\\d+","",names(grouped_indications_codelist))

json_folder_path <- here::here("inst","concept_sets","prespecified_individual_indications")
individual_indications_codelist <- omopgenerics::importConceptSetExpression(json_folder_path, type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(individual_indications_codelist) <- gsub("p4_c1_020_|_\\d+","",names(individual_indications_codelist))

# Ensure concept set names are under 63 characters limit (PostgreSQL) ----


shortenNames <- function(x, max_len = 45) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- tolower(gsub("^_+|_+$", "", x))
  substr(x, 1, max_len)
}

ensure_unique <- function(x, max_len = 45) {
  base <- shortenNames(x, max_len)
  # make.unique adds _1, _2, ... to duplicates (may exceed max_len)
  make.unique(base, sep = "_")
}

# Example (per-list only)
names(indication_of_use_codelist)   <- ensure_unique(names(indication_of_use_codelist))
names(grouped_indications_codelist) <- ensure_unique(names(grouped_indications_codelist))
names(individual_indications_codelist) <- ensure_unique(names(individual_indications_codelist))




# Characteristics will be described for each pre-specified immunoglobulin brand 
cdm$ig_init <- cdm$ig_init %>% PatientProfiles::addCohortName() %>% dplyr::compute(name = "ig_init", temporary = FALSE) # Add 'cohort_name' column (brand)

msgLog("Characterising Prespecified Indications")
charIndications <- cdm$ig_init %>%
  PatientProfiles::addConceptIntersectFlag(
    conceptSet = indication_of_use_codelist,
    censorDate = NULL,
    indexDate = "cohort_start_date",
    window = list(c(-Inf,7),c(-30,7),c(-7,7)),
    nameStyle = 'i_{concept_name}_{window_name}') %>%
  PatientProfiles::addConceptIntersectFlag(
    conceptSet = grouped_indications_codelist,
    censorDate = NULL,
    indexDate = "cohort_start_date",
    window = list(c(-Inf,7),c(-30,7),c(-7,7)),
    nameStyle = 'g_{concept_name}_{window_name}') %>%
  PatientProfiles::addConceptIntersectFlag(
    conceptSet = individual_indications_codelist,
    censorDate = NULL,
    indexDate = "cohort_start_date",
    window = list(c(-Inf,7),c(-30,7),c(-7,7)),
    nameStyle = 'iid_{concept_name}_{window_name}') %>%
  PatientProfiles::summariseResult(
    estimates = c("count", "percentage"),
    group = list("cohort_name") # Group by 'cohort_name' column (brand)
  ) %>%
  omopgenerics::suppress(minCellCount = minCellCount)

# ____________________________________________________________________________________________________________________________________________________________________

# Comorbidities --------
# ____________________________________________________________________________________________________________________________________________________________________
#Large-scale characterisation of comorbidities prior to immunoglobulin treatment will be assessed pg 9/60 and 18/60 
#within 30 days prior to the index date, 
#within 1 year prior to the index date, 
#and any time prior to the index date.

msgLog("Large Scale Characterisation")

charComorbidities <- cdm$ig_init %>%
  dplyr::rename(brand = cohort_name) %>% 
  CohortCharacteristics::summariseLargeScaleCharacteristics(
    window = list(c(-30,0), c(-365,0), c(-Inf,0)),
    eventInWindow = c("condition_occurrence"),
    strata = list("brand")
  ) %>%
  omopgenerics::suppress(minCellCount = minCellCount)

# ____________________________________________________________________________________________________________________________________________________________________

# Common Infections Post-Index --------
# ____________________________________________________________________________________________________________________________________________________________________
#Characterisation of prespecified common infections and prespecified antibiotics treatment among prespecified immunoglobulin brand initiators will be assessed up to 30 days post-index and up to 365 days post-index.
# page 18/59

json_folder_path <- here::here("inst","concept_sets","prespecified_infection_group")
infection_codelist <- omopgenerics::importConceptSetExpression(json_folder_path, type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(infection_codelist) <- gsub("p4_c1_020_|_\\d+","",names(infection_codelist))

msgLog("Characterising Prespecified Infections")
charInfection <- cdm$ig_init %>%
  PatientProfiles::addConceptIntersectFlag(
    conceptSet = infection_codelist,
    censorDate = NULL,
    indexDate = "cohort_start_date",
    window = list(c(0,30),c(0,365)),
    nameStyle = '{concept_name}_{window_name}') %>%
  PatientProfiles::summariseResult(
    estimates = c("count", "percentage"),
    group = list("cohort_name") # Group by 'cohort_name' column (brand)
  ) %>%
  omopgenerics::suppress(minCellCount = minCellCount)

# ____________________________________________________________________________________________________________________________________________________________________

# Antibiotics Post-Index --------
# ____________________________________________________________________________________________________________________________________________________________________
#Characterisation of prespecified common infections and prespecified antibiotics treatment among prespecified immunoglobulin brand initiators will be assessed up to 30 days post-index and up to 365 days post-index.

json_folder_path <- here::here("inst","concept_sets","prespecified_antibiotic_class")
antibiotics_codelist <- omopgenerics::importConceptSetExpression(json_folder_path, type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(antibiotics_codelist) <- gsub("p4_c1_020_|_\\d+","",names(antibiotics_codelist))

msgLog("Characterising Prespecified Antibiotics Treatment")
charAntibiotics <- cdm$ig_init %>%
  PatientProfiles::addConceptIntersectFlag(
    conceptSet = antibiotics_codelist,
    censorDate = NULL,
    indexDate = "cohort_start_date",
    window = list(c(0,30),c(0,365)),
    nameStyle = '{concept_name}_{window_name}') %>%
  PatientProfiles::summariseResult(
    estimates = c("count", "percentage"),
    group = list("cohort_name") # Group by 'cohort_name' column (brand)
  ) %>%
  omopgenerics::suppress(minCellCount = minCellCount)

# ____________________________________________________________________________________________________________________________________________________________________

# Export Results --------
# ____________________________________________________________________________________________________________________________________________________________________
msgLog("Exporting characterisation results")
IncidencePrevalence::exportSummarisedResult(charDemographics, fileName = paste0(resultsFolder, "/charDemographics_", dbName, ".csv"),minCellCount = minCellCount)
IncidencePrevalence::exportSummarisedResult(charIndications, fileName = paste0(resultsFolder, "/charIndications_", dbName, ".csv"),minCellCount = minCellCount)
IncidencePrevalence::exportSummarisedResult(charComorbidities, fileName = paste0(resultsFolder, "/charComorbidities_", dbName, ".csv"),minCellCount = minCellCount)
IncidencePrevalence::exportSummarisedResult(charInfection, fileName = paste0(resultsFolder, "/charInfection_", dbName, ".csv"),minCellCount = minCellCount)
IncidencePrevalence::exportSummarisedResult(charAntibiotics, fileName = paste0(resultsFolder, "/charAntibiotics_", dbName, ".csv"),minCellCount = minCellCount)
