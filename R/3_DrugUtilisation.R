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

#number of immunoglobulin prescriptions, treatment duration, and initial and cumulative dose of the index drug will be provided 
#per prespecified immunoglobulin brand as minimum,q25 median, q75, and maximum. pg 9/59
#follow up incident immunoglobulin users starts on the first immunoglobulin prescription record of prespecified immunoglobulin brands during study period. page 13/59
# The dose, duration of use, and number of immunoglobulin prescriptions will be calculated per prespecified immunoglobulin brand pg21/59

# ____________________________________________________________________________________________________________________________________________________________________

# Flagging Cohort --------
# ____________________________________________________________________________________________________________________________________________________________________
msgLog(glue::glue("Import ig_brands_codelist, obj3"))
ig_brands_codelist <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","immunoglobulin_brands"),type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(ig_brands_codelist) <- gsub("p4_c1_020_|_\\d+","",names(ig_brands_codelist))
# names(ig_brands_codelist)[which(names(ig_brands_codelist)=="octagam_immunoglobulins_and")] <- "octagam_immunoglobulins_5_and_10"

ingredient_concept_ids <- c(
  501343,
  535714,
  537647,
  543291,
  586491,
  702418,
  739418,
  746155,
  1253346,
  1536976,
  1718211,
  19013765,
  19031041,
  19117912,
  19122168,
  19135830,
  35197905,
  35197913,
  35197969,
  35198039,
  35198103,
  35198193,
  35198206,
  35603994,
  35604680,
  35605786,
  36851985,
  36858135,
  37003288,
  40798902,
  40798903,
  40798985,
  43013161,
  43013193,
  43531966,
  43532492
)

msgLog(glue::glue("Generating cohort: ig_init_obj3"))
cdm$ig_init_obj3 <- cdm$ig_init %>%
  PatientProfiles::addConceptIntersectDate(
    conceptSet = ig_brands_codelist,
    window = list(c(0,Inf)),
    indexDate = "cohort_start_date",
    order = "first",
    inObservation = TRUE,
    nameStyle = "{concept_name}_presc_date") %>%
  PatientProfiles::addCohortName() %>%
  dplyr::compute(name = "ig_init_obj3", temporary = FALSE)


# ____________________________________________________________________________________________________________________________________________________________________

# Maps for code  --------
# ____________________________________________________________________________________________________________________________________________________________________

msgLog(glue::glue("Creating populated_cohorts map"))
ig_brands_names <- names(ig_brands_codelist)
ig_prescription_col <- paste0(ig_brands_names,"_presc_date")

populated_cohorts <- cdm$ig_init_obj3 %>% dplyr::distinct(cohort_name) %>% dplyr::pull(cohort_name)
brand_names_map <- setNames(lapply(populated_cohorts, function(pattern) ig_brands_names[grepl(pattern, ig_brands_names)]), populated_cohorts)
prescription_col_map  <- setNames(lapply(populated_cohorts, function(pattern) ig_prescription_col[grepl(pattern, ig_prescription_col)]), populated_cohorts)

# dus_results <- list()

# ____________________________________________________________________________________________________________________________________________________________________

# DUS  --------
# ____________________________________________________________________________________________________________________________________________________________________

for (i in seq_along(populated_cohorts)) {
  cohort <- populated_cohorts[i]
  msgLog(glue::glue("Evaluating DUS for cohort {cohort}"))
  dus_result <- cdm$ig_init_obj3 %>%
    dplyr::filter(cohort_name  == !!populated_cohorts[i]) %>% 
    DrugUtilisation::summariseDrugUtilisation(
      gapEra = 30,
      estimates = c("q25", "median", "q75", "min", "max"),
      conceptSet = ig_brands_codelist[paste(brand_names_map[i])],
      ingredientConceptId = ingredient_concept_ids,
      restrictIncident = TRUE,
      numberExposures = TRUE,
      numberEras = FALSE,
      daysExposed = TRUE,
      daysPrescribed = FALSE,
      timeToExposure = FALSE,
      initialExposureDuration = FALSE,
      initialQuantity = FALSE,
      cumulativeQuantity = FALSE,
      initialDailyDose = TRUE,
      cumulativeDose = TRUE,
      indexDate = paste(prescription_col_map[i])) %>% 
    omopgenerics::suppress(minCellCount = minCellCount)
  
  msgLog(glue::glue("Exporting DUS results {cohort}"))
  DrugUtilisation::exportSummarisedResult(dus_result, fileName = here::here(resultsFolder, glue::glue("dus_obj3_{cohort}_{dbName}_{export_date}.csv")), minCellCount = minCellCount)
  }


