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
# 0_CohortGeneration.R
study_period_start <- as.Date("2017-01-01")
study_period_end <- as.Date("2024-12-31")
censor_date <- clock::add_days(as.Date(refresh_date),-180L) # To ensure sufficient follow-up (obj. 3-4), individuals with a first Ig record of at least 180 days before the end of data availability in each data source will be included.

# TARGET COHORTS ----------------------------------------------

#---------------------------------------------------
# Immunoglobulin users (Obj.1)
#---------------------------------------------------
if(isTRUE(runObj1)){
immunoglobulin_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","immunoglobulins","p4_c1_020_immunoglobulins_3767.json"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(immunoglobulin_cs) <- gsub("p4_c1_020_|_\\d+","",names(immunoglobulin_cs))

msgLog("Generating Ig users cohort")
cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = immunoglobulin_cs, # drugs + procedure concepts
  name = "ig_users_all",
  limit = "all",
  end = 0 #"event_end_date"
 )

msgLog("Follow-up until death, loss to follow-up or end of latest available data")
cdm[["ig_users_all"]] <- cdm[["ig_users_all"]] %>%
  PatientProfiles::addDeathDate() %>%
  dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
  dplyr::compute(name = "ig_users_all", temporary = FALSE, overwrite = TRUE) %>%
  CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
  dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
  dplyr::filter(cohort_start_date <= cohort_end_date) %>%
  dplyr::compute(name = "ig_users_all",temporary = FALSE) %>%
  CDMConnector::recordCohortAttrition("Follow up to latest available data")
}
#---------------------------------------------------
# Numerator cohort for Obj2 
#---------------------------------------------------
if(isTRUE(runObj2)){
# Generate a cohort for Ig users but limit it to the end of observation period (for Obj2) ---
msgLog("Generating numerator cohort ig_users_all_num_obj2 ")

cdm <- CDMConnector::generateConceptCohortSet(
  cdm = cdm,
  conceptSet = immunoglobulin_cs, # drugs + procedure concepts
  name = "ig_users_all_obj2",
  limit = "all", # all
  end = 0
)

msgLog("Follow-up until death, loss to follow-up or end of latest available data")
cdm[["ig_users_all_obj2"]] <- cdm[["ig_users_all_obj2"]] %>%
  PatientProfiles::addDeathDate() %>%
  dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
  dplyr::compute(name = "ig_users_all_obj2", temporary = FALSE, overwrite = TRUE) %>%
  CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
  dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
  dplyr::filter(cohort_start_date <= cohort_end_date) %>%
  dplyr::compute(name = "ig_users_all_obj2",temporary = FALSE) %>%
  CDMConnector::recordCohortAttrition("Follow up to latest available data")
} 

#---------------------------------------------------
# Obj1 Ingredient and Route of Administration cohorts
#------------------------------------------------------
if(isTRUE(runObj1)){
# Flag and split target cohorts by type of ingredient and route of administration for Obj1 stratification ----
ingredients_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","ingredients"),type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(ingredients_cs) <- gsub("p4_c1_020_|_\\d+","",names(ingredients_cs))

# Use short cohort names to avoid PostgreSQL issue: https://github.com/darwin-eu-studies/P4-C1-020/issues/1
ing_map <- readr::read_csv(here::here("R/support/ingredient_concept_set_names_map.csv"),show_col_types = F)
ig_ingredients_current <- tibble::tibble(current = names(ingredients_cs))
safe_names_ing <- inner_join(ig_ingredients_current, ing_map, by=c("current"="original"))
names(ingredients_cs) <- safe_names_ing$safe_key

ig_ingredients <- names(ingredients_cs)

for(ingredient in ig_ingredients){
  msgLog("Generating type of ingredient cohort: {ingredient}")
  cohort_name <- paste0("ing_",ingredient)
  
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = ingredients_cs[ingredient], 
    name = cohort_name,
    limit = "all",
    end = 0 #"event_end_date"
  )
  
  msgLog("Follow-up until death, loss to follow-up or end of latest available data")
  cdm[[cohort_name]] <- cdm[[cohort_name]] %>%
    PatientProfiles::addDeathDate() %>%
    dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
    dplyr::compute(name = cohort_name, temporary = FALSE, overwrite = TRUE) %>%
    CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
    dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
    dplyr::filter(cohort_start_date <= cohort_end_date) %>%
    dplyr::compute(name = cohort_name,temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition("Follow up to latest available data")
  
}

admin_route_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","route_of_administration"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(admin_route_cs) <- gsub("p4_c1_020_|_\\d+","",names(admin_route_cs))

# Use short cohort names to avoid PostgreSQL issue: https://github.com/darwin-eu-studies/P4-C1-020/issues/1
adr_map <- readr::read_csv(here::here("R/support/route_of_admin_concept_set_names_map.csv"),show_col_types = F)
ig_adr_current <- tibble::tibble(current = names(admin_route_cs))
safe_names_adr <- inner_join(ig_adr_current, adr_map, by=c("current"="original"))
names(admin_route_cs) <- safe_names_adr$safe_key

ig_admin_routes <- names(admin_route_cs)

for(route in ig_admin_routes){
  msgLog("Generating route of administration cohort: {route}")
  cohort_name <- paste0("adr_",route)
  
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = admin_route_cs[route], 
    name = cohort_name,
    limit = "all",
    end = 0 #"event_end_date"
  )
  
  msgLog("Follow-up until death, loss to follow-up or end of latest available data")
  cdm[[cohort_name]] <- cdm[[cohort_name]] %>%
    PatientProfiles::addDeathDate() %>%
    dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
    dplyr::compute(name = cohort_name, temporary = FALSE, overwrite = TRUE) %>%
    CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
    dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
    dplyr::filter(cohort_start_date <= cohort_end_date) %>%
    dplyr::compute(name = cohort_name,temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition("Follow up to latest available data")
 
}

#_______________________________________________________________________
# Generate 'missing' route of administration cohort:
#_______________________________________________________________________
missing_admin_route_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","missing_route_of_administration"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(missing_admin_route_cs) <- gsub("p4_c1_020_|_\\d+","",names(missing_admin_route_cs))

# Check concept set names are compliant with 63 char limit for PostgreSQL.
names(missing_admin_route_cs) <- gsub("_$","",make.unique(shortenNames(names(missing_admin_route_cs))))
missing_admin_routes <- names(missing_admin_route_cs)

for(missing_route in missing_admin_routes){
  msgLog("Generating missing route of administration cohort: {missing_route}")
  cohort_name <- paste0("adr_",missing_route)
  
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = missing_admin_route_cs[missing_route], 
    name = cohort_name,
    limit = "all",
    end = 0 #"event_end_date"
  )
  
  msgLog("Follow-up until death, loss to follow-up or end of latest available data")
  cdm[[cohort_name]] <- cdm[[cohort_name]] %>%
    PatientProfiles::addDeathDate() %>%
    dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
    dplyr::compute(name = cohort_name, temporary = FALSE, overwrite = TRUE) %>%
    CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
    dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
    dplyr::filter(cohort_start_date <= cohort_end_date) %>%
    dplyr::compute(name = cohort_name,temporary = FALSE) %>%
    CDMConnector::recordCohortAttrition("Follow up to latest available data")
  
}
}

# Obj1 re-run: Incorporate missing route of administration per ingredient: ----

if(isTRUE(runObj1_missing_adr)){
  #_______________________________________________________________________
  # Generate 'missing' route of administration cohort:
  #_______________________________________________________________________
  missing_admin_route_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","missing_route_of_administration"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
  names(missing_admin_route_cs) <- gsub("p4_c1_020_|_\\d+","",names(missing_admin_route_cs))
  
  # Check concept set names are compliant with 63 char limit for PostgreSQL.
  names(missing_admin_route_cs) <- gsub("_$","",make.unique(shortenNames(names(missing_admin_route_cs))))
  missing_admin_routes <- names(missing_admin_route_cs)
  
  for(missing_route in missing_admin_routes){
    msgLog("Generating missing route of administration cohort: {missing_route}")
    cohort_name <- paste0("adr_",missing_route)
    
    cdm <- CDMConnector::generateConceptCohortSet(
      cdm = cdm,
      conceptSet = missing_admin_route_cs[missing_route], 
      name = cohort_name,
      limit = "all",
      end = 0 #"event_end_date"
    )
    
    msgLog("Follow-up until death, loss to follow-up or end of latest available data")
    cdm[[cohort_name]] <- cdm[[cohort_name]] %>%
      PatientProfiles::addDeathDate() %>%
      dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
      dplyr::compute(name = cohort_name, temporary = FALSE, overwrite = TRUE) %>%
      CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
      dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
      dplyr::filter(cohort_start_date <= cohort_end_date) %>%
      dplyr::compute(name = cohort_name,temporary = FALSE) %>%
      CDMConnector::recordCohortAttrition("Follow up to latest available data")
    
  }
}

#_______________________________________________________________________
# Generate denominator cohort for Obj1 prevalence of immunoglobulin use in the
# general population -----
#_______________________________________________________________________
if(isTRUE(runObj1)|isTRUE(runObj1_missing_adr)){
msgLog(glue::glue("Generating Denominator Cohort - Ig users"))

cdm <- generateDenominatorCohortSet(
  cdm,
  name = "denom_general_pop",
  cohortDateRange = as.Date(c(study_period_start, study_period_end)),
  ageGroup = list(
    c(0, 120), # Append overall age group
    c(1,18),c(19,65),c(66,120)
  ), 
  sex = c("Female","Male","Both"),
  daysPriorObservation = 0,
  requirementInteractions = TRUE
)

msgLog("Follow-up until death, loss to follow-up or end of latest available data")
cdm[["denom_general_pop"]] <- cdm[["denom_general_pop"]] %>%
  PatientProfiles::addDeathDate() %>%
  dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
  dplyr::compute(name = "denom_general_pop", temporary = FALSE, overwrite = TRUE) %>%
  CDMConnector::recordCohortAttrition("Follow-up censored at Death") %>%
  dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
  dplyr::filter(cohort_start_date <= cohort_end_date) %>%
  dplyr::compute(name = "denom_general_pop",temporary = FALSE) %>%
  CDMConnector::recordCohortAttrition("Follow up to latest available data")

msgLog("Export attrition: denom_general_pop")
attrition_denom_gen_pop <- CDMConnector::attrition(cdm[["denom_general_pop"]])
file_attrition <- here::here(resultsFolder, glue::glue("attrition_denom_general_pop_obj1_{dbName}_{export_date}.csv"))
readr::write_csv(attrition_denom_gen_pop, file_attrition)
}

#__________________________________
# Immunoglobulin users with a prespecified immunoglobulin brand prescription.
# (Obj.2) -----
# Follow up will start on the latest of i) study start date (1st of January
# 2017) or ii) date of immunoglobulin prescription during the study period
# Notes: 
#       Follow-up is handled in the denominator cohort, see below.
#       No gap specified in protocol for obj2, records won't be collapsed
#__________________________________
if(isTRUE(runObj2)){
immunoglobulin_brands_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","immunoglobulin_brands"), type = "json")  %>% CodelistGenerator::asCodelist(cdm)
names(immunoglobulin_brands_cs) <- gsub("p4_c1_020_|_\\d+","",names(immunoglobulin_brands_cs))

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet( 
  cdm = cdm,
  conceptSet = immunoglobulin_brands_cs, # Ig brands
  name = "ig_brand_users",
  gapEra = 0 # 
)

#__________________________________
# Denominator cohort: Prevalent Immunoglobulin users (Obj.2) -----
# Prevalence of individuals with a prespecified immunoglobulin brands prescription among prevalent immunoglobulin users 
#__________________________________

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm = cdm,
  name = "denom_ig_users",
  targetCohortTable = "ig_users_all_obj2", # prevalent Ig users (changed from limit ="first", end = "observation_period_end_date" to limit = "all", end = 0)
  targetCohortId = NULL,
  cohortDateRange = as.Date(c(study_period_start, study_period_end)),
  ageGroup = list(
    c(0, 120), # Append overall age group
    c(1,18),c(19,65),c(66,120)
  ), 
  sex = c("Female","Male","Both"),
  daysPriorObservation = 0,
  requirementsAtEntry = FALSE, # TRUE 
  requirementInteractions = TRUE
)

msgLog("Follow-up: earliest of loss to follow-up, death, or study end")
cdm[["denom_ig_users"]] <- cdm[["denom_ig_users"]] %>%
  PatientProfiles::addDeathDate() %>%
  dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
  dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
  dplyr::filter(cohort_start_date <= cohort_end_date) %>%
  dplyr::compute(name = "denom_ig_users",temporary = FALSE) %>%
  CDMConnector::recordCohortAttrition("Follow-up until earliest of loss to follow-up, death, or study end")

msgLog("Export attrition: denom_ig_users")
attrition_denom_ig_users <- CDMConnector::attrition(cdm[["denom_ig_users"]])
file_attrition <- here::here(resultsFolder, glue::glue("attrition_denom_ig_users_obj2_{dbName}_{export_date}.csv"))
readr::write_csv(attrition_denom_ig_users, file_attrition)
}
#____________________________________________________
# Immunoglobulin users initiating treatment (Obj.3 and 4) ----
#____________________________________________________
# Obj3.	Initial and cumulative dose and number of prescriptions, and treatment
# duration of immunoglobulin users.
# Obj4. Characterisation of Ig initiators. 
# No Ig record 180 days prior study inclusion.
# At least 180 days prior obs. 
# To ensure sufficient follow-up (objectives 3-4), only individuals who with a first
# record of initiated immunoglobulin treatment at least 180 days before the end
# of data availability in each data source will be included. 
# For hospital care data settings (CDW Bordeaux), individuals are not required to have prior data
# availability and no washout
if(isTRUE(runObj3) |isTRUE(runObj4)){
msgLog("Generating cohort: Immunoglobulin initiators")
cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
  cdm = cdm,
  conceptSet = immunoglobulin_brands_cs,
  name = "ig_init",
  gapEra = 30
)

msgLog("Applying inclusion criteria: ")
if(dbName %in% c("IQVIA DA Germany","CPRD GOLD")){

  cdm[["ig_init"]] <- cdm[["ig_init"]] %>% 
  DrugUtilisation::requirePriorDrugWashout(days = 180) %>% # No Ig record 180 days prior study inclusion 
  DrugUtilisation::requireObservationBeforeDrug(days = 180) %>% # At least 180 days prior obs.
  DrugUtilisation::requireDrugInDateRange(dateRange = as.Date(c(study_period_start,censor_date))) %>% # Ensure sufficient 180 days follow-up
  DrugUtilisation::requireIsFirstDrugEntry()
}else{
  
  #For hospital care data settings (CDW Bordeaux), individuals are not required
  #to have prior data availability and no washout period will be applied prior
  #to study inclusion.
  cdm[["ig_init"]] <- cdm[["ig_init"]] %>% 
    DrugUtilisation::requireDrugInDateRange(dateRange = as.Date(c(study_period_start,censor_date))) %>%  # Ensure sufficient 180 days follow-up
    DrugUtilisation::requireIsFirstDrugEntry()
}

msgLog("Cohort start within observation period")
cdm[["ig_init"]] <- cdm[["ig_init"]] %>%
  dplyr::inner_join(cdm$observation_period, by =c("subject_id"="person_id")) %>%
  dplyr::filter(cohort_start_date >= observation_period_start_date, cohort_start_date <= observation_period_end_date) %>%
  compute(name = "ig_init", temporary = FALSE) %>%
  CDMConnector::recordCohortAttrition("Cohort start within observation period")

msgLog("Follow-up: earliest of loss to follow-up, death, or study end")
cdm[["ig_init"]] <- cdm[["ig_init"]] %>%
  PatientProfiles::addDeathDate() %>%
  dplyr::mutate(cohort_end_date = if_else(!is.na(date_of_death) & (date_of_death < cohort_end_date), date_of_death, cohort_end_date)) %>% 
  dplyr::mutate(cohort_end_date = if_else(study_period_end < cohort_end_date, study_period_end, cohort_end_date)) %>%
  dplyr::filter(cohort_start_date <= cohort_end_date) %>%
  dplyr::compute(name = "ig_init",temporary = FALSE)

# Attrition Immunoglobulin initiators Obj.3 and 4 ------
attrition_ig_init_obj3_4 <- CDMConnector::attrition(cdm[["ig_init"]])
file_attrition <- here::here(resultsFolder, glue::glue("attrition_Ig_initiators_obj3_4_{dbName}_{export_date}.csv"))
readr::write_csv(attrition_ig_init_obj3_4, file_attrition)
}

