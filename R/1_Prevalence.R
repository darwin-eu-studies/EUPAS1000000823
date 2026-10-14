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
# Obj1. Prevalence of individuals with a recorded immunoglobulin prescription
# among the general population, including type of immunoglobulin, route of
# administration ----

# Helper function 'estimatePrevalence' ---
estimatePrevalence <- function(obj_name,denominator_table_name,strata,analysis,type, target_id, outcome_table, interval) {
  msgLog(glue::glue("{obj_name}: Estimating Annual Prevalence - Analysis: {analysis} - {type}"))
  
  prev_result <- IncidencePrevalence::estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = outcome_table,
    outcomeCohortId = target_id,
    interval = interval,
    completeDatabaseIntervals = FALSE,
    level = "person",
    strata = strata,
    includeOverallStrata = TRUE
  ) %>% omopgenerics::suppress(minCellCount = minCellCount)
  
  msgLog("Check prevalence results are suppressed")
  omopgenerics::isResultSuppressed(prev_result, minCellCount = minCellCount)
  
  # Export prevalence results ----
  file_prev <- here::here(resultsFolder, glue::glue("prev_{obj_name}_{type}_{outcome_table}_{dbName}_{export_date}.csv"))
  msgLog(glue::glue("Exporting prevalence results - {obj_name} - {outcome_table}"))
  IncidencePrevalence::exportSummarisedResult(prev_result, fileName = file_prev, minCellCount = minCellCount)
  
  # Assign result to environment ----
  assign(glue::glue("prev_{obj_name}_{type}_{outcome_table}"), prev_result,envir = .GlobalEnv)
  
}

if(isTRUE(runObj1)|isTRUE(runObj1_missing_adr)){
# Obj.1 Prevalence ----
# Overall and stratified by:
# i) type of immunoglobulin, and
# ii) route of administration. 
# Estimates will be stratified by age group and sex
msgLog("Importing concept sets for Obj1")
ingredients_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","ingredients"),type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(ingredients_cs) <- gsub("p4_c1_020_|_\\d+","",names(ingredients_cs))

# Use short cohort names to avoid PostgreSQL issue: https://github.com/darwin-eu-studies/P4-C1-020/issues/1
ing_map <- readr::read_csv(here::here("R/support/ingredient_concept_set_names_map.csv"),show_col_types = F)
ig_ingredients_current <- tibble::tibble(current = names(ingredients_cs))
safe_names_ing <- inner_join(ig_ingredients_current, ing_map, by=c("current"="original"))
names(ingredients_cs) <- safe_names_ing$safe_key

ig_ingredients <- names(ingredients_cs)

admin_route_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","route_of_administration"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(admin_route_cs) <- gsub("p4_c1_020_|_\\d+","",names(admin_route_cs))

# Use short cohort names to avoid PostgreSQL issue: https://github.com/darwin-eu-studies/P4-C1-020/issues/1
adr_map <- readr::read_csv(here::here("R/support/route_of_admin_concept_set_names_map.csv"),show_col_types = F)
ig_adr_current <- tibble::tibble(current = names(admin_route_cs))
safe_names_adr <- inner_join(ig_adr_current, adr_map, by=c("current"="original"))
names(admin_route_cs) <- safe_names_adr$safe_key

ig_admin_routes <- names(admin_route_cs)

# missing route of administration ---
missing_admin_route_cs <- omopgenerics::importConceptSetExpression(path = here::here("inst","concept_sets","missing_route_of_administration"), type = "json") %>% CodelistGenerator::asCodelist(cdm)
names(missing_admin_route_cs) <- gsub("p4_c1_020_|_\\d+","",names(missing_admin_route_cs))

# Check concept set names are compliant with 63 char limit for PostgreSQL.
names(missing_admin_route_cs) <- gsub("_$","",make.unique(shortenNames(names(missing_admin_route_cs))))
missing_admin_routes <- names(missing_admin_route_cs)

# Cohort names for type of ingredient, route of administration and missing route of administration: ----
ig_ingredient_cohorts <- paste0("ing_",ig_ingredients)
ig_admin_route_cohorts <- paste0("adr_",ig_admin_routes)
ig_missing_admin_route_cohorts <- paste0("adr_",missing_admin_routes)



  if(isTRUE(runObj1)){
    obj1_cohorts <- c("ig_users_all",ig_ingredient_cohorts,ig_admin_route_cohorts,ig_missing_admin_route_cohorts)
  }

  if(isTRUE(runObj1_missing_adr)){
    obj1_cohorts <- ig_missing_admin_route_cohorts
    }

for(target_cohort in obj1_cohorts){
    
    # Parameters for IncidencePrevalence
    interval <- c("overall","years")
    denominator_name <- "denom_general_pop"
    strata <- list() # Stratification by sex and age group is included when generating the denominator #list("sex","age_group")
    
    # labels for file exporting
    obj_name <- "obj1"
    analysis <- "main"
    # Strata type label (Overall, Type of Immunoglobulin (Ingredient), Route of Administration)
    type <- case_when(
      target_cohort %in% "ig_users_all" ~ "overall",
      target_cohort %in% ig_admin_route_cohorts ~ "ig_admin_route",
      target_cohort %in% ig_ingredient_cohorts ~ "ig_ingredient",
      target_cohort %in% ig_missing_admin_route_cohorts ~ "ig_missing_admin_route",
    )
    
    msgLog(glue::glue("Evaluating prevalence - Strata: {type} \n Target Cohort {target_cohort} \n Denom.: {denominator_name}"))
    estimatePrevalence(obj_name = obj_name,
                       denominator_table_name = denominator_name,
                       strata = strata, 
                       analysis = analysis, 
                       type =  type, # (Overall, Type of Immunoglobulin (Ingredient), Route of Administration)
                       target_id = NULL,
                       interval = interval,
                       outcome_table = target_cohort
    )
  }

}




