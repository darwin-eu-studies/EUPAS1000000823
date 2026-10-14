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

library(dplyr)
library(IncidencePrevalence)
library(omopgenerics)
library(rlang)
library(purrr)

loadDatabaseResults <- function(dataFolder,dbName){
  
  addCDMLabel <- function(df){
    db_name <- case_when(
      grepl("NAJS",dbName,ignore.case = T) ~ "NAJS", 
      grepl("DK",dbName,ignore.case = T) ~ "DK-DHR",
      grepl("CPRD",dbName,ignore.case = T) ~ "CPRD GOLD",
      grepl("TaUH",dbName,ignore.case = T) ~ "FinOMOP-TaUH Pirha", 
      grepl("THL",dbName,ignore.case = T) ~ "FinOMOP-THL", 
      grepl("Bordeaux",dbName,ignore.case = T) ~ "CDW Bordeaux", 
      grepl("INGEF",dbName,ignore.case = T) ~ "InGef RDB", 
      grepl("SUCD",dbName,ignore.case = T) ~ "SUCD", 
      grepl("IPCI",dbName,ignore.case = T) ~ "IPCI", 
      grepl("NLHR",dbName,ignore.case = T) ~ "NLHR", 
      grepl("BIFAP",dbName,ignore.case = T) ~ "BIFAP", 
      grepl("SIDIAP",dbName,ignore.case = T) ~ "SIDIAP",
      grepl("(IQVIA-DA|IQVIA Germany)",dbName,ignore.case = T) ~ "IQVIA DA Germany",
      TRUE ~ dbName
    )
    
    df <- df %>% mutate(cdm_name=db_name)
    return(df)
  }
  
  labelDenominator <- function(df){
    # df <- df %>% mutate(group_level = stringr::str_replace(group_level, "(?<=_)\\d+_", ""))
    set_df <- settings(df) %>% select(result_id,denominator_age_group,denominator_sex)
    df <- df %>% left_join(set_df,by="result_id")
    df <- df %>% mutate(group_level = gsub("(.*)(&&&)","",group_level))
    df <- df %>% mutate(group_level = glue::glue("denominator_ageGroup:{denominator_age_group}_sex:{denominator_sex} &&&{group_level}")) %>% 
      select(-denominator_age_group,-denominator_sex)
    df <- df %>% mutate(group_level = gsub("denominator_cohort_\\d+ "," ",group_level))
    
    return(df)
  }
  
 
  files <- list.files(dataFolder,pattern = ".csv$",full.names = TRUE,recursive = TRUE)
  files <- grep("TreatmentPatterns/",files,invert = TRUE, value = TRUE)
  
  # Database Details ----------------------------------------------------------
  
  database_snapshot_file <- grep("cdm_snapshot_",files, value = TRUE)
  if(length(database_snapshot_file) > 0){
  snapshot <- omopgenerics::importSummarisedResult(database_snapshot_file) 
  snapshot <- snapshot %>% addCDMLabel()
  }else{
    snapshot <- NULL
  }
  
  # Load Attrition ----------------------------------------------------------
  
  
  if(dbName %in% c("IQVIA DA Germany","CDW Bordeaux", "CPRD GOLD")){
    # Update 20260223: Get attrition from incidencePrevalence summarised results:
    attrition_files <- grep("prev_(obj1|obj2)_overall|attrition_Ig_initiators",files, value = TRUE)
    settings_dir <- get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = dataOutputFolder)
    if(length(attrition_files) > 0){
      
      for(i in attrition_files){
        
        #name <- gsub("prev_(obj1|obj2)_overall|_","",basename(i))
        obj_name <- stringr::str_extract(string = i,pattern = "(O|o)bj.") %>% tolower()

        
        att_type <- case_when(
                obj_name == "obj1" ~ "denom_general_pop",
                obj_name == "obj2" ~ "denom_ig_users"
              )
        
        if(obj_name %in% c("obj1","obj2")){
        df <- IncidencePrevalence::importSummarisedResult(i) %>% IncidencePrevalence::tablePrevalenceAttrition(type = "tibble")
        colnames(df) <- omopgenerics::toSnakeCase(colnames(df))
        
        if(dbName == "CPRD GOLD old"){
          message("CPRD GOLD attrition 1,2 file - skipping")
          df <- NULL
          
        }else{
        df <- df %>% mutate(denominator_age_group = case_when(denominator_age_group == "0 to 120" ~ "overall", TRUE ~ denominator_age_group)) 
        
        df <- df %>%
          mutate(denominator_cohort_name = glue::glue("denominator_ageGroup:{denominator_age_group}_sex:{denominator_sex}")) %>% 
          mutate(objective_name = obj_name) %>% 
          dplyr::rename(
            "number_subjects" = header_name_variable_name_header_level_number_subjects,
            "number_records" = header_name_variable_name_header_level_number_records,
            "excluded_records" = header_name_variable_name_header_level_excluded_records,
            "excluded_subjects" = header_name_variable_name_header_level_excluded_subjects
            )
        
        cohort_name <- case_when(
          obj_name == "obj1" ~ "denom_general_pop",
          obj_name == "obj2" ~ "denom_ig_users",
        )
        
        df <- df %>% mutate("objective_name"= obj_name, cohort_name = cohort_name)
        }
        }else{
          cohort_settings_att3_4 <- readr::read_csv(file.path(settings_dir, "settings_ig_init.csv"))
          
          message("Loading: ",basename(i))
          
          name <- gsub("attrition_|_*.csv","",basename(i))
          
          df <- readr::read_csv(
            i, show_col_types = FALSE,
            col_types = readr::cols(.default = readr::col_character()))
          
          obj_name <- stringr::str_extract(string = i,pattern = "(O|o)bj.") %>% tolower()
          if(obj_name == "obj3"){
            obj_name <- "obj3_4" # Same cohort used for obj3 and 4
          
          
          
          message("name:", name)
          message("obj: ", obj_name)
         
          
          # Add objective name column
          if(!("objective_name" %in% colnames(df))){
          
            att_settings <- cohort_settings_att3_4
            
            att_type <- case_when(
              obj_name == "obj1" ~ "denom_general_pop",
              obj_name == "obj2" ~ "denom_ig_users",
              obj_name == "obj3_4" ~ "ig_initiators"
            )
            
            att_settings <- att_settings %>% dplyr::select(cohort_definition_id,cohort_name) %>% mutate(cohort_definition_id = as.character(cohort_definition_id)) %>% distinct()
            
            df <- df %>% mutate("objective_name"= obj_name)
            
            df <- df %>% inner_join(att_settings, by = "cohort_definition_id") %>% relocate(cohort_name, .after = "cohort_definition_id")
          }
          }
          
          colnames(df) <- colnames(df) %>% omopgenerics::toSnakeCase()
          
          
          df <- df %>% mutate(cdm_name = dbName) %>% relocate(cdm_name,.after = "cohort_definition_id")
          
        }

          att_result <- paste0("attrition_",obj_name,"_",att_type)
          message("att_result: ",att_result)
          assign(att_result,df, envir = environment())  
        
        
      }
    }
    }
  
  # else{
  # 
  # # attrition_df <- NULL
  # df <- NULL
  # attrition_files <- grep("attrition",files, value = TRUE)
  # attrition_files <- grep("TreatmentPatterns/|treatment_patterns_",attrition_files,invert = TRUE, value = TRUE)
  # 
  # # cohort_settings_att1 <- readr::read_csv(file = here::here("data/settings_ig_users_all_att.csv")) # ig_users_all_att
  # # cohort_settings_att2 <- readr::read_csv(file = here::here("data/settings_ig_brand_users_att.csv"))
  # # cohort_settings_att3_4 <- readr::read_csv(file = here::here("data/settings_ig_init.csv"))
  # 
  # 
  # settings_dir <- get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = dataOutputFolder)
  # cohort_settings_att1 <- readr::read_csv(file.path(settings_dir, "settings_ig_users_all_att.csv"))
  # cohort_settings_att2 <- readr::read_csv(file.path(settings_dir, "settings_ig_brand_users_att.csv"))
  # cohort_settings_att3_4 <- readr::read_csv(file.path(settings_dir, "settings_ig_init.csv"))
  # 
  # 
  # if(length(attrition_files) > 0 ){
  # for(i in attrition_files){
  #   message("Loading: ",basename(i))
  # 
  #   name <- gsub("attrition_|_*.csv","",basename(i))
  # 
  #   df <- readr::read_csv(
  #     i, show_col_types = FALSE,
  #     col_types = readr::cols(.default = readr::col_character()))
  # 
  #   obj_name <- stringr::str_extract(string = i,pattern = "(O|o)bj.") %>% tolower()
  #   if(obj_name == "obj3"){
  #     obj_name <- "obj3_4" # Same cohort used for obj3 and 4
  #   
  #   
  #   
  #   message("name:", name)
  #   message("obj: ", obj_name)
  # 
  #   # Add objective name column
  #   if(!("objective_name" %in% colnames(df))){
  # 
  # 
  #     att_settings <- case_when(
  #       obj_name == "obj1" & grepl("Ig_users_all",i) ~ cohort_settings_att1,
  #       obj_name == "obj1" & grepl("denom_general_pop_",i) ~ tibble::tibble(cohort_definition_id = 1, cohort_name = "denom_general_pop"),
  #       obj_name == "obj2" & grepl("Ig_brand_users",i) ~ cohort_settings_att2,
  #       obj_name == "obj2" & grepl("denom_ig_users",i) ~ tibble::tibble(cohort_definition_id = 1, cohort_name = "denom_ig_users"),
  #       obj_name == "obj3_4" ~ cohort_settings_att3_4
  #     )
  # 
  #     att_type <- case_when(
  #       obj_name == "obj1" & grepl("Ig_users_all",i) ~ "ig_users_all",
  #       obj_name == "obj1" & grepl("denom_general_pop_",i) ~ "denom_general_pop",
  #       obj_name == "obj2" & grepl("Ig_brand_users",i) ~ "ig_brand_users",
  #       obj_name == "obj2" & grepl("denom_ig_users",i) ~ "denom_ig_users",
  #       obj_name == "obj3_4" ~ "ig_initiators"
  #     )
  # 
  #     att_settings <- att_settings %>% dplyr::select(cohort_definition_id,cohort_name) %>% mutate(cohort_definition_id = as.character(cohort_definition_id)) %>% distinct()
  # 
  #     df <- df %>% mutate("objective_name"= obj_name)
  # 
  #     df <- df %>% inner_join(att_settings, by = "cohort_definition_id") %>% relocate(cohort_name, .after = "cohort_definition_id")
  #   }
  #   }else{
  #     cohort_name <- case_when(
  #       obj_name == "obj1" ~ "denom_general_pop",
  #       obj_name == "obj2" ~ "denom_ig_users",
  #       )
  #     df <- df %>% mutate("objective_name"= obj_name, cohort_name = cohort_name) %>% relocate(cohort_name, .after = "cohort_definition_id")
  #   }
  # 
  #   colnames(df) <- colnames(df) %>% omopgenerics::toSnakeCase()
  # 
  # 
  #     df <- df %>% mutate(cdm_name = dbName) %>% relocate(cdm_name,.after = "cohort_definition_id")
  # 
  #     # Add cohort name label for attrition table -----
  #     # if(obj_name %in% c("obj1","obj2")){
  #     #   df <- df %>% mutate(
  #     #     denominator_cohort_name = "denominator_ageGroup:0 to 120_sex:Both",
  #     #     denominator_sex = "Both",
  #     #     denominator_age_group = "0 to 120"
  #     #                       ) %>% 
  #     #     dplyr::rename(outcome_cohort_name = cohort_name)
  #     # }
  #     
  # 
  #   att_result <- paste0("attrition_",obj_name,"_",att_type)
  #   message("att_result: ",att_result)
  #   assign(att_result,df, envir = environment())
  # 
  # }
  # }else{
  # 
  #   #assign("attrition_obj1_ig_users_all",NULL, envir = environment())
  #   assign("attrition_obj1_denom_general_pop",NULL, envir = environment())
  #   #assign("attrition_obj2_ig_brand_users",NULL, envir = environment())
  #   assign("attrition_obj2_denom_ig_users",NULL, envir = environment())
  #   assign("attrition_obj3_4_ig_initiators",NULL, envir = environment())
  # }
  # }

# Import summarised results -----------------------------------------------

  # Import Prevalence Obj1 files:
  obj1_files <- grep("prev_obj1_overall",files,value = TRUE)
  obj1_files_adr <- grep("(prev_obj1_ig_admin_|prev_obj1_ig_missing_)",files,value = TRUE)
  obj1_files_ing <- grep("prev_obj1_ig_ingredient_",files,value = TRUE)
  
  
  
  if( length(grep("prev_obj1_",files,value = TRUE)) > 0){
    prevObj1 <- importSummarisedResult(obj1_files)
    prevObj1_adr <- importSummarisedResult(obj1_files_adr)
    prevObj1_ing <- importSummarisedResult(obj1_files_ing)
    
    # Map cohort names (this were trimmed to less than 63 characters to comply with Postgresql limit) ----
    route_of_admin_concept_set_names_map <- readr::read_csv(file.path(settings_dir, "route_of_admin_concept_set_names_map.csv"))
    ingredient_concept_set_names_map <- readr::read_csv(file.path(settings_dir,"ingredient_concept_set_names_map.csv"))

    if(dbName %in% c("IQVIA DA Germany","CDW Bordeaux", "CPRD GOLD")){
      # prevObj1_adr <- prevObj1_adr %>% 
      #   mutate(group_level = stringr::str_remove(group_level,"ig_adr_"))
      # 
      # prevObj1_ing <- prevObj1_ing %>% mutate(group_level = stringr::str_remove(group_level,"ig_ing_"))
      
      prevObj1_adr <- prevObj1_adr %>%
        mutate(cohort_name = stringr::str_remove(group_level,"denominator_cohort_.*&&& ")) %>%
        mutate(denominator_name = stringr::str_remove(group_level,"&&& .*")) %>%
        left_join(route_of_admin_concept_set_names_map, by=c("cohort_name"="safe_key")) %>%
        mutate(group_level = case_when(!is.na(original) ~ paste0(denominator_name,"&&& ",original), TRUE ~ group_level)) %>%
        select(-cohort_name, -denominator_name, -original)
      
      prevObj1_ing <- prevObj1_ing %>%
        mutate(cohort_name = stringr::str_remove(group_level,"denominator_cohort_.*&&& ")) %>%
        mutate(denominator_name = stringr::str_remove(group_level,"&&& .*")) %>%
        left_join(ingredient_concept_set_names_map, by=c("cohort_name"="safe_key")) %>%
        mutate(group_level = case_when(!is.na(original) ~ paste0(denominator_name,"&&& ",original), TRUE ~ group_level)) %>%
        select(-cohort_name, -denominator_name,-original)


    }else{
      message("Mapping cohort names")
      prevObj1_adr <- prevObj1_adr %>%
        mutate(cohort_name = stringr::str_remove(group_level,"denominator_cohort_.*&&& adr_")) %>%
        mutate(denominator_name = stringr::str_remove(group_level," adr_.*")) %>%
        left_join(route_of_admin_concept_set_names_map, by=c("cohort_name"="safe_key")) %>%
        mutate(group_level = case_when(!is.na(original) ~ paste0(denominator_name," ",original), TRUE ~ group_level)) %>%
        select(-cohort_name, -denominator_name, -original)

      prevObj1_ing <- prevObj1_ing %>%
        mutate(cohort_name = stringr::str_remove(group_level,"denominator_cohort_.*&&& ing_")) %>%
        mutate(denominator_name = stringr::str_remove(group_level," ing_.*")) %>%
        left_join(ingredient_concept_set_names_map, by=c("cohort_name"="safe_key")) %>%
        mutate(group_level = case_when(!is.na(original) ~ paste0(denominator_name," ",original), TRUE ~ group_level)) %>%
        select(-cohort_name, -denominator_name,-original)
    }
    
    # change denominator_age_group '0 to 120' to 'overall', '66 to 120' to '>65'
    sett_prevObj1 <- settings(prevObj1) %>% mutate(
      denominator_age_group = case_when(
        denominator_age_group %in% c("0 to 120","1 to 120") ~ "overall",
        denominator_age_group %in% c("66 to 120") ~ ">65",
        TRUE ~denominator_age_group
        )
      )
    
    sett_prevObj1_adr <- settings(prevObj1_adr) %>% mutate(
      denominator_age_group = case_when(
        denominator_age_group %in% c("0 to 120","1 to 120") ~ "overall",
        denominator_age_group %in% c("66 to 120") ~ ">65",
        TRUE ~denominator_age_group
      )
    )
    
    sett_prevObj1_ing <- settings(prevObj1_ing) %>% mutate(
      denominator_age_group = case_when(
        denominator_age_group %in% c("0 to 120","1 to 120") ~ "overall",
        denominator_age_group %in% c("66 to 120") ~ ">65",
        TRUE ~denominator_age_group
      )
    )
    
    prevObj1 <- prevObj1 %>% newSummarisedResult(settings = sett_prevObj1)
    prevObj1_adr <- prevObj1_adr %>% newSummarisedResult(settings = sett_prevObj1_adr)
    prevObj1_ing <- prevObj1_ing %>% newSummarisedResult(settings = sett_prevObj1_ing)

    
  }else{
    message("No obj1 files found.")
  }
  
  #TODO, NOTE: Because we defined time-varying age bands in the denominator, but the protocol required the prev across the general population (all ages), we don't need the results per denominator age band, so need to select denominator_age_group = c(1,120), denominator_sex = "Both to filter the results.
  
 # Import obj2 results -----
  obj2_files <- grep("prev_obj2_",files,value = TRUE)
  if(length(obj2_files) > 0){
  prevObj2 <- importSummarisedResult(obj2_files)
  
  
  sett_prevObj2 <- settings(prevObj2) %>% mutate(
    denominator_age_group = case_when(
      denominator_age_group %in% c("0 to 120","1 to 120") ~ "overall",
      denominator_age_group %in% c("66 to 120") ~ ">65",
      TRUE ~denominator_age_group
    )
  )
  
  # change denominator_age_group '0 to 120' to 'overall'
  prevObj2 <- prevObj2 %>% newSummarisedResult(settings = sett_prevObj2)
  
  }else{
    message("No obj2 files found.")
  }
  
  # Import obj3 results (DUS) ----
  obj3_files <- grep("dus_obj3_",files,value = TRUE)
  if(length(obj3_files) > 0){
  dusObj3 <- importSummarisedResult(obj3_files)
  }else{
    message("No obj3 files found.")
  }
  
  # Import obj4 results (Characterisation) ----
  
  charDemographics <- importSummarisedResult(grep("charDemographics",files,value = TRUE))
  char_indications_files <- grep("charIndications",files,value = TRUE)
  message("Found ", length(char_indications_files), " charIndications files:")
  print(char_indications_files)
  charIndications <- importSummarisedResult(grep("charIndications",files,value = TRUE))
  
  charComorbidities <- importSummarisedResult(grep("charComorbidities",files,value = TRUE))
  charInfection <- importSummarisedResult(grep("charInfection",files,value = TRUE))
  charAntibiotics <- importSummarisedResult(grep("charAntibiotics",files,value = TRUE))
  
  # c(0,30),c(0,365)
  charAntibiotics <- charAntibiotics %>% 
    mutate(time_window = case_when(
    grepl("(0_to_365)",variable_name) ~ "0 to 365",
    grepl("(0_to_30)",variable_name) ~ "0 to 30",
    TRUE ~ NA_character_)) %>% 
    mutate(variable_name = gsub("(_0_to_365|_0_to_30)","",variable_name)) %>% 
    as.data.frame()
    
  # c(-Inf,7),c(-30,7),c(-7,7)
  charIndications <- charIndications %>% 
    mutate(time_window = case_when(
      grepl("(minf_to_7)",variable_name) ~ "-Inf to 7",
      grepl("(m30_to_7)",variable_name) ~ "-30 to 7",
      grepl("(m7_to_7)",variable_name) ~ "-7 to 7",
      TRUE ~ NA_character_)) %>% 
    mutate(variable_name = gsub("(_minf_to_7|_m30_to_7|_m7_to_7)","",variable_name))
  
  charIndications_name_mapping <- readr::read_csv(
    file.path(
      get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = "data"),
      "charIndications_name_mapping.csv"
    ),
    show_col_types = FALSE
  )

  charIndications <- charIndications %>%  mutate(
      variable_name = case_when(
        variable_name == "i_primary_immunodeficiency_syndrome_indications" ~ "primary_immunodeficiency_syndrome_indications",
        variable_name == "i_secondary_immunodeficiencies_indications"      ~ "secondary_immunodeficiencies_indications",
        variable_name == "i_transplantation_indications"                    ~ "transplantation_indications",
        variable_name == "g_internal_medicine_indications"                  ~ "internal_medicine_indications",
        variable_name == "g_neurology_indications"                          ~ "neurology_indications",
        variable_name == "g_other_indications"                              ~ "other_indications",
        variable_name == "g_hepatology_indications"                         ~ "hepatology_indications",
        variable_name == "g_infectious_diseases_indications"                ~ "infectious_diseases_indications",
        variable_name == "g_haematology_indications"                        ~ "haematology_indications",
        variable_name == "iid_other_indications_only_epilepsy"              ~ "other_indications_only_epilepsy",
        variable_name == "iid_other_indications_only_oncology"              ~ "other_indications_only_oncology",
        variable_name == "iid_other_indications_only_covid"                 ~ "other_indications_only_covid",
        variable_name == "iid_other_indications_only_sjogren_s_syndrome"    ~ "other_indications_only_sjogren_s_syndrome",
        variable_name == "iid_other_indications_only_autoimmune_or_paraneop"~ "other_indications_only_autoimmune_or_paraneoplastic_encephalitis",
        variable_name == "iid_other_indications_only_alzheimer"             ~ "other_indications_only_alzheimer",
        variable_name == "iid_other_indications_only_progressive_systemic_s"~ "other_indications_only_progressive_systemic_sclerosis",
        TRUE                                                                ~ variable_name  # fallback: keep original if not listed
      )
    ) %>% as.data.frame()
  

  
  # list(c(0,30),c(0,365))
  charInfection <- charInfection %>% 
    mutate(time_window = case_when(
      grepl("(0_to_365)",variable_name) ~ "0 to 365",
      grepl("(0_to_30)",variable_name) ~ "0 to 30",
      TRUE ~ NA_character_)) %>% 
    mutate(variable_name = gsub("(_0_to_365|_0_to_30)","",variable_name)) %>% 
    as.data.frame()
   
  
   
# Append Non-summarised results (e.g. attrition) to results_db list
  # if(grepl("attrition|incTerbinafine",summarisedResultFiles)){
  results_db <- list()
  results_db$cdm_snapshot <- snapshot #%>% addCDMLabel()
  #results_db$attrition_obj1_ig_users_all <- attrition_obj1_ig_users_all #%>% addCDMLabel()
  results_db$attrition_obj1_denom_general_pop <- attrition_obj1_denom_general_pop #%>% addCDMLabel()
  #results_db$attrition_obj2_ig_brand_users <- attrition_obj2_ig_brand_users #%>% addCDMLabel()
  results_db$attrition_obj2_denom_ig_users <- attrition_obj2_denom_ig_users #%>% addCDMLabel()
  results_db$attrition_obj3_4_ig_initiators <- attrition_obj3_4_ig_initiators #%>% addCDMLabel()
  results_db$prevObj1 <- prevObj1 %>% labelDenominator() #%>% filter(grepl("denominator_ageGroup:overall",group_level))
  results_db$prevObj1_adr <- prevObj1_adr %>% labelDenominator() #%>% filter(grepl("denominator_ageGroup:overall",group_level))
  results_db$prevObj1_ing <- prevObj1_ing %>% labelDenominator() #%>% filter(grepl("denominator_ageGroup:overall",group_level))
  results_db$prevObj2 <- prevObj2 %>% labelDenominator() #%>% filter(grepl("denominator_ageGroup:overall",group_level))
  results_db$dusObj3 <- dusObj3
  results_db$charDemographics <- charDemographics
  results_db$charIndications <- charIndications
  results_db$charComorbidities <- charComorbidities
  results_db$charInfection <- charInfection
  results_db$charAntibiotics <- charAntibiotics
 
  
  results_db <- results_db %>% purrr::map(.,addCDMLabel)
  
  return(results_db)
  
 
}


#--------------------------------------------------------------------------------------------------------------------
# LOAD RESULTS FROM ALL DPs AND GENERATE data.rds (reload it if exists)
#--------------------------------------------------------------------------------------------------------------------#



if (!exists("shinySettings", envir = .GlobalEnv)) {
  # Standalone run: load from app data/raw (optional). Prefer launchResultsExplorer() for study package.
  dataOutputFolder <- get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = file.path(getwd(), "data"))
  dataFolder <- get0(
    "dataFolder",
    envir = .GlobalEnv,
    ifnotfound = file.path(dirname(dataOutputFolder), "data", "raw")
  )
  if (!dir.exists(dataFolder)) {
    dataFolder <- file.path(dataOutputFolder, "raw")
  }
  data_rds <- get0(
    "DATA_RDS",
    envir = .GlobalEnv,
    ifnotfound = file.path(dataOutputFolder, "data.rds")
  )
  message("Setting dataFolder to:", dataFolder)
  message("Setting dataOutputFolder to:", dataOutputFolder)
} else {
  dataFolder <- .GlobalEnv$shinySettings$dataFolder
  useCachedData <- .GlobalEnv$shinySettings$useCachedData
  dataOutputFolder <- get0(
    "PKG_DATA_DIR",
    envir = .GlobalEnv,
    ifnotfound = file.path(getwd(), "data")
  )
  data_rds <- get0(
    "DATA_RDS",
    envir = .GlobalEnv,
    ifnotfound = file.path(dataOutputFolder, "data.rds")
  )
  message("Setting dataFolder to:", dataFolder)
  message("Setting dataOutputFolder to:", dataOutputFolder)
}

# Force a rebuild by removing the old cache when useCachedData = FALSE
if (isFALSE(get0("shinySettings", .GlobalEnv, ifnotfound = list(useCachedData = TRUE))$useCachedData)) {
  if (file.exists(data_rds)) {
    message("useCachedData = FALSE - removing existing cache at: ", data_rds)
    unlink(data_rds)
  }
}



results_db <- list()

if(!file.exists(data_rds)){ #paste0(dataOutputFolder,"/data.rds")
  
  results_db <- list()
  
  for (dbName in list.files(dataFolder)) {
    databaseFolder <- file.path(dataFolder, dbName)
    if (!dir.exists(databaseFolder)) {
      next
    }
    cat(dbName)
    cli::cli_alert_info("Loading data from {dbName} to data.rds file")
    results_db_i <- loadDatabaseResults(dataFolder = databaseFolder, dbName)
    
    if(dbName %in% c("IQVIA DA Germany","CDW Bordeaux", "CPRD GOLD")){
    empty_df1 <- results_db_i$attrition_obj1_denom_general_pop %>%  slice(0)
    empty_df2 <- results_db_i$attrition_obj2_denom_ig_users %>%  slice(0)
    
    results_db_i$prevObj1 <- results_db_i$prevObj1 %>%
      filter(group_level == "denominator_ageGroup:overall_sex:Both &&& immunoglobulins") %>% 
      filter(strata_name == "overall", strata_level == "overall")
    
    # results_db_i$prevObj1_adr <- results_db_i$prevObj1_adr %>% 
    #   filter(cdm_name == "CPRD GOLD") #%>% 
    #   #filter(strata_name == "overall", strata_level == "overall")
    
    # results_db_i$prevObj1_ing <- results_db_i$prevObj1_ing %>% 
    #   filter(cdm_name == "CPRD GOLD") #%>% 
    # filter(grepl("denominator_ageGroup:overall_sex:Both",group_level )) %>% 
    #filter(strata_name == "overall", strata_level == "overall")
    
    }
    
    # if(dbName == "CPRD GOLD old"){
    #   results_db_i$attrition_obj1_denom_general_pop <- empty_df1
    #   results_db_i$attrition_obj2_denom_ig_users <- empty_df2
    #   results_db_i$prevObj1 <- results_db_i$prevObj1 %>% filter(cdm_name == "CPRD GOLD") %>% 
    #     filter(group_level == "denominator_ageGroup:overall_sex:Both &&& immunoglobulins") %>% 
    #     filter(strata_name == "overall", strata_level == "overall")
    #   
    #    results_db_i$prevObj1_adr <- results_db_i$prevObj1_adr %>% 
    #      filter(cdm_name == "CPRD GOLD") #%>% 
    #   #   #filter(strata_name == "overall", strata_level == "overall")
    #   
    #    results_db_i$prevObj1_ing <- results_db_i$prevObj1_ing %>% 
    #      filter(cdm_name == "CPRD GOLD") #%>% 
    #     # filter(grepl("denominator_ageGroup:overall_sex:Both",group_level )) %>% 
    #     #filter(strata_name == "overall", strata_level == "overall")
    # }
    
    tab_order <- c(
    "cdm_snapshot",
    #"attrition_obj1_ig_users_all",
    "attrition_obj1_denom_general_pop",
    #"attrition_obj2_ig_brand_users",
    "attrition_obj2_denom_ig_users",
    "attrition_obj3_4_ig_initiators",
    "prevObj1",
    "prevObj1_adr",
    "prevObj1_ing",
    "prevObj2",
    "dusObj3",
    "charDemographics",
    "charIndications",
    "charComorbidities",
    "charInfection",
    "charAntibiotics"
    )
    
    tab_order <- tab_order[match(names(results_db_i),tab_order)]
    
    if (length(results_db) == 0) {
      results_db <- results_db_i
    } else {
      results_db <- purrr::map2(results_db[tab_order], results_db_i[tab_order], function(x, y) {
        if (!is.null(x) && !is.null(y)) {
          if ("summarised_result" %in% class(x)) {
            omopgenerics::bind(x, y)
          } else {
            rbind(x, y)
          }
        } else {
          x %||% y # null coalescing (return x if not null, otherwise return y)
        }
      })
    }
  }
  
  
  cdm_snapshot <- results_db$cdm_snapshot
  #attrition_obj1_ig_users_all <- results_db$attrition_obj1_ig_users_all
  attrition_obj1_denom_general_pop <- results_db$attrition_obj1_denom_general_pop
  #attrition_obj2_ig_brand_users <- results_db$attrition_obj2_ig_brand_users
  attrition_obj2_denom_ig_users <- results_db$attrition_obj2_denom_ig_users
  attrition_obj3_4_ig_initiators <- results_db$attrition_obj3_4_ig_initiators
  prevObj1 <- results_db$prevObj1
  prevObj1_adr <- results_db$prevObj1_adr
  prevObj1_ing <- results_db$prevObj1_ing
  prevObj2 <- results_db$prevObj2
  dusObj3 <- results_db$dusObj3
  charDemographics <- results_db$charDemographics
  charIndications <- results_db$charIndications
  charComorbidities <- results_db$charComorbidities
  charInfection <- results_db$charInfection
  charAntibiotics <- results_db$charAntibiotics
  # 

  
  # Save data ----
  itemsToSave <- list(
  "cdm_snapshot" = cdm_snapshot,
  #"attrition_obj1_ig_users_all" = attrition_obj1_ig_users_all,
  "attrition_obj1_denom_general_pop" = attrition_obj1_denom_general_pop,
  #"attrition_obj2_ig_brand_users" = attrition_obj2_ig_brand_users,
  "attrition_obj2_denom_ig_users" = attrition_obj2_denom_ig_users,
  "attrition_obj3_4_ig_initiators" = attrition_obj3_4_ig_initiators,
  "prevObj1" = prevObj1,
  "prevObj1_adr" = prevObj1_adr,
  "prevObj1_ing" = prevObj1_ing,
  "prevObj2" = prevObj2,
  "dusObj3" = dusObj3,
  "charDemographics" = charDemographics,
  "charIndications" = charIndications,
  "charComorbidities" = charComorbidities,
  "charInfection" = charInfection,
  "charAntibiotics" = charAntibiotics
  )
  saveRDS(itemsToSave, data_rds) #paste0(dataOutputFolder,"/data.rds")
  
}else{
  
  #if(isTRUE(useCachedData)){
  message("Loading results from data.rds file")
  results <- readRDS(data_rds) #paste0(dataOutputFolder,"/data.rds")
  
  cdm_snapshot <- results$cdm_snapshot
  #attrition_obj1_ig_users_all <- results$attrition_obj1_ig_users_all
  attrition_obj1_denom_general_pop <- results$attrition_obj1_denom_general_pop
  #attrition_obj2_ig_brand_users <- results$attrition_obj2_ig_brand_users
  attrition_obj2_denom_ig_users <- results$attrition_obj2_denom_ig_users
  attrition_obj3_4_ig_initiators <- results$attrition_obj3_4_ig_initiators
  prevObj1 <- results$prevObj1
  prevObj1_adr <- results$prevObj1_adr
  prevObj1_ing <- results$prevObj1_ing
  prevObj2 <- results$prevObj2
  dusObj3 <- results$dusObj3
  charDemographics <- results$charDemographics
  charIndications <- results$charIndications
  charComorbidities <- results$charComorbidities
  charInfection <- results$charInfection
  charAntibiotics <- results$charAntibiotics
 
  rm(results)
  #}
}

