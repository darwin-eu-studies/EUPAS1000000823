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

library(DarwinShinyModules)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(here)
library(DT)
library(reactable)
library(shinyWidgets)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(IncidencePrevalence)
library(ggplot2)
library(gt)
library(PatientProfiles)
library(lubridate)
library(future)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(zoo)
library(scales)
library(glue)
library(visOmopResults)
library(DrugUtilisation)
library(omopgenerics)
library(flextable)
library(openxlsx2)
library(flexlsx)
library(stringr)
library(CohortSurvival)
library(patchwork)
library(mapproj)
library(OmopSketch)

# Prefer paths set by launchResultsExplorer(); fall back to app cwd for standalone runs
app_dir <- get0("APP_DIR", envir = .GlobalEnv, ifnotfound = NULL)
if (is.null(app_dir) || !nzchar(app_dir)) {
  app_dir <- getwd()
}
data_dir <- get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = NULL)
if (is.null(data_dir) || !nzchar(data_dir)) {
  data_dir <- file.path(app_dir, "data")
}
data_rds <- get0("DATA_RDS", envir = .GlobalEnv, ifnotfound = NULL)
if (is.null(data_rds) || !nzchar(data_rds)) {
  data_rds <- file.path(data_dir, "data.rds")
}
if (exists("shinySettings", envir = .GlobalEnv)) {
  results_dir <- .GlobalEnv$shinySettings$dataFolder
} else {
  results_dir <- get0(
    "dataFolder",
    envir = .GlobalEnv,
    ifnotfound = file.path(app_dir, "data", "raw")
  )
}

# Make these visible to preprocessing.R
assign("current_path", app_dir, envir = .GlobalEnv)
assign("data_dir", data_dir, envir = .GlobalEnv)
assign("APP_DIR", app_dir, envir = .GlobalEnv)
assign("PKG_DATA_DIR", data_dir, envir = .GlobalEnv)
assign("DATA_RDS", data_rds, envir = .GlobalEnv)
assign("dataFolder", results_dir, envir = .GlobalEnv)

# Optional Calibri fonts (soft-fail if showtext/sysfonts missing from study library)
source(file.path(app_dir, "R", "darwin_fonts.R"))
register_darwin_fonts()
ggplot2::theme_set(darwin_fig_theme())

source(file.path(app_dir, "R", "utils.R"))
source(file.path(app_dir, "R", "preprocessing.R"))
sapply(
  grep(
    "preprocessing.R",
    list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE),
    value = TRUE,
    invert = TRUE
  ),
  source
)

###### Adapt outcome_cohort_name (cohort_name) for attrition module table structure:
attrition_obj1_denom_general_pop <- attrition_obj1_denom_general_pop %>% mutate(outcome_cohort_name = "denom_general_pop") %>% distinct()
attrition_obj2_denom_ig_users <- attrition_obj2_denom_ig_users %>% filter(outcome_cohort_name %in% c("denom_ig_users","hyqvia_immunoglobulins")) %>% mutate(outcome_cohort_name = "denom_ig_users") %>% distinct()
attrition_obj3_4_ig_initiators <- attrition_obj3_4_ig_initiators %>% dplyr::rename(outcome_cohort_name = cohort_name)
######
prevObj1_ing_new <- prevObj1_ing %>% 
  excludeRouteOfAdminFromIngredients() %>% 
  mutate(
    group_level = stringr::str_replace(
      group_level,
      stringr::fixed("immunoglobulin_anti_tickborne_encephalitis_other"),
      "tickborne encephalitis IG (other)"
    )
  ) %>% 
  mutate(
    group_level = stringr::str_replace(
      group_level,
      stringr::fixed("immunoglobulin_anti_tickborne_encephalitis"),
      "tickborne encephalitis IG"
    )
  )


prevObj1_adr_new <- prevObj1_adr %>% labelMissingRouteOfAdmin()

prevObj2_new <- prevObj2 %>% 
  mutate(
  group_level = stringr::str_replace(
    group_level,
    stringr::regex("_immunoglobulin.*$"),
    ""
  )
)

prevMod1 <- PrevalenceCustom$new(data = prevObj1)
prevMod1_adr <- PrevalenceCustom$new(data = prevObj1_adr_new)
prevMod1_ing <- PrevalenceCustom$new(data = prevObj1_ing_new)
prevMod2 <- PrevalenceCustom$new(data = prevObj2_new %>% filter(grepl("denominator_ageGroup:overall_sex:Both", group_level)))


# Remove Privigen results for IQVIA DA Germany due to incorrect vocabulary mapping ---
dusObj3 <- dusObj3 %>% filter( !(.data$cdm_name == "IQVIA DA Germany" & .data$group_level == "privigen_immunoglobulins"))



# labelling of attrition tables should align with report (cohort attrition) ---

attrition_obj1_denom_general_pop <- attrition_obj1_denom_general_pop %>%
  mutate(
    reason = case_when(
      reason == "Starting population" ~ "Starting population",
      reason == "Missing year of birth" ~ "Year of birth present",
      reason == "Missing sex" ~ "Sex present",
      reason == "Cannot satisfy age criteria during the study period based on year of birth" ~
        "Satisfies age criteria during the study period based on year of birth",
      reason == "No observation time available during study period" ~
        "Observation time available during study period",
      reason == "Doesn't satisfy age criteria during the study period" ~
        "Satisfies age criteria during the study period based on full birth date",
      reason == "Prior history requirement not fulfilled during study period" ~
        "Prior history requirement fulfilled during study period",
      reason == "No observation time available after applying age, prior observation and, if applicable, target criteria" ~
        "Observation time available after applying age, prior observation and, if applicable, target criteria",
      reason == "Follow-up censored at Death" ~ "Follow-up censored at Death",
      reason == "Follow up to latest available data" ~ "Follow up to latest available data",
      TRUE ~ reason
    )
  )

attrition_obj2_denom_ig_users <- attrition_obj2_denom_ig_users %>%
  filter(outcome_cohort_name %in% c("denom_ig_users", "hyqvia_immunoglobulins")) %>%
  # mutate(outcome_cohort_name = "denom_ig_users") %>%
  mutate(
    reason = case_when(
      reason == "Starting population" ~ "Starting population",
      reason == "Missing year of birth" ~ "Year of birth present",
      reason == "Missing sex" ~ "Sex present",
      reason == "Cannot satisfy age criteria during the study period based on year of birth" ~
        "Satisfies age criteria during the study period based on year of birth",
      reason == "No observation time available during study period" ~
        "Observation time available during study period",
      reason == "Doesn't satisfy age criteria during the study period" ~
        "Satisfies age criteria during the study period based on full birth date",
      reason == "Prior history requirement not fulfilled during study period" ~
        "Prior history requirement fulfilled during study period",
      reason == "No observation time available after applying age, prior observation and, if applicable, target criteria" ~
        "Observation time available after applying age, prior observation and, if applicable, target criteria",
      reason == "Follow-up until earliest of loss to follow-up, death, or study end" ~
        "Follow-up until earliest of loss to follow-up, death, or study end",
      TRUE ~ reason
    )
  ) 

databaseMenu <- shinydashboard::menuItem(text = "Data source details",tabName = "databaseDetails")
attritionMenu <- shinydashboard::menuItem(text = "Cohort Attrition",tabName = "cohortAttrition")
prevalenceObj1Menu <- shinydashboard::menuItem(text = "Obj1. Prevalence",tabName = "prevalence_obj1")
prevalenceObj2Menu <- shinydashboard::menuItem(text = "Obj2. Prevalence",tabName = "prevalence_obj2")
dusMenu <- shinydashboard::menuItem(text = "Obj.3 Drug Utilisation",tabName = "drugUtilisation")
characterisationMenu <- shinydashboard::menuItem(text = "Obj. 4 Characterisation",tabName = "characterisation")


ui <- shinydashboard::dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      databaseMenu,
      attritionMenu,
      prevalenceObj1Menu,
      prevalenceObj2Menu,
      dusMenu,
      characterisationMenu
      )
  ), dashboardBody(
    darwin_table_css(),
    tabItems(
      tabItem(tabName = "databaseDetails", databaseDetailsUI("database_details",cdm_snapshot)),
      tabItem(tabName = "cohortAttrition",
              tabsetPanel(id = "attrition_tabs", type = "tabs",
                          tabPanel("Attrition Obj1", attritionIncidencePrevalenceUI("attrition_obj1_denom",attrition_obj1_denom_general_pop)),
                          tabPanel("Attrition Obj2", attritionIncidencePrevalenceUI("attrition_obj2_denom",attrition_obj2_denom_ig_users)),
                          tabPanel("Attrition Obj3 and 4", attritionUI("attrition_obj3",attrition_obj3_4_ig_initiators)),
              )
      ),
      tabItem(
        tabName = "prevalence_obj1",
        tabsetPanel(id = "prevalence_tabs",
                    tabPanel("Overall", prevMod1$UI()),
                    tabPanel("Route of Administration", prevMod1_adr$UI()),
                    tabPanel("Immunoglobulin Ingredients",prevMod1_ing$UI())
                  )
               ),
      tabItem(tabName = "prevalence_obj2", prevMod2$UI()),
      
      
      tabItem( tabName = "characterisation",
               tabsetPanel(id ="characterisation_tabs",type = "tabs",
                           tabPanel("Demographics", charDemographicsUI("charDemographics",data = charDemographics)),
                           tabPanel("Indications", charSummariseResultUI("charIndications", data = charIndications)),
                           tabPanel("Comorbidities",charComorbiditiesUI("charComorbidities", data = charComorbidities)),
                           tabPanel("Antibiotics", charSummariseResultUI("charAntibiotics", data = charAntibiotics)),
                           tabPanel("Infections", charSummariseResultUI("charInfection", data = charInfection))
                           )
               ),
      
      tabItem(tabName = "drugUtilisation",charDUSUI("drugUtilisation", data = dusObj3))
               

)
)
)


server <- function(input, output, session) {
  databaseDetailsServer("database_details",cdm_snapshot)
  attritionIncidencePrevalenceServer("attrition_obj1_denom",attrition_obj1_denom_general_pop)
  attritionIncidencePrevalenceServer("attrition_obj2_denom",attrition_obj2_denom_ig_users)
  attritionServer("attrition_obj3",attrition_obj3_4_ig_initiators)
  prevMod1$server(input, output, session)
  prevMod1_adr$server(input, output, session)
  prevMod1_ing$server(input, output, session)
  prevMod2$server(input, output, session)
  charDUSServer("drugUtilisation", data = dusObj3)
  charDemographicsServer("charDemographics",data=charDemographics)
  charSummariseResultServer("charIndications",data=charIndications)
  charSummariseResultServer("charAntibiotics",data=charAntibiotics)
  charComorbiditiesServer("charComorbidities",data=charComorbidities)
  charSummariseResultServer("charInfection",data=charInfection)
  # 
  
}


shiny::shinyApp(ui = ui, server = server)

