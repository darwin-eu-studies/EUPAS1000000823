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

# ---------------- UI ----------------
characterisationUI <- function(id, data, data_indication,data_antifungal,data_comorbidities) {
  ns <- NS(id)
  style <- "display: inline-block;vertical-align:top; width: 200px;"
  
  fluidRow(
    tagList(
      div(style = style, pickerInput(ns("cdm_name"), "CDM name",
                                     choices = unique(c(data$cdm_name, data_indication$cdm_name,data_antifungal$cdm_name,data_comorbidities$cdm_name)),
                                     selected = unique(c(data$cdm_name, data_indication$cdm_name,data_antifungal$cdm_name,data_comorbidities$cdm_name)),
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
                                     multiple = TRUE
      )),
      div(style = style, pickerInput(ns("cohort_name"), "Cohort name",
                                     choices = unique(c(data$group_level, data_indication$group_level,data_antifungal$group_level,data_comorbidities$group_level)),
                                     selected = unique(c(data$group_level, data_indication$group_level,data_antifungal$group_level,data_comorbidities$group_level)),
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
                                     multiple = TRUE
      )),
      div(style = style, pickerInput(ns("strata_name"), "Strata name",
                                     choices = unique(c(data$strata_name, data_indication$strata_name,data_antifungal$strata_name,data_comorbidities$strata_name)),
                                     selected = unique(c(data$strata_name, data_indication$strata_name,data_antifungal$strata_name,data_comorbidities$strata_name)),
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
                                     multiple = TRUE
      )),
      div(style = style, pickerInput(ns("strata_level"), "Strata level",
                                     choices = unique(c(data$strata_level, data_indication$strata_level,data_antifungal$strata_level,data_comorbidities$strata_level)),
                                     selected = unique(c(data$strata_level, data_indication$strata_level,data_antifungal$strata_level,data_comorbidities$strata_level)),
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
                                     multiple = TRUE
      )),
      div(style = style, pickerInput(ns("variable_name"), "Variable name",
                                     choices = unique(c(data$variable_name, data_indication$variable_name,data_antifungal$variable_name,data_comorbidities$variable_name)),
                                     selected = unique(c(data$variable_name, data_indication$variable_name,data_antifungal$variable_name,data_comorbidities$variable_name)),
                                     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
                                     multiple = TRUE
      ))
    ),
    tabsetPanel(type = "tabs",
                # Demographics ----
                tabPanel("Demographics",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Raw", icon = icon("th"),
                                              downloadButton(ns("downloadTable"), "Download table", icon = icon("download")),
                                              reactableOutput(ns("table_demographics"))
                                     ),
                                     tabPanel("Tidy table", icon = icon("th"),
                                              downloadButton(ns("downloadTable_tidy_demographics"), "Download table", icon = icon("download")),
                                              gt_output(ns("table_demographics_tidy"))
                                     )
                         )
                ),
                # Comorbidities ----
                tabPanel("Comorbidities",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Raw", icon = icon("th"),
                                              downloadButton(ns("downloadTable_comorb"), "Download table", icon = icon("download")),
                                              reactableOutput(ns("table_comorbidities"))
                                     ),
                                     tabPanel("Tidy table", icon = icon("th"),
                                              downloadButton(ns("downloadTable_tidy_comorbidities"), "Download table", icon = icon("download")),
                                              gt_output(ns("table_comorbidities_tidy"))
                                     )
                         )
                ),
                
                #Indications -----
                tabPanel("Indications",
                         tabsetPanel(type = "tabs",
                                     tabPanel("Raw", icon = icon("th"),
                                              downloadButton(ns("downloadTable_ind"), "Download table", icon = icon("download")),
                                              reactableOutput(ns("table_ind"))
                                     ),
                                     tabPanel("Tidy table", icon = icon("th"),
                                              downloadButton(ns("downloadTable_tidy_indications"), "Download table", icon = icon("download")),
                                              gt_output(ns("table_ind_tidy"))
                                     )
                         )
                ),
                
                # Antifungal ab
                tabPanel(
                  "Antifungal antibiotics",
                  tabsetPanel(type = "tabs",
                              tabPanel("Raw", icon = icon("th"),
                                       downloadButton(ns("downloadTable_antifungal"), "Download table", icon = icon("download")),
                                       reactableOutput(ns("table_antifungal"))
                              ),
                              tabPanel("Tidy table", icon = icon("th"),
                                       downloadButton(ns("downloadTable_tidy_antifungal"), "Download table", icon = icon("download")),
                                       gt_output(ns("table_antifungal_tidy"))
                              )
                  )
                  
                )
    )
  )
}

# ---------------- SERVER ----------------
characterisationServer <- function(id, data, data_indication,data_antifungal,data_comorbidities) {
  moduleServer(id, function(input, output, session) {
    
    # Export flextables to excel helper ----
    exportMultipleFlextablesToExcel <- function(ft_list, file) {
      wb <- openxlsx2::wb_workbook()
      
      for (sheet_name in names(ft_list)) {
        ft <- ft_list[[sheet_name]]
        wb$add_worksheet(sheet_name)
        wb <- flexlsx::wb_add_flextable(wb, sheet = sheet_name, ft = ft, dims = "C2")
      }
      
      wb$save(file)
    }
    
    
    # Demographics ----
    get_charDemographicsTable_data <- reactive({
      req(input$cdm_name, input$cohort_name, input$strata_name, input$strata_level, input$variable_name)
      dplyr::filter(
        data,
        cdm_name    %in% input$cdm_name,
        group_level %in% input$cohort_name,
        strata_name %in% input$strata_name,
        strata_level%in% input$strata_level,
        variable_name %in% input$variable_name
      )
    })
    
    output$table_demographics <- renderReactable({
      summaryTable <- get_charDemographicsTable_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() paste0("charDemographics_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_charDemographicsTable_data(), file, row.names = FALSE)
    )
    
    output$table_demographics_tidy <- gt::render_gt({
      summaryTable <- get_charDemographicsTable_data()
      req(nrow(summaryTable) > 0)
      
      # Note: Because charDemographics was generated using PatientProfiles, it is needed to assign 
      #'summarise_characteristics' to the summarise results settings to generate table with CohortCharacteristics::tableCharacteristics:
      set_char <- settings(summaryTable)
      if("summarise_table" %in% set_char$result_type){
      set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
      summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
      }
      CohortCharacteristics::tableCharacteristics(summaryTable, type = "gt", header = "cdm_name") 
  
    })
    
    # Download tidy char demographics table ----
    output$downloadTable_tidy_demographics <-  downloadHandler(
      filename = function() {
        # paste("charDemographics_table_", Sys.Date(), ".csv", sep="")
        paste("charDemographics_tidy_table_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        summaryTable <- get_charDemographicsTable_data()
        # Note: Because charDemographics was generated using PatientProfiles, it is needed to assign 
        #'summarise_characteristics' to the summarise results settings to generate table with CohortCharacteristics::tableCharacteristics:
        set_char <- settings(summaryTable)
        if("summarise_table" %in% set_char$result_type){
          set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
          summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
        }
        
        charTable_demographics <- CohortCharacteristics::tableCharacteristics(summaryTable, type = "flextable", header = "cdm_name") 
        exportMultipleFlextablesToExcel(ft_list = list("charDemographics"=charTable_demographics) ,file = file)
      }
    )

    # -------------------------------------------------------------------------

    # charAntifungalAntibiotics -------------------------------------------------------------------------
    # AntifungalAntibiotics ----
    get_charAntifungalAntibioticsTable_data <- reactive({
      req(input$cdm_name, input$cohort_name, input$strata_name, input$strata_level, input$variable_name)
      
      data_antifungal %>% dplyr::filter(
        cdm_name    %in% input$cdm_name,
        group_level %in% input$cohort_name,
        strata_name %in% input$strata_name,
        strata_level%in% input$strata_level,
        variable_name %in% input$variable_name
      )
    })
    
    output$table_antifungal <- renderReactable({
      summaryTable <- get_charAntifungalAntibioticsTable_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable_antifungal <- downloadHandler(
      filename = function() paste0("charAntifungalAntibiotics_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_charAntifungalAntibioticsTable_data(), file, row.names = FALSE)
    )
    
    output$table_antifungal_tidy <- gt::render_gt({
      summaryTable <- get_charAntifungalAntibioticsTable_data()
      req(nrow(summaryTable) > 0)
      
      # Note: Because charAntifungalAntibiotics was generated using PatientProfiles, it is needed to assign 
      #'summarise_characteristics' to the summarise results settings to generate table with CohortCharacteristics::tableCharacteristics:
      set_char <- settings(summaryTable)
      if("summarise_table" %in% set_char$result_type){
        set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
        summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
      }
      CohortCharacteristics::tableCharacteristics(summaryTable, type = "gt", header = "cdm_name") 
      
    })
    
    # Download tidy char antifungal table ----
    output$downloadTable_tidy_antifungal <-  downloadHandler(
      filename = function() {
        # paste("charAntifungalAntibiotics_table_", Sys.Date(), ".csv", sep="")
        paste("charAntifungalAntibiotics_tidy_table_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        summaryTable <- get_charAntifungalAntibioticsTable_data()
        # Note: Because charAntifungalAntibiotics was generated using PatientProfiles, it is needed to assign 
        #'summarise_characteristics' to the summarise results settings to generate table with CohortCharacteristics::tableCharacteristics:
        set_char <- settings(summaryTable)
        if("summarise_table" %in% set_char$result_type){
          set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
          summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
        }
        
        charTable_antifungal <- CohortCharacteristics::tableCharacteristics(summaryTable, type = "flextable", header = "cdm_name") 
        exportMultipleFlextablesToExcel(ft_list = list("charAntifungalAntibiotics"=charTable_antifungal) ,file = file)
      }
    )
    # -------------------------------------------------------------------------

    
    
    # Indications ----
    get_charIndicationsTable_data <- reactive({
      req(input$cdm_name, input$cohort_name, input$strata_name, input$strata_level, input$variable_name)
      dplyr::filter(
        data_indication,
        cdm_name    %in% input$cdm_name,
        group_level %in% input$cohort_name,
        strata_name %in% input$strata_name,
        strata_level%in% input$strata_level,
        variable_name %in% input$variable_name
      )
    })
    
    output$table_ind <- renderReactable({
      summaryTable <- get_charIndicationsTable_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable_ind <- downloadHandler(
      filename = function() paste0("charIndications_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_charIndicationsTable_data(), file, row.names = FALSE)
    )
    
    output$table_ind_tidy <- gt::render_gt({
      summaryTable <- get_charIndicationsTable_data()
      req(nrow(summaryTable) > 0)
      DrugUtilisation::tableIndication(summaryTable, type = "gt", header = "cdm_name")
    })
    
    # Download tidy char demographics table ----
    output$downloadTable_tidy_indications <-  downloadHandler(
      filename = function() {
        paste("charIndications_tidy_table_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        summaryTable <- get_charIndicationsTable_data()
        req(nrow(summaryTable) > 0)
        charTable_indications <- DrugUtilisation::tableIndication(summaryTable, type = "flextable", header = "cdm_name")
        exportMultipleFlextablesToExcel(ft_list = list("charIndications"=charTable_indications) ,file = file)
      }
    )
    
    
    # Comorbidities ----
    get_charComorbiditiesTable_data <- reactive({
      req(input$cdm_name, input$cohort_name, input$strata_name, input$strata_level, input$variable_name)
      dplyr::filter(
        data_comorbidities,
        cdm_name    %in% input$cdm_name,
        group_level %in% input$cohort_name,
        strata_name %in% input$strata_name,
        strata_level%in% input$strata_level,
        variable_name %in% input$variable_name
      )
    })
    
    output$table_comorbidities <- renderReactable({
      summaryTable <- get_charComorbiditiesTable_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable_comorb <- downloadHandler(
      filename = function() paste0("charComorbidities_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_charComorbiditiesTable_data(), file, row.names = FALSE)
    )
    
    output$table_comorbidities_tidy <- gt::render_gt({
      summaryTable <- get_charComorbiditiesTable_data()
      req(nrow(summaryTable) > 0)
      CohortCharacteristics::tableLargeScaleCharacteristics(summaryTable, type = "gt")
    })
    
    # Download tidy char demographics table ----
    output$downloadTable_tidy_comorbidities <-  downloadHandler(
      filename = function() {
        paste("charComorbidities_tidy_table_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        summaryTable <- get_charComorbiditiesTable_data()
        req(nrow(summaryTable) > 0)
        charTable_comorbidities <- CohortCharacteristics::tableLargeScaleCharacteristics(summaryTable, type = "flextable")
        exportMultipleFlextablesToExcel(ft_list = list("charComorbidities"=charTable_comorbidities) ,file = file)
      }
    )
   
    
  })
}
