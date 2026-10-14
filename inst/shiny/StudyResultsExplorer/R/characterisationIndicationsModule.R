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

# Front-end structure
# Define UI --------------------

characterisationIndicationsUI <- function(id,data) {
  
  ns <- NS(id)
  
  
  style <- "display: inline-block;vertical-align:top; width: 200px;"
  fluidRow(
    tagList(
      div(
        style = style,
        pickerInput(
          inputId = ns("cdm_name"),
          label = "CDM name",
          choices = unique(data$cdm_name),
          selected =  unique(data$cdm_name),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
          multiple = TRUE
        )
      ),
      div(
        style = style,
        pickerInput(
          inputId = ns("cohort_name"),
          label = "Cohort name",
          choices = unique(data$group_level) ,#unique(data$cohort_name),
          selected = unique(data$group_level),   #unique(data$cohort_name),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
          multiple = TRUE
        )
      ),
      
      div(
        style = style,
        pickerInput(
          inputId = ns("variable_name"),
          label = "Variable name",
          choices = unique(data$variable_name),
          selected =  unique(data$variable_name),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
          multiple = TRUE
        )
      ),
      div(
        style = style,
        pickerInput(
          inputId = ns("strata_name"),
          label = "Strata name",
          choices = unique(data$strata_name) ,
          selected = unique(data$strata_name), 
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
          multiple = TRUE
        )
      ),
      div(
        style = style,
        pickerInput(
          inputId = ns("strata_level"),
          label = "Strata level",
          choices = unique(data$strata_level) ,
          selected = unique(data$strata_level), 
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
          multiple = TRUE
        )
      ),
      
      # div(
      #   style = style,
      #   pickerInput(
      #     inputId = ns("variable_level"),
      #     label = "Variable level",
      #     choices = unique(data$variable_level),
      #     selected =  unique(data$variable_level),
      #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
      #     multiple = TRUE
      #   )
      # )
      
    ),
    tabsetPanel(
      type = "tabs",
      # Add Incidence Table panel -------------------
      tabPanel(
        "Characterisation (Demographics)",
        icon = icon("th"),
        downloadButton(
          outputId = ns("downloadTable"),
          label = "Download current estimates (CSV)",
          icon = shiny::icon("download")
        ),
        gt_output(ns("table"))
        #reactableOutput(ns("table"))
      )
    )
  )
  
  
  
}


# Back-end functionality
# Define Server --------------------

characterisationIndicationsServer <- function(id,data) {
  #stopifnot(is.reactive(cdm_name))
  
  
  
  moduleServer(id, function(input,output,session) {
    
    
    get_charDemographicsTable_data <- reactive({
      if(any("Sex" %in% input$variable_name)){
        data %>%
          filter(cdm_name %in% input$cdm_name) %>%
          filter(group_level %in% input$cohort_name) %>%
          filter(strata_name %in% input$strata_name) %>%
          filter(strata_level %in% input$strata_level) %>%
          filter(variable_name %in% input$variable_name)
      }
      
      # if(all(unique(data$variable_name) %in% input$variable_name)){
      #   data %>%
      #     filter(cdm_name %in% input$cdm_name) %>%
      #     filter(cohort_name %in% input$cohort_name)
      # }
      
    })
    
    
    
    
    # output$table <- renderReactable({
    output$table <- render_gt({
      
      summaryTable <- get_charDemographicsTable_data()
      shiny::validate(need(nrow(summaryTable) > 0, "No results for selected inputs"))
      DrugUtilisation::tableIndication(summaryTable,type = "gt",header = "additional_level")
      # reactable(summaryTable,filterable = T,striped = T,resizable = T, bordered = T)
    })
    
    
    exportMultipleFlextablesToExcel <- function(ft_list, file) {
      wb <- openxlsx2::wb_workbook()
      
      for (sheet_name in names(ft_list)) {
        ft <- ft_list[[sheet_name]]
        wb$add_worksheet(sheet_name)
        wb <- flexlsx::wb_add_flextable(wb, sheet = sheet_name, ft = ft, dims = "C2")
      }
      
      wb$save(file)
    }
    
    # Download filtered incidence table
    output$downloadTable <-  downloadHandler(
      filename = function() {
        # paste("charDemographics_table_", Sys.Date(), ".csv", sep="")
        paste("charDemographics_table_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        summaryTable <- get_charDemographicsTable_data()
        charTable <- CohortCharacteristics::tableCharacteristics(summaryTable,type = "flextable")
        exportMultipleFlextablesToExcel(ft_list = list("char"=charTable) ,file = file)
        # write.csv(charTable, file, row.names = FALSE)
      }
    )
    
  })
}



