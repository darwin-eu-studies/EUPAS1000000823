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

drugUtilisationUI <- function(id,data) {
  
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
          choices = unique(data$cohort_name),
          selected =  unique(data$cohort_name),
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
      )
    ),
    
    div(
      style = style,
      pickerInput(
        inputId = ns("concept_set"),
        label = "Concept set",
        choices = unique(data$concept_set),
        selected =  unique(data$concept_set),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
        multiple = TRUE
      )
    ),
    
    div(
      style = style,
      pickerInput(
        inputId = ns("ingredient"),
        label = "Ingredient",
        choices = unique(data$ingredient),
        selected =  unique(data$ingredient),
        options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
        multiple = TRUE
      )
    ),
    tabsetPanel(
      type = "tabs",
      # Add Incidence Table panel -------------------
      tabPanel(
        "Drug Duration",
        icon = icon("th"),
        downloadButton(
          outputId = ns("downloadTable"),
          label = "Download current estimates (CSV)",
          icon = shiny::icon("download")
        ),
        reactableOutput(ns("table"))
      )
    )
  )
  
  
  
}


# Back-end functionality
# Define Server --------------------

drugUtilisationServer <- function(id,data) {
  #stopifnot(is.reactive(cdm_name))
  
  
  
  moduleServer(id, function(input,output,session) {
    
    
    
    # Data rendering for Incidence All Products -------------------------------
    
    get_drugUtilisationTable_data <- reactive({
      
      data %>%
        filter(cdm_name %in% input$cdm_name) %>%
        filter(cohort_name %in% input$cohort_name) %>%
        filter(variable_name %in% input$variable_name) %>%
        filter(concept_set %in% input$concept_set) %>%
        filter(ingredient %in% input$ingredient) %>%
        select(-variable_level)
      
    })
    
    
    
    
    output$table <- renderReactable({
      
      summaryTable <- get_drugUtilisationTable_data()
      shiny::validate(need(nrow(summaryTable) > 0, "No results for selected inputs"))
      reactable(summaryTable,filterable = T,striped = T,resizable = T, bordered = T)
      
    })
    
    
    
    # Download filtered incidence table
    output$downloadTable <-  downloadHandler(
      filename = function() {
        paste("drugUtilisation_table_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        summaryTable <- get_drugUtilisationTable_data()
        write.csv(summaryTable, file, row.names = FALSE)
      }
    )
    
  })
}
