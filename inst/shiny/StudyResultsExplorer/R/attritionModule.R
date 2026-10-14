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

attritionUI <- function(id,data) {
  
  ns <- NS(id)
  
  
  style <- "display: inline-block;vertical-align:top; width: 200px;"
  fluidRow(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table",
        icon = icon("th"),
        div(
          style = style,
          pickerInput(
            inputId = ns("cdm_name_1"),
            label = DATA_SOURCE_LABEL,
            choices = unique(data$cdm_name),
            selected =  unique(data$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("objective_name_1"),
            label = "Objective name",
            choices = unique(data$objective_name),
            selected =  unique(data$objective_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        # div(
        #   style = style,
        #   pickerInput(
        #     inputId = ns("target_cohort_name_1"),
        #     label = "Target Cohort Name",
        #     choices = unique(data$target_cohort_name),
        #     selected =  unique(data$target_cohort_name)[1],
        #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
        #     multiple = FALSE
        #   )
        # ),
        div(
          style = style,
          pickerInput(
            inputId = ns("outcome_cohort_name_1"),
            label = "Outcome Cohort Name",
            choices = unique(data$outcome_cohort_name),
            selected =  unique(data$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        downloadButton(
          outputId = ns("downloadCohortAttritionTable"),
          label = "Download CSV"
        ),
        reactableOutput(ns("cohortAttritionTable"))
      ),
      
      tabPanel(
        "Diagram",
        icon = icon("chart-line"),
        div(
          style = style,
          pickerInput(
            inputId = ns("cdm_name_2"),
            label = DATA_SOURCE_LABEL,
            choices = unique(data$cdm_name),
            selected =  unique(data$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        div(
          style = style,
          pickerInput(
            inputId = ns("objective_name_2"),
            label = "Objective name",
            choices = unique(data$objective_name),
            selected =  unique(data$objective_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        # div(
        #   style = style,
        #   pickerInput(
        #     inputId = ns("target_cohort_name_2"),
        #     label = "Target Cohort Name",
        #     choices = unique(data$target_cohort_name),
        #     selected =  unique(data$target_cohort_name)[1],
        #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
        #     multiple = FALSE
        #   )
        # ),
        div(
          style = style,
          pickerInput(
            inputId = ns("outcome_cohort_name_2"),
            label = "Outcome Cohort Name",
            choices = unique(data$outcome_cohort_name),
            selected =  unique(data$outcome_cohort_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = FALSE
          )
        ),
        downloadButton(
          outputId = ns("downloadCohortAttritionDiagram"),
          label = "Download PNG"
        ),
        box(
          width = 12,
          grVizOutput(ns("cohortAttritionDiagram"), height = "1200px")
        )
      )
    )
  )
  
  
  
}


# Back-end functionality
# Define Server --------------------

attritionServer <- function(id,data) {
  #stopifnot(is.reactive(cdm_name))
  
  
  
  moduleServer(id, function(input,output,session) {
    
    
    
    # Data rendering for Incidence All Products -------------------------------
    
    get_attritionTable_data <- reactive({
      
      data %>% 
        filter(cdm_name %in% input$cdm_name_1) %>%
        filter(objective_name %in% input$objective_name_1) %>%
        # filter(target_cohort_name %in% input$target_cohort_name_1) %>%
        filter(outcome_cohort_name %in% input$outcome_cohort_name_1)
      
      
    })
    
    
    
    
    output$cohortAttritionTable <- renderReactable({
      summaryTable <- get_attritionTable_data()
      shiny::validate(need(nrow(summaryTable) > 0, "No results for selected inputs"))
      reactable(summaryTable,filterable = T,striped = T,resizable = T, bordered = T,pagination = F, defaultPageSize = nrow(summaryTable))
    })
    
    output$downloadCohortAttritionTable <-  downloadHandler(
      filename = function() {
        paste("cohort_attrition_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(get_attritionTable_data(), file, row.names = FALSE)
      }
    )
    
    
    get_attritionTable_diagram_data <- reactive({
      data %>%
        filter(cdm_name %in% input$cdm_name_2) %>%
        filter(objective_name %in% input$objective_name_2) %>%
        # filter(target_cohort_name %in% input$target_cohort_name_2) %>%
        filter(outcome_cohort_name %in% input$outcome_cohort_name_2)
      
    })
    
    
    get_cohortDefinitionId_diagram <- reactive({
      summaryTable <- get_attritionTable_diagram_data()
      #cohortId <- unique(summaryTable$cohort_definition_id)
      
      #plotCohortAttrition(summaryTable,cohortId = cohortId)
      # Custom plotCohortAttrition function for non-summarised results (see utils.R)
      plotCohortAttritionCustom(summaryTable) 
      #CohortCharacteristics::plotCohortAttrition(summaryTable)
    })
    
    output$cohortAttritionDiagram <- renderGrViz({
      get_cohortDefinitionId_diagram()
      
    })
    
    
    output$downloadCohortAttritionDiagram <-  downloadHandler(
      filename = function() {
        #cohortId <- as.character(input$outcome_cohort_name)
        cdm_name <- as.character(input$cdm_name)
        paste("attrition_",cdm_name,"_attrition_flos_", Sys.Date(), ".png", sep="")
      },
      content = function(file) {
        # Convert grViz diagram to SVG
        svg <- get_cohortDefinitionId_diagram() %>%
          DiagrammeRsvg::export_svg() %>% 
          charToRaw()
        # Convert the SVG diagram to PNG
        rsvg_png(svg,file = file,width = 800, height = 600)
        
      }
    )
    
    
  })
}


# ---- RUN ----
# shinyApp(
#   ui = fluidPage(attritionUI("cohortAttrition",attrition_obj1)),
#   server = function(input, output, session) {
#     attritionServer("cohortAttrition",attrition_obj1)
#   }
# )

