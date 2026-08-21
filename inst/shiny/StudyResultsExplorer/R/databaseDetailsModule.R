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

databaseDetailsUI <- function(id,data) {
  
  ns <- NS(id)
  
  
  style <- "display: inline-block;vertical-align:top; width: 200px;"
  fluidRow(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Table",
        icon = icon("th"),
        h4("Study data sources"),
        div(
          style = style,
          pickerInput(
            inputId = ns("cdm_name"),
            label = DATA_SOURCE_LABEL,
            choices = unique(data$cdm_name),
            selected =  unique(data$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 0"),
            multiple = TRUE
          )
        ),
        downloadButton(
          outputId = ns("downloadDatabaseDetails"),
          label = "Download"
        ),
        #reactableOutput(ns("databaseDetailsTable"))
        gt::gt_output(ns("databaseDetailsTable"))
      )
    )
  )
  
  
  
}


# Back-end functionality
# Define Server --------------------

databaseDetailsServer <- function(id,data) {
  
  moduleServer(id, function(input,output,session) {
    
    # Data rendering for Incidence All Products -------------------------------
    
    get_databaseDetailsTable_data <- reactive({
      
      database_table <- data %>% filter(cdm_name %in% input$cdm_name)
    })
    
    
    output$databaseDetailsTable <- gt::render_gt({
      summaryTable <- get_databaseDetailsTable_data()
      shiny::validate(need(nrow(summaryTable) > 0, "No results for selected inputs"))
      summaryTable %>% OmopSketch::tableOmopSnapshot(type = "gt")
    })
    
    output$downloadDatabaseDetails <-  downloadHandler(
      filename = function() {
        paste("database_details_", Sys.Date(), ".docx", sep="")
      },
      content = function(file) {
        summaryTable <- get_databaseDetailsTable_data()
        databaseDetailsTable_gt <- summaryTable %>% OmopSketch::tableOmopSnapshot(type = "gt")
        gt::gtsave(databaseDetailsTable_gt,file)
        # write.csv(get_databaseDetailsTable_data(), file, row.names = FALSE)
        
      }
    )
    
    
  })
}



