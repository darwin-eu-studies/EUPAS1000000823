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

# comorbidities_module.R
charComorbiditiesUI <- function(id, show_filters = TRUE, data = NULL, width_px = 200) {
  ns <- NS(id)
  style <- sprintf("display: inline-block;vertical-align:top; width: %dpx;", width_px)
  
  tagList(
    if (isTRUE(show_filters) && !is.null(data)) {
      fluidRow(
        div(style = style, pickerInput(ns("cdm_name"), DATA_SOURCE_LABEL,
                                       choices = unique(data$cdm_name),
                                       selected = unique(data$cdm_name),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("cohort_name"), "Cohort name",
                                       choices = unique(data$group_level),
                                       selected = unique(data$group_level)[1],
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("strata_name"), "Strata name",
                                       choices = unique(data$strata_name),
                                       selected = unique(data$strata_name)[1],
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("strata_level"), "Strata level",
                                       choices = unique(data$strata_level),
                                       selected = unique(data$strata_level)[1],
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("variable_name"), "Condition",
                                       choices = unique(data$variable_name),
                                       selected = unique(data$variable_name),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("time_window"), "Time window",
                                       choices = unique(data$variable_level),
                                       selected = unique(data$variable_level)[1],
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE))
      )
    },
    tabsetPanel(type = "tabs",
                tabPanel("Raw", icon = icon("th"),
                         downloadButton(ns("downloadTable_comorb"), "Download table", icon = icon("download")),
                         reactableOutput(ns("table_comorbidities"))
                ),
                tabPanel("Tidy table", icon = icon("th"),
                         downloadButton(ns("downloadTable_tidy_comorbidities"), "Download table", icon = icon("download")),
                         #DT::DTOutput(ns("table_comorbidities_tidy"))
                         gt_output(ns("table_comorbidities_tidy"))
                )
    )
  )
}

charComorbiditiesServer <- function(id, data_comorbidities, filters = NULL) {
  moduleServer(id, function(input, output, session) {
    filters_r <- if (!is.null(filters)) filters else reactive(list(
      cdm_name     = input$cdm_name,
      cohort_name  = input$cohort_name,
      strata_name  = input$strata_name,
      strata_level = input$strata_level,
      variable_name = input$variable_name,
      variable_level = input$time_window
    ))
    
    get_data <- reactive({
      f <- filters_r()
      req(f$cdm_name, f$cohort_name, f$strata_name, f$strata_level, f$variable_name)
      dplyr::filter(
        data_comorbidities,
        .data$cdm_name     %in% f$cdm_name,
        .data$group_level  %in% f$cohort_name,
        .data$strata_name  %in% f$strata_name,
        .data$strata_level %in% f$strata_level,
        .data$variable_name %in% f$variable_name,
        .data$variable_level %in% f$variable_level
      )
    })
    
    output$table_comorbidities <- renderReactable({
      summaryTable <- get_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable_comorb <- downloadHandler(
      filename = function() paste0("charComorbidities_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_data(), file, row.names = FALSE)
    )
    
    output$table_comorbidities_tidy <-  gt::render_gt({
      summaryTable <- get_data()
      summaryTable %>% visOmopResults::visOmopTable( 
        header = "cdm_name", 
        estimateName = c("N(%)" = "<count> (<percentage>%)"),
        groupColumn = "variable_level",
        type = "gt")
    })
    
    output$downloadTable_tidy_comorbidities <- downloadHandler(
      filename = function() paste("charComorbidities_tidy_table_", Sys.Date(), ".xlsx", sep = ""),
      content = function(file) {
        summaryTable <- get_data()
        ft <- summaryTable %>% visOmopResults::visOmopTable( 
          header = "cdm_name", 
          estimateName = c("N(%)" = "<count> (<percentage>%)"),
          groupColumn = "variable_level",
          type = "flextable")
        exportMultipleFlextablesToExcel(ft_list = list("charComorbidities" = ft), file = file)
      }
    )
    
    
    # output$table_comorbidities_tidy <- DT::renderDT({
    #   summaryTable <- get_data()
    #   req(nrow(summaryTable) > 0)
    #   
    #   w <- CohortCharacteristics::tableLargeScaleCharacteristics(summaryTable, type = "DT")
    #   
    #   # Try augment options if not set internally
    #   w$dependencies <- unique(c(w$dependencies, htmltools::htmlDependency(
    #     name = "dt-buttons", version = "1.0",
    #     package = "DT", src = "htmlwidgets",
    #     script = c("datatable.js", "datatable-binding.js")
    #   )))
    #   w <- DT::formatStyle(w, columns = names(summaryTable)) # no-op to touch object
    #   
    #   # Rebuild with buttons (fallback if the above doesn’t work: use Option A)
    #   DT::datatable(
    #     w$x$data,
    #     extensions = "Buttons",
    #     options = list(
    #       dom = "Bfrtip",
    #       buttons = c("copy", "csv", "excel"),
    #       pageLength = 25,
    #       scrollX = TRUE
    #     ),
    #     rownames = FALSE
    #   )
    # })
    
    # output$table_comorbidities_tidy <- DT::renderDT({
    #   # summaryTable <- get_data()
    #   # req(nrow(summaryTable) > 0)
    #   # CohortCharacteristics::tableLargeScaleCharacteristics(summaryTable, type = "DT")
    # })
    # 
    # output$downloadTable_tidy_comorbidities_docx <- downloadHandler(
    #   filename = function() paste("charComorbidities_tidy_table_", Sys.Date(), ".csv", sep = ""),
    #   content = function(file) {
    #     summaryTable <- get_data()
    #     req(nrow(summaryTable) > 0)
    #     ft <- CohortCharacteristics::tableLargeScaleCharacteristics(summaryTable, type = "DT")
    #     write.csv(ft, file, row.names = FALSE)
    #   }
    # )
    
  })
}


# ---- RUN ----
# shinyApp(
#   ui = fluidPage(charComorbiditiesUI("charComorbidities", data = charComorbidities)),
#   server = function(input, output, session) {
#     charComorbiditiesServer("charComorbidities",data=charComorbidities)
#   }
# )
