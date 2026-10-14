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

# _module.R
charSummariseResultUI <- function(id, show_filters = TRUE, data = NULL, width_px = 200) {
  ns <- NS(id)
  style <- sprintf("display: inline-block;vertical-align:top; width: %dpx;", width_px)
  
  tagList(
    if (isTRUE(show_filters) && !is.null(data)) {
      fluidRow(
        div(style = style, pickerInput(ns("cdm_name"), "CDM name",
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
                                       selected = unique(data$strata_name),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("strata_level"), "Strata level",
                                       choices = unique(data$strata_level),
                                       selected = unique(data$strata_level),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("variable_name"), "Variable name",
                                       choices = unique(data$variable_name),
                                       selected = unique(data$variable_name),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE)),
        div(style = style, pickerInput(ns("time_window"), "Time window",
                                       choices = unique(data$time_window),
                                       selected = unique(data$time_window),
                                       options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                       multiple = TRUE))
      )
    },
    tabsetPanel(type = "tabs",
                tabPanel("Raw", icon = icon("th"),
                         downloadButton(ns("downloadTable_"), "Download table", icon = icon("download")),
                         reactableOutput(ns("table_"))
                ),
                tabPanel("Tidy table", icon = icon("th"),
                         downloadButton(ns("downloadTable_tidy_"), "Download table", icon = icon("download")),
                         gt_output(ns("table__tidy"))
                )
    )
  )
}

charSummariseResultServer <- function(id, data_, filters = NULL) {
  moduleServer(id, function(input, output, session) {
    filters_r <- if (!is.null(filters)) filters else reactive(list(
      cdm_name     = input$cdm_name,
      cohort_name  = input$cohort_name,
      strata_name  = input$strata_name,
      strata_level = input$strata_level,
      variable_name = input$variable_name,
      time_window = input$time_window #
    ))
    
    get_data <- reactive({
      f <- filters_r()
      req(f$cdm_name, f$cohort_name, f$strata_name, f$strata_level, f$variable_name)
      dplyr::filter(
        data_,
        .data$cdm_name     %in% f$cdm_name,
        .data$group_level  %in% f$cohort_name,
        .data$strata_name  %in% f$strata_name,
        .data$strata_level %in% f$strata_level,
        .data$variable_name %in% f$variable_name,
        .data$time_window %in% f$time_window #
        
      )
    })
    
    output$table_ <- renderReactable({
      summaryTable <- get_data()
      req(nrow(summaryTable) > 0)
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable_ <- downloadHandler(
      filename = function() paste0("charAntibiotics_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_data(), file, row.names = FALSE)
    )
    
    output$table__tidy <- gt::render_gt({
      summaryTable <- get_data()
      req(nrow(summaryTable) > 0)
      
      # set_char <- settings(summaryTable)
      # if ("summarise_table" %in% set_char$result_type) {
      #   set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
      #   summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
      # }
      # 
      # CohortCharacteristics::tableCharacteristics(summaryTable, type = "gt", header = "cdm_name", groupColumn = "time_window")
      
      visOmopResults::visTable(summaryTable, 
                               type = "gt", 
                               header = "cdm_name", 
                               groupColumn = c("group_level","time_window"),
                               estimateName = c(`N (%)` = "<count> (<percentage>%)"), 
                               hide = c("strata_name","strata_level","estimate_type",
                                        "additional_name","additional_level","group_name")
      )
      
      
    })
    
    output$downloadTable_tidy_ <- downloadHandler(
      filename = function() paste("charAntibiotics_tidy_table_", Sys.Date(), ".xlsx", sep = ""),
      content = function(file) {
        summaryTable <- get_data()
        
        # set_char <- settings(summaryTable)
        # if ("summarise_table" %in% set_char$result_type) {
        #    set_char <- settings(summaryTable) %>% mutate(result_type = "summarise_characteristics", package_name = "CohortCharacteristics")
        #    summaryTable <- summaryTable %>% omopgenerics::newSummarisedResult(settings = set_char)
        # }
        # ft <- CohortCharacteristics::tableCharacteristics(summaryTable, type = "flextable", header = "cdm_name")
        
        ft <- visOmopResults::visTable(summaryTable, 
                                       type = "flextable", 
                                       header = "cdm_name", 
                                       groupColumn = c("group_level","time_window"),
                                       estimateName = c(`N (%)` = "<count> (<percentage>%)"), 
                                       hide = c("strata_name","strata_level","estimate_type",
                                                "additional_name","additional_level","group_name")
        )
        exportMultipleFlextablesToExcel(ft_list = list("charAntibiotics" = ft), file = file)
      }
    )
  })
}


# ---- RUN ----
# shinyApp(
#   ui = fluidPage(charSummariseResultUI("charAntibiotics", data = charAntibiotics)),
#   server = function(input, output, session) {
#     charSummariseResultServer("charAntibiotics",data=charAntibiotics)
#   }
# )
