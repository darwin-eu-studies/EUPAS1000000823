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

# resistance_module.R
charDUSUI <- function(id, show_filters = TRUE, data = NULL, width_px = 200) {
  ns <- NS(id)
  style <- sprintf("display: inline-block;vertical-align:top; width: %dpx;", width_px)
  
  tagList(
    if (isTRUE(show_filters) && !is.null(data)) {
      # Local filters (standalone mode)
      tagList(
        fluidRow(
          div(style = style, pickerInput(ns("cdm_name"), DATA_SOURCE_LABEL,
                                         choices = unique(data$cdm_name),
                                         selected = unique(data$cdm_name),
                                         options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
                                         multiple = TRUE)),
          div(style = style, pickerInput(ns("cohort_name"), "Cohort name",
                                         choices = unique(data$group_level),
                                         selected = unique(data$group_level),
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
                                         multiple = TRUE)) #,
          # div(style = style, pickerInput(ns("concept_set"), "Concept set",
          #                                choices = unique(data$concept_set),
          #                                selected = unique(data$concept_set),
          #                                options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
          #                                multiple = TRUE)),
          # div(style = style, pickerInput(ns("ingredient"), "Ingredient",
          #                                choices = unique(data$ingredient),
          #                                selected = unique(data$ingredient),
          #                                options = list(`actions-box`=TRUE, size=10, `selected-text-format`="count > 0"),
          #                                multiple = TRUE))
        )
      )
    },
    tabsetPanel(type = "tabs",
                tabPanel("Raw", icon = icon("th"),
                         downloadButton(ns("downloadTable"), "Download table", icon = icon("download")),
                         reactableOutput(ns("table_resistance"))
                ),
                tabPanel("Tidy table", icon = icon("th"),
                         downloadButton(ns("downloadTable_tidy_dus"), "Download table", icon = icon("download")),
                         gt_output(ns("table_dus_tidy"))
                )
    )
  )
}

charDUSServer <- function(id, data, filters = NULL) {
  moduleServer(id, function(input, output, session) {
    # Resolve filters: from shared module or local inputs
    filters_r <- if (!is.null(filters)) filters else reactive(list(
      cdm_name     = input$cdm_name,
      cohort_name  = input$cohort_name,
      strata_name  = input$strata_name,
      strata_level = input$strata_level,
      variable_name = input$variable_name #,
      #concept_set = input$concept_set, #
      #ingredient = input$ingredient #
    ))
    
    get_data <- reactive({
      f <- filters_r()
      req(f$cdm_name, f$cohort_name, f$strata_name, f$strata_level, f$variable_name)
      dplyr::filter(
        data,
        .data$cdm_name     %in% f$cdm_name,
        .data$group_level  %in% f$cohort_name,
        .data$strata_name  %in% f$strata_name,
        .data$strata_level %in% f$strata_level,
        .data$variable_name %in% f$variable_name#,
        # .data$concept_set %in% f$concept_set, #
        # .data$ingredient %in% f$ingredient #
      )
    })
    
    get_censored_data <- reactive({
      summaryTable <- get_data()
      req(nrow(summaryTable) > 0)
      applyDusSecondaryCensoring(summaryTable)
    })
    
    output$table_resistance <- renderReactable({
      summaryTable <- get_censored_data()
      reactable::reactable(summaryTable, filterable = TRUE, striped = TRUE, resizable = TRUE, bordered = TRUE)
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() paste0("charResistance_table_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(get_censored_data(), file, row.names = FALSE)
    )
    
    
    output$table_dus_tidy <- gt::render_gt({
      summaryTable <- get_censored_data()
      set_char <- settings(summaryTable)
      
      result <- summaryTable #%>% splitAdditional()
      if (nrow(result) == 0) {
        cli::cli_warn("There are no results with `result_type = {resultType}`")
        return(emptyTable(type))
      }
      
      # setColumns <- names(purrr::compact(purrr::map(dplyr::filter(omopgenerics::settings(result), 
      #                                                             .data$result_id %in% unique(.env$result$result_id)), 
      #                                               function(x) x[!is.na(x)])))
      # setColumns <- setColumns[!setColumns %in% c("result_id", 
      #                                             "result_type", "package_name", "package_version", "group", 
      #                                             "strata", "additional", "min_cell_count")]
      # cols <- c("cdm_name", setColumns, groupColumns(result), strataColumns(result), 
      #           additionalColumns(result), "variable_name", "variable_level", 
      #           "estimate_name", "estimate_value")
      # 
      # header = c("cdm_name")
      # groupColumn = c("cohort_name", CohortCharacteristics::strataColumns(result),"concept_set","ingredient","variable_name")
      # hide = c("variable_level", "censor_date", "cohort_table_name", "gap_era", "index_date", "restrict_incident")
      # .options = list()
      # 
      # visOmopResults::visTable(result = result, 
      #                              estimateName = 
      #                                c(`missing N (%)` = "<count_missing> (<percentage_missing> %)", 
      #                                N = "<count>", `Mean (SD)` = "<mean> (<sd>)", `Median (Q25 - Q75)` = "<median> (<q25> - <q75>)"), 
      #                              header = header, 
      #                              groupColumn = c("concept_set"),#groupColumn, 
      #                              hide = hide, 
      #                              #settingsColumn = setColumns, 
      #                              type = "gt", 
      #                              #columnOrder = cols[!cols %in% c(hide, groupColumn, header)], 
      #                              .options = .options)
      
      DrugUtilisation::tableDrugUtilisation(summaryTable,type = "gt",header = "cdm_name", groupColumn = "strata_name")
      
    })
    
    output$downloadTable_tidy_dus <- downloadHandler(
      filename = function() paste("dus_tidy_table_", Sys.Date(), ".xlsx", sep = ""),
      content = function(file) {
        summaryTable <- get_censored_data()
        ft <- DrugUtilisation::tableDrugUtilisation(summaryTable,type = "flextable",header = "cdm_name", groupColumn = "strata_name")
        exportMultipleFlextablesToExcel(ft_list = list("DUS" = ft), file = file)
      }
    )
    
    
  })
}


# ---- RUN ----
# shinyApp(
#   ui = fluidPage(charDUSUI("dusObj3", data = dusObj3)),
#   server = function(input, output, session) {
#     charDUSServer("dusObj3",data=dusObj3)
#   }
# )
