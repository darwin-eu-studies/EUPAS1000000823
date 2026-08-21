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

#' @title Prevalence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Prevalence module that shows prevalence results from the IncidencePrevalence package.
#'
#' @export
#'
#' @examples{
#' \donttest{
#'  library(DarwinShinyModules)
#'
#'  if (
#'    require(
#'      "IncidencePrevalence",
#'      character.only = TRUE,
#'      quietly = TRUE,
#'      warn.conflicts = FALSE
#'    )
#'  ) {
#'     inc <- omopgenerics::importSummarisedResult(system.file(
#'       package = "DarwinShinyModules",
#'       "dummyData/IncidencePrevalence/1.2.0/prevalence.csv"
#'     ))
#'
#'     prevMod <- Prevalence$new(data = inc)
#'
#'     ui <- shiny::fluidPage(
#'       prevMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       prevMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
PrevalenceCustom <- R6::R6Class(
  classname = "Prevalence",
  inherit = DarwinShinyModules::ShinyModule,
  
  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object from Prevalence.
    data = function(data) {
      if (missing(data)) {
        return(private$.data)
      } else {
        # Checks on data
        checkmate::assertClass(data, "summarised_result")
        private$.data <- data
      }
    },
    
    #' @field pickers (`list`) List of pickers
    pickers = function() {
      return(private$.pickers)
    }
  ),
  
  # Public ----
  public = list(
    
    #' @description
    #' Initializer method
    #'
    #' @param data (`summarised_result`) Result object from the `IncidencePrevalence` package.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @returns `self`
    initialize = function(data, ...) {
      super$initialize(...)
      private$assertInstall("IncidencePrevalence", "1.2.0")
      private$assertInstall("visOmopResults", "1.0.2")
      private$assertPrevalenceData(data)
      private$.data <- data
      private$.cleanData <- private$restoreGroupLevel(data)
      private$.tidyData <- private$transformData(data)
      private$initPickers()
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    .data = NULL,
    .cleanData = NULL,
    .tidyData = NULL,
    .strata = NULL,
    .strata_sex = NULL,
    .strata_age_group = NULL,
    .pickers = NULL,
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "prevalence"),
          shiny::h3("Prevalence estimates"),
          shiny::p("Prevalence estimates are shown below, please select configuration to filter them:"),
          shiny::p("Data source, study outcome and strata"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          # private$.pickers[["strata"]]$UI(),
          private$.pickers[["strata_sex"]]$UI(),
          private$.pickers[["strata_age_group"]]$UI(),
          if (!is.null(private$.pickers[["denominatorTargetName"]])) {
            private$.pickers[["denominatorTargetName"]]$UI()
          },
          p("Denominator population settings"),
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["denomSex"]]$UI(),
          private$.pickers[["denomPriorObs"]]$UI(),
          private$.pickers[["denomStartDate"]]$UI(),
          private$.pickers[["denomEndDate"]]$UI(),
          private$.pickers[["denomTimeAtRisk"]]$UI(),
          p("Analysis settings"),
          private$.pickers[["analysisType"]]$UI(),
          private$.pickers[["completePeriod"]]$UI(),
          private$.pickers[["fullContribution"]]$UI(),
          private$.pickers[["minCounts"]]$UI(),
          p("Dates"),
          private$.pickers[["interval"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Tidy Table",
              private$.pickers[["headerColumn"]]$UI(),
              private$.pickers[["groupColumn"]]$UI(),
              private$.pickers[["settingsColumn"]]$UI(),
              private$.pickers[["hideColumn"]]$UI(),
              p(),
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTidyTable"), "Download table"),
              p(),
              gt::gt_output(shiny::NS(private$.namespace, "tidyTable")) %>% shinycssloaders::withSpinner()
            ),
            shiny::tabPanel(
              "Plot",
              p("Plotting options"),
              private$.pickers[["xAxis"]]$UI(),
              private$.pickers[["facet"]]$UI(),
              private$.pickers[["color"]]$UI(),
              private$.pickers[["ribbon"]]$UI(),
              private$.pickers[["confInterval"]]$UI(),
              plotly::plotlyOutput(
                shiny::NS(private$.namespace, "plot"),
                height = "800px"
              ) %>%
                shinycssloaders::withSpinner(),
              shiny::h4("Download figure"),
              shiny::div("height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_height"), "", 10, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block;",
                shiny::textInput(shiny::NS(private$.namespace, "download_width"), "", 20, width = "50px")
              ),
              shiny::div("cm", style = "display: inline-block; margin-right: 25px;"),
              shiny::div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
              shiny::div(
                style = "display: inline-block; margin-right:",
                shiny::textInput(shiny::NS(private$.namespace, "download_dpi"), "", 300, width = "50px")
              ),
              shiny::downloadButton(shiny::NS(private$.namespace, "download_plot"), "Download plot")
            ),
            shiny::tabPanel(
              "Table",
              shiny::downloadButton(shiny::NS(private$.namespace, "downloadTable"), "Download current estimates"),
              DT::DTOutput(shiny::NS(private$.namespace, "table")) %>% shinycssloaders::withSpinner()
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      for (module in private$.pickers) {
        module$server(input, output, session)
      }
      
      # Observer to update age_group picker when sex picker changes to 'overall'
      observeEvent(input$strata_sex, {
        if (length(input$strata_sex) == 1 && input$strata_sex == "overall") {
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "strata_age_group",
            selected = "overall"
          )
        }
      })
      
      # Prevalence
      getPrevalenceEstimates <- reactive({
        result <- private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          # dplyr::filter(strata %in% private$.pickers[["strata"]]$inputValues$strata) %>%
          dplyr::filter(sex %in% private$.pickers[["strata_sex"]]$inputValues$strata_sex) %>%
          dplyr::filter(age_group %in% private$.pickers[["strata_age_group"]]$inputValues$strata_age_group) %>%
          {
            if (!is.null(private$.pickers[["denominatorTargetName"]])) {
              dplyr::filter(., denominator_target_cohort_name %in% private$.pickers[["denominatorTargetName"]]$inputValues$denominator_target)
            } else {
              .
            }
          } %>%
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs) %>%
          dplyr::filter(denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date) %>%
          dplyr::filter(denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date) %>%
          dplyr::filter(denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          dplyr::filter(analysis_type %in% private$.pickers[["analysisType"]]$inputValues$analysis_type) %>%
          dplyr::filter(analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period) %>%
          dplyr::filter(analysis_full_contribution %in% private$.pickers[["fullContribution"]]$inputValues$full_contribution) %>%
          dplyr::filter(analysis_min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count) %>%
          dplyr::filter(analysis_interval %in% private$.pickers[["interval"]]$inputValues$interval) %>%
          dplyr::filter(prevalence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          dplyr::mutate(
            n_cases = round(suppressWarnings(as.numeric(n_cases))),
            n_population = round(suppressWarnings(as.numeric(n_population))),
            prevalence = round(suppressWarnings(as.numeric(prevalence)), 4),
            prevalence_95CI_lower = round(suppressWarnings(as.numeric(prevalence_95CI_lower)), 4),
            prevalence_95CI_upper = round(suppressWarnings(as.numeric(prevalence_95CI_upper)), 4),
            prevalence_start_date = as.Date(prevalence_start_date)
          )
        return(result)
      })
      
      # Filtered data (for Tidy Table / gt output)
      # Uses .cleanData (with restored group_level) so that tablePrevalence
      # can parse the group structure correctly.
      summarised_result_data <- reactive({
        sett_cols <- names(settings(private$.cleanData))
        
        sett_filter <- settings(private$.cleanData) %>%
          dplyr::filter(
            analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period,
            analysis_full_contribution %in% private$.pickers[["fullContribution"]]$inputValues$full_contribution,
            analysis_type %in% private$.pickers[["analysisType"]]$inputValues$analysis_type,
            min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count,
            denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date,
            denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date,
            denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs,
            denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex,
            denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group,
            denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk
          )
        
        if ("outcome_cohort_name" %in% sett_cols) {
          sett_filter <- sett_filter %>%
            dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome)
        }
        
        if (!is.null(private$.pickers[["denominatorTargetName"]]) &&
            "denominator_target_cohort_name" %in% sett_cols) {
          sett_filter <- sett_filter %>%
            dplyr::filter(denominator_target_cohort_name %in%
                            private$.pickers[["denominatorTargetName"]]$inputValues$denominator_target)
        }
        
        sett_ids <- sett_filter %>% dplyr::pull(result_id)
        
        # .cleanData has restored group_level, so filterGroup works
        filtered <- private$.cleanData %>%
          dplyr::filter(result_id %in% sett_ids) %>%
          dplyr::filter(cdm_name %in% private$.pickers[["cdm"]]$inputValues$cdm)
        
        # Filter by outcome via group_level if not filtered via settings
        if (!"outcome_cohort_name" %in% sett_cols) {
          sel_outcomes <- private$.pickers[["outcome"]]$inputValues$outcome
          filtered <- filtered %>%
            dplyr::filter(grepl(
              paste0("(^|&&& )(", paste(sel_outcomes, collapse = "|"), ")$"),
              group_level
            ))
        }
        
        sel_interval <- private$.pickers[["interval"]]$inputValues$interval
        sel_dates <- private$.pickers[["startDate"]]$inputValues$year
        if ("additional_level" %in% names(filtered)) {
          filtered <- filtered %>%
            dplyr::filter(grepl(paste(sel_interval, collapse = "|"), additional_level)) %>%
            dplyr::filter(grepl(paste(sel_dates, collapse = "|"), additional_level))
        }
        
        filtered
      })
      
      summarised_gt_table <- reactive({
        req(summarised_result_data())
        IncidencePrevalence::tablePrevalence(result = summarised_result_data(),
                                             header = private$.pickers[["headerColumn"]]$inputValues$headerColumn,
                                             groupColumn = private$.pickers[["groupColumn"]]$inputValues$groupColumn,
                                             settingsColumn = private$.pickers[["settingsColumn"]]$inputValues$settingsColumn,
                                             hide = private$.pickers[["hideColumn"]]$inputValues$hideColumn,
                                             .options = list(style = "darwin"))
      })
      
      # Tidy table
      output$tidyTable <- gt::render_gt({
        req(summarised_gt_table())
        summarised_gt_table()
      })
      
      # Download tidy table
      output$downloadTidyTable <- downloadHandler(
        filename = function() {
          "Prevalence-Table.docx"
        },
        content = function(file) {
          gt::gtsave(summarised_gt_table(), file)
        }
      )
      
      ### download table ----
      output$downloadTable <- downloadHandler(
        filename = function() {
          "prevalenceEstimatesTable.csv"
        },
        content = function(file) {
          utils::write.csv(getPrevalenceEstimates(), file)
        }
      )
      
      ### table estimates ----
      output$table <- DT::renderDT({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        
        table <- table %>%
          mutate(`prevalence (%)` = paste0(
            prevalence, " (", prevalence_95CI_lower, " to ",
            prevalence_95CI_upper, " )"
          )) %>%
          select(database, outcome_cohort_name, sex,age_group, denominator_age_group, denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, denominator_time_at_risk, analysis_type, analysis_complete_database_intervals, analysis_full_contribution, analysis_min_cell_count, analysis_interval, prevalence_start_date, n_cases, n_population, "prevalence (%)")
        
        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })
      
      ### make plot ----
      plotPrevalenceEstimates <- reactive({
        table <- getPrevalenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("PrevalenceResult", "IncidencePrevalenceResult", class(table))
        
        ribbon_on <- as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon)
        ci_on <- as.logical(private$.pickers[["confInterval"]]$inputValues$confInterval)
        
        plot <- IncidencePrevalence::plotPrevalence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "prevalence",
          line = FALSE,
          point = TRUE,
          ribbon = ribbon_on,
          ymin = "prevalence_95CI_lower",
          ymax = "prevalence_95CI_upper",
          facet = private$.pickers[["facet"]]$inputValues$facet_by,
          colour = private$.pickers[["color"]]$inputValues$color_by
        )
        # With ribbon + CI off: drop ribbon layer and add a line (previous behaviour).
        # With ribbon + CI on: add an explicit line — plotPrevalence(..., line = FALSE) omits it, so only
        # ribbon + points were drawn; geom_line() was only added in the CI-off branch (bug).
        if (!ci_on) {
          plot$layers <- plot$layers[2]
          if (ribbon_on) {
            plot <- plot + ggplot2::geom_line()
          }
        } else if (ribbon_on) {
          plot <- plot + ggplot2::geom_line()
        }
        
        y_fmt <- private$getPrevalenceYAxisFormat(table, ribbon_on = ribbon_on)
        color_by <- private$.pickers[["color"]]$inputValues$color_by
        
        # Reduce duplicate fill/alpha legends in ggplot; filled symbols in colour legend (plotly matches Incidence module).
        plot <- plot + ggplot2::guides(fill = "none", alpha = "none", size = "none") +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(shape = 16, alpha = 1))) +
          ggplot2::scale_y_continuous(
            name = "Prevalence (%)",
            labels = scales::label_percent(accuracy = y_fmt$accuracy)
          )
        if (length(color_by) == 1L && identical(color_by, "database")) {
          plot <- plot + ggplot2::labs(colour = DATA_SOURCE_LABEL)
        }
        
        pl <- plotly::ggplotly(plot, dynamicTicks = TRUE)
        pl <- private$fixGgplotlyLineColours(pl)
        pl <- private$dedupeGgplotlyLegends(pl)
        pl <- plotly::layout(pl, yaxis = list(tickformat = y_fmt$tickformat))
        list(gg = plot, pl = pl)
      })
      
      ### download plot ----
      output$download_plot <- downloadHandler(
        filename = function() {
          "prevalenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            plotPrevalenceEstimates()$gg,
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- renderPlotly({
        plotPrevalenceEstimates()$pl
      })
    },
    assertPrevalenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("prevalence", "prevalence_attrition"))) {
        stop("Cannot assert `Prevalence` result")
      }
    },
    transformData = function(data) {
      # set strata
      strataColumn <- unique(settings(data) %>% dplyr::filter(strata != "reason") %>% dplyr::pull(strata))
      private$.strata <- unique(data %>% dplyr::filter(strata_name != "reason") %>% dplyr::pull(strata_level))
      private$.strata_sex <- case_when(
        strataColumn == "" ~ "overall",
        TRUE ~ c("overall","Female","Male")
      )
      private$.strata_age_group <- case_when(
        strataColumn == "" ~ "overall", 
        TRUE ~ c("overall","1 to 18","19 to 65","66 to 120")
      )
      
      # transform to readable format
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asPrevalenceResult(data) %>%
        
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_cases = outcome_count,
          n_population = denominator_count
        )
      # add strata column
      if (length(private$.strata) == 0 || all(private$.strata == "overall")) {
        data <- data %>% dplyr::mutate(strata = "overall")
      } else {
        # The strata_level column is already in the data, just rename it to strata
        if ("strata_level" %in% names(data)) {
          data <- data %>% dplyr::rename(strata = strata_level)
        } else {
          data <- data %>% dplyr::mutate(strata = "overall")
        }
      }
      
      #NEW ---
      if(strataColumn == ""){
        data <- data %>% dplyr::mutate(sex = "overall",age_group = "overall")
      }
      #\NEW ---
      
      age_grp_levels <- c("overall", "1 to 18", "19 to 65", ">65")
      existing <- intersect(age_grp_levels, unique(data$denominator_age_group))
      data <- data %>%
        dplyr::mutate(denominator_age_group = factor(denominator_age_group, levels = existing))
      
      return(data)
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = DATA_SOURCE_LABEL, choices = unique(private$.tidyData$database), selected = unique(private$.tidyData$database), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace
      
      # outcome
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = unique(private$.tidyData$outcome_cohort_name), selected = unique(private$.tidyData$outcome_cohort_name)[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace
      
      # strata
      # private$.pickers[["strata"]] <- InputPanel$new(
      #   funs = list(strata = shinyWidgets::pickerInput),
      #   args = list(strata = list(
      #     inputId = "strata", label = "Strata", choices = unique(private$.strata), selected = unique(private$.strata), multiple = TRUE,
      #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
      #   )),
      #   growDirection = "horizontal"
      # )
      # private$.pickers[["strata"]]$parentNamespace <- self$namespace
      
      #NEW strata_sex---
      private$.pickers[["strata_sex"]] <- InputPanel$new(
        funs = list(strata_sex = shinyWidgets::pickerInput),
        args = list(strata_sex = list(
          inputId = "strata_sex", label = "Sex", choices = unique(private$.strata_sex), selected = unique(private$.strata_sex), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["strata_sex"]]$parentNamespace <- self$namespace
      
      #NEW strata_age_group---
      private$.pickers[["strata_age_group"]] <- InputPanel$new(
        funs = list(strata_age_group = shinyWidgets::pickerInput),
        args = list(strata_age_group = list(
          inputId = "strata_age_group", label = "Age group", choices = unique(private$.strata_age_group), selected = unique(private$.strata_age_group), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["strata_age_group"]]$parentNamespace <- self$namespace
      #\NEW ----
      
      # Denominator target cohort (when present in IncidencePrevalence prevalence exports)
      if ("denominator_target_cohort_name" %in% names(private$.tidyData)) {
        dtc_choices <- unique(as.character(stats::na.omit(private$.tidyData$denominator_target_cohort_name)))
        if (length(dtc_choices) > 0) {
          private$.pickers[["denominatorTargetName"]] <- InputPanel$new(
            funs = list(denominator_target = shinyWidgets::pickerInput),
            args = list(denominator_target = list(
              inputId = "denominator_target",
              label = "Denominator target cohort",
              choices = dtc_choices,
              selected = dtc_choices,
              multiple = TRUE,
              options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
            )),
            growDirection = "horizontal"
          )
          private$.pickers[["denominatorTargetName"]]$parentNamespace <- self$namespace
        }
      }
      
      # denominator age group
      denom_age_choices <- levels(private$.tidyData$denominator_age_group)
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Age group", choices = denom_age_choices, selected = denom_age_choices, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomAgeGroup"]]$parentNamespace <- self$namespace
      
      # denominator sex
      private$.pickers[["denomSex"]] <- InputPanel$new(
        funs = list(denom_sex = shinyWidgets::pickerInput),
        args = list(denom_sex = list(
          inputId = "denom_sex", choices = unique(private$.tidyData$denominator_sex), label = "Sex", selected = unique(private$.tidyData$denominator_sex), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomSex"]]$parentNamespace <- self$namespace
      
      # prior observation
      private$.pickers[["denomPriorObs"]] <- InputPanel$new(
        funs = list(prior_obs = shinyWidgets::pickerInput),
        args = list(prior_obs = list(
          inputId = "prior_obs", choices = unique(private$.tidyData$denominator_days_prior_observation), label = "Prior observation", selected = unique(private$.tidyData$denominator_days_prior_observation), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomPriorObs"]]$parentNamespace <- self$namespace
      
      # denominator start date
      private$.pickers[["denomStartDate"]] <- InputPanel$new(
        funs = list(start_date = shinyWidgets::pickerInput),
        args = list(start_date = list(
          inputId = "start_date", choices = unique(private$.tidyData$denominator_start_date), label = "Start date", selected = unique(private$.tidyData$denominator_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomStartDate"]]$parentNamespace <- self$namespace
      
      # denominator end date
      private$.pickers[["denomEndDate"]] <- InputPanel$new(
        funs = list(end_date = shinyWidgets::pickerInput),
        args = list(end_date = list(
          inputId = "end_date", choices = unique(private$.tidyData$denominator_end_date), label = "End date", selected = unique(private$.tidyData$denominator_end_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomEndDate"]]$parentNamespace <- self$namespace
      
      # denominator time at risk
      private$.pickers[["denomTimeAtRisk"]] <- InputPanel$new(
        funs = list(time_at_risk = shinyWidgets::pickerInput),
        args = list(time_at_risk = list(
          inputId = "time_at_risk", choices = unique(private$.tidyData$denominator_time_at_risk), label = "Time at risk", selected = unique(private$.tidyData$denominator_time_at_risk), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["denomTimeAtRisk"]]$parentNamespace <- self$namespace
      
      # analysis type
      private$.pickers[["analysisType"]] <- InputPanel$new(
        funs = list(analysis_type = shinyWidgets::pickerInput),
        args = list(analysis_type = list(inputId = "analysis_type", choices = unique(private$.tidyData$analysis_type), label = "Prevalence type", selected = unique(private$.tidyData$analysis_type), multiple = FALSE)),
        growDirection = "horizontal"
      )
      private$.pickers[["analysisType"]]$parentNamespace <- self$namespace
      
      # complete period
      private$.pickers[["completePeriod"]] <- InputPanel$new(
        funs = list(complete_period = shinyWidgets::pickerInput),
        args = list(complete_period = list(
          inputId = "complete_period", choices = unique(private$.tidyData$analysis_complete_database_intervals), label = "Complete period", selected = unique(private$.tidyData$analysis_complete_database_intervals), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["completePeriod"]]$parentNamespace <- self$namespace
      
      # full contribution
      private$.pickers[["fullContribution"]] <- InputPanel$new(
        funs = list(full_contribution = shinyWidgets::pickerInput),
        args = list(full_contribution = list(
          inputId = "full_contribution", choices = unique(private$.tidyData$analysis_full_contribution), label = "Full contribution", selected = unique(private$.tidyData$analysis_full_contribution), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["fullContribution"]]$parentNamespace <- self$namespace
      
      # min counts
      private$.pickers[["minCounts"]] <- InputPanel$new(
        funs = list(min_cell_count = shinyWidgets::pickerInput),
        args = list(min_cell_count = list(
          inputId = "min_cell_count", choices = unique(private$.tidyData$analysis_min_cell_count), label = "Minimum counts", selected = unique(private$.tidyData$analysis_min_cell_count), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["minCounts"]]$parentNamespace <- self$namespace
      
      # interval
      private$.pickers[["interval"]] <- InputPanel$new(
        funs = list(interval = shinyWidgets::pickerInput),
        args = list(interval = list(
          inputId = "interval", choices = unique(private$.tidyData$analysis_interval), label = "Interval", selected = unique(private$.tidyData$analysis_interval)[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["interval"]]$parentNamespace <- self$namespace
      
      # start date
      private$.pickers[["startDate"]] <- InputPanel$new(
        funs = list(year = shinyWidgets::pickerInput),
        args = list(year = list(
          inputId = "year", choices = unique(private$.tidyData$prevalence_start_date), label = "Year", selected = unique(private$.tidyData$prevalence_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace
      
      # plot pickers
      plotDataChoices <- labelDataSourceChoices(c(
        "database", "outcome_cohort_name", "strata",
        if (!is.null(private$.pickers[["denominatorTargetName"]])) "denominator_target_cohort_name",
        "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_complete_database_intervals",
        "analysis_min_cell_count", "analysis_interval", "prevalence_start_date"
      ))
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Incidence_start_date", selected = "prevalence_start_date", multiple = F,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["xAxis"]]$parentNamespace <- self$namespace
      
      # facet by
      private$.pickers[["facet"]] <- InputPanel$new(
        funs = list(facet_by = shinyWidgets::pickerInput),
        args = list(facet_by = list(
          inputId = "facet_by", choices = plotDataChoices, label = "Facet by", selected = c("outcome_cohort_name", "database"), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["facet"]]$parentNamespace <- self$namespace
      
      # color by
      private$.pickers[["color"]] <- InputPanel$new(
        funs = list(color_by = shinyWidgets::pickerInput),
        args = list(color_by = list(
          inputId = "color_by", choices = plotDataChoices, label = "Colour by", selected = c(), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["color"]]$parentNamespace <- self$namespace
      
      # ribbon
      private$.pickers[["ribbon"]] <- InputPanel$new(
        funs = list(ribbon = shinyWidgets::pickerInput),
        args = list(ribbon = list(
          inputId = "ribbon", choices = c(TRUE, FALSE), label = "Ribbon", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["ribbon"]]$parentNamespace <- self$namespace
      
      # confidence interval
      private$.pickers[["confInterval"]] <- InputPanel$new(
        funs = list(confInterval = shinyWidgets::pickerInput),
        args = list(confInterval = list(
          inputId = "confInterval", choices = c(TRUE, FALSE), label = "Confidence interval", selected = TRUE, multiple = FALSE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["confInterval"]]$parentNamespace <- self$namespace
      
      # headerColumn
      headerColumnOptions <- labelDataSourceChoices(c("cdm_name", "estimate_name"))
      private$.pickers[["headerColumn"]] <- InputPanel$new(
        funs = list(headerColumn = shinyWidgets::pickerInput),
        args = list(headerColumn = list(
          inputId = "headerColumn", choices = headerColumnOptions, label = "Header", selected = unname(headerColumnOptions), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["headerColumn"]]$parentNamespace <- self$namespace
      
      # groupColumn
      groupColumnOptions <- labelDataSourceChoices(c("outcome_cohort_name", "cdm_name"))
      private$.pickers[["groupColumn"]] <- InputPanel$new(
        funs = list(groupColumn = shinyWidgets::pickerInput),
        args = list(groupColumn = list(
          inputId = "groupColumn", choices = groupColumnOptions, label = "Group columns", selected = groupColumnOptions[1], multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["groupColumn"]]$parentNamespace <- self$namespace
      
      # settingsColumn
      settingColumnOptions <- c("denominator_time_at_risk", "denominator_age_group", "denominator_sex")
      private$.pickers[["settingsColumn"]] <- InputPanel$new(
        funs = list(settingsColumn = shinyWidgets::pickerInput),
        args = list(settingsColumn = list(
          inputId = "settingsColumn", choices = settingColumnOptions, label = "Settings columns", selected = settingColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["settingsColumn"]]$parentNamespace <- self$namespace
      
      # hideColumn
      hideColumnOptions <- c("denominator_time_at_risk", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "analysis_interval")
      private$.pickers[["hideColumn"]] <- InputPanel$new(
        funs = list(hideColumn = shinyWidgets::pickerInput),
        args = list(hideColumn = list(
          inputId = "hideColumn", choices = hideColumnOptions, label = "Hide columns", selected = hideColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["hideColumn"]]$parentNamespace <- self$namespace
    },
    fixGgplotlyLineColours = function(pl) {
      for (i in seq_along(pl$x$data)) {
        tr <- pl$x$data[[i]]
        col <- tr$marker$color %||% tr$line$color
        if (!is.null(col)) {
          pl$x$data[[i]]$line$color <- col
        }
      }
      pl
    },
    dedupeGgplotlyLegends = function(pl) {
      traces <- pl$x$data
      n <- length(traces)
      
      has_marker <- vapply(seq_len(n), function(i) {
        tr <- traces[[i]]
        !is.null(tr$marker) && (tr$mode %in% c("markers", "lines+markers") ||
                                  (!is.null(tr$type) && tr$type == "scatter" && is.null(tr$fill)))
      }, logical(1))
      
      trace_name <- vapply(seq_len(n), function(i) traces[[i]]$name %||% "", character(1))
      
      # For each unique name, prefer showing legend on a marker trace
      best <- list()
      for (i in seq_len(n)) {
        nm <- trace_name[i]
        if (nm == "") next
        if (is.null(best[[nm]]) || (!has_marker[best[[nm]]] && has_marker[i])) {
          best[[nm]] <- i
        }
      }
      
      for (i in seq_len(n)) {
        nm <- trace_name[i]
        pl$x$data[[i]]$showlegend <- !is.null(best[[nm]]) && best[[nm]] == i
      }
      
      age_order <- c("overall", "1 to 18", "19 to 65", ">65")
      for (i in seq_len(n)) {
        nm <- trace_name[i]
        idx <- which(vapply(age_order, function(a) grepl(a, nm, fixed = TRUE), logical(1)))
        if (length(idx) == 1) {
          pl$x$data[[i]]$legendrank <- idx
        }
      }
      
      pl$x$layout$legend$traceorder <- "normal"
      
      pl
    },
    getPrevalenceYAxisFormat = function(table, ribbon_on = FALSE) {
      y_cols <- c("prevalence")
      if (isTRUE(ribbon_on)) {
        y_cols <- c(y_cols, "prevalence_95CI_lower", "prevalence_95CI_upper")
      }
      y_vals <- unlist(lapply(y_cols, function(col) {
        if (col %in% names(table)) suppressWarnings(as.numeric(table[[col]]))
      }))
      y_max <- max(y_vals, na.rm = TRUE)
      if (!is.finite(y_max) || y_max < 0.01) {
        list(accuracy = 0.0001, tickformat = ".4%")
      } else {
        list(accuracy = 0.01, tickformat = ".2%")
      }
    },
    restoreGroupLevel = function(data) {
      # labelDenominator transforms group_level into:
      #   "denominator_ageGroup:X_sex:Y &&&<outcome_name>"
      # Strip the prepended prefix to recover the outcome name, then
      # rebuild group_level to match the number of parts in group_name.
      data %>% dplyr::mutate(
        .outcome_tail = sub("^.*&&&\\s*", "", group_level),
        .n_name_parts = lengths(strsplit(group_name, " &&& ", fixed = TRUE)),
        group_level = dplyr::case_when(
          .n_name_parts == 1L ~ .outcome_tail,
          .n_name_parts == 2L ~ paste0("None &&& ", .outcome_tail),
          TRUE ~ group_level
        )
      ) %>%
        dplyr::select(-.outcome_tail, -.n_name_parts)
    }
  )
)
