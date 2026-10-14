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

#' @title Incidence Module Class
#'
#' @include ShinyModule.R
#'
#' @description
#' Incidence module that shows incidence results from the IncidencePrevalence package.
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
#'       "dummyData/IncidencePrevalence/1.2.0/incidence.csv"
#'     ))
#'
#'     incMod <- Incidence$new(data = inc)
#'
#'     ui <- shiny::fluidPage(
#'       incMod$UI()
#'     )
#'
#'     server <- function(input, output, session) {
#'       incMod$server(input, output, session)
#'     }
#'
#'     if (interactive()) {
#'       shiny::shinyApp(ui = ui, server = server)
#'     }
#'   }
#' }
#' }
IncidenceCustom <- R6::R6Class(
  classname = "Incidence",
  inherit = DarwinShinyModules::ShinyModule,
  
  # Active ----
  active = list(
    #' @field data (`summarisedResult`) SummarisedResult object from Incidence.
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
      private$assertIncidenceData(data)
      private$.data <- data
      private$.tidyData <- private$transformData(data)
      private$initPickers()
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    .data = NULL,
    .tidyData = NULL,
    .strata = NULL,
    .pickers = NULL,
    .db_palette = NULL,   # <-- AM: add this to allow setting same colors for each database
    .db_levels  = c("NAJS", "DK-DHR", "FinOMOP-THL","IQVIA DA Germany","BIFAP","SIDIAP","CPRD GOLD"), #NULL,   # <-- AM: and this
    .UI = function() {
      shiny::tagList(
        shinydashboard::tabItem(
          tabName = shiny::NS(private$.namespace, "incidence"),
          shiny::h3("Incidence estimates"),
          shiny::p("Incidence estimates are shown below, please select configuration to filter them:"),
          shiny::p("Database, study outcome and strata"),
          private$.pickers[["cdm"]]$UI(),
          private$.pickers[["outcome"]]$UI(),
          private$.pickers[["strata"]]$UI(),
          #private$.pickers[["wound_type"]]$UI(), # AM wound type ----
          p("Denominator population settings"),
          private$.pickers[["denomAgeGroup"]]$UI(),
          private$.pickers[["denomSex"]]$UI(),
          private$.pickers[["denomPriorObs"]]$UI(),
          private$.pickers[["denomStartDate"]]$UI(),
          private$.pickers[["denomEndDate"]]$UI(),
          private$.pickers[["denomTimeAtRisk"]]$UI(),
          p("Analysis settings"),
          private$.pickers[["washout"]]$UI(),
          private$.pickers[["repeatedEvents"]]$UI(),
          private$.pickers[["completePeriod"]]$UI(),
          private$.pickers[["minCounts"]]$UI(),
          p("Dates"),
          private$.pickers[["interval"]]$UI(),
          private$.pickers[["startDate"]]$UI(),
          shiny::tabsetPanel(
            id = shiny::NS(private$.namespace, "tabsetPanel"),
            type = "tabs",
            shiny::tabPanel(
              "Tidy table",
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
      
      # AM: Assign colors to databases: ----
      # Build stable levels and palette from the full tidy data (not filtered)
      # private$.db_levels <- c("NAJS","InGef-RDB","IPCI","BIFAP","CPRD-GOLD") #sort(unique(private$.tidyData$database))
      
      # Pick a deterministic set of colours of the right length
      base_cols <- scales::hue_pal(l = 60, c = 100)(length(private$.db_levels))
      
      # Name the palette by database
      private$.db_palette <- stats::setNames(base_cols, private$.db_levels)
      
      # Incidence
      getIncidenceEstimates <- reactive({
        result <- private$.tidyData %>%
          dplyr::filter(database %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          dplyr::filter(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome) %>%
          # dplyr::filter(wound_type %in% private$.pickers[["wound_type"]]$inputValues$wound_type) %>% ## AM wound type filter ---
          dplyr::filter(strata %in% private$.pickers[["strata"]]$inputValues$strata) %>%
          dplyr::filter(denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group) %>%
          dplyr::filter(denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex) %>%
          dplyr::filter(denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs) %>%
          dplyr::filter(denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date) %>%
          dplyr::filter(denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date) %>%
          dplyr::filter(denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          dplyr::filter(analysis_outcome_washout %in% private$.pickers[["washout"]]$inputValues$washout) %>%
          dplyr::filter(analysis_repeated_events %in% private$.pickers[["repeatedEvents"]]$inputValues$repeated_events) %>%
          dplyr::filter(analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period) %>%
          dplyr::filter(analysis_min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count) %>%
          dplyr::filter(analysis_interval %in% private$.pickers[["interval"]]$inputValues$interval) %>%
          dplyr::filter(incidence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) #%>%
        
        
        # AM: Modified to include Incidence by 1000 pys: ---------------
        result <- result %>% 
          mutate(incidence_1000_pys = as.numeric(incidence_100000_pys)/100,
                 incidence_1000_pys_95CI_lower = incidence_100000_pys_95CI_lower/100,
                 incidence_1000_pys_95CI_upper = incidence_100000_pys_95CI_upper/100) %>% 
          
          dplyr::mutate(
            person_years = round(suppressWarnings(as.numeric(person_years))),
            person_days = round(suppressWarnings(as.numeric(person_days))),
            n_events = round(suppressWarnings(as.numeric(n_events))),
            
            incidence_1000_pys = round(suppressWarnings(as.numeric(incidence_1000_pys)),digits = 10),
            incidence_1000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_1000_pys_95CI_lower)),digits = 10),
            incidence_1000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_1000_pys_95CI_upper)),digits = 10),
            
            incidence_100000_pys = round(suppressWarnings(as.numeric(incidence_100000_pys)),digits = 10),
            incidence_100000_pys_95CI_lower = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_lower)),digits = 10),
            incidence_100000_pys_95CI_upper = round(suppressWarnings(as.numeric(incidence_100000_pys_95CI_upper)),digits = 10)
          )
        return(result)
      })
      
      # Filtered data
      summarised_result_data <- reactive({
        
        
        
        private$.data %>%
          dplyr::filter(
            cdm_name %in% private$.pickers[["cdm"]]$inputValues$cdm) %>%
          omopgenerics::filterSettings(
            analysis_repeated_events %in% private$.pickers[["repeatedEvents"]]$inputValues$repeated_events,
            analysis_outcome_washout %in% private$.pickers[["washout"]]$inputValues$washout,
            analysis_complete_database_intervals %in% private$.pickers[["completePeriod"]]$inputValues$complete_period,
            min_cell_count %in% private$.pickers[["minCounts"]]$inputValues$min_cell_count,
            denominator_start_date %in% private$.pickers[["denomStartDate"]]$inputValues$start_date,
            denominator_end_date %in% private$.pickers[["denomEndDate"]]$inputValues$end_date,
            denominator_days_prior_observation %in% private$.pickers[["denomPriorObs"]]$inputValues$prior_obs,
            denominator_sex %in% private$.pickers[["denomSex"]]$inputValues$denom_sex,
            denominator_age_group %in% private$.pickers[["denomAgeGroup"]]$inputValues$age_group,
            denominator_time_at_risk %in% private$.pickers[["denomTimeAtRisk"]]$inputValues$time_at_risk) %>%
          omopgenerics::filterAdditional(analysis_interval == private$.pickers[["interval"]]$inputValues$interval,
                                         incidence_start_date %in% private$.pickers[["startDate"]]$inputValues$year) %>%
          omopgenerics::filterGroup(outcome_cohort_name %in% private$.pickers[["outcome"]]$inputValues$outcome)
      })
      
      
      # AM: Custom table incidence function (report by 1,000 pys) -----
      tableIncidenceCustom <- function(result,
                                       type = "gt",
                                       header = c("estimate_name"),
                                       groupColumn = c("cdm_name","outcome_cohort_name","strata_level"),
                                       settingsColumn = c("denominator_age_group", "denominator_sex"),
                                       hide = c("denominator_cohort_name", "analysis_interval"),
                                       style = "default", .options = list())
      {
        rlang::check_installed("visOmopResults", version = "1.0.2")
        IncidencePrevalence:::tableInternal(result = result,
                                            formatEstimateName =
                                              c(`Denominator (N)` = "<denominator_count>",
                                                `Person-years` = "<person_years>",
                                                `Outcome (N)` = "<outcome_count>",
                                                `Incidence 1,000 person-years [95% CI]` = "<incidence_100000_pys> (<incidence_100000_pys_95CI_lower> -\n      <incidence_100000_pys_95CI_upper>)"), #Original column names incidence_100000_pys will be kept after adjusting their values to 1,000 pys
                                            header = header, groupColumn = groupColumn, type = type,
                                            hide = hide, settingsColumn = settingsColumn, resultType = "incidence",
                                            style = style, .options = .options)
      }
      
      # TABLE
      summarised_gt_table <- reactive({
        req(summarised_result_data())
        
        # AM  NEW -------------------------------------------------------------------------
        #  include the steps below to convert to 1000 pys from the inc summarised results table
        summarised_result <- summarised_result_data() 
        summarised_result <- summarised_result %>% mutate(estimate_value = as.numeric(estimate_value))
        summarised_result <- summarised_result %>%
          mutate(estimate_value =  ifelse(estimate_name %in% c("incidence_100000_pys","incidence_100000_pys_95CI_lower","incidence_100000_pys_95CI_upper"),
                                          estimate_value/100,estimate_value)) # Original column names will be kept because plotIncidenceEstimates only accepts the incidence_100000_pys column
        # Order data partners based on pre-defined order:
        db_levels  <- private$.db_levels
        summarised_result <- summarised_result %>% dplyr::mutate(cdm_name = factor(cdm_name, levels = db_levels))
        
        # -------------------------------------------------------------------------
        
        
        # IncidencePrevalence::tableIncidence(result = summarised_result_data(),
        tableIncidenceCustom(result = summarised_result, #AM
                             header = private$.pickers[["headerColumn"]]$inputValues$headerColumn,
                             groupColumn = private$.pickers[["groupColumn"]]$inputValues$groupColumn,
                             settingsColumn = private$.pickers[["settingColumn"]]$inputValues$settingColumn,
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
          "Incidence-Table.docx"
        },
        content = function(file) {
          gt::gtsave(summarised_gt_table(), file)
        }
      )
      
      ### download table ----
      output$downloadTable <- downloadHandler(
        filename = function() {
          "incidenceEstimatesTable.csv"
        },
        content = function(file) {
          utils::write.csv(getIncidenceEstimates(), file)
        }
      )
      
      ### table estimates ----
      output$table <- DT::renderDT({
        table <- getIncidenceEstimates()
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        
        table <- table %>%
          
          # AM NEW --------------------------------------------------
        # Format Incidence 1000 pys and CIs for table -------
        mutate(incidence_1000_pys = paste0(
          incidence_1000_pys, " (", incidence_1000_pys_95CI_lower, " to ",
          incidence_1000_pys_95CI_upper, " )"
        )) %>%
          
          mutate(incidence_100000_pys = paste0(
            incidence_100000_pys, " (", incidence_100000_pys_95CI_lower, " to ",
            incidence_100000_pys_95CI_upper, " )"
          )) %>%
          select(database, outcome_cohort_name, strata, denominator_cohort_name, denominator_age_group, 
                 denominator_sex, denominator_days_prior_observation, denominator_start_date, denominator_end_date, 
                 denominator_time_at_risk, analysis_outcome_washout, analysis_repeated_events,
                 analysis_complete_database_intervals, analysis_min_cell_count, analysis_interval, 
                 incidence_start_date, n_events, n_persons, person_years,
                 incidence_1000_pys, #AM: include 1000 pys columns
                 incidence_100000_pys
          )
        
        # -------------------------------------------------------------------------
        
        
        DT::datatable(
          table,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(scrollX = TRUE, scrollCollapse = TRUE)
        )
      })
      
      #_________________________________________________________________________________________________________
      # AM: Custom function for improved legend color render in Plotly --------
      #_________________________________________________________________________________________________________       
      #### AM: filterLegends function (handle extra fill legends when rendering ggplot in plotly)
      `%||%` <- function(x, y) if (is.null(x)) y else x # Helper: Include coalesce operator
      
      
      filterLegends <- function(p) {
        pp <- plotly::ggplotly(p, tooltip = "text") # This works as long as you set a global aesthetic on the plot e.g. plot <- plot + aes(text = tooltip_txt) where 'tooltip_txt' is a custom column with the things you'd like to display
        
        # Small helper: get a scalar color (some traces store a vector)
        first_val <- function(v) {
          if (is.null(v)) return(NULL)
          if (is.list(v)) v <- unlist(v, use.names = FALSE)
          if (length(v) >= 1) return(v[[1]])
          NULL
        }
        
        # Build a grouping key for legend dedupe: prefer legendgroup, else name
        key_for <- function(tr) {
          lg <- tr$legendgroup %||% ""
          nm <- tr$name %||% ""
          if (nzchar(lg)) lg else nm
        }
        
        n <- length(pp$x$data)
        keys <- vapply(pp$x$data, key_for, character(1))
        groups <- split(seq_len(n), keys)
        
        # Process each legend group independently
        for (g in names(groups)) {
          idx_all <- groups[[g]]
          if (length(idx_all) == 0) next
          
          # 1) Hide everything initially
          for (i in idx_all) pp$x$data[[i]]$showlegend <- FALSE
          
          # 2) Identify ribbons (fill/fillcolor) and non-ribbons
          is_ribbon <- function(i) !is.null(pp$x$data[[i]]$fill) || !is.null(pp$x$data[[i]]$fillcolor)
          ribbons <- Filter(is_ribbon, idx_all)
          non_ribbons <- setdiff(idx_all, ribbons)
          
          # 3) Pick a representative among non-ribbons: prefer markers, then lines, else first
          is_markers <- function(i) grepl("markers", pp$x$data[[i]]$mode %||% "")
          is_lines   <- function(i) grepl("lines",   pp$x$data[[i]]$mode %||% "")
          cand_mark  <- Filter(is_markers, non_ribbons)
          cand_line  <- Filter(is_lines,   non_ribbons)
          rep <- if (length(cand_mark)) cand_mark[[1]] else if (length(cand_line)) cand_line[[1]] else if (length(non_ribbons)) non_ribbons[[1]] else idx_all[[1]]
          
          # 4) Ensure a name
          if (!nzchar(pp$x$data[[rep]]$name %||% "")) {
            if (nzchar(g)) {
              pp$x$data[[rep]]$name <- g
            }
          }
          
          # 5) Ensure a visible color on the representative
          # Source priority: marker.color -> line.color -> (fallback) any ribbon fillcolor
          col <- first_val(pp$x$data[[rep]]$marker$color) %||%
            first_val(pp$x$data[[rep]]$line$color)
          
          if (is.null(col) && length(ribbons)) {
            # harvest a color from the group's ribbon (use the first)
            col <- first_val(pp$x$data[[ribbons[[1]]]]$fillcolor)
          }
          
          # Clean up transparent values
          if (!is.null(col) && (identical(col, "transparent") || grepl("rgba\\(0\\s*,\\s*0\\s*,\\s*0\\s*,\\s*0\\)", col))) {
            col <- NULL
          }
          
          # Apply the color + enforce filled legend glyph
          mode_rep <- pp$x$data[[rep]]$mode %||% ""
          if (!is.null(col)) {
            if (grepl("markers", mode_rep)) {
              pp$x$data[[rep]]$marker$color <- col
              pp$x$data[[rep]]$marker$symbol <- "circle"
              pp$x$data[[rep]]$marker$opacity <- 1
              pp$x$data[[rep]]$marker$line$width <- 0
              if (is.null(pp$x$data[[rep]]$marker$size)) pp$x$data[[rep]]$marker$size <- 6
            } else {
              pp$x$data[[rep]]$line$color <- col
              pp$x$data[[rep]]$line$width <- pp$x$data[[rep]]$line$width %||% 2
            }
          } else {
            # last resort: pick a default dark grey so we never get a blank swatch
            default_col <- "rgba(80,80,80,1)"
            if (grepl("markers", mode_rep)) {
              pp$x$data[[rep]]$marker$color <- default_col
              pp$x$data[[rep]]$marker$symbol <- "circle"
              pp$x$data[[rep]]$marker$opacity <- 1
              pp$x$data[[rep]]$marker$line$width <- 0
            } else {
              pp$x$data[[rep]]$line$color <- default_col
              pp$x$data[[rep]]$line$width <- pp$x$data[[rep]]$line$width %||% 2
            }
          }
          
          # 6) Finally show only the representative for this group
          pp$x$data[[rep]]$showlegend <- TRUE
          # keep ribbons hidden
          for (i in ribbons) pp$x$data[[i]]$showlegend <- FALSE
        }
        
        pp
      }
      #_________________________________________________________________________________________________________     
      
      
      ### make plot ----
      plotIncidenceEstimates <- reactive({
        table <- getIncidenceEstimates()
        
        # AM Note: It is needed to assign the values of 1000 pys to the 1000000 for plotIncidenceEstimates to work:
        # Error in `IncidencePrevalence::plotIncidence()`:
        # `y` a choice between: incidence_100000_pys, outcome_count, denominator_count, and person_days.
        # ! `y` must be a choice between: incidence_100000_pys, outcome_count, denominator_count, and person_days; 
        # it can not contain NA; it can not be NULL.
        # Overwrite incidence_100000_pys column with incidence_1000_pys for plotIncidence to work:
        table <- table %>% 
          mutate(incidence_100000_pys = incidence_1000_pys,
                 incidence_100000_pys_95CI_lower = incidence_1000_pys_95CI_lower,
                 incidence_100000_pys_95CI_upper = incidence_1000_pys_95CI_upper
          )
        
        shiny::validate(need(nrow(table) > 0, "No results for selected inputs"))
        class(table) <- c("IncidenceResult", "IncidencePrevalenceResult", class(table))
        
        # AM - NEW ---------------------------------------------
        # Read database names
        db_levels  <- private$.db_levels
        db_palette <- private$.db_palette
        
        # Lock factor levels for stable mapping (keep specific colors for a defined data partner order)
        table <- table %>% dplyr::mutate(database = factor(database, levels = db_levels))
        
        # Determine colour variable
        color_var <- private$.pickers[["color"]]$inputValues$color_by
        if (length(color_var) == 0) color_var <- "database"
        #---------------------------------------------
        #AM NEW: include tooltip column for custom tooltip on plotly
        table <- table %>% mutate(
          tooltip_txt = paste0(
            "database: ", database, "\n",
            "outcome_cohort_name: ",outcome_cohort_name,"\n",
            "denominator_cohort_name: ",denominator_cohort_name, "\n",
            "incidence start date: ", format(incidence_start_date, "%Y-%m-%d"), "\n",
            "incidence end date: ",   format(incidence_end_date,   "%Y-%m-%d"), "\n",
            "incidence 1,000 pys: ",scales::comma(incidence_1000_pys, accuracy = 0.01), "\n",
            "95% CI: ",scales::comma(incidence_1000_pys_95CI_lower, accuracy = 0.01), " – ",
            scales::comma(incidence_1000_pys_95CI_upper, accuracy = 0.01), "\n",
            "person_years: ",person_years,"\n"
          )
        )
        # -------------------------------------------------------------------------
        
        
        plot <- IncidencePrevalence::plotIncidence(
          result = table,
          x = private$.pickers[["xAxis"]]$inputValues$xAxis,
          y = "incidence_100000_pys",
          line = FALSE,
          point = TRUE,
          ribbon = as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon),
          ymin = "incidence_100000_pys_95CI_lower",
          ymax = "incidence_100000_pys_95CI_upper",
          facet = private$.pickers[["facet"]]$inputValues$facet_by,
          colour = color_var #private$.pickers[["color"]]$inputValues$color_by
        ) +
          
          #AM NEW -------------------------------------------------
        # Apply palette only when colouring by database
        if (identical(color_var, "database")) {
          plot <- plot + 
            ggplot2::scale_colour_manual(values = db_palette,limits = db_levels, drop   = FALSE) + # keep absent dbs in legend for stability 
            ggplot2::scale_fill_manual(values   = db_palette, limits = db_levels, drop = FALSE) # Do the same for ribbon fill colors
        } else {
          # Generic fallback for other colour variables
          plot <- plot + 
            ggplot2::scale_colour_viridis_d(option = "D") +
            ggplot2::scale_fill_viridis_d(option   = "D")
          
        }
        # -------------------------------------------------------------------------
        
        # remove confidence interval
        if (!as.logical(private$.pickers[["confInterval"]]$inputValues$confInterval)) {
          plot$layers <- plot$layers[2]
          if (as.logical(private$.pickers[["ribbon"]]$inputValues$ribbon)) {
            plot <- plot + ggplot2::geom_line()
          }
        }
        plot <- plot + labs(y="Incidence (1,000 person-years)") #AM
        plot <- plot + ylim(c(0,NA))
        
        #----------------------------------------------------------------------------------
        # AM: modify guide legend parameters to allow proper color rendering in plotly -----
        # When CI (and/or ribbon) is on, ggplotly() emits multiple traces per group
        # Hide fill-based legends to avoid ribbon entries
        plot <- plot + ggplot2::guides(fill = "none", alpha = "none", size = "none")
        # (Optional) make sure legend uses filled symbols
        plot <- plot + ggplot2::guides(
          colour = ggplot2::guide_legend(override.aes = list(shape = 16, alpha = 1)) #solid filled symbols in legend
        )
        
        # -------------------------------------------------------------------------  
        # AM: Important: Add a global text aesthetic so every existing layer carries the tooltip
        plot <- plot + aes(text = tooltip_txt)
        # -------------------------------------------------------------------------
        # AM: Filter extra fill legends when rendering in plotly -----
        plot_plotly <-  filterLegends(plot)
        # Because filterLegends already returns a plotly object, we can return a list with ggplot and plotly objects 
        # for rendering and saving, respectively
        list(gg = plot, pl = plot_plotly)
        #----------------------------------------------------------------------
        #plot
      })
      
      ### download plot ----
      output$download_plot <- downloadHandler(
        filename = function() {
          "incidenceEstimatesPlot.png"
        },
        content = function(file) {
          ggplot2::ggsave(
            file,
            # plotIncidenceEstimates(),
            plotIncidenceEstimates()$gg,  # pass the ggplot here (included change after using custom filterLegends)
            width = as.numeric(input$download_width),
            height = as.numeric(input$download_height),
            dpi = as.numeric(input$download_dpi),
            units = "cm"
          )
        }
      )
      ### plot ----
      output$plot <- renderPlotly({
        # plotIncidenceEstimates()
        plotIncidenceEstimates()$pl # get plotly object (included change after using custom filterLegens function)
      })
    },
    assertIncidenceData = function(data) {
      resSettings <- attr(data, "settings")
      if (is.null(resSettings)) {
        stop("Data does not appear to be a result object of `IncidencePrevalence`")
      }
      if (!all(resSettings$result_type %in% c("incidence", "incidence_attrition"))) {
        stop("Cannot assert `Incidence` result")
      }
    },
    transformData = function(data) {
      # set strata
      strataColumn <- unique(settings(data) %>% dplyr::filter(strata != "reason") %>% dplyr::pull(strata))
      private$.strata <- unique(data %>% dplyr::filter(strata_name != "reason") %>% dplyr::pull(strata_level))
      
      # transform to readable format
      minCellCount <- attr(data, "settings") %>%
        dplyr::pull(min_cell_count) %>%
        unique()
      data <- IncidencePrevalence::asIncidenceResult(data) %>%
        { if (!"analysis_interval" %in% names(.)) dplyr::mutate(., analysis_interval = "overall") else .} %>%
        dplyr::mutate(analysis_min_cell_count = !!minCellCount) %>%
        dplyr::rename(
          database = cdm_name,
          n_events = outcome_count,
          n_persons = denominator_count
        )
      # AM: Fix label for analysis outcome whashout (365 TIG; 180 TPW) all the inc table had overwritten the 365 value after combining TIG and TPW objects with omopgenerics::bind
      # data <- data %>% mutate(analysis_outcome_washout = ifelse(grepl("tpw",outcome_cohort_name),"180","365"))
      # add strata column
      if (strataColumn == "") {
        data <- data %>% dplyr::mutate(strata = "overall")
      } else {
        data <- data %>% dplyr::rename(strata = strataColumn)
      }
      return(data)
    },
    initPickers = function() {
      # cdm
      private$.pickers[["cdm"]] <- InputPanel$new(
        funs = list(cdm = shinyWidgets::pickerInput),
        args = list(cdm = list(
          inputId = "cdm", label = "Database", choices = unique(private$.tidyData$database), selected = unique(private$.tidyData$database), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["cdm"]]$parentNamespace <- self$namespace
      
      # outcome
      private$.pickers[["outcome"]] <- InputPanel$new(
        funs = list(outcome = shinyWidgets::pickerInput),
        args = list(outcome = list(
          inputId = "outcome", label = "Outcome", choices = unique(private$.tidyData$outcome_cohort_name), selected = unique(private$.tidyData$outcome_cohort_name), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["outcome"]]$parentNamespace <- self$namespace
      
      
      # AM: Include type of wound column -----------------------------------------------------------
      # private$.pickers[["wound_type"]] <- InputPanel$new(
      #   funs = list(outcome = shinyWidgets::pickerInput),
      #   args = list(outcome = list(
      #     inputId = "wound_type", label = "Type of wound", choices = unique(private$.tidyData$wound_type), selected = unique(private$.tidyData$wound_type), multiple = TRUE,
      #     options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
      #   )),
      #   growDirection = "horizontal"
      # )
      # private$.pickers[["outcome"]]$parentNamespace <- self$namespace
      
      # strata
      private$.pickers[["strata"]] <- InputPanel$new(
        funs = list(strata = shinyWidgets::pickerInput),
        args = list(strata = list(
          inputId = "strata", label = "Strata", choices = unique(private$.strata), selected = unique(private$.strata), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["strata"]]$parentNamespace <- self$namespace
      
      # denominator age group
      private$.pickers[["denomAgeGroup"]] <- InputPanel$new(
        funs = list(age_group = shinyWidgets::pickerInput),
        args = list(age_group = list(
          inputId = "age_group", label = "Age group", choices = unique(private$.tidyData$denominator_age_group), selected = unique(private$.tidyData$denominator_age_group), multiple = TRUE,
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
      
      # washout
      private$.pickers[["washout"]] <- InputPanel$new(
        funs = list(washout = shinyWidgets::pickerInput),
        args = list(washout = list(
          inputId = "washout", choices = unique(private$.tidyData$analysis_outcome_washout), label = "Outcome washout", selected = unique(private$.tidyData$analysis_outcome_washout), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["washout"]]$parentNamespace <- self$namespace
      
      # repeated events
      private$.pickers[["repeatedEvents"]] <- InputPanel$new(
        funs = list(repeated_events = shinyWidgets::pickerInput),
        args = list(repeated_events = list(
          inputId = "repeated_events", choices = unique(private$.tidyData$analysis_repeated_events), label = "Repeated events", selected = unique(private$.tidyData$analysis_repeated_events), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["repeatedEvents"]]$parentNamespace <- self$namespace
      
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
          inputId = "year", choices = unique(private$.tidyData$incidence_start_date), label = "Incidence Start Date", selected = unique(private$.tidyData$incidence_start_date), multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["startDate"]]$parentNamespace <- self$namespace
      
      # plot pickers
      plotDataChoices <- c(
        "database", "outcome_cohort_name", "strata", "denominator_cohort_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
        "denominator_start_date", "denominator_end_date", "denominator_time_at_risk", "analysis_outcome_washout", "analysis_repeated_events",
        "analysis_complete_database_intervals", "analysis_min_cell_count", "analysis_interval", "incidence_start_date"
      )
      # x-axis
      private$.pickers[["xAxis"]] <- InputPanel$new(
        funs = list(xAxis = shinyWidgets::pickerInput),
        args = list(xAxis = list(
          inputId = "xAxis", choices = plotDataChoices, label = "Incidence_start_date", selected = "incidence_start_date", multiple = F,
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
      headerColumnOptions <- c("cdm_name", "estimate_name")
      private$.pickers[["headerColumn"]] <- InputPanel$new(
        funs = list(headerColumn = shinyWidgets::pickerInput),
        args = list(headerColumn = list(
          inputId = "headerColumn", choices = headerColumnOptions, label = "Header", selected = headerColumnOptions, multiple = TRUE,
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3")
        )),
        growDirection = "horizontal"
      )
      private$.pickers[["headerColumn"]]$parentNamespace <- self$namespace
      
      # groupColumn
      groupColumnOptions <- c("outcome_cohort_name")
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
    }
  )
)
