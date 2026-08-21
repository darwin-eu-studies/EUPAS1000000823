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

#' @title TreatmentPathways
#'
#' @description
#' Module that displays the Treatment Pathways from the `TreatmentPatterns` package.
#'
#' @export
#'
#' @examples{
#'   if (interactive()) {
#'     library(DarwinShinyModules)
#'
#'     tpr <- TreatmentPatterns::TreatmentPatternsResults$new(
#'       filePath = system.file(package = "DarwinShinyModules", "dummyData/TreatmentPatterns/3.0.0/")
#'     )
#'
#'     treatmentPathways <- TreatmentPathways$new(
#'       treatmentPathways = tpr$treatment_pathways,
#'       cdmSourceInfo = tpr$cdm_source_info
#'     )
#'
#'     preview(treatmentPathways)
#'   }
#' }
#'
TreatmentPathwaysCustom <- R6::R6Class(
  classname = "TreatmentPathways",
  inherit = ShinyModule,
  
  # Active ----
  active = list(
    #' @field colours (`list`) Hex colour values used in the Sunburst Plots and
    #' Sankey Diagrams.
    colours = function() {
      return(private$.colours)
    },
    
    #' @field sunburst (`PlotWidget`) Module.
    sunburst = function() {
      return(private$.sunburst)
    },
    
    #' @field sankey (`PlotWidget`) Module.
    sankey = function() {
      return(private$.sankey)
    },
    
    #' @field inputPanel (`InputPanel`) Module.
    inputPanel = function() {
      return(private$.inputPanel)
    },
    
    #' @field tableCombination Table displaying the `treatment_pathways` csv-file (drug combination).
    tableCombination = function() {
      return(private$.tableCombination)
    },
    
    #' @field tablePathway Table displaying the `treatment_pathways` csv-file (pathway).
    tablePathway = function() {
      return(private$.tablePathway)
    },
    
    #' @field sunburstOverview (`list`) Containing Sunburst `PlotWidget` modules.
    sunburstOverview = function() {
      return(private$.sunburstOverview)
    },
    
    #' @field treatmentPathways (`data.frame`)
    treatmentPathways = function(treatmentPathways) {
      if (missing(treatmentPathways)) {
        return(private$.treatmentPathways)
      } else {
        private$.treatmentPathways <- treatmentPathways
      }
    },
    
    #' @field cdmSourceInfo (`data.frame`)
    cdmSourceInfo = function(cdmSourceInfo) {
      if (missing(cdmSourceInfo)) {
        return(private$.cdmSourceInfo)
      } else {
        private$.cdmSourceInfo <- cdmSourceInfo
      }
    }
  ),
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param treatmentPathways (`data.frame`) `treatment_pathways` field from the `TreatmentPatternsResult` object.
    #' @param cdmSourceInfo (`data.frame`) `cdm_source_info` field from the `TreatmentPatternsResult` object.
    #' @param ... Additional parameters to set fields from the `ShinyModule` parent.
    #'
    #' @return `self`
    initialize = function(treatmentPathways, cdmSourceInfo, ...) {
      super$initialize(...)
      private$assertInstall("TreatmentPatterns", "3.0.0")
      private$.treatmentPathways <- treatmentPathways
      private$.cdmSourceInfo <- cdmSourceInfo
      
      private$setColours()
      private$initSankey()
      private$initSunburst()
      private$initInputPanel()
      private$initTableCombination()
      private$initTablePathway()
      
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .colours = NULL,
    .treatmentPathways = NULL,
    .cdmSourceInfo = NULL,
    
    ## NEW Helper: Canonical label cleaning ----
    # clean_labels = function(x) {
    #   x <- gsub("_event_\\d+", "", x)   # remove event suffix
    #   x <- gsub("_", ",", x)            # replace underscores with commas
    #   trimws(x)
    # },
    
    ## Nested Modules ----
    .sunburst = NULL,
    .sankey = NULL,
    .inputPanel = NULL,
    .tableCombination = NULL,
    .tablePathway = NULL,
    .sunburstOverview = list(),
    
    ## Overrided ----
    .UI = function() {
      shiny::fluidPage(
        
        # White background for exandable bslib cards
        shiny::tags$style(HTML("

/* Fullscreen card background */
.bslib-card-fullscreen {
  background-color: #ffffff !important; /* Solid white */
  opacity: 1 !important;               /* Remove transparency */
}

/* Remove overlay dim effect completely */
.shiny-card-fullscreen-overlay {
  background-color: #ffffff !important; /* White instead of semi-transparent */
  opacity: 1 !important;               /* Ensure no dimming */
  backdrop-filter: none !important;    /* Disable blur/transparency */
  filter: none !important;             /* Remove any filter effects */
    ")),
        
        
        shiny::column(
          width = 1,#2,
          private$.inputPanel$UI()
        ),
        shiny::column(
          width = 11, #10,
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = "Overview",
              shiny::htmlOutput(outputId = shiny::NS(self$namespace, id = "overview"))
            ),
            shiny::tabPanel(
              title = "Detail",
              shiny::tabsetPanel(
                
                shiny::tabPanel(
                  title = "Sunburst",
                  shiny::div(
                    id = shiny::NS(self$namespace, "tpDetailSunburst"),
                    {
                      id <- shiny::NS(self$namespace, "tpDetailSunburst")
                      shiny::tags$style(shiny::HTML(sprintf("
    /* Make the whole sunburst section taller and use the horizontal space */
    #%s {
      min-height: 100vh;        /* <-- increase the 'box' height */
      width: 100%%;            /* <-- ensure full width of its column */
    }

    /* (existing flex layout) */
    #%s .tp-sunburst-wrap {
      display: flex;
      flex-direction: row;
      align-items: flex-start;
      gap: 20px; /*32px*/
      width: 100%%;
    }
    #%s .tp-sunburst-plot {
      /* flex: 3; */ /* Make plot take 3x space */
      flex: 1 1 auto; /* flex: 1 1 auto;*/
      min-width: 100;
      min-height: 100vh;
      margin: 0 !important;      /* new */
      padding: 0 !important;     /* new */
      /* Let the plot area be tall; htmlwidget will fill it */
      /*min-height: 300vh;*/
    }
    #%s .tp-sunburst-legend {
      flex: 1;
      /* flex: 0 0 150px;*/           /* was 340px */
      max-width: 32%%;           /* was 40%% */
      margin: 0 !important;      /* new */
      padding-left: -30px;         /* small spacer */
      overflow: auto;
    }

    /* larger legend text */
    #%s .sunburst-legend text,
    #%s g.legend text,
    #%s .legend text {
      font-size: 18px !important;
    }
  ", id, id, id, id, id, id, id)))
                      
                      
                      # 2) switch to a vertical layout
                      #                 shiny::tags$style(shiny::HTML(sprintf("
                      #   /* Switch wrap to column so legend sits under the plot */
                      #   #%s .tp-sunburst-wrap { display: flex; flex-direction: column; }
                      # 
                      #   /* Plot first, tall */
                      #   #%s .tp-sunburst-plot  { order: 1; min-height: 70vh; min-width: 100%%; }
                      # 
                      #   /* Legend second, full width */
                      #   #%s .tp-sunburst-legend { order: 2; align-self: stretch; max-width: 100%%; }
                      # ", id, id, id)))
                      
                      
                    },
                    # Content
                    shiny::div(
                      full_screen = TRUE,  
                      class = "tp-sunburst-wrap",
                      shiny::div(class = "tp-sunburst-plot",   private$.sunburst$UI()),
                      shiny::div(class = "tp-sunburst-legend")
                    )#,
                    #shiny::downloadButton("download_sunburst", "Download Sunburst (PNG)")
                  )
                )
                ,
               
                shiny::tabPanel(
                  title = "Sankey",
                  # AM: added shiny div to adapt style for overflow ----
                  shiny::div(
                    style = "overflow-x: auto; width: 100%;",
                    private$.sankey$UI() # this is the original line
                  )
                )
              ),
              #private$.tableCombination$UI(),
              
              # NEW: Split table into Drug Combination and Pathway tables:
                private$.tableCombination$UI(),
                shiny::div(style = "margin-top: 30px;"),  # Spacer: Adjust px as needed
                private$.tablePathway$UI()
              #\NEW ---
            )
          )
        )
      )
    },
    .server = function(input, output, session) {
      private$.inputPanel$server(input, output, session)
      private$.tableCombination$server(input, output, session)
      private$.tablePathway$server(input, output, session)
      
      cdmSourceInfo <- DarwinShinyModules:::renameDatabases(private$.cdmSourceInfo)
      
      private$updatePickers(cdmSourceInfo, session)
      
      shiny::observeEvent(
        list(
          private$.inputPanel$inputValues$database,
          private$.inputPanel$inputValues$targetCohort,
          private$.inputPanel$inputValues$age,
          private$.inputPanel$inputValues$sex,
          private$.inputPanel$inputValues$indexYear,
          private$.inputPanel$inputValues$minFreq,
          private$.inputPanel$inputValues$groupCombinations,
          #AM inputs for sankey diagram -----
          private$.inputPanel$inputValues$sankey_fontSize,
          private$.inputPanel$inputValues$sankey_nodeWidth,
          private$.inputPanel$inputValues$sankey_height,
          private$.inputPanel$inputValues$sankey_width
        ),
        {
          private$.sunburst$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations
          private$.sankey$args$groupCombinations <- private$.inputPanel$inputValues$groupCombinations
          
          overviewDat <- private$filterOverview(cdmSourceInfo)
          private$createSunbursts(overviewDat)
          private$renderOverview(output)
          
          detailDat <- private$filterDetail(cdmSourceInfo) %>% 
            # NEW -- simplify strings
            dplyr::mutate(pathway = gsub("_event_\\d+", "", pathway)) %>% 
            dplyr::mutate(pathway = gsub("_", ",", pathway)) %>%  # Replace underscores with spaces
            dplyr::mutate(pathway = stringr::str_wrap(pathway, width = 30))
          
          private$.sunburst$args$treatmentPathways <- detailDat
          private$.sankey$args$treatmentPathways <- detailDat
          # -------------------------------------------------------------------------
          # private$.table$reactiveValues$data <- detailDat
          #AM NEW -- Include treatmentPatterns table with calculated percentages
          
          detailDatForSunburst <- private$filterDetailForSunburst(cdmSourceInfo)
          
          treatmentPathways_long <- detailDatForSunburst %>% #private$.treatmentPathways %>%
            # select(-c("age","sex","index_year","target_cohort_id","target_cohort_name","tp")) %>%
            filter(freq >= 0) %>%
            group_by(cdm_name,analysis_id) %>%
            mutate(total_size = sum(freq)) %>%
            mutate(pathway_id = row_number()) %>%
            mutate(pathway_orig = pathway) %>% # Keep original pathway column
            mutate(pathway_orig = gsub("_event_\\d+","",pathway_orig)) %>%
            mutate(pathway_orig = gsub("_", ",", pathway_orig)) %>%
            mutate(pathway = pathway_orig) %>%
            separate_longer_delim(pathway, delim = "-") %>%
            relocate(pathway_id,pathway_orig)
          
          # ---- AM: split pathway into hierarchy levels ----
          table_with_pct <- treatmentPathways_long %>%
            group_by(cdm_name,analysis_id,pathway_id,pathway_orig) %>%
            mutate(
              pathway_percentage = round(100 * .data$freq / .data$total_size, 2), # Calculate percentage per pathway (freq/total_size) 
              levels = n_distinct(pathway), #str_split(pathway_orig, "-"),  # adjust delimiter if needed
              depth = row_number() #lengths(levels)
            ) %>% ungroup() %>% relocate(c("levels","depth"), .after ="pathway")
  
          
          # Calculate percentage per drug combination (sum of frfreq for each line of treatment and drug combination group/ total_size), these are the values in the sunburst plot
          table_with_pct <- table_with_pct %>%
            group_by(cdm_name,analysis_id,pathway,depth) %>%
            mutate(
              drug_combination_freq = sum(.data$freq),
              drug_combination_percentage = round(100 * drug_combination_freq / .data$total_size, 2)  # same formula as JS
            ) %>%
            ungroup() %>%
            relocate(drug_combination_percentage, .after = "freq")
          
          #----- AM: Rename columns
          table_with_pct <- table_with_pct %>%
            dplyr::rename(drug_combination = pathway) %>%
            dplyr::rename(pathway = pathway_orig) %>%
            dplyr::rename(combination_number = levels) %>%
            dplyr::rename(line_of_treatment = depth) %>%
            dplyr::rename(pathway_freq = freq) %>%
            dplyr::rename(total_freq = total_size) %>%
            #select(cdm_name,pathway_id,pathway,combination,line_of_treatment,percentage,freq) %>% 
            select(cdm_name,
                   drug_combination,line_of_treatment,drug_combination_percentage, drug_combination_freq,
                   pathway_id, pathway,pathway_percentage,pathway_freq,
                   total_freq) %>% distinct() %>% 
            arrange(desc(pathway_percentage),line_of_treatment,desc(drug_combination_percentage))
          
          # Update 20251210: Simplify percentage tables; the percentage corresponding to the whole pathway 
          # e.g. acetaminophen-amoxicilin+clavulanate has two levels (acetaminophen followed by amoxicilin+clavulanate), 
          # so the percentage value for the whole pathway in the sunburst plot is the one calculated for top-level line of treatment (line_of_treatment = 2), 
          # The percentages of lower levels correspond to the percentages in the sunburst plot for the single drug combinations (acetaminophen alone for this pathway).
          #table_with_pct <- table_with_pct %>% dplyr::group_by(pathway) %>% arrange(desc(line_of_treatment)) %>% slice_head(n=1) %>% dplyr::ungroup()
          
          
          private$.tableCombination$reactiveValues$data <- table_with_pct %>% 
            dplyr::select(cdm_name, drug_combination, line_of_treatment,
                          drug_combination_percentage, drug_combination_freq) %>% 
            dplyr::distinct() %>% 
            dplyr::rename(percentage = drug_combination_percentage,freq = drug_combination_freq) %>% 
            dplyr::arrange(line_of_treatment)
          
          private$.tablePathway$reactiveValues$data <- table_with_pct %>% 
            dplyr::select(cdm_name,pathway_id,pathway,pathway_percentage,pathway_freq) %>% 
            dplyr::distinct() %>%
            dplyr::rename(percentage = pathway_percentage, freq= pathway_freq)
          
          # --- NEW: push Sankey size + style args from inputs ---
          num_or_default <- function(x, default) {
            if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) return(default)
            as.numeric(x)
          }
          
          fontSize <- as.integer(num_or_default(private$.inputPanel$inputValues$sankey_fontSize, 20))
          nodeWidth <- as.integer(num_or_default(private$.inputPanel$inputValues$sankey_nodeWidth, 20))
          
          height_pct <- num_or_default(private$.inputPanel$inputValues$sankey_height, 50)
          width_pct  <- num_or_default(private$.inputPanel$inputValues$sankey_width,  75)
          
          h_px <- max(1, floor(height_pct * 10))
          w_px <- max(1, floor(width_pct  * 10))
          
          private$.sankey$args$fontSize <- fontSize
          private$.sankey$args$nodeWidth <- nodeWidth
          private$.sankey$args$height    <- shiny::validateCssUnit(h_px)
          private$.sankey$args$width     <- shiny::validateCssUnit(w_px)
          # ---                                                                   
          
          private$.sunburst$server(input, output, session)
          private$.sankey$server(input, output, session)
        }
      )
    },
    
    ## Methods ----
    filterOverview = function(cdmSourceInfo) {
      cdmSourceInfo |>
        dplyr::inner_join(
          private$.treatmentPathways,
          by = dplyr::join_by(
            analysis_id == analysis_id
          ), copy = TRUE
        ) |>
        dplyr::filter(
          .data$target_cohort_name == private$.inputPanel$inputValues$targetCohort,
          .data$age == private$.inputPanel$inputValues$age,
          .data$sex == private$.inputPanel$inputValues$sex,
          .data$index_year == private$.inputPanel$inputValues$indexYear,
          .data$freq >= private$.inputPanel$inputValues$minFreq
        ) |>
        dplyr::select(
          "pathway",
          "freq",
          # "total_freq", # AM: Included total freq column
          # "pathway_percentage", #AM: Included pathway percent = freq/total_freq
          "age",
          "sex",
          "index_year",
          "analysis_id",
          "target_cohort_id",
          "target_cohort_name",
          "cdm_source_abbreviation"
        ) |>
        dplyr::collect()
    },
    createSunbursts = function(data) {
      session <- shiny::getDefaultReactiveDomain()
      
      # NEW: handle censored values in sunburst ----------
      # data <- data |>
      #    dplyr::mutate(freq = ifelse(freq == -5, 1, freq))
      #
      data <- data |> filter(freq >= 5) # Keep uncensored data for the sunburst plot only
      #------
      dfs <- data |>
        dplyr::group_by(.data$cdm_source_abbreviation) |>
        dplyr::group_split()
      
      private$.sunburstOverview[[session$token]] <- lapply(dfs, function(df) {
        mod <- PlotWidget$new( #PlotWidget$new(
          fun = TreatmentPatterns::createSunburstPlot,
          args = list(
            treatmentPathways = df,
            groupCombinations = private$.inputPanel$inputValues$groupCombinations,
            colors = list(
              domain = names(private$.colours),
              range = as.character(private$.colours)
            ),
            legend = list(w = 600),#FALSE,
            height = "100vh",#"78vh",#"70vh",
            width = "100%"#"40vw"
          ),
          title = unique(df$cdm_source_abbreviation),
          parentNamespace = self$namespace
        )
        return(mod)
      })
      # }
    },
    renderOverview = function(output) {
      session <- shiny::getDefaultReactiveDomain()
      uis <- lapply(private$.sunburstOverview[[session$token]], function(mod) {
        mod$server(input, output, session)
        mod$UI()
      })
      
      output$overview <- shiny::renderUI({
        shiny::fluidPage(
          lapply(seq(1, length(uis), 2), function(i) {
            shiny::fluidRow(
              lapply(list(uis[i], uis[i + 1]), function(ui) {
                shiny::column(ui, width = 6)
              })
            )
          })
        )
      })
    },
    filterDetail = function(cdmSourceInfo) {
      cdmSourceInfo |>
        dplyr::inner_join(
          private$.treatmentPathways,
          by = dplyr::join_by(
            analysis_id == analysis_id
          ), copy = TRUE
        ) |>
        dplyr::filter(
          .data$target_cohort_name %in% private$.inputPanel$inputValues$targetCohort,
          .data$cdm_source_abbreviation == private$.inputPanel$inputValues$database,
          .data$age == private$.inputPanel$inputValues$age,
          .data$sex == private$.inputPanel$inputValues$sex,
          .data$index_year == private$.inputPanel$inputValues$indexYear,
          .data$freq >= private$.inputPanel$inputValues$minFreq
        ) |>
        dplyr::select(
          "pathway",
          "freq",
          #"total_freq", # AM: Included total freq column
          #"pathway_percentage", #AM: Included pathway percent = freq/total_freq
          "age",
          "sex",
          "index_year",
          "analysis_id",
          "target_cohort_id",
          "target_cohort_name"
        ) |>
        dplyr::collect()
      # }
    },
    
    # -------------------------------------------------------------------------
    #AM NEW: filterDetail modification to include calculated percentages below sunburst plot
    filterDetailForSunburst = function(cdmSourceInfo) {
      cdmSourceInfo |>
        dplyr::inner_join(
          private$.treatmentPathways,
          by = dplyr::join_by(
            analysis_id == analysis_id,
            cdm_name == cdm_name,
            tp == tp
          ), copy = TRUE
        ) |>
        dplyr::filter(
          .data$target_cohort_name %in% private$.inputPanel$inputValues$targetCohort,
          .data$cdm_source_abbreviation == private$.inputPanel$inputValues$database,
          .data$age == private$.inputPanel$inputValues$age,
          .data$sex == private$.inputPanel$inputValues$sex,
          .data$index_year == private$.inputPanel$inputValues$indexYear,
          .data$freq >= private$.inputPanel$inputValues$minFreq
        ) %>% 
        select(cdm_name,analysis_id,tp,pathway,freq,age,sex,index_year,target_cohort_id,target_cohort_name) %>% 
        dplyr::collect()
      # }
    },
    
    # -------------------------------------------------------------------------
    initInputPanel = function() {
      databaseLabels <- private$.cdmSourceInfo |>
        #AM NEW --- use cdm_name label instead as fixed by addCDMlabel() on preprocessing.R
        dplyr::pull(.data$cdm_name) |>
        # dplyr::pull(.data$cdm_source_abbreviation) |>
        unique()
      
      targets <- private$.treatmentPathways |>
        dplyr::pull(.data$target_cohort_name) |>
        unique()
      
      private$.inputPanel <- InputPanel$new(
        funs = list(
          database = shinyWidgets::pickerInput,
          targetCohort = shinyWidgets::pickerInput,
          age = shinyWidgets::pickerInput,
          sex = shinyWidgets::pickerInput,
          indexYear = shinyWidgets::pickerInput,
          minFreq = shiny::sliderInput,
          groupCombinations = shiny::checkboxInput,
          # AM Add funs for adjusting sankey diagram size -----
          sankey_fontSize = shiny::numericInput,
          sankey_nodeWidth = shiny::numericInput,
          sankey_height = shiny::numericInput,
          sankey_width = shiny::numericInput
        ),
        args = list(
          database = list(
            inputId = shiny::NS(self$namespace, "database"),
            label = "Database",
            choices = databaseLabels,
            selected = databaseLabels[1],
            multiple = FALSE
          ),
          targetCohort = list(
            inputId = shiny::NS(self$namespace, "targetCohort"),
            label = "Target Cohort",
            choices = targets,
            selected = targets[1],
            multiple = FALSE
          ),
          age = list(
            inputId = shiny::NS(self$namespace, "age"),
            label = "Age",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$age) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          sex = list(
            inputId = shiny::NS(self$namespace, "sex"),
            label = "Sex",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$sex) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          indexYear = list(
            inputId = shiny::NS(self$namespace, "indexYear"),
            label = "Index Year",
            choices = private$.treatmentPathways |>
              dplyr::pull(.data$index_year) |>
              unique(),
            selected = "all",
            multiple = FALSE
          ),
          minFreq = list(
            inputId = shiny::NS(self$namespace, "minFreq"),
            label = "Minimum Frequency",
            min = private$.treatmentPathways |>
              # NEW add limit of minFreq > -5 to skip censonred data
              dplyr::filter(.data$freq >= 5) |>
              dplyr::pull(.data$freq) |>
              min(),
            max = private$.treatmentPathways |>
              dplyr::pull(.data$freq) |>
              max(),
            value = private$.treatmentPathways |>
              # NEW add limit of minFreq > -5 to skip censonred data
              dplyr::filter(.data$freq >= 5) |>
              dplyr::pull(.data$freq) |>
              min()
          ),
          groupCombinations = list(
            inputId = shiny::NS(self$namespace, "groupCombinations"),
            label = "Group Combinations",
            value = FALSE
          ),
          #AM add sankey diagram arguments for sizing ----
          sankey_fontSize = list(
            inputId = shiny::NS(self$namespace, "sankey_fontSize"),
            label = "Font Size",
            value = 20,
            min = 8,
            max = 40
          ),
          sankey_nodeWidth = list(
            inputId = shiny::NS(self$namespace, "sankey_nodeWidth"),
            label = "Node Width",
            value = 20,
            min = 1,
            max = 100
          ),
          sankey_height = list(
            inputId = shiny::NS(self$namespace, "sankey_height"),
            label = "Height (%)",
            value = 50,
            min = 10,
            max = 100
          ),
          sankey_width = list(
            inputId = shiny::NS(self$namespace, "sankey_width"),
            label = "Width (%)",
            value = 75,
            min = 10,
            max = 100
          )
          #-----
          
        ),
        growDirection = "vertical",
        parentNamespace = self$namespace
      )
    },
    #AM: Custom fetch colors function using paired scale:
    # fetchColours = function(){
    #   # colors <- pals::brewer.paired(n)
    #   
    #   label_colors <- c(
    #     "ivacaftor+ivacaftor,tezacaftor,elexacaftor"                          = "#1F78B4",
    #     "ivacaftor,lumacaftor"                                                = "#FF7F00",
    #     "ivacaftor,tezacaftor,elexacaftor"                                    = "#6A3D9A",
    #     "ivacaftor"                                                           = "#33A02C",
    #     "ivacaftor+ivacaftor,tezacaftor"                                      = "#E31A1C",
    #     "ivacaftor+ivacaftor,tezacaftor,elexacaftor+ivacaftor,tezacaftor"     = "#FB9A99",
    #     "ivacaftor,tezacaftor"                                                = "#FDBF6F",
    #     "ivacaftor+ivacaftor,lumacaftor+ivacaftor,tezacaftor"                 = "#A6CEE3",
    #     "ivacaftor,tezacaftor,elexacaftor+ivacaftor,tezacaftor"               = "#CAB2D6",
    #     "ivacaftor+ivacaftor,lumacaftor+ivacaftor,tezacaftor,elexacaftor"     = "#B2DF8A",
    #     "ivacaftor,lumacaftor+ivacaftor,tezacaftor,elexacaftor"               = "#FFFF99"
    #   )
    #   
    #   return(label_colors)
    # },
    
    # fetchColours = function(ncolor, s = 0.5, v = 0.95, seed = 40) {
    #   # From: https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r/61183611#61183611
    #   golden_ratio_conjugate <- 0.618033988749895
    #   set.seed(seed)
    #   h <- runif(1)
    #   H <- vector("numeric", ncolor)
    #   for (i in seq_len(ncolor)) {
    #     h <- (h + golden_ratio_conjugate) %% 1
    #     H[i] <- h
    #   }
    #   as.list(hsv(H, s = s, v = v))
    # },
    fetchColours = function(ncolor, s = 0.5,v = 0.95,seed = 40, base_palette = NULL, shuffle_base = FALSE) {
      # From: https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r/61183611#61183611
      # base_palette: character vector of HEX colors, e.g. c("#1F78B4", "#FF7F00", ...)
      # shuffle_base: if TRUE, shuffles the base colors to reduce adjacency bias
      
      # Helper: generate distinct HSV colors via golden ratio (your original approach)
      gen_golden_hsv <- function(k, s, v, seed) {
        golden_ratio_conjugate <- 0.618033988749895
        set.seed(seed)
        h <- runif(1)
        H <- numeric(k)
        for (i in seq_len(k)) {
          h <- (h + golden_ratio_conjugate) %% 1
          H[i] <- h
        }
        hsv(H, s = s, v = v)
      }
      
      # If a base palette is provided, use it first
      if (!is.null(base_palette)) {
        bp <- base_palette
        # Basic sanity: make sure hex strings are valid-ish
        bp <- gsub("\\s+", "", bp)
        if (shuffle_base) {
          set.seed(seed)
          bp <- sample(bp, length(bp))
        }
        
        if (ncolor <= length(bp)) {
          return(as.list(bp[seq_len(ncolor)]))
        } else {
          # Need more colors than base: generate the remainder and append
          remainder <- ncolor - length(bp)
          extra <- gen_golden_hsv(remainder, s = s, v = v, seed = seed)
          # Return base first, then extras
          return(as.list(c(bp, extra)))
        }
      }
      
      # No base palette supplied: fall back entirely to golden-ratio HSV
      as.list(gen_golden_hsv(ncolor, s = s, v = v, seed = seed))
    },
    setColours = function() {
      # if (is.null(private$.colours)) {
      nodes <- private$.treatmentPathways |>
        dplyr::pull(.data$pathway) |>
        #private$clean_labels() |> # NEW
        stringr::str_split(pattern = "-") |>
        unlist() |>
        unique()
      
      # NEW ---- simplify strings
      nodes <- gsub("_event_\\d+","",nodes)
      #AM replace underscores by spaces
      nodes <- gsub("_", ",", nodes)
      
      nodes <- if ("None" %in% nodes) {
        c(nodes, "Stopped", "Combination")
      } else {
        c(nodes, "None", "Stopped", "Combination")
      }
      
      # private$.colours <- private$fetchColours(length(nodes))
      private$.colours <- private$fetchColours(ncolor = length(nodes), base_palette = c("#1F78B4", "#FF7F00", "#6A3D9A", "#33A02C", "#E31A1C", "#FB9A99", "#FDBF6F", "#A6CEE3", "#CAB2D6", "#B2DF8A", "#FFFF99"))
      
      # private$.colours <- private$fetchColours()
      names(private$.colours) <- unique(nodes)
      # }
    },
    initSankey = function() {
      
      # AM ---- take numeric inputs for adjusting Sankey size ----
      # ---- Safe defaults (match initInputPanel) ----
      default_fontSize   <- 20L
      default_nodeWidth  <- 20L
      default_height_pct <- 50   # labeled as "(%)" in the panel
      default_width_pct  <- 75   # labeled as "(%)" in the panel
      
      # ---- Small helper to coerce numerics safely ----
      num_or_default <- function(x, default) {
        if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) return(default)
        as.numeric(x)
      }
      
      # ---- Pull values from the input panel if it's available; otherwise use defaults ----
      if (!is.null(private$.inputPanel) && !is.null(private$.inputPanel$inputValues)) {
        height_pct <- num_or_default(private$.inputPanel$inputValues$sankey_height, default_height_pct)
        width_pct  <- num_or_default(private$.inputPanel$inputValues$sankey_width,  default_width_pct)
        fontSize   <- as.integer(num_or_default(private$.inputPanel$inputValues$sankey_fontSize, default_fontSize))
        nodeWidth  <- as.integer(num_or_default(private$.inputPanel$inputValues$sankey_nodeWidth,  default_nodeWidth))
      } else {
        height_pct <- default_height_pct
        width_pct  <- default_width_pct
        fontSize   <- default_fontSize
        nodeWidth  <- default_nodeWidth
      }
      
      # ---- Convert "% of viewport" sliders into pixels safely ----
      # (keeping your 50 -> 500px scaling; clamp to at least 1 to avoid 0px)
      h_px <- max(1, floor(height_pct * 10))
      w_px <- max(1, floor(width_pct  * 10))
      
      # ---- Validate CSS units: numeric -> "px" via Shiny ----
      height_css <- shiny::validateCssUnit(h_px)
      width_css  <- shiny::validateCssUnit(w_px)
      
      # createSankeyDiagramCustom <- function (treatmentPathways, groupCombinations = FALSE, colors = NULL, ...) {
      #   TreatmentPatterns:::validateCreateSankeyDiagram()
      #   treatmentPathways <- TreatmentPatterns:::doGroupCombinations(treatmentPathways = treatmentPathways, 
      #                                                                groupCombinations = groupCombinations)
      #   data <- TreatmentPatterns:::splitPathItems(treatmentPathways)
      #   if (ncol(data) <= 2) {
      #     stop("Cannot compute Sankey Diagram as there is only one level in the data.")
      #   }
      #   
      #   # Create linkedData BEFORE colour mapping
      #   linkedData <- TreatmentPatterns:::createLinkedData(data)
      #   
      #   # NEW Normalise labels and map colours
      #   # colors_df <- linkedData$nodes$names |>
      #   #   #clean_labels() |>
      #   #   tibble::enframe(name = NULL, value = "clean_node_label")
      #   # 
      #   # colors_df <- colors_df |>
      #   #   left_join(
      #   #     tibble::enframe(c(private$.colours, "Stopped" = "#555555"), name = "label", value = "color"),
      #   #     by = c("clean_node_label" = "label")
      #   #   )
      #   # linkedData$nodes$range <- colors_df$color
      #   
      #   
      #   # NEW Prepare colour mapping
      #   # label_colors <- c(private$.colours, "Stopped" = "#555555")
      #   # label_colors_df <- tibble::enframe(label_colors, name = "label", value = "color")
      #   
      #   # colors_df <- tibble::enframe(linkedData$nodes$names, name = NULL, value = "clean_node_label") |>
      #   #   dplyr::mutate(clean_node_label = gsub("^\\d+\\.", "", clean_node_label)) |>
      #   #   dplyr::mutate(clean_node_label = gsub("_event_\\d+", "", clean_node_label)) |>
      #   #   dplyr::mutate(clean_node_label = gsub("_", ",", clean_node_label)) |>
      #   #   tidyr::separate_longer_delim(clean_node_label, delim = "-") |>
      #   #   dplyr::left_join(label_colors_df, by = c("clean_node_label" = "label"))
      #   # 
      #   # linkedData$nodes$range <- colors_df$color
      #   
      #   label_colors <- c(private$.colours, "Stopped" = "#555555")
      #   
      #   nodes_map <- tibble::tibble(name = linkedData$nodes$names) |>
      #     dplyr::mutate(clean_label = gsub("^\\d+\\.", "", name)) |>
      #     dplyr::mutate(clean_label = gsub("_event_\\d+", "", clean_label)) |>
      #     dplyr::mutate(clean_label = gsub("_", ",", clean_label)) |>
      #     dplyr::mutate(color = label_colors[clean_label]) |>
      #     dplyr::mutate(color = ifelse(is.na(color), "#555555", color))
      #   
      #   linkedData$nodes$range <- nodes_map$color
      #   
      #   
      #   
      #   
      #   setColourScaleCustom <- function (linkedData, colors) {
      #     domain <- stringr::str_split_i(linkedData$nodes$names, "\\d\\.", i = 2)
      #     
      #     if (!is.null(colors)) {
      #       # Named list OR named vector path
      #       if (is.list(colors) || (!is.null(names(colors)) && any(nzchar(names(colors))))) {
      #         # Deduplicate by name (first occurrence wins)
      #         nm <- names(colors)
      #         keep <- !duplicated(nm)
      #         nm <- nm[keep]
      #         col <- unname(unlist(colors))[keep]
      #         
      #         # Build labels table; always append Stopped
      #         labels <- data.frame(
      #           name = c(nm, "Stopped"),
      #           col  = c(col, "#555555"),
      #           stringsAsFactors = FALSE
      #         )
      #         
      #       } else {
      #         # Unnamed vector: rely on position vs unique(domain)
      #         dom_unique <- unique(domain)
      #         # Ensure we have at least as many colors as domains (or warn)
      #         if (length(colors) < length(dom_unique)) {
      #           warning(sprintf("Missing %s colour(s)", length(dom_unique) - length(colors)))
      #         }
      #         labels <- data.frame(
      #           name = dom_unique,
      #           # Using 1:length(dom_unique) is a bit clearer than c(0:length(.))
      #           col  = colors[seq_len(length(dom_unique))],
      #           stringsAsFactors = FALSE
      #         )
      #         # Fallbacks for short vectors
      #         labels$col[is.na(labels$col)] <- "#555555"
      #       }
      #       
      #       # Map to nodes
      #       linkedData$nodes$range <- labels$col[match(domain, labels$name)]
      #       
      #       # Fallback for any unmapped domain
      #       linkedData$nodes$range[is.na(linkedData$nodes$range)] <- "#555555"
      #       
      #       return(sprintf(
      #         "d3.scaleOrdinal().domain([%s]).range([%s])",
      #         paste0("'", linkedData$nodes$names, "'", collapse = ", "),
      #         paste0("'", linkedData$nodes$range, "'", collapse = ", ")
      #       ))
      #     } else {
      #       return("d3.scaleOrdinal(d3.schemeCategory20)")
      #     }
      #   }
      #   
      #   # linkedData <- TreatmentPatterns:::createLinkedData(data)
      #   
      #   # label_colors <- c(
      #   #   "ivacaftor+ivacaftor,tezacaftor,elexacaftor"                          = "#1F78B4",
      #   #   "ivacaftor,lumacaftor"                                                = "#FF7F00",
      #   #   "ivacaftor,tezacaftor,elexacaftor"                                    = "#6A3D9A",
      #   #   "ivacaftor"                                                           = "#33A02C",
      #   #   "ivacaftor+ivacaftor,tezacaftor"                                      = "#E31A1C",
      #   #   "ivacaftor+ivacaftor,tezacaftor,elexacaftor+ivacaftor,tezacaftor"     = "#FB9A99",
      #   #   "ivacaftor,tezacaftor"                                                = "#FDBF6F",
      #   #   "ivacaftor+ivacaftor,lumacaftor+ivacaftor,tezacaftor"                 = "#A6CEE3",
      #   #   "ivacaftor,tezacaftor,elexacaftor+ivacaftor,tezacaftor"               = "#CAB2D6",
      #   #   "ivacaftor+ivacaftor,lumacaftor+ivacaftor,tezacaftor,elexacaftor"     = "#B2DF8A",
      #   #   "ivacaftor,lumacaftor+ivacaftor,tezacaftor,elexacaftor"               = "#FFFF99",
      #   #   "Stopped" = "#555555"
      #   # )
      #   
      #   # label_colors <- c(private$.colours,"Stopped" = "#555555")
      #   # 
      #   # label_colors_df <- tibble::enframe(label_colors, name = "label", value = "color")
      #   # 
      #   # colors_df <- as.data.frame(unique(linkedData$nodes$names)) 
      #   # names(colors_df) <- "nodes" #%>% rename(nodes = names)
      #   # colors_df$domain <- stringr::str_split_i(linkedData$nodes$names, "\\d\\.", i = 2)
      #   # 
      #   # colors_df <- colors_df %>% 
      #   #   mutate(clean_node_label = gsub("^\\d+\\.","",nodes)) %>%
      #   #   dplyr::mutate(clean_node_label = gsub("_event_\\d+", "", clean_node_label)) %>% 
      #   #   dplyr::mutate(clean_node_label = gsub("_", ",", clean_node_label)) %>%
      #   #   separate_longer_delim(clean_node_label, delim = "-")
      #   # 
      #   # colors_df <- colors_df %>% left_join(label_colors_df, by=c("clean_node_label" = "label"))
      #   
      #   #colors_sankey <- colors_df$color
      #   #names(colors_sankey) <- colors_df$domain
      #   
      #   networkD3::sankeyNetwork(Links = linkedData$links, Nodes = linkedData$nodes, 
      #                            Source = "source", Target = "target", Value = "value", 
      #                            NodeID = "names", units = "%", colourScale = setColourScaleCustom(linkedData, label_colors), ...) #TreatmentPatterns:::setColourScale(linkedData, colors_sankey), ...)
      # }
      
      createSankeyDiagramCustom <- function(treatmentPathways, groupCombinations = FALSE, colors = NULL, ...) {
        # Validate and prepare data
        TreatmentPatterns:::validateCreateSankeyDiagram()
        treatmentPathways <- TreatmentPatterns:::doGroupCombinations(treatmentPathways, groupCombinations)
        data <- TreatmentPatterns:::splitPathItems(treatmentPathways)
        
        if (ncol(data) <= 2) {
          stop("Cannot compute Sankey Diagram as there is only one level in the data.")
        }
        
        # Create linkedData BEFORE colour mapping
        linkedData <- TreatmentPatterns:::createLinkedData(data)
        
        # Prepare colour mapping using direct lookup
        label_colors <- c(private$.colours, "Stopped" = "#555555")
        
        nodes_map <- tibble::tibble(name = linkedData$nodes$names) |>
          dplyr::mutate(clean_label = gsub("^\\d+\\.", "", name)) |>
          dplyr::mutate(clean_label = gsub("_event_\\d+", "", clean_label)) |>
          dplyr::mutate(clean_label = gsub("_", ",", clean_label)) |>
          dplyr::mutate(color = label_colors[clean_label]) |>
          dplyr::mutate(color = ifelse(is.na(color), "#555555", color))
        
        # Assign colours to nodes
        linkedData$nodes$range <- nodes_map$color
        
        # Define colour scale for Sankey
        setColourScaleCustom <- function(linkedData, colors) {
          domain <- stringr::str_split_i(linkedData$nodes$names, "\\d\\.", i = 2)
          
          if (!is.null(colors)) {
            nm <- names(colors)
            keep <- !duplicated(nm)
            nm <- nm[keep]
            col <- unname(unlist(colors))[keep]
            
            labels <- data.frame(
              name = c(nm, "Stopped"),
              col  = c(col, "#555555"),
              stringsAsFactors = FALSE
            )
            
            linkedData$nodes$range <- labels$col[match(domain, labels$name)]
            linkedData$nodes$range[is.na(linkedData$nodes$range)] <- "#555555"
            
            return(sprintf(
              "d3.scaleOrdinal().domain([%s]).range([%s])",
              paste0("'", linkedData$nodes$names, "'", collapse = ", "),
              paste0("'", linkedData$nodes$range, "'", collapse = ", ")
            ))
          } else {
            return("d3.scaleOrdinal(d3.schemeCategory20)")
          }
        }
        
        # Render Sankey diagram
        networkD3::sankeyNetwork(
          Links = linkedData$links,
          Nodes = linkedData$nodes,
          Source = "source", Target = "target", Value = "value",
          NodeID = "names", units = "%",
          colourScale = setColourScaleCustom(linkedData, label_colors),
          ...
        )
      }
      
      
      # ---- Build the widget with guaranteed good values ----
      private$.sankey <- PlotWidget$new(
        fun  = createSankeyDiagramCustom,#TreatmentPatterns::createSankeyDiagram,
        args = list(
          colors      = private$.colours,
          fontSize    = fontSize,
          nodeWidth   = nodeWidth,
          nodePadding = 1,
          height      = height_css,  # e.g. "500px"
          width       = width_css    # e.g. "750px"
        ),
        title = NULL,
        parentNamespace = self$namespace
      )
      
      
      
      # private$.sankey <- PlotWidget$new(
      #   fun = TreatmentPatterns::createSankeyDiagram,
      #   args = list(
      #     colors = private$.colours,
      #     fontSize = private$.inputPanel$inputValues$sankey_fontSize,
      #     nodeWidth = private$.inputPanel$inputValues$sankey_nodeWidth,
      #     nodePadding = 1,
      #     height = sankeyHeightPx,
      #     width = sankeyWidthPx
      #   ),
      #   title = NULL,
      #   parentNamespace = self$namespace
      # )
    },
    initSunburst = function() {
      # Drop "Stopped"
      private$.sunburst <- PlotWidget$new(
        fun = TreatmentPatterns::createSunburstPlot,
        args = list(
          colors = list(
            domain = names(private$.colours),#clean_labels(names(private$.colours)), #names(private$.colours),
            range = as.character(private$.colours)
          ),
          legend = list(w = 550, s = 1), #600
          height = "120vh", #"70vh",
          width = "81vw", #"50vw"
          
          sizingPolicy = htmlwidgets::sizingPolicy(
            defaultWidth = "100%",       # Fill available width
            defaultHeight = "100%",      # Fill available height
            padding = 0,                 # Remove all padding
            viewer.defaultWidth = NULL,  # Let viewer auto-size
            viewer.defaultHeight = NULL,
            viewer.padding = 0,          # No padding in viewer
            viewer.fill = TRUE,          # Fill viewer pane
            viewer.suppress = FALSE,
            viewer.paneHeight = NULL,
            browser.defaultWidth = NULL, # Auto-size in browser
            browser.defaultHeight = NULL,
            browser.padding = 0,         # No padding in browser
            browser.fill = TRUE,         # Fill browser window
            browser.external = FALSE,
            knitr.defaultWidth = NULL,
            knitr.defaultHeight = NULL,
            knitr.figure = TRUE,
            fill = TRUE                  # Allow widget to fill container
          )
          
        ),
        title = NULL,
        parentNamespace = self$namespace
      )
    },
    initTableCombination = function() {
      private$.tableCombination <- Table$new(
        data = NULL,
        title = "Drug Combination Table",
        filter = "top", # NEW "top"
        # NEW ---
        options = list(
          scrollX       = TRUE,             # <-- horizontal scrollbar
          scrollCollapse = TRUE,            # collapses width when few cols; keeps scrollbar logic
          autoWidth     = FALSE,             # let DataTables manage column widths
          pageLength    = 10
          # vertical scroll instead of pagination:
          # scrollY     = "60vh",
          # scroller    = TRUE,             # needs extensions='Scroller' (see section 3)
          # paging      = FALSE
        ),
        #\NEW ----
        parentNamespace = self$namespace
      )
    },
    initTablePathway = function() {
      private$.tablePathway <- Table$new(
        data = NULL,
        title = "Pathway Table",
        filter = "top", # NEW "top"
        # NEW ---
        options = list(
          scrollX       = TRUE,             # <-- horizontal scrollbar
          scrollCollapse = TRUE,            # collapses width when few cols; keeps scrollbar logic
          autoWidth     = FALSE,             # let DataTables manage column widths
          pageLength    = 10
          # vertical scroll instead of pagination:
          # scrollY     = "60vh",
          # scroller    = TRUE,             # needs extensions='Scroller' (see section 3)
          # paging      = FALSE
        ),
        #\NEW ----
        parentNamespace = self$namespace
      )
      },
    updatePickers = function(cdmSourceInfo, session) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = shiny::NS(private$.inputPanel$moduleId, "database"),
        choices = cdmSourceInfo$cdm_source_abbreviation
      )
    }
  )
)
