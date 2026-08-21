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

DATA_SOURCE_LABEL <- "Data source"

labelDataSourceChoices <- function(choices) {
  labels <- choices
  labels[choices %in% c("database", "cdm_name")] <- DATA_SOURCE_LABEL
  stats::setNames(choices, labels)
}

# Route-of-administration concept set names (Obj.1); also present under ingredients/.
ROUTES_OF_ADMINISTRATION <- c(
  "ansuvimab_other",
  "anthrax_immune_globulin_other",
  "bezlotoxumab_other",
  "casirivimab_other",
  "cytomegalovirus_immune_globulin_intravenous",
  "cytomegalovirus_immune_globulin_other",
  "diphtheria_antitoxin_other",
  "freeze_dried_peg_treated_human_normal_ig_other",
  "freeze_dried_pepsin_treated_human_normal_immunoglobulin_other",
  "freeze_dried_ph4_treated_human_normal_immunoglobulin_other",
  "freeze_dried_sulfonated_human_normal_immunoglobulin_other",
  "hepatitis_b_immune_globulin_intramuscular",
  "hepatitis_b_immune_globulin_intravenous",
  "hepatitis_b_immune_globulin_other",
  "human_vaccinia_immune_globulin_intramuscular",
  "human_vaccinia_immune_globulin_other",
  "immunoglobulin_anti_rubella_other",
  "immunoglobulin_anti_tickborne_encephalitis_other",
  "immunoglobulin_g_intramuscular",
  "immunoglobulin_g_intravenous",
  "immunoglobulin_g_other",
  "immunoglobulin_g_subcutaenous",
  "immunoglobulin_m_human_other",
  "nebacumab_other",
  "nirsevimab_other",
  "obiltoxaximab_other",
  "palivizumab_intramuscular",
  "palivizumab_other",
  "ph4_treated_acidic_human_normal_immunoglobulin_other",
  "ph4_treated_acidic_human_normal_immunoglobulin_subcutaneous",
  "rabies_immune_globulin_human_intramuscular",
  "rabies_immune_globulin_human_other",
  "rabies_immune_globulin_human_subcutaneous",
  "raxibacumab_other",
  "regdanvimab_other",
  "respiratory_syncytial_virus_immune_globulin_intravenous",
  "rho_d_immune_globulin_intramuscular",
  "rho_d_immune_globulin_other",
  "sotrovimab_other",
  "staphylococcus_aureus_immunoserum_other",
  "tetanus_immune_globulin_intramuscular",
  "tetanus_immune_globulin_other",
  "tixagevimab_other",
  "varicella_zoster_immune_globulin_intramuscular",
  "varicella_zoster_immune_globulin_other"
)

extractPrevalenceOutcome <- function(group_level) {
  sub("^.*&&&\\s*", "", group_level)
}

excludeRouteOfAdminFromIngredients <- function(
    result,
    routes = ROUTES_OF_ADMINISTRATION) {
  result <- omopgenerics::validateResultArgument(result)
  outcome_tail <- extractPrevalenceOutcome(result$group_level)
  filtered <- result %>%
    dplyr::filter(!outcome_tail %in% routes)
  if (nrow(filtered) == 0) {
    return(filtered)
  }
  set <- omopgenerics::settings(result) %>%
    dplyr::filter(.data$result_id %in% unique(filtered$result_id))
  omopgenerics::newSummarisedResult(filtered, settings = set)
}

getMissingRouteOfAdminLabelMap <- function() {
  map_paths <- c(
    file.path(
      get0("PKG_DATA_DIR", envir = .GlobalEnv, ifnotfound = ""),
      "missing_route_of_admin_concept_set_names_map.csv"
    ),
    file.path(
      get0("current_path", envir = .GlobalEnv, ifnotfound = ""),
      "..", "..", "..", "R", "support",
      "missing_route_of_admin_concept_set_names_map.csv"
    )
  )
  map_paths <- unique(map_paths[nzchar(map_paths) & file.exists(map_paths)])
  if (length(map_paths) == 0) {
    return(character(0))
  }
  df <- readr::read_csv(map_paths[[1]], show_col_types = FALSE)
  stats::setNames(df$original, df$safe_key)
}

labelMissingRouteOfAdmin <- function(
    result,
    label_map = getMissingRouteOfAdminLabelMap()) {
  result <- omopgenerics::validateResultArgument(result)
  if (length(label_map) == 0) {
    return(result)
  }
  
  data <- result
  keys <- names(label_map)[order(-nchar(names(label_map)), names(label_map))]
  
  for (key in keys) {
    display <- unname(label_map[[key]])
    patterns <- unique(c(key, paste0("adr_", key)))
    patterns <- patterns[patterns != display]
    for (pat in patterns) {
      data <- data %>%
        dplyr::mutate(
          group_level = stringr::str_replace(
            group_level,
            stringr::fixed(pat),
            display
          )
        )
    }
  }
  
  set <- omopgenerics::settings(result)
  label_cols <- intersect(
    c("outcome_cohort_name", "outcome_table", "group_name"),
    names(set)
  )
  if (length(label_cols) > 0) {
    for (col in label_cols) {
      for (key in keys) {
        display <- unname(label_map[[key]])
        patterns <- unique(c(key, paste0("adr_", key)))
        patterns <- patterns[patterns != display]
        for (pat in patterns) {
          set[[col]] <- stringr::str_replace(
            set[[col]],
            stringr::fixed(pat),
            display
          )
        }
      }
    }
  }
  
  omopgenerics::newSummarisedResult(data, settings = set)
}

#' create a ggplot from the output of summariseLargeScaleCharacteristics.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param x attrition table
#' @param cohortId target cohort_definition_id
#'
#' @return A dgr_graph
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(omopgenerics)
#' library(dplyr)
#' library(DiagrammeR)
#'
#' cdm <- mockCohortCharacteristics(numberIndividuals = 1000)
#'
#' cdm[["cohort1"]] <- cdm[["cohort1"]] |>
#'   filter(year(cohort_start_date) >= 2000) |>
#'   recordCohortAttrition("Restrict to cohort_start_date >= 2000") |>
#'   filter(year(cohort_end_date) < 2020) |>
#'   recordCohortAttrition("Restrict to cohort_end_date < 2020") |>
#'   compute(temporary = FALSE, name = "cohort1")
#'
#' cdm$cohort1 |>
#'   summariseCohortAttrition() |>
#'   plotCohortAttrition(cohortId = 2)
#' }
#'

# Note: The following function was adapted from the original version 
# (https://raw.githubusercontent.com/darwin-eu-dev/CohortCharacteristics/d07f14e7d5e417f84235c35ddbb8e0925e290c0d/R/plotCohortAttrition.R)
# This adapted version includes changes for plotting the attrition table 
# previously loaded from a csv file instead of from a cdm object.

plotCohortAttritionCustom <- function(x, cohortId = NULL) {
  
  rlang::check_installed("DiagrammeR")
  
  #if (!inherits(x, "summarised_result")) {
  #  cli::cli_abort("x must be the output of summariseCohortAttrition()")
  #}
  if (nrow(x) == 0) {
    cli::cli_warn("Empty result object")
    return(emptyTable("Empty result object"))
  }
  #x <- x |>
  #  visOmopResults::filterSettings(.data$result_type == "cohort_attrition")
  if (nrow(x) == 0) {
    cli::cli_warn("No attrition found in the results")
    return(emptyTable("No attrition found in the results"))
  }
  if (!is.null(cohortId)) {
    #x <- x |>
    #  visOmopResults::filterSettings(
    #    .data$cohort_definition_id == .env$cohortId
    #  )
    x <- x %>% filter(cohort_definition_id == cohortId)
  }
  #if (x$result_id |> unique() |> length() > 1) {
  #  return(
  #    emptyTable("More than one cohort found, please select only one cohort to show attrition.")
  #  )
  #}
  
  #x <- x |>
  #  visOmopResults::splitAll() |>
  #  visOmopResults::pivotEstimates(
  #    pivotEstimatesBy = c("variable_name", "estimate_name"),
  #    nameStyle = "{variable_name}"
  #  ) |>
  #  dplyr::select(
  #    "reason_id", "reason", "number_records", "number_subjects",
  #    "excluded_records", "excluded_subjects"
  #  ) |>
  #  dplyr::mutate(reason_id = as.numeric(.data$reason_id)) |>
  #  dplyr::arrange(.data$reason_id) |>
  #  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  # Create table to be used in the graph
  xn <- createLabels(x)
  
  y <- selectLabels(xn)
  xn <- y$xn
  att <- y$att
  
  # Create graph
  n <- nrow(x)
  xg <- DiagrammeR::create_graph()
  
  w1 <- getWidthMainBox(xn)
  
  if (nrow(x) == 1) {
    xg <- getSingleNode(xg, xn, w1)
  } else {
    att <- validateReason(att)
    
    h2 <- getHeightMiddleBox(att)
    
    p1 <- getPositionMainBox(xn, n, h2)
    
    w2 <- getWidthMiddleBox(att)
    
    p2 <- getPositionMiddleBox(p1)
    
    xg <- getNodes(xn, att, n, xg, h2, w1, p1, w2, p2)
  }
  return(DiagrammeR::render_graph(xg))
}

emptyTable <- function(message) {
  DiagrammeR::create_graph() |>
    DiagrammeR::add_node(
      label = message,
      node_aes = DiagrammeR::node_aes(
        shape = "box",
        fontcolor = "black",
        fillcolor = "white",
        fontname = "Calibri",
        fontsize = 10,
        x = 1, y = 1,
        width = 4, penwidth = 0
      )
    ) |>
    DiagrammeR::render_graph()
}

formatNum <- function(col) {
  dplyr::if_else(
    !is.na(as.numeric(col)),
    gsub(" ", "", format(as.integer(col), big.mark = ",")),
    col
  )
}

createLabels <- function(x) {
  x <- x |>
    dplyr::mutate(
      number_subjects = formatNum(.data$number_subjects),
      number_records = formatNum(.data$number_records),
      excluded_subjects = formatNum(.data$excluded_subjects),
      label = paste0(
        "N subjects = ", .data$number_subjects, "\nN records = ", .data$number_records
      )
    )
  return(x)
}

selectLabels <- function(xn) {
  if (nrow(xn) == 1) {
    xn <- xn |>
      dplyr::mutate(label = paste0("Qualifying events", "\n", .data$label)) |>
      dplyr::select("label")
    
    att <- NULL
  } else {
    att <- xn |>
      dplyr::filter(.data$reason_id > min(.data$reason_id)) |>
      dplyr::mutate(label = paste0(
        "N subjects = ", .data$excluded_subjects, "\nN records = ", .data$excluded_records
      )) |>
      dplyr::select("reason", "label")
    
    xn <- xn |>
      dplyr::mutate(reason_id = as.numeric(.data$reason_id)) |>
      dplyr::mutate(
        label = dplyr::if_else(
          .data$reason_id == min(.data$reason_id),
          paste0("Initial events", "\n", .data$label),
          dplyr::if_else(
            .data$reason_id == max(.data$reason_id),
            paste0("Final events", "\n", .data$label),
            .data$label
          )
        )
      ) |>
      dplyr::select("label")
  }
  return(list("xn" = xn, "att" = att))
}

getWidthMainBox <- function(xn) {
  return(0.08 * max(nchar(strsplit(xn$label[1], split = "\n")[[1]])))
}

getSingleNode <- function(xg, xn, w1) {
  k <- 1
  xn$label[k] <- gsub("Qualifying events", "Initial events", xn$label[k])
  xg <- xg %>%
    DiagrammeR::add_node(
      label = xn$label[k],
      node_aes = DiagrammeR::node_aes(
        shape = "box",
        x = 1,
        width = w1,
        y = 1,
        height = 0.6,
        fontsize = 11, fontcolor = "black",
        fontname = "Calibri",
        penwidth = 2,
        color = "black",
        fillcolor = "#F0F8FF"
      )
    )
}

getPositionMainBox <- function(xn, n, h2) {
  return(n + 1 - seq_len(n) - cumsum(append(0, h2 - 0.2)))
}

getPositionMiddleBox <- function(p1) {
  return((p1[1:(length(p1) - 1)] - p1[2:length(p1)]) / 2 + (p1[2:(length(p1))]))
}

validateReason <- function(att) {
  max_value <- 39
  
  n_char <- nchar(att$reason)
  n_char_count <- round(n_char / max_value)
  n_char_count[n_char_count > 1] <- n_char_count[n_char_count > 1]
  n_char_count[n_char_count == 1 & n_char < max_value] <- 0
  
  for (k in seq_len(nrow(att))) {
    cut <- seq_len(n_char_count[k])
    empty_positions <- stringr::str_locate_all(att$reason[k]," ") |> unlist() |> unique()
    
    if(n_char_count[k] != 0){
      p <- stats::quantile(empty_positions, probs = seq_len(n_char_count[k])/(n_char_count[k]+1))
      matrix_positions <- matrix(empty_positions, length(cut), length(empty_positions), byrow = TRUE)
      positions <- unique(matrix_positions[seq_len(length(cut)), apply(abs(matrix_positions - p), 1, which.min)])
      for(kk in positions){
        substr(att$reason[k], start = kk, stop = kk) <- "\n"
      }
    }
  }
  
  return(att)
}

getHeightMiddleBox <- function(att) {
  return((stringr::str_count(att$reason, pattern = "\n") + 1) * 0.2)
}

getWidthMiddleBox <- function(att) {
  return(min(2.25, 0.08 * max(nchar(unlist(strsplit(att$reason, "\n"))))))
}

getNodes <- function(xn, att, n, xg, h2, w1, p1, w2, p2) {
  for (k in seq_len(n)) {
    xg <- xg %>%
      DiagrammeR::add_node(
        label = xn$label[k],
        node_aes = DiagrammeR::node_aes(
          shape = "box",
          x = 1,
          width = w1,
          y = p1[k] + ifelse(k == 1, 0.1, 0) + ifelse(k == n, -0.1, 0),
          height = ifelse(k == 1 | k == n, 0.6, 0.4),
          fontsize = 11, fontcolor = "black",
          fontname = "Calibri",
          penwidth = ifelse(k == 1 | k == n, 2, 1),
          color = "black",
          fillcolor = "#F0F8FF"
        )
      )
    if (k > 1) {
      xg <- xg %>%
        DiagrammeR::add_edge(from = k - 1, to = k, edge_aes = DiagrammeR::edge_aes(color = "black"))
    }
  }
  
  if (n > 1) {
    for (k in seq_len(nrow(att))) {
      xg <- xg %>%
        DiagrammeR::add_node(
          label = att$label[k],
          node_aes = DiagrammeR::node_aes(
            shape = "box",
            x = 3,
            width = 1.2,
            y = p2[k],
            height = 0.4,
            fontsize = 9,
            fillcolor = "grey",
            fontcolor = "black",
            color = "black",
            fontname = "Calibri"
          )
        ) %>%
        DiagrammeR::add_node(
          label = att$reason[k],
          node_aes = DiagrammeR::node_aes(
            shape = "box",
            x = 1,
            width = w2,
            y = p2[k],
            height = h2[k],
            fillcolor = "white",
            color = "black",
            fontcolor = "back",
            fontname = "Calibri",
            fontsize = 10
          )
        ) %>%
        DiagrammeR::add_edge(
          from = 2 * k + n, to = 2 * k + n - 1, edge_aes = DiagrammeR::edge_aes(color = "black")
        )
    }
  }
  return(xg)
}


# Export flextables to excel helper ----
exportMultipleFlextablesToExcel <- function(ft_list, file) {
  wb <- openxlsx2::wb_workbook()
  
  for (sheet_name in names(ft_list)) {
    ft <- darwin_apply_flextable_theme(ft_list[[sheet_name]])
    wb$add_worksheet(sheet_name)
    wb <- flexlsx::wb_add_flextable(wb, sheet = sheet_name, ft = ft, dims = "C2")
  }
  
  wb$save(file)
}


#' Apply secondary min-cell-count censoring for drug utilisation tables.
#'
#' When number records or number subjects are censored for a CDM within a
#' cohort/strata context, all other estimates for that CDM are set to "-".
#' Grouping ignores additional_name/additional_level so censoring propagates
#' across concept set and ingredient splits (unlike omopgenerics::suppress()).
applyDusSecondaryCensoring <- function(result, minCellCount = NULL) {
  result <- omopgenerics::validateResultArgument(result)
  
  if (is.null(minCellCount)) {
    set <- omopgenerics::settings(result)
    if (!is.null(set) && "min_cell_count" %in% colnames(set)) {
      minCellCount <- unique(stats::na.omit(as.numeric(set$min_cell_count)))
    }
    if (length(minCellCount) != 1) {
      minCellCount <- 5L
    }
  }
  minCellCount <- as.integer(minCellCount)
  censor_label <- paste0("<", minCellCount)
  
  is_censored_count <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    x == "-" |
      x == censor_label |
      (!is.na(num) & num > 0 & num < minCellCount)
  }
  
  group_cols <- intersect(
    c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level"),
    colnames(result)
  )
  
  censored_groups <- result %>%
    dplyr::filter(
      .data$variable_name %in% c("number records", "number subjects"),
      .data$estimate_name == "count",
      is_censored_count(.data$estimate_value)
    ) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))
  
  if (nrow(censored_groups) == 0) {
    return(result)
  }
  
  result %>%
    dplyr::left_join(
      censored_groups %>% dplyr::mutate(.censor_group = TRUE),
      by = group_cols
    ) %>%
    dplyr::mutate(
      .censor_group = dplyr::coalesce(.data$.censor_group, FALSE),
      estimate_value = dplyr::if_else(
        .data$.censor_group &
          !(.data$variable_name %in% c("number records", "number subjects") &
              .data$estimate_name == "count"),
        "-",
        dplyr::if_else(
          .data$.censor_group &
            .data$variable_name %in% c("number records", "number subjects") &
            .data$estimate_name == "count" &
            is_censored_count(.data$estimate_value),
          "-",
          .data$estimate_value
        )
      )
    ) %>%
    dplyr::select(-".censor_group")
}

# Clean labels for TreatmentPatternsModule
clean_labels = function(x) {
  x <- gsub("_event_\\d+", "", x)   # remove event suffix
  x <- gsub("_", ",", x)            # replace underscores with commas
  trimws(x)
}
