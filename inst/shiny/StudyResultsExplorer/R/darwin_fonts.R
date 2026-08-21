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

# DARWIN deliverable typography (QC Checklist V6.0): Calibri 11 pt figures, 9 pt tables.
# See .cursor/rules/darwin-deliverables-table-figure-formatting.mdc

DARWIN_BLUE <- "#003399"
DARWIN_FIG_FONT_SIZE <- 11L
DARWIN_TABLE_FONT_SIZE <- 9L

.darwin_showtext_active <- FALSE

#' Resolved font family (Calibri when available, else Carlito / sans-serif).
darwin_font_family <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    path <- systemfonts::match_font("Calibri")$path
    if (nzchar(path) && file.exists(path)) {
      return("Calibri")
    }
  }
  "Carlito, Calibri, sans-serif"
}

#' Register Calibri for ggplot/ggsave when showtext is installed.
register_darwin_fonts <- function() {
  family <- darwin_font_family()
  
  if (requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE) &&
      identical(family, "Calibri")) {
    path <- systemfonts::match_font("Calibri")$path
    if (nzchar(path) && file.exists(path)) {
      sysfonts::font_add("Calibri", regular = path)
      showtext::showtext_auto()
      .darwin_showtext_active <<- TRUE
    }
  }
  
  if (requireNamespace("flextable", quietly = TRUE)) {
    flextable::set_flextable_defaults(
      font.family = if (identical(family, "Calibri")) "Calibri" else "Carlito",
      font.size = DARWIN_TABLE_FONT_SIZE,
      border.color = DARWIN_BLUE
    )
  }
  
  invisible(family)
}

darwin_showtext_active <- function() {
  isTRUE(.darwin_showtext_active)
}

#' ggplot2 theme: 11 pt Calibri, white background (DARWIN figure spec).
darwin_fig_theme <- function() {
  ggplot2::theme_minimal(
    base_family = darwin_font_family(),
    base_size = DARWIN_FIG_FONT_SIZE
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.text = ggplot2::element_text(
        size = DARWIN_FIG_FONT_SIZE,
        family = darwin_font_family()
      ),
      axis.text = ggplot2::element_text(
        size = DARWIN_FIG_FONT_SIZE,
        family = darwin_font_family()
      ),
      axis.title = ggplot2::element_text(
        size = DARWIN_FIG_FONT_SIZE,
        family = darwin_font_family()
      ),
      plot.title = ggplot2::element_text(
        size = DARWIN_FIG_FONT_SIZE,
        family = darwin_font_family()
      )
    )
}

#' Apply 11 pt Calibri to a plotly object.
darwin_plotly_font <- function(pl) {
  plotly::layout(
    pl,
    font = list(
      family = darwin_font_family(),
      size = DARWIN_FIG_FONT_SIZE,
      color = "black"
    ),
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  )
}

#' Global CSS for Shiny DT / dashboard tables (9 pt Calibri).
darwin_table_css <- function() {
  fam <- darwin_font_family()
  htmltools::tags$style(htmltools::HTML(paste0(
    "table.dataTable, table.dataTable th, table.dataTable td ",
    "{ font-family: ", fam, "; font-size: ", DARWIN_TABLE_FONT_SIZE, "pt; }\n",
    ".dataTables_wrapper { font-family: ", fam, "; }\n",
    ".content-wrapper, .main-header { font-family: ", fam, ", sans-serif; }"
  )))
}

#' Call before ggsave so showtext honours export dpi.
darwin_prepare_ggsave <- function(dpi = 96) {
  if (darwin_showtext_active() && requireNamespace("showtext", quietly = TRUE)) {
    showtext::showtext_opts(dpi = dpi)
  }
  invisible(NULL)
}

#' Post-process flextable exports to DARWIN table typography.
darwin_apply_flextable_theme <- function(ft) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(ft)
  }
  fam <- if (identical(darwin_font_family(), "Calibri")) "Calibri" else "Carlito"
  ft %>%
    flextable::font(fontname = fam, part = "all") %>%
    flextable::fontsize(size = DARWIN_TABLE_FONT_SIZE, part = "body") %>%
    flextable::fontsize(size = DARWIN_TABLE_FONT_SIZE, part = "footer")
}
