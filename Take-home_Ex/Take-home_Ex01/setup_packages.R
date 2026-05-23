# =============================================================================
# Take-home Exercise 1 — R package setup
# Run this ONCE before rendering index.qmd or exec_summary.qmd
# =============================================================================

# Choose a fast CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Bootstrap pacman, then use it
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  # ---- Core data manipulation ----
  tidyverse,    # readr, dplyr, tidyr, ggplot2, forcats, purrr, stringr, tibble
  lubridate,    # date handling
  scales,       # axis formatters

  # ---- Reporting & tables ----
  knitr, kableExtra, glue,

  # ---- ggplot2 extensions ----
  ggridges,     # density ridges
  ggdist,       # raincloud, halfeye, dot-density
  ggrepel,      # smart label placement
  ggstatsplot,  # statistical comparison plots with tests embedded
  ggcorrplot,   # correlation matrix visualisation
  ggpubr,       # publication-ready helpers
  patchwork,    # combining plots
  ggh4x,        # nested facets
  ggtext,       # markdown in text geoms

  # ---- Interactive visualisation (Bonus) ----
  plotly,       # plot_ly() interactive charts
  ggiraph,      # interactive ggplot via SVG
  crosstalk,    # linked brushing across widgets
  DT,           # interactive data tables

  # ---- Statistics / extras ----
  performance,  # model diagnostics
  FSA           # Dunn post-hoc test
)

cat("\n========================================\n")
cat("  All packages loaded successfully.\n")
cat("  You can now render index.qmd or exec_summary.qmd.\n")
cat("========================================\n")
