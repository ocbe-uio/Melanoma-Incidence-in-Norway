## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
INC_ROOT <- file.path(ROOT, "01-incidence")
INC <- Sys.getenv("DataInc")

plot_path <- here::here(INC_ROOT, "02-Results", "PlotData")
if (!dir.exists(plot_path)) dir.create(plot_path, recursive = TRUE)

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))
source(here::here(INC_ROOT, "01-Scripts", "03a-PlotData.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "tidytable", "data.table", "purrr", "stringr", 
  "plotly", "ggplot2"
))

## -- Plot Data: Spline Plots ----
splNames <- names(PlotData[["Spline"]])
for (fname in splNames) {
  cat(glue::glue("Saving results {fname}: {plot_path}/Spline-{fname}"), "\n")
  invisible(with(PlotData, Spline[[fname]]()))
  invisible(with(PlotData, Spline[[fname]](logY = FALSE)))
}

## -- Plot Data: Age-adjusted Plots ----
adjNames <- names(PlotData[["AgeAdjusted"]])
for (fname in adjNames) {
  cat(glue::glue("Saving results {fname}: {plot_path}/AgeAdj-{fname}"), "\n")
  invisible(with(PlotData, AgeAdjusted[[fname]](segmented = FALSE)))
  invisible(with(PlotData, AgeAdjusted[[fname]](segmented = FALSE, logY = FALSE)))
}

## -- Plot Data: Segmented Plots ----
adjNames <- names(PlotData[["AgeAdjusted"]])
for (fname in adjNames) {
  cat(glue::glue("Saving results {fname}: {plot_path}/AgeAdj-{fname}"), "\n")
  invisible(with(PlotData, AgeAdjusted[[fname]](segmented = TRUE)))
  invisible(with(PlotData, AgeAdjusted[[fname]](segmented = TRUE, logY = FALSE)))
}

## -- Plot Data: Age-specific Plots ----
cat(glue::glue("Saving results ASP by sex and tstage: {plot_path}/AgeSp-BySexTstage"), "\n")
invisible(PlotData[["AgeSpecific"]](BySex = TRUE, Group = "Tstage", filter = NULL))

## -- Plot Data: APC Plots ----
for (sex in c(TRUE, FALSE)) {
  for (group in c("Overall", "Tstage")) {
    fname <- glue::glue("APC")
    if (sex) fname <- glue::glue(fname, "-Sex")
    if (group == "Tstage") fname <- glue::glue(fname, "-Tstage")
    
    cat(glue::glue("Saving results {fname}: {plot_path}/{fname}"), "\n")
    if (group == "Overall") {
      invisible(PlotData$APC(BySex = sex, Group = NULL))
    } else {
      invisible(PlotData$APC(BySex = sex, Group = group))
    }
    
  }
}

## -- Plot Data: Missing Trend ----
cat(glue::glue("Saving results MissingTrend-Overall."), "\n")
invisible(PlotData$MissingTrend())

## -- Plot Data: Count and Proportion ----------------
cat(glue::glue("Saving results Count and Proportion."), "\n")
invisible(PlotData$PropData())

