## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
INC <- Sys.getenv("DataInc")
INC_ROOT <- file.path(ROOT, "01-incidence")
TDATA <- file.path(INC_ROOT, "02-results", "TableData")

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))

source(here::here(INC_ROOT, "01-Scripts", "04a-TableData.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "tidytable", "data.table", "purrr", "stringr", 
  "gt", "gtsummary", "kableExtra"
))

## -- Run all the functions in TableData ----------------
#| Running all the function will create and 
#| save the table data in Results > TableData

c("CaseExclusion", "AltTable", "SummaryTable") %>% 
  walk(function(.name) {
    cat(glue::glue("Running and saving {.name}."), "\n")
    invisible(TableData[[.name]]())
  })

## -- Table Data: Sensitivity: Count Comparison ----
cat(glue::glue("Saving count comparison by tstage"), "\n")
invisible(TableData$CountComparison())

## -- Table Data: Sensitivity: Count Comparison by Type ----
cat(glue::glue("Saving count comparison by subtype"), "\n")
invisible(TableData$CountComparison(by = "MelanomaType"))

## -- Table Data: Sensitivity: HR Comparison ----
cat(glue::glue("Saving surv comparison by tstage"), "\n")
invisible(TableData$SurvComparison())

## -- Table Data: APC  ----
tblNames <- ls(TableData[["AAPC"]], pattern = "^By*")
for (fname in tblNames) {
  cat(glue::glue("Saving AAPC table: {fname}"), "\n")
  invisible(with(TableData, AAPC[[fname]]()))
}

## -- Table Data: Count and Proportion ----------------
cat(glue::glue("Saving results Count and Proportion."), "\n")
invisible(TableData$PropData())
