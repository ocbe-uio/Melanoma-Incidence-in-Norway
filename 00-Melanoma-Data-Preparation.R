## -- PATHS ----------------
ROOT <- Sys.getenv("ROOT")
DATA <- Sys.getenv("Data")
METADATA <- file.path(DATA, "00-metadata")
RAW <- file.path(DATA, "00-raw")
PROCESSED <- file.path(DATA, "00-processed")

## -- Source Script ----------------
source(file.path(ROOT, "00-common/00-CodeMap.R"))
source(file.path(ROOT, "00-common/00-Functions.R"))

## -- Load Packages ----
Fn$quietly_load(c(
  "tidytable", "data.table", "purrr", "stringr", 
  "lubridate", "readxl", "here", "haven"
))

## -- Local Function ----
MapApply <- function(map, var) {
  var[var == ""] <- NA
  fifelse(is.na(var), map(var), paste(var, map(var), sep = ": "))
}

## -- Raw Excel Data -> CSV and Rds  ----------------
Original <- data.table::fread(
  file = here(RAW, 'Original.csv'), 
  sep = ";", 
  dec = ',',
  header = TRUE, 
  colClasses = 'character', 
  na.strings = c('NA', '')
)

if (!file.exists(here(RAW, "Raw.xlsx"))) {
  writexl::write_xlsx(
    x = Original, 
    path = here(RAW, "Raw.xlsx"), 
    col_names = TRUE, 
    format_headers = TRUE
  )
}

if (!file.exists(here(RAW, "Raw.csv"))) {
  fwrite(
    file = here(RAW, "Raw.csv"),
    x = Original, sep = ";", dec = ","
  )
}

if (!file.exists(here(RAW, "Raw.rds"))) {
  saveRDS(Original, here(RAW, "Raw.rds"))
}

if (!file.exists(here(RAW, "Raw.dta"))) {
  write_dta(Original, here(RAW, "Raw.dta"))
}


## -- Raw -> Rename -> New variables -> Mutate variables ----------------
#| Read CSV and rename variable from Norsk to English
#| Create additional variables
#| Save the clean data in 00-processed folder for further use
Data <- copy(as_tidytable(Original))

## -- Rename ----------------
local({
  names_map <- readxl::read_excel(
    path = here(METADATA, "CodeBook.xlsx"), 
    sheet = 2, 
    range = "B1:C50"
  )
  
  setnames(
    Data, 
    names_map[["Original Variable"]], 
    names_map[["New Variable"]], 
    skip_absent = TRUE
  )
})

## -- Make some variable factor ----------------
Data[, c(1:ncol(Data)) := Map(CodeMap$var_class, names(.SD), .SD)]

## -- Additional variables ----------------
setnames(Data, "Gender", "Sex", skip_absent = TRUE)
Data[, Sex := as.factor(CodeMap$sex_map(Sex))]
Data[, Season := as.factor(CodeMap$season(DiagDate))]
Data[, DiagYear := as.numeric(format(DiagDate, "%Y"))]
Data[, StatusYear := as.numeric(format(StatusDate, "%Y"))]
Data[, AgeDiag := DiagYear - BirthYear]
Data[, AgeStatus := StatusYear - BirthYear]
Data[, AnatomicSite := as.factor(CodeMap$localization_class(Localization))]
Data[, AgeGroup := CodeMap$age_map(AgeDiag)]
Data[, AgeCat := CodeMap$age_map(AgeDiag, breaks = c(0, seq(40, 80, 20), Inf))]
Data[, MelanomaType := as.factor(CodeMap$histology_map(substr(MorphologyICDO3, 0, 5)))]
Data[, ClinicalStage := as.factor(CodeMap$ClinicalStageMap(Metastasis))]
Data[, HealthRegion := as.factor(CodeMap$HealthRegion(HealthRegion))]
Data[, EndDate := StatusDate]
Data[is.na(EndDate) & Status == 1, EndDate := as.Date("2019-12-15")]
Data[, SurvivalMonth := Fn$diff_month2(EndDate, DiagDate, unit = "months")]

## ---- Parse Tumour variables ----
local({
  tumour_vars <- stringr::str_subset(names(Data), "Tumour")
  Data[, c(tumour_vars) := lapply(.SD, Fn$parse_breslow), .SDcols = tumour_vars]
})

## ---- Parse Ulceration variables ----
setnames(Data, "Ulceration", "Ulceration1")

local({
  ulceration_vars <- grep("Ulceration", names(Data), value = TRUE)
  Data[, (ulceration_vars) := lapply(.SD, Fn$parse_ulceration), 
            .SDcols = ulceration_vars]
})

## ---- Combine Breslow variables into one variable ----
Data[, BreslowThickness := Fn$combine_breslow(.SD), 
          .SDcols = grep('Tumour|Ulceration', names(Data))]

## ---- Combine Ulceration variables into one variable ----
Data[, Ulceration := Fn$combine_ulceration(.SD), 
          .SDcols = grep('Tumour|Ulceration', names(Data))]

## ---- Stages from Breslow and Ulceration ----
Data[, BreslowTstage := CodeMap$get_Tstage(BreslowThickness)]
Data[, BreslowTSubStage := Fn$get_Tsubstage(
  BreslowThickness,
  yes = fifelse(Ulceration == "Present", TRUE, NA),
  no = fifelse(Ulceration == "Absent", TRUE, NA),
  missing = fifelse(Ulceration == "Missing", TRUE, NA)
)]

## ---- Change Levels of Ordered Factors ----
local({
  ordered_factor_cols <- c(
    "AJCC", "pTstage", "pNstage", "pMstage", "AgeGroup", "AgeCat",
    "BreslowTstage", "BreslowTSubStage"
  )
  unordered_factor_cols <- c(
    "HealthRegion", "Season", "AnatomicSite",
    "MelanomaType", "ClinicalStage", "Sex"
  )
  
  Data[, (ordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
            .SDcols = ordered_factor_cols
  ]
  Data[, (unordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
            .SDcols = unordered_factor_cols
  ]
})

## -- Drop the levels from Tstage and DiagSafety if any ------------------------------
Data[, pTstage := droplevels(pTstage)]
Data[, DiagSafety := droplevels(DiagSafety)]

## -- Re-code variables ----
Data[, DiagSafety := as.factor(MapApply(CodeMap$diag_safety_map, DiagSafety))]
Data[, Localization := as.factor(MapApply(CodeMap$localization_map, Localization))] 
Data[, Topography := as.factor(MapApply(CodeMap$topo_icdo3_map, Topography))] 
Data[, TopographyICDO3 := as.factor(MapApply(CodeMap$topo_icdo3_map, TopographyICDO3))] 
Data[, Histology := as.factor(Histology)] 
Data[, MorphologyICDO3 := as.factor(MapApply(CodeMap$morphology_icdo3_map, MorphologyICDO3))] 
Data[, DiagBasis := as.factor(MapApply(CodeMap$diag_basis_map, DiagBasis))] 
Data[, Metastasis := as.factor(MapApply(CodeMap$MetastasisMap, Metastasis))] 
Data[, Surgery := as.factor(MapApply(CodeMap$surgery_map, Surgery))] 
Data[, Radiotherapy := as.factor(MapApply(CodeMap$radiotherapy_map, Radiotherapy))] 
Data[, Side := fifelse(Side == "H", "Right", "Left")] 
Data[, TopographyICD10 := as.factor(MapApply(CodeMap$topo_icd10_map, TopographyICD10))]
Data[, Status := as.factor(MapApply(CodeMap$status_map, Status))] 
Data[, UVRegion := as.factor(MapApply(CodeMap$uvr_map, as.numeric(as.character(County))))] 
Data[, CountyCode := formatC(as.numeric(as.character(County)), width = 2, flag = "0")] 
Data[, County := as.factor(MapApply(CodeMap$county_map, County))]
Data[Ulceration == "Missing", Ulceration := NA_character_]
Data[, DiagYear10 := CodeMap$year_10(DiagYear)]
Data[, DiagYear5 := CodeMap$year_5(DiagYear)]
Data[, YearCat := CodeMap$year_cat(DiagYear)]
Data[, AgeGroup5 := CodeMap$age_map2(AgeDiag, step = 5)]
Data[, AgeGroup10 := CodeMap$age_map2(AgeDiag, step = 10)]
Data[, BirthCohort5 := CodeMap$year_range(1850, 2020, 5)(BirthYear)]
Data[, BirthCohort10 := CodeMap$year_range(1850, 2020, 10)(BirthYear)]

## -- Save Processed Data ----------------
if (!file.exists(here(PROCESSED, "Processed.csv"))) {
  fwrite(
    file = here(PROCESSED, "Processed.csv"),
    x = Data, sep = ";", dec = ","
  )
}

if (!file.exists(here(PROCESSED, "Processed.rds"))) {
  saveRDS(Data, here(PROCESSED, "Processed.rds"))
}

if (!file.exists(here(PROCESSED, "Processed.dta"))) {
  write_dta(Data, here(PROCESSED, "Processed.dta"))
}



