## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
DATA <- Sys.getenv("Data")
INC <- file.path(DATA, "01-incidence")

STEP_PATH <- file.path(INC, "StepData")
if (!dir.exists(STEP_PATH)) dir.create(STEP_PATH)

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))


## -- Load Packages ----
Fn$quietly_load(c(
  "tidytable", "data.table", "forcats",
  "purrr", "ggplot2", "stringr"
))

## -- Load the processed data ----------------
Data <- readRDS(file.path(DATA, "00-processed", "Processed.rds"))
Before80 <- fread(file.path(DATA, "00-raw", "CaseCount-Before-1980.csv"))

## == Exclusion Inclusion Criteria ======================================

## -- Step1: All cases from CRN ----------------
StepData <- list(Step1 = copy(Data))
attr(StepData[["Step1"]], "Comment") <- paste(
  "All melanoma diagnosis from the Cancer Registry of Norway 1980-2019"
)

## -- Step2: Remove all patients who were diagnosed before 1983 ----------------
Data <- Data[!(PID %in% unique(c(Data[DiagYear < 1983, PID], Before80[, PID])))]
# Number of Cases: N = 48517 (Before: 50527)
StepData <- append(StepData, list(Step2 = copy(Data)))
attr(StepData[["Step2"]], "Comment") <- paste(
  "Patients having a melanoma diagnosis before 1983 were excluded."
)

## -- Step3: Take only the first invasive cases ----
Data <- slice(Data, which.min(DiagDate), .by = PID)
# Data <- Data[Data[, .I[which.min(DiagDate)], by = PID]$V1]
# Number of Cases: N = 45639 (Before: 48517)
StepData <- append(StepData, list(Step3 = copy(Data)))
attr(StepData[["Step3"]], "Comment") <- paste(
  "Keeping only first primary cases.",
  "Here, we only keep the first diagnosed case for each patients.<br>",
  "If there are more than one case on the same day,",
  "we keep the first instance of the cases as we received from CRN."
)

## -- Step3a: Remove carcinoma category (Morph. Code: 8010) ----
#| This step is removed as there are not any cases of carcinoma after 1983
# Data <- Data[str_detect(MorphologyICDO3, "8010", negate = TRUE)]
# # Number of Cases: N = 45639 (Before: 45639)
# StepData <- append(StepData, list(Step3a = copy(Data)))
# attr(StepData[["Step3a"]], "Comment") <- paste(
#   "Any cases with carcinoma category were removed",
#   "i.e. Morph. Code: 8010"
# )

## -- Step4: Remove In-situ melanoma ------------------------------
Data <- Data[pTstage != "is" | is.na(pTstage)]
# Number of Cases: N = 45635 (Before: 45639)
StepData <- append(StepData, list(Step4 = copy(Data)))
attr(StepData[["Step4"]], "Comment") <- paste(
  "Exclusion of in-situ melanoma.",
  "Only first primary invasive melanoma were left after this step."
)

## -- Step5: Remove cases not histologically verified ------------------------------
Data <- Data[!grepl("^5", DiagSafety) | is.na(DiagSafety)]
# Number of Cases: N = 45631 (Before: 45635)
StepData <- append(StepData, list(Step5 = copy(Data)))
attr(StepData[["Step5"]], "Comment") <- paste(
  "Cases that were not histologically verified were excluded",
  "i.e., `!grepl('^5', DiagSafety) | is.na(DiagSafety)`"
)

## -- Step6: Melanoma not verified ----
Data <- Data[!grepl("^98", DiagBasis)]
# Number of Cases: N = 45615 (Before: 45631)
StepData <- append(StepData, list(Step6 = copy(Data)))
attr(StepData[["Step6"]], "Comment") <- paste(
  "Cases with melanoma not verified were excluded,",
  "i.e, `!grepl('^98', DiagBasis)`"
)

## -- Step7: Remove the cases where StatusDate < DiagDate ----
Data <- Data[EndDate >= DiagDate] 
# Number of Cases: N = 45586 (Before: 45615)
StepData <- append(StepData, list(Step7 = copy(Data)))
attr(StepData[["Step7"]], "Comment") <- paste(
  "Cases with `StatusDate < DiagDate` were excluded,",
  "i.e., emigrated before diagnosis."
)

## -- Step8: Remove cases with death certificate only ------------------------------
Data <- Data[!grepl("^90", DiagBasis)]
# Number of Cases: N = 45550 (Before: 45586)
StepData <- append(StepData, list(Step8 = copy(Data)))
attr(StepData[["Step8"]], "Comment") <- paste(
  "Cases diagnosed based on death certificate only,",
  "i.e. exclude cases if DiagBasis started from 90"
)

## -- Step9: Remove cases with autopsy ------------------------------
Data <- Data[!grepl("^8", DiagBasis)]
# Number of Cases: N = 45524 (Before: 45550)
StepData <- append(StepData, list(Step9 = copy(Data)))
attr(StepData[["Step9"]], "Comment") <- paste(
  "Cases diagnosed based on  autopsy only,",
  "i.e., exclude if DiagBasis started from 8"
)

## ---- Save dataset at different steps -------------------------------
if (!file.exists(file.path(INC, "StepData.rds"))) {
  saveRDS(StepData, file.path(INC, "StepData.rds"))
}

iwalk(StepData, function(dta, .name) {
  fpath <- here::here(STEP_PATH, glue::glue("{.name}"))
  if (!dir.exists(fpath)) dir.create(fpath)
  
  if (!file.exists(glue::glue("{fpath}/{.name}.csv"))) {
    fwrite(dta, glue::glue("{fpath}/{.name}.csv"))
  }
  if (!file.exists(glue::glue("{fpath}/{.name}.dta"))) {
    haven::write_dta(dta, glue::glue("{fpath}/{.name}.dta"))
  }
  if (!file.exists(glue::glue("{fpath}/{.name}.rds"))) {
    saveRDS(dta, glue::glue("{fpath}/{.name}.rds"))
  }
})

## -- Clean data removing unnecessary variables ----------------
Complete <- copy(Data)
## ---- Stage1: Removing variables ----
local({
  var_discarded_stage1 <- c(
    "TumourThickness", "TumourThickness1", "TumourThickness2",
    "BreslowMissing1", "BreslowMissing2", "LocalRelapseBiopsy",
    "LocalRelapseBiopsyDate", "ExcisionLocalRelapse", "ExcisionLocalRelapseDate",
    "ExtExcisionLocalRelapse", "ExtExcisionLocalRelapseDate", "DiagYear1",
    "DiagYear2", "UlcerationNoReport1", "UlcerationNoReport2", "UlcerationYes1",
    "UlcerationYes2", "UlcerationNO1", "UlcerationNO2", "Ulceration1",
    "Topography", "TopographyICDO3", "Histology", 
    "MorphologyICDO3", "TopographyICD10"
  )
  Complete[, (var_discarded_stage1) := NULL]
})

## ---- Save clean data with PID and SID ----
if (!file.exists(file.path(INC, "Complete.csv"))) {
  fwrite(Complete, file.path(INC, "Complete.csv"))
}
if (!file.exists(file.path(INC, "Complete.dta"))) {
  haven::write_dta(Complete, file.path(INC, "Complete.dta"))
}
if (!file.exists(file.path(INC, "Complete.rds"))) {
  saveRDS(Complete, file.path(INC, "Complete.rds"))
}

## -- Slim data: Smaller set of variables ----
Slim <- copy(Complete)
Slim <- Slim[, .(
  PID,
  SID,
  BirthYear,
  Sex,
  DiagYear,
  DiagDate,
  StatusYear,
  StatusDate,
  EndDate,
  Status,
  Thickness = BreslowThickness,
  Tstage = BreslowTstage,
  TSubStage = BreslowTSubStage,
  Ulceration,
  ClinicalStage,
  DeathCause,
  HealthRegion,
  County,
  CountyCode,
  Age = AgeDiag,
  AgeStatus,
  Season,
  AnatomicSite,
  MelanomaType,
  AgeGroup,
  AgeCat,
  AgeGroup5,
  AgeGroup10,
  YearCat,
  DiagYear5,
  DiagYear10,
  BirthYear5 = BirthCohort5,
  BirthYear10 = BirthCohort10,
  SurvivalMonth
)]

if (!file.exists(file.path(INC, "Slim.csv"))) {
  fwrite(Slim, file.path(INC, "Slim.csv"))
}
if (!file.exists(file.path(INC, "Slim.dta"))) {
  haven::write_dta(Slim, file.path(INC, "Slim.dta"))
}
if (!file.exists(file.path(INC, "Slim.rds"))) {
  saveRDS(Slim, file.path(INC, "Slim.rds"))
}

