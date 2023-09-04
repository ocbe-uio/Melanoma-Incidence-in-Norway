## -- Source Scripts ----
source(here::here(BASE_PATH, "Scripts/00-CodeMap.R"))
source(here::here(BASE_PATH, "Scripts/00-Functions.R"))

## -- Load Packages ----
Fn$quietly_load(c("purrr", "ggplot2", "stringr"))

if (file.exists(here::here(CodeMap$DATA_PATH, "Composite", "Data.rds"))) {
  Data <- readRDS(here::here(CodeMap$DATA_PATH, "Composite", "Data.rds"))
} else {
  ## -- Prepare Data ----
  Data <- new.env()
  
  evalq({
    DATA_PATH <- evalq(DATA_PATH, CodeMap)
    
    ## -- Read data from csv if Rds format not found ----
    # Cases in Original Data: 50527
    if (file.exists(file.path(DATA_PATH, "Original.rds"))) {
      Original <- readRDS(file.path(DATA_PATH, "Original.rds"))
    } else {
      if (file.exists(file.path(DATA_PATH, "Main", "Original.csv"))) {
        Original <- data.table::fread(
          file = file.path(DATA_PATH, "Main", 'Original.csv'), 
          sep = ";", 
          dec = ',',
          header = TRUE, 
          colClasses = 'character', 
          na.strings = c('NA', '')
        )
      } else {
        Fn$quietly_load("readxl")
        Original <- read_excel(
          path = file.path(DATA_PATH, "Main", "Original.xlsx"),
          sheet = "Dataset",
          col_types = "text"
        )
        setDT(Original)
      }
      saveRDS(Original, file.path(DATA_PATH, "Original.rds"))
    }
    
    ## -- Change variable name to English ----
    if (file.exists(file.path(DATA_PATH, "RawData.rds"))) {
      RawData <- readRDS(file.path(DATA_PATH, "RawData.rds"))
    } else {
      RawData <- copy(Original)
      local({
        names_map <- readxl::read_excel(
          path = file.path(DATA_PATH, "Main", "CodeBook.xlsx"), 
          sheet = 2, 
          range = "B1:C50"
        )
        
        setnames(
          RawData, 
          names_map[["Original Variable"]], 
          names_map[["New Variable"]], 
          skip_absent = TRUE
        )
      })
      
      ## -- Change variable types ----
      RawData[, 1:ncol(RawData) := Map(CodeMap$var_class, names(.SD), .SD)]
      
      
      ## == START: REMOVE IRRELEVANT OBSERVATION ==============================
      StepData <- list(Step1 = copy(RawData))
      
      ## -- Only take the cases from the year 1983 ------------------------------
      RawData <- RawData[year(DiagDate) >= 1983]
      
      # Number of Cases: N = 48901 (Before: 50527)
      StepData <- append(StepData, list(Step2 = copy(RawData)))
      
      ## -- Take only the first invasive cases ----
      RawData <- RawData[RawData[, .I[which.min(DiagDate)], by = PID]$V1]
      
      # Number of Cases: N = 45929 (Before: 48901)
      StepData <- append(StepData, list(Step3 = copy(RawData)))
      
      ## -- Remove carcinoma category (Morph. Code: 8010) ----
      RawData <- RawData[str_detect(MorphologyICDO3, "8010", negate = TRUE)]
      # Number of Cases: N = 45929 (Before: 45929)
      StepData <- append(StepData, list(Step3a = copy(RawData)))
      
      ## -- Remove In-situ melanoma ------------------------------
      RawData <- RawData[Tstage != "is" | is.na(Tstage)]
      # Number of Cases: N = 45925 (Before: 45929)
      StepData <- append(StepData, list(Step4 = copy(RawData)))
      
      ## -- Remove cases not histologically verified ------------------------------
      RawData <- RawData[DiagSafety != "5" | is.na(DiagSafety)]
      # Number of Cases: N = 45921 (Before: 45925)
      StepData <- append(StepData, list(Step5 = copy(RawData)))
      
      ## -- Melanoma not verified ----
      RawData <- RawData[!grepl("^98", DiagBasis)]
      # Number of Cases: N = 45905 (Before: 45921)
      StepData <- append(StepData, list(Step6 = copy(RawData)))
      
      ## -- Remove the cases where StatusDate < DiagDate ----
      RawData <- RawData[, EndDate := StatusDate]
      RawData <- RawData[is.na(EndDate) & Status == 1, EndDate := as.Date("2019-12-15")]
      RawData <- RawData[EndDate >= DiagDate] 
      # Number of Cases: N = 45875 (Before: 45905)
      StepData <- append(StepData, list(Step7 = copy(RawData)))
      
      ## -- Remove cases with death certificate only ------------------------------
      RawData <- RawData[!grepl("^90", DiagBasis)]
      # Number of Cases: N = 45839 (Before: 45875)
      StepData <- append(StepData, list(Step8 = copy(RawData)))
      
      ## -- Remove cases with autopsy ------------------------------
      RawData <- RawData[!grepl("^8", DiagBasis)]
      # Number of Cases: N = 45812 (Before: 45839)
      StepData <- append(StepData, list(Step9 = copy(RawData)))
      
      ## == END: REMOVE IRRELEVANT OBSERVATION ============================== 
      
      ## ---- Rename Gender to Sex -------------------------------
      setnames(RawData, "Gender", "Sex", skip_absent = TRUE)
      StepData <- map(StepData, setnames, "Gender", "Sex", skip_absent = TRUE)
      
      ## ---- Extra Variables ----
      RawData[, Sex := as.factor(CodeMap$sex_map(Sex))]
      RawData[, Season := as.factor(CodeMap$season(DiagDate))]
      RawData[, DiagYear := as.numeric(format(DiagDate, "%Y"))]
      RawData[, StatusYear := as.numeric(format(StatusDate, "%Y"))]
      RawData[, AgeDiag := DiagYear - BirthYear]
      RawData[, AgeStatus := StatusYear - BirthYear]
      RawData[, AnatomicSite := as.factor(CodeMap$localization_class(Localization))]
      RawData[, AgeGroup := CodeMap$age_map(AgeDiag)]
      RawData[, MelanomaType := as.factor(CodeMap$histology_map(substr(MorphologyICDO3, 0, 5)))]
      RawData[, ClinicalStage := as.factor(CodeMap$ClinicalStageMap(Metastasis))]
      RawData[, HealthRegion := as.factor(CodeMap$HealthRegion(HealthRegion))]
      RawData[, SurvivalMonth := Fn$diff_month2(EndDate, DiagDate, unit = "months")]
      ## RawData[, SurvivalMonth := Fn$diff_month(EndDate, DiagDate)]
      
      ## ---- Save Dataset with English variable names -------------------------------
      saveRDS(RawData, file.path(DATA_PATH, "RawData.rds"))
      
      
      ## ---- Extra Variables in StepData ----
      StepData <- map(StepData, function(dta) {
        dta[, Sex := as.factor(CodeMap$sex_map(Sex))]
        dta[, Season := as.factor(CodeMap$season(DiagDate))]
        dta[, DiagYear := as.numeric(format(DiagDate, "%Y"))]
        dta[, StatusYear := as.numeric(format(StatusDate, "%Y"))]
        dta[, AgeDiag := DiagYear - BirthYear]
        dta[, AgeStatus := StatusYear - BirthYear]
        dta[, AnatomicSite := as.factor(CodeMap$localization_class(Localization))]
        dta[, AgeGroup := CodeMap$age_map(AgeDiag)]
        dta[, MelanomaType := as.factor(CodeMap$histology_map(substr(MorphologyICDO3, 0, 5)))]
        dta[, ClinicalStage := as.factor(CodeMap$ClinicalStageMap(Metastasis))]
        dta[, HealthRegion := as.factor(CodeMap$HealthRegion(HealthRegion))]
        
        ## ---- Parse Tumour variables ----
        local({
          tumour_vars <- stringr::str_subset(names(dta), "Tumour")
          dta[, c(tumour_vars) := lapply(.SD, Fn$parse_breslow), .SDcols = tumour_vars]
        })
        ## ---- Parse Ulceration variables ----
        setnames(dta, "Ulceration", "Ulceration1")
        
        local({
          ulceration_vars <- grep("Ulceration", names(dta), value = TRUE)
          dta[, (ulceration_vars) := lapply(.SD, Fn$parse_ulceration), 
              .SDcols = ulceration_vars]
        })
        
        ## ---- Combine Breslow variables into one variable ----
        dta[, BreslowThickness := Fn$combine_breslow(.SD), 
            .SDcols = grep('Tumour|Ulceration', names(dta))]
        
        ## ---- Combine Ulceration variables into one variable ----
        dta[, Ulceration := Fn$combine_ulceration(.SD), 
            .SDcols = grep('Tumour|Ulceration', names(dta))]
        
        ## ---- Stages from Breslow and Ulceration ----
        dta[, BreslowTstage := CodeMap$get_Tstage(BreslowThickness)]
        dta[, BreslowTSubStage := Fn$get_Tsubstage(
          BreslowThickness,
          yes = fifelse(Ulceration == "Present", TRUE, NA),
          no = fifelse(Ulceration == "Absent", TRUE, NA),
          missing = fifelse(Ulceration == "Missing", TRUE, NA)
        )]
        
        ## ---- Change Levels of Ordered Factors ----
        local({
          ordered_factor_cols <- c(
            "AJCC", "Tstage", "Nstage", "Mstage", "AgeGroup", 
            "BreslowTstage", "BreslowTSubStage"
          )
          unordered_factor_cols <- c(
            "HealthRegion", "Season", "AnatomicSite",
            "MelanomaType", "ClinicalStage", "Sex"
          )
          
          dta[, (ordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
              .SDcols = ordered_factor_cols
          ]
          dta[, (unordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
              .SDcols = unordered_factor_cols
          ]
        })
        
        ## -- Drop the levels from Tstage and DiagSafety if any ------------------------------
        dta[, Tstage := droplevels(Tstage)]
        dta[, DiagSafety := droplevels(DiagSafety)]
        
      })
      
      ## ---- Save dataset at different steps -------------------------------
      saveRDS(StepData, file.path(DATA_PATH, "StepData.rds"))
    }
    
    ## -- Clean Data removing unnecessary variables ----
    if (file.exists(file.path(DATA_PATH, "CleanData.rds"))) {
      CleanData <- readRDS(file.path(DATA_PATH, "CleanData.rds"))
    } else {
      CleanData <- copy(RawData)
      
      ## ---- Parse Tumour variables ----
      local({
        tumour_vars <- stringr::str_subset(names(CleanData), "Tumour")
        CleanData[, c(tumour_vars) := lapply(.SD, Fn$parse_breslow), .SDcols = tumour_vars]
      })
      
      ## ---- Parse Ulceration variables ----
      setnames(CleanData, "Ulceration", "Ulceration1")
      
      local({
        ulceration_vars <- grep("Ulceration", names(CleanData), value = TRUE)
        CleanData[, (ulceration_vars) := lapply(.SD, Fn$parse_ulceration), 
                  .SDcols = ulceration_vars]
      })
      
      ## ---- Combine Breslow variables into one variable ----
      CleanData[, BreslowThickness := Fn$combine_breslow(.SD), 
                .SDcols = grep('Tumour|Ulceration', names(CleanData))]
      
      ## ---- Combine Ulceration variables into one variable ----
      CleanData[, Ulceration := Fn$combine_ulceration(.SD), 
                .SDcols = grep('Tumour|Ulceration', names(CleanData))]
      
      ## ---- Stages from Breslow and Ulceration ----
      CleanData[, BreslowTstage := CodeMap$get_Tstage(BreslowThickness)]
      CleanData[, BreslowTSubStage := Fn$get_Tsubstage(
        BreslowThickness,
        yes = fifelse(Ulceration == "Present", TRUE, NA),
        no = fifelse(Ulceration == "Absent", TRUE, NA),
        missing = fifelse(Ulceration == "Missing", TRUE, NA)
      )]
      
      ## ---- Change Levels of Ordered Factors ----
      local({
        ordered_factor_cols <- c(
          "AJCC", "Tstage", "Nstage", "Mstage", "AgeGroup", 
          "BreslowTstage", "BreslowTSubStage"
        )
        unordered_factor_cols <- c(
          "HealthRegion", "Season", "AnatomicSite",
          "MelanomaType", "ClinicalStage", "Sex"
        )
        
        CleanData[, (ordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
                  .SDcols = ordered_factor_cols
        ]
        CleanData[, (unordered_factor_cols) := Map(CodeMap$factor_map, .SD, names(.SD)),
                  .SDcols = unordered_factor_cols
        ]
      })
      
      ## -- Drop the levels from Tstage and DiagSafety if any ------------------------------
      CleanData[, Tstage := droplevels(Tstage)]
      CleanData[, DiagSafety := droplevels(DiagSafety)]
      
      ## ---- Removing Variables in Stage1 ----
      local({
        var_discarded_stage1 <- c(
          "TumourThickness", "TumourThickness1", "TumourThickness2",
          "BreslowMissing1", "BreslowMissing2", "LocalRelapseBiopsy",
          "LocalRelapseBiopsyDate", "ExcisionLocalRelapse", "ExcisionLocalRelapseDate",
          "ExtExcisionLocalRelapse", "ExtExcisionLocalRelapseDate", "DiagYear1",
          "DiagYear2", "UlcerationNoReport1", "UlcerationNoReport2", "UlcerationYes1",
          "UlcerationYes2", "UlcerationNO1", "UlcerationNO2", "Ulceration1"
        )
        CleanData[, (var_discarded_stage1) := NULL]
      })
      
      ## ---- Save Clean CleanData ----
      saveRDS(CleanData, file = file.path(DATA_PATH, "CleanData.rds"))
    }
    
    ## -- Complete Dataset ----
    if (file.exists(file.path(DATA_PATH, "Complete.rds"))) {
      Complete <- readRDS(file.path(DATA_PATH, "Complete.rds"))
    } else {
      Complete <- copy(CleanData)
      
      ## -- Local Function ----
      MapApply <- function(map, var) {
        var[var == ""] <- NA
        fifelse(is.na(var), map(var), paste(var, map(var), sep = ": "))
      }
      
      ## -- Re-code variables ----
      Complete[, DiagSafety := as.factor(MapApply(CodeMap$diag_safety_map, DiagSafety))]
      Complete[, Localization := as.factor(MapApply(CodeMap$localization_map, Localization))] 
      Complete[, Topography := as.factor(MapApply(CodeMap$topo_icdo3_map, Topography))] 
      Complete[, TopographyICDO3 := as.factor(MapApply(CodeMap$topo_icdo3_map, TopographyICDO3))] 
      Complete[, Histology := as.factor(Histology)] 
      Complete[, MorphologyICDO3 := as.factor(MapApply(CodeMap$morphology_icdo3_map, MorphologyICDO3))] 
      Complete[, DiagBasis := as.factor(MapApply(CodeMap$diag_basis_map, DiagBasis))] 
      Complete[, Metastasis := as.factor(MapApply(CodeMap$MetastasisMap, Metastasis))] 
      Complete[, Surgery := as.factor(MapApply(CodeMap$surgery_map, Surgery))] 
      Complete[, Radiotherapy := as.factor(MapApply(CodeMap$radiotherapy_map, Radiotherapy))] 
      Complete[, Side := fifelse(Side == "H", "Right", "Left")] 
      Complete[, TopographyICD10 := as.factor(MapApply(CodeMap$topo_icd10_map, TopographyICD10))]
      Complete[, Status := as.factor(MapApply(CodeMap$status_map, Status))] 
      Complete[, UVRegion := as.factor(MapApply(CodeMap$uvr_map, as.numeric(as.character(County))))] 
      Complete[, CountyCode := formatC(as.numeric(as.character(County)), width = 2, flag = "0")] 
      Complete[, County := as.factor(MapApply(CodeMap$county_map, County))]
      Complete[Ulceration == "Missing", Ulceration := NA_character_]
      Complete[, DiagYear10 := CodeMap$year_10(DiagYear)]
      Complete[, DiagYear5 := CodeMap$year_5(DiagYear)]
      Complete[, YearCat := CodeMap$year_cat(DiagYear)]
      Complete[, AgeGroup5 := CodeMap$age_map2(AgeDiag, step = 5)]
      Complete[, AgeGroup10 := CodeMap$age_map2(AgeDiag, step = 10)]
      Complete[, BirthCohort5 := CodeMap$year_range(1850, 2020, 5)(BirthYear)]
      Complete[, BirthCohort10 := CodeMap$year_range(1850, 2020, 10)(BirthYear)]
      
      ## ---- Discart some variables at stage 2 ----
      local({
        var_discarded_stage2 <- c(
          "Topography", "TopographyICDO3", "Histology", 
          "MorphologyICDO3", "TopographyICD10"
        )
        Complete[, (var_discarded_stage2) := NULL]
      })
      
      ## -- Save the complete dataset ----
      saveRDS(Complete, file = file.path(CodeMap$DATA_PATH, "Complete.rds"))
      write.table(
        Complete, 
        file = file.path(CodeMap$DATA_PATH, "Complete.csv"), 
        sep = ";", 
        quote = TRUE, 
        dec = ",", 
        row.names = FALSE,
        na = ""
      )
      
      ## -- Clean up ----
      rm(MapApply)
    }
    
    ## -- Slim Data ----
    if (file.exists(file.path(DATA_PATH, "Slim.rds"))) {
      Slim <- readRDS(file.path(DATA_PATH, "Slim.rds"))
    } else {
      Slim <- copy(Complete)
      Slim <- Slim[, .(
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
        AgeGroup5,
        AgeGroup10,
        YearCat,
        DiagYear5,
        DiagYear10,
        BirthYear5 = BirthCohort5,
        BirthYear10 = BirthCohort10,
        SurvivalMonth
      )]
      
      ## -- Save slim data set ----
      saveRDS(Slim, file = file.path(CodeMap$DATA_PATH, "Slim.rds"))
      write.table(
        Slim, 
        file = file.path(CodeMap$DATA_PATH, "Slim.csv"), 
        sep = ";", 
        quote = TRUE, 
        dec = ",", 
        row.names = FALSE,
        na = ""
      )
      
    }
    
    ## -- External Data ----
    if (file.exists(file.path(DATA_PATH, "CountyPop.rds"))) {
      CountyPop <- readRDS(file.path(DATA_PATH, "CountyPop.rds"))
    } else {
      CountyPop <- foreign::read.dta(
        file.path(CodeMap$DATA_PATH, "Main", "CountyPop-CRN.dta")
      )
      setDT(CountyPop)
      
      setnames(CountyPop, 1:5, c("Year", "CountyCode", "Sex", "AgeGroup", "Population"))
      CountyPop <- CountyPop[Year >= 1983]
      CountyPop[, Sex := fifelse(Sex == "Females", "Women", "Men", na = NA_character_)]
      CountyPop[, AgeGroup := CodeMap$age_map2(idx = AgeGroup)]
      
      saveRDS(CountyPop, file.path(DATA_PATH, "CountyPop.rds"))
    }
    
    if (file.exists(file.path(DATA_PATH, "Population.rds"))) {
      AvgPop <- readRDS(file.path(DATA_PATH, "Population.rds"))
    } else {
      ## ---- population: Year, Sex and Age-Group ------------------------------------
      AvgPop <- copy(CountyPop)
      AvgPop <- AvgPop[, .(Population = sum(Population)), by = .(Year, Sex, AgeGroup5 = AgeGroup)]
      fwrite(AvgPop, file.path(DATA_PATH, "Population.csv"))
      saveRDS(AvgPop, file.path(DATA_PATH, "Population.rds"))
    }
    
    ## -- Clean up ----
    rm(DATA_PATH)
    
  }, envir = Data)
  
  ## -- Save all dataset as a single file ----
  saveRDS(Data, file = here::here(CodeMap$DATA_PATH, "Composite", "Data.rds"))
  
}
