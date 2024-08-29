## -- Function for multiple imputation ----------------
#| Highly depends on the variable name in the data
#| Also, may defaults are hard coded

Impute <- function(data) {
  TempData <- data
  TempData[, c("EndYear") := .(year(EndDate))]
  TempData[, YearCat := droplevels(YearCat)]
  
  Mice <- function(data, var = NULL, period = year, seed = 2022) {
    data <- copy(data)
    dta <- data[, c(
      "Sex", "YearCat", "Status", "Thickness", "HealthRegion",
      "SurvivalMonth", "Age", "PID", "EndYear", var
    ), with = FALSE]
    
    dta[, LogThickness := log1p(Thickness)]
    dta[, Thickness := NULL]
    dta[, SurvCat := cut.default(
      SurvivalMonth,
      breaks = c(0, 1, 5, 10, Inf) * 12,
      include.lowest = TRUE
    )]
    dta[grepl("Alive|Lost", Status), StatusNew := 0]
    dta[grepl("Dead", Status), StatusNew := 1]
    dta[, CumSurv := mice::nelsonaalen(.SD, SurvivalMonth, StatusNew)]
    # Either include CumSurv but not Survival and Status
    # Or include Survial and Status not CumSurv
    # Check and compare
    
    dta[, StatusNew := NULL]
    dta[, SurvivalMonth := NULL]
    
    RunMice <- function(data, seed = 2022) {
      Methods <- vector(length = ncol(data))
      names(Methods) <- names(data)
      TargetVars <- c("LogThickness", "MelanomaType",
                      "AnatomicSite", "ClinicalStage")
      MissingProp <- sort(
        apply(apply(data, 2, is.na), 2, sum) / nrow(data), decreasing = FALSE
      )
      VarOrder <- c(
        "LogThickness",
        names(MissingProp[names(MissingProp) %in% TargetVars[-1]])
      )
      
      # if (!is.null(var)) {
      #   dta[, c(var) := lapply(
      #   .SD, forcats::fct_na_value_to_level, "Unspecified"
      # ), .SDcols = var]
      # }
      
      Methods[] <- ""
      Methods[TargetVars] <- "pmm"
      
      PredMat <- make.predictorMatrix(data)
      PredMat["PID", ] <- 0
      PredMat[, "PID"] <- 0
      PredMat[!(rownames(PredMat) %in% TargetVars), ] <- 0
      PredMat <- PredMat[names(Methods), names(Methods)]
      Imp <- mice(
        data = data,
        predictorMatrix = PredMat,
        method = Methods,
        visitSequence = VarOrder,
        m = 30,
        maxit = 20,
        seed = seed
      )
      return(Imp)
    }
    
    ImpList <- dta %>%
      split(by = "YearCat") %>%
      map(~ .x[, YearCat := NULL]) %>%
      map(RunMice, seed = seed)
    
    CompileData <- function(Imp, period) {
      ImpDta <- mice::complete(Imp, include = FALSE, "all") %>% 
        map(as.data.table)
      
      ImpData <- map(ImpDta, function(imp_data) {
        data <- copy(data)[YearCat == period]
        data[, Thickness := imp_data[, exp(LogThickness) - 1]]
        data[, SurvCat := imp_data[, SurvCat]]
        data[, AnatomicSite := imp_data[, AnatomicSite]]
        data[, MelanomaType := imp_data[, MelanomaType]]
        
        data[, Tstage := CodeMap$get_Tstage(Thickness)]
        data[, Tstage := CodeMap$factor_map(Tstage, "BreslowTstage")]
        return(data)
      })
      return(ImpData)
    }
  
    ImpData <- map_dfr(
      imap(ImpList, CompileData),
      rbindlist,
      idcol = "Imp",
      .id = "YearCat"
    ) %>%
      split(by = "Imp")
    
    return(list(Data = ImpData, Obj = ImpList))
  }
  
  Imputed <- Mice(
    TempData,
    c("ClinicalStage", "AnatomicSite", "MelanomaType")
  )
  
  Complete <- copy(TempData)
  Complete[, Imp := "0"]
  Complete[, SurvCat := cut.default(
    SurvivalMonth,
    breaks = c(0, 1, 5, 10, Inf) * 12,
    include.lowest = TRUE
  )]
  setcolorder(Complete, names(Imputed$Data[[1]]))
  Imputed$Data <- append(list(`0` = Complete), Imputed$Data)
  
  return(Imputed)
}
