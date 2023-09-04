## -- Source scripts and functions --------
source(here::here(BASE_PATH, "Scripts/00-CodeMap.R"))
source(here::here(BASE_PATH, "Scripts/00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c("data.table", "mice", "purrr", "stringr"))

if (!exists("Results")) {
  if (file.exists(here::here(CodeMap$DATA_PATH, "Composite", "Results.rds"))) {
    Results <- readRDS(here::here(CodeMap$DATA_PATH, "Composite", "Results.rds"))
  } else {
    
    ## -- Result Environments --------
    Results <- new.env()
    
    ## -- Computing functions ------------------------------
    evalq({
      GetSlimData <- function(CompleteData) {
        CompleteData %>% 
          as.data.table() %>% 
          .[, .(
            BirthYear,
            Sex,
            DiagDate,
            DiagYear,
            StatusDate,
            StatusYear,
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
            BirthYear10 = BirthCohort10
          )]
      }
      GetIncData <- function(cases, person_year) {
        setnames(person_year, "Year", "DiagYear", skip_absent = TRUE)
        if (!is.data.frame(cases)) {
          by <- purrr::reduce(
            purrr::map2(
              cases, list(person_year),
              ~intersect(names(.x), names(.y))),
            intersect
          )
          map2_df(
            cases, list(person_year),
            ~merge.data.table(.x, .y, by = by, all.x = TRUE),
            .id = "Imp"
          )
        } else {
          by <- intersect(names(cases), names(person_year))
          out <- merge.data.table(cases, person_year, by = by, all.x = TRUE)
          out[, Imp := 0]
          setcolorder(out, "Imp")
        }
      }
      GetIncDataByGroup <- function(cases, person_year, group) {
        if (!is.data.frame(cases)) {
          map2_df(
            cases, list(person_year),
            function(.x, .y) {
              case_group <- intersect(names(.x), group)
              py_group <- intersect(names(.y), group)
              CaseData <- Fn$by_group(.x, case_group)
              PyData <- Fn$by_group(.y, py_group)
              GetIncData(CaseData, PyData)
            },
            .id = "Imp"
          )
        } else {
          case_group <- intersect(names(cases), group)
          py_group <- intersect(names(person_year), group)
          CaseData <- Fn$by_group(cases, case_group)
          PyData <- Fn$by_group(person_year, py_group)
          GetIncData(CaseData, PyData) %>% 
            .[, Imp := 0] %>% 
            setcolorder("Imp")
        }
      }
      GetIncRate <- function(data) {
        Fn$quietly_load(c("epiR"))
        
        out <- data[, epiR::epi.conf(
          cbind(N, Population),
          method = "exact",
          ctype = "inc.rate"
        ) * 1e5, by = setdiff(names(data), c("N", "Population"))]
        
        setkeyv(out, intersect(
          names(out), setdiff(names(out), c("est", "lower", "upper"))
        ))
        setkeyv(data, intersect(
          names(data), setdiff(names(out), c("est", "lower", "upper"))
        ))
        data[out][, c("Population") := NULL][]
      }
      AgeAdjRate <- function(
    cases, person_year, group, std_pop = "europe", 
    margin = NULL, scale = 1e5 
      ) {
        Fn$quietly_load(c("epitools"))
        group_vars <- setdiff(group, c("AgeGroup5", "Age"))
        if (!"AgeGroup5" %in% group) group <- c("AgeGroup5", group)
        data <- GetIncDataByGroup(cases, person_year, group)
        data <- merge.data.table(data, CodeMap$std_pop(std_pop), by = "AgeGroup5")
        if (!is.null(margin)) {
          data <- Fn$add_margin(data, "Sex", c("N", "Population"))
        }
        adj_rate <- 
          function(count, pop, stdpop, rate = NULL, conf.level = 0.95, scale = 1e5) {
            if (missing(count) & !missing(pop) & is.null(rate)) 
              count <- rate * pop
            if (missing(pop) & !missing(count) & is.null(rate)) 
              pop <- count/rate
            if (is.null(rate) & !missing(count) & !missing(pop)) 
              rate <- count/pop
            alpha <- 1 - conf.level
            cruderate <- sum(count)/sum(pop)
            stdwt <- stdpop/sum(stdpop)
            dsr <- sum(stdwt * rate)
            dsr.var <- sum((stdwt^2) * (count/pop^2))
            wm <- max(stdwt/pop)
            out <- c(crude.rate = cruderate * scale, 
                     adj.rate = dsr * scale, 
                     adj.rate.var = dsr.var * scale^2, 
                     max.wt = wm * scale)
            return(out)
          }
        adj_ci <- function(obj, alpha = 0.05) {
          dsr <- obj[["adj.rate"]]
          dsr.var = obj[["adj.rate.var"]]
          wm <- obj[["max.wt"]]
          gamma.lci <- qgamma(alpha/2, shape = (dsr^2)/dsr.var, scale = dsr.var/dsr)
          gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr + wm)^2) /
                                (dsr.var +  wm^2), 
                              scale = (dsr.var + wm^2)/(dsr + wm))
          c(lci = gamma.lci, uci = gamma.uci)
        }
        out <- data[, as.data.table(
          t(adj_rate(N, Population, StdPop, scale = scale))
        ), by = c("Imp", group_vars)] %>% 
          .[data[, .(N = sum(N)), by = c("Imp", group_vars)], 
            on = c("Imp", group_vars)] %>% 
          setcolorder(c("Imp", group_vars, "N"))
        if (out[, uniqueN(Imp) > 1]) {
          imputed_data <- out[Imp != 0, .(Data = list(.SD)), by = group_vars]
          imputed_data[, Pooled := map(Data, ~list(mice::pool.scalar(
            .x$adj.rate,
            .x$adj.rate.var,
            round(mean(.x$N))
          ))), by = group_vars]
          imputed_data <- imputed_data[, map2_df(Data, Pooled, function(dta, pr) {
            data.table(
              # N = dta[, round(mean(N))], 
              N = dta[, sum(N)],
              imp = pr$m,
              crude.rate = dta[, mean(crude.rate)],
              adj.rate = pr$qbar, 
              adj.rate.var = pr$ubar, 
              max.wt = dta[, max(max.wt)]
            )
          }), by = group_vars] %>% 
            .[, Imp := "Pooled"] %>% 
            setcolorder(c("Imp", group_vars))
          out <- rbind(
            out[Imp == 0][, Imp := "Complete"], 
            imputed_data, 
            fill = TRUE
          )
          out[, imp := nafill(imp, type = "const", fill = 1)]
        }
        out <- out[out[, as.data.table(t(adj_ci(.SD))), by = c("Imp", group_vars)], 
                   on = c("Imp", group_vars)]
        out[, c("adj.rate.var", "max.wt") := NULL]
        return(out[])
      }
      Segmented <- function(rate_data, which = "fit", logY = TRUE) {
        Fn$quietly_load(c("segmented", "purrr"))
        
        warn <- options(warn = -1)
        on.exit(options(warn))
        rate_vars <- c("est", "adj.rate", "AdjRate")
        rate_var <- rate_vars[rate_vars %in% names(rate_data)]
        args <- match.arg(
          arg = which,
          choices = c("fit", "psi", "summary", "segments", 
                      "aapc", "spline", "fitted"),
          several.ok = TRUE
        )
        group_vars <- setdiff(
          x = names(rate_data),
          y = c("N", "Year", "DiagYear", "crude.rate", "adj.rate", "imp",
                "lci", "uci", "est", "lower", "upper", "max.wt",
                "fit_lwr", "fit_upr", "fit", "adj.rate.var")
        )
        fit_df <- rate_data[, .(data = list(.SD)), by = group_vars]
        fit_df[, fit := list(map(
          data, Fn$fit_segmented, logY = logY
        )), by = group_vars]
        fit_df[, N := map_dbl(data, ~sum(.x[["N"]]))]
        fit_df[, imp := map_dbl(data, ~unique(.x[["imp"]]))]
        out <- lapply(args, function(arg) {
          if (arg %in% c("psi", "summary")) {
            out <- fit_df[, map_df(fit, Fn$segreg_get(arg)), by = group_vars]
            ret <- out
            # ret <- out[fit_df[, .(N, imp), by = group_vars], on = group_vars]
            return(ret)
          }
          if (arg == "aapc") {
            out <- fit_df[, map_df(
              fit, Fn$segreg_get(arg), logY = logY
            ), by = group_vars]
            ret <- out[fit_df[, .(N, imp), by = group_vars], on = group_vars]
            return(ret)
          }
          if (arg %in% c("spline", "fitted", "segments")) {
            out <- fit_df[, map2_df(
              data, fit, Fn$segreg_get(arg), logY = logY
            ), by = group_vars]
            ret <- out
            # ret <- out[fit_df[, .(N, imp), by = group_vars], on = group_vars]
            return(ret)
          }
          return(fit_df)
        })
        names(out) <- args
        if (length(out) == 1) out <- out[[1]]
        setcolorder(out, intersect(names(out), c(group_vars, "N", "imp")))
        return(out)
      }
      APCData <- function(inc_data, period_step = 5, age_step = 5) {
        Fn$quietly_load(c("purrr", "stringr"))
        data <- copy(inc_data)
        
        year_var <- grep("Year", names(data), value = TRUE)
        age_var <- grep("Age", names(data), value = TRUE)
        
        if (data[, is.numeric(get(year_var))]) {
          # Assumes that the year_var is numeric
          data[, DYear := floor(get(year_var) / period_step) * period_step]
          data[, DYearCat := CodeMap$year_range(
            min(DYear), max(DYear), period_step
          )(get(year_var))]
        }
        if (data[, is.factor(get(year_var)) | is.character(get(year_var))]) {
          # Assumes that the year_var is in 5-year or 10-year category
          data[, DYear := str_extract(get(year_var), "^\\d+") %>% as.numeric()]
          data[, DYearCat := get(year_var)]
        }
        if (data[, is.numeric(get(age_var))]) {
          # Assumes that the age_var is numeric
          data[, DAge := floor(get(age_var) / age_step) * age_step]
          data[, DAgeCat := CodeMap$age_map2(get(age_var), step = age_step)]
        }
        
        if (data[, is.factor(get(age_var)) | is.character(get(age_var))]) {
          age_gap <- data[str_detect(get(age_var), "-")] %>%
            .[1, str_split(get(age_var), "-") %>% unlist() %>% 
                as.numeric() %>% diff()]
          
          if (age_gap <= 5 & age_step == 10) {
            data[, DAgeCat := CodeMap$age5210(get(age_var))]
          }
          data[, DAge := str_extract(get(age_var), "^\\d+") %>% as.numeric()]
          data[, DAgeCat := get(age_var)]
        }
        group_vars <- setdiff(
          names(data), 
          c(year_var, age_var, "DYear", "DAge", "DYearCat", 
            "DAgeCat", "Sex", "N", "Population")
        )
        apc_vars <- intersect(
          c("DYear", "DYearCat", "DAge", "DAgeCat", "Sex"), names(data)
        )
        
        
        data[, .(dta = list(.SD)), by = group_vars] %>%
          .[, map_df(dta, function(d) {
            out <- d[, .(N = sum(N), Population = sum(Population)), by = apc_vars]
            out[, Cohort := Fn$get_cohort(DYearCat, DAgeCat)[["lower"]]]
            out[, CohortCat := Fn$get_cohort(DYearCat, DAgeCat)[["range"]]]
          }), by = group_vars]
      }
      APC <- function(inc_data, period_step = 5, age_step = 5) {
        Fn$quietly_load(c("purrr", "stringr"))
        if (!(period_step %in% c(5, 10))) {
          message("Period step must be either 5 or 10")
          return(NULL)
        }
        if (!(age_step %in% c(5, 10))) {
          message("Age step must be either 5 or 10")
          return(NULL)
        }
        
        data <- copy(inc_data)
        if (data[, is.numeric(DiagYear)]) {
          data[, DiagYear := floor(DiagYear / period_step) * period_step]
        }
        if (data[, is.factor(DiagYear) | is.character(DiagYear)]) {
          data[, DiagYear := str_extract(DiagYear, "^\\d+") %>% as.numeric()]
        }
        if (data[, is.numeric(AgeGroup5)]) {
          data[, AgeGroup5 := floor(AgeGroup5 / age_step) * age_step]
        }
        if (data[, is.factor(AgeGroup5) | is.character(AgeGroup5)]) {
          age_gap <- data[str_detect(AgeGroup5, "-")] %>%
            .[1, str_split(AgeGroup5, "-") %>% unlist() %>% as.numeric() %>% diff()]
          if (age_gap <= 5 & age_step == 10) {
            data[, AgeGroup5 := CodeMap$age5210(AgeGroup5)]
          }
          data[, AgeGroup5 := str_extract(AgeGroup5, "^\\d+") %>% as.numeric()]
        }
        group_vars <- setdiff(
          names(data), 
          c("DiagYear", "Year", "AgeGroup5", "Sex", "N", "Population")
        )
        apc_vars <- intersect(
          c("DiagYear", "Year", "AgeGroup5", "Sex"), 
          names(data)
        )
        
        ret <- data[, .(dta = list(.SD)), by = group_vars] %>%
          .[, map_df(dta, function(d) {
            out <- d[, .(N = sum(N), Population = sum(Population)), by = apc_vars]
            out[, Cohort := DiagYear - AgeGroup5]
            return(out)
          }), by = group_vars]
        
        setnames(
          ret, 
          c("DiagYear", "AgeGroup5"), 
          c("Period", "Age"),
          skip_absent = TRUE
        )
        cohort_step <- ret[, Cohort] %>% 
          unique() %>% sort() %>% 
          diff() %>% pluck(2) %>% 
          abs()
        cohort_cat <- ret[, CodeMap$year_range(
          min(Cohort), max(Cohort) + cohort_step, cohort_step
        )]
        if (period_step == 5) {
          ret[, PeriodCat := CodeMap$year_5(Period)]
        }
        if (period_step == 10) {
          ret[, PeriodCat := CodeMap$year_10(Period)]
        }
        ret[, AgeCat := CodeMap$age_map2(Age, step = age_step)]
        ret[, CohortCat := cohort_cat(Cohort)]
        return(ret[])
      }
      ModelAPC <- function(apc_data, Param = "ACP") {
        Fn$quietly_load(c("purrr", "Epi"))
        
        group_var <- intersect(
          names(apc_data), 
          c("Sex", "Tstage", "AnatomicSite", "MelanomaType", "Imp")
        )
        apc_models <- apc_data[, .(fit = list(Epi::apc.fit(
          A = Age, 
          P = Period, 
          D = N, 
          Y = Population,
          parm = Param,
          ref.c = 1945, 
          ref.p = 2008,
          scale = 1e5,
          print.AOV = FALSE
        ))), by = group_var]
        
        effect <- rbindlist(list(
          Age = apc_models[, map_df(
            fit, ~as.data.table(.x[["Age"]])
          ), by = group_var] %>% 
            setnames(c("Age", "Rate"), c("Level", "Measure")) %>% 
            cbind(MeasureType = "Rate", .),
          Period = apc_models[, map_df(
            fit, ~as.data.table(.x[["Per"]])
          ), by = group_var] %>% 
            setnames(c("Per", "P-RR"), c("Level", "Measure")) %>% 
            cbind(MeasureType = "Rate-Ratio", .),
          Cohort = apc_models[, map_df(
            fit, ~as.data.table(.x[["Coh"]])
          ), by = group_var] %>% 
            setnames(c("Coh", "C-RR"), c("Level", "Measure")) %>% 
            cbind(MeasureType = "Rate-Ratio", .)
        ), idcol = "EffectType")
        effect <- effect[, .(eff = list(.SD)), by = group_var]
        if (length(group_var) > 0) {
          out <- merge.data.table(apc_models, effect, by = group_var)
        } else {
          out <- cbind(apc_models, effect)
        }
        return(out)
      }
      Impute <- function(data = Data()) {
        TempData <- data
        TempData[, c("ID", "EndYear") := .(.I, year(EndDate))]
        TempData[, YearCat := droplevels(YearCat)]
        
        Mice <- function(data, var = NULL, period = year, seed = 2022) {
          data <- copy(data)
          dta <- data[, c(
            "Sex", "YearCat", "Status", "Thickness", "HealthRegion",
            "SurvivalMonth", "Age", "ID", "EndYear", var
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
            PredMat["ID", ] <- 0
            PredMat[, "ID"] <- 0
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
            ImpDta <- complete(Imp, include = FALSE, "all") %>% map(as.data.table)
            
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
    }, Results)
    
    ## -- Dataset in the environment --------
    evalq(
      {
        CompleteData <- function() {
          readRDS(here::here(file.path(CodeMap$DATA_PATH, "Complete.rds")))
        }
        Data <- function() {
          readRDS(here::here(file.path(CodeMap$DATA_PATH, "Slim.rds")))
        }
        Population <- function() {
          readRDS(here::here(file.path(CodeMap$DATA_PATH, "Population.rds")))
        }
        if (!exists("Imputed")) {
          require(mice)
          
          if (file.exists(here::here(
            paste0(file.path(CodeMap$DATA_PATH, "Imputed.rds"))
          ))) {
            Imputed <- readRDS(
              here::here(paste0(file.path(CodeMap$DATA_PATH, "Imputed.rds")))
            )
          } else {
            Imputed <- Impute()
            saveRDS(
              Imputed, 
              file = here::here(paste0(file.path(CodeMap$DATA_PATH, "Imputed.rds")))
            )
          }
        }
        ImpData <- Imputed[["Data"]]
        
        if (!exists("surv_data")) {
          if (file.exists(here::here(CodeMap$DATA_PATH, "melo.Rds"))) {
            surv_data <- readRDS(here::here(CodeMap$DATA_PATH, "melo.Rds"))
          } else {
            surv_data <- Fn$prepare_surv_data(
              data = ImpData,
              remove_date = FALSE,
              hide_cause = FALSE,
              cause_specific = FALSE,
              group_vars = c("Tstage")
            )
            surv_data <- surv_data[, AgeGroup := CodeMap$age_map3(age)] %>%
              .[CodeMap$icss_wt(which = 2), on = "AgeGroup"] %>%
              setnames(Fn$camel2snake(names(.)))
            
            saveRDS(surv_data, here::here(CodeMap$DATA_PATH, "melo.Rds"))
          }
        }
      },
      Results
    )
    
    ## -- Cases, overall and by groups ------------------------------
    evalq({
      GetDeaths <- function(data) {
        Deaths <- xtabs(
          ~StatusYear + Sex + AgeGroup5 + Tstage + AnatomicSite + MelanomaType,
          data = data[DeathCause == "C43" & grepl("Dead", Status)],
          addNA = TRUE
        ) %>% as.data.table()
        setnames(Deaths, c("StatusYear", "AgeGroup5"), c("Year", "Age"))
        
        Deaths[, Year := as.integer(Year)]
        Deaths[, Age := CodeMap$factor_map(Age, "AgeGroup5")]
        Deaths[, Sex := CodeMap$factor_map(Sex, "Sex")]
        Deaths[, Tstage := CodeMap$factor_map(Tstage, "BreslowTstage")]
        Deaths[, AnatomicSite := CodeMap$factor_map(AnatomicSite, "AnatomicSite")]
        Deaths[, MelanomaType := CodeMap$factor_map(MelanomaType, "MelanomaType")]
        Deaths[, AgeGroup := CodeMap$age_map(from = "5years", Age)]
        Deaths[, AgeGroup := CodeMap$factor_map(AgeGroup, "AgeGroup")]
        return(Deaths[])
      }
      GetCases <- function(
    data, 
    case_group = c(
      "DiagYear", "Sex", "AgeGroup5", "Tstage", 
      "AnatomicSite", "MelanomaType"
    ) 
      ) {
        Cases <- xtabs(
          reformulate(case_group),
          data = data,
          addNA = TRUE
        ) %>% as.data.table()
        if ("DiagYear" %in% names(Cases)) {
          Cases[, DiagYear := as.integer(DiagYear)]
        }
        if ("Age" %in% names(Cases)) {
          Cases[, Age := as.integer(Age)]
          Cases[, AgeGroup5 := CodeMap$age_map2(Age)]
          Cases[, AgeGroup := CodeMap$age_map(from = "numeric", Age)]
        }
        if ("AgeGroup5" %in% names(Cases)) {
          Cases[, AgeGroup := CodeMap$age_map(from = "5years", AgeGroup5)]
        } 
        
        if ("Sex" %in% names(Cases)) 
          Cases[, Sex := CodeMap$factor_map(Sex, "Sex")]
        if ("Tstage" %in% names(Cases)) 
          Cases[, Tstage := CodeMap$factor_map(Tstage, "BreslowTstage")]
        if ("AnatomicSite" %in% names(Cases)) 
          Cases[, AnatomicSite := CodeMap$factor_map(AnatomicSite, "AnatomicSite")]
        if ("MelanomaType" %in% names(Cases)) 
          Cases[, MelanomaType := CodeMap$factor_map(MelanomaType, "MelanomaType")]
        if ("AgeGroup5" %in% names(Cases)) 
          Cases[, AgeGroup5 := CodeMap$factor_map(AgeGroup5, "AgeGroup5")]
        if ("AgeGroup" %in% names(Cases)) 
          Cases[, AgeGroup := CodeMap$factor_map(AgeGroup, "AgeGroup")]
        if (all(c("DiagYear", "Age") %in% names(Cases))) {
          if (Cases[, is.numeric(DiagYear) & is.numeric(Age)]) {
            Cases[, BirthYear := DiagYear - Age]
          }
        }
        
        return(Cases[])
      }
    }, Results)
    evalq({
      Cases <- GetCases(Data())
      Deaths <- GetDeaths(Data())
    }, Results)
    
    ## -- Imputed cases, overall and by groups ----
    evalq({
      ImpCases <- function(...) {
        out <- purrr::map(ImpData, function(dta) {
          with(dta, {
            Cases <- GetCases(dta, ...)
            return(Cases)
          })
        })
      }
      ImpDeaths <- function() {
        out <- purrr::map(ImpData, function(dta) {
          with(dta, {
            Deaths <- GetDeaths(dta)
            return(Deaths)
          })
        })
      }
    }, Results)
    
    ## -- Person-Year (using Population data) ------------------------------
    evalq({
      PersonYear <- Population()[Year >= 1983] %>%
        .[CJ(Year, Sex, AgeGroup5, unique = TRUE),
          on = .(Year, Sex, AgeGroup5),
          .(Population = sum(Population)),
          by = .EACHI
        ]
      PersonYear[, Sex := CodeMap$factor_map(Sex, "Sex")]
      PersonYear[, AgeGroup := CodeMap$age_map(AgeGroup5, "5years")]
      PersonYear[, AgeGroup := CodeMap$factor_map(AgeGroup, "AgeGroup")]
      setnames(PersonYear, "Year", "DiagYear")
    }, Results)
    
    
    ## -- Save the Results environment ----
    saveRDS(Results, here::here(CodeMap$DATA_PATH, "Composite", "Results.rds"))
    
  }
}

## -- Make some objects available in Global envirnoment ----
if (exists("Results")) {
  EssentialResults <- list(
    Imputed = Results$Imputed,
    ImpData = Results$ImpData,
    Cases = Results$Cases,
    PersonYear = Results$PersonYear,
    Population = Results$Population(),
    SurvData = Results$surv_data
  )
  if (!file.exists(here::here(CodeMap$DATA_PATH, "Composite", "EssentialResults.rdata"))) {
    save(EssentialResults, file = here::here(CodeMap$DATA_PATH, "Composite", "EssentialResults.rdata"))
  }
} else {
  if (file.exists(here::here(CodeMap$DATA_PATH, "Composite", "EssentialResults.rdata"))) {
   load(here::here(CodeMap$DATA_PATH, "Composite", "EssentialResults.rdata"))
  }
}

