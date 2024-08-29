## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
INC <- Sys.getenv("DataInc")
INC_ROOT <- file.path(ROOT, "01-incidence")

TDATA <- file.path(INC_ROOT, "02-Results", "TableData")
if (!dir.exists(TDATA)) dir.create(TDATA)

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "tidytable", "data.table", "stringr",
  "purrr", "gt", "gtsummary"
))

## -- Get the results ----
Results <- readRDS(here::here(INC, "Results.rds"))
# Data <- readRDS(here::here(INC, "Slim.rds"))

## -- Table Data ----
TableData <- new.env(parent = Results)

## -- Case Exclusion data ----
evalq({
  CaseExclusion <- function() {
    fpath <- file.path(TDATA, glue::glue("ExInclusion.rds"))
    Fn$load_or_run(
      rds_path = fpath,
      expression = {
        if (!exists("StepData")) {
          StepData <- readRDS(here::here(INC, "StepData.rds"))
        }
        ncases <- map_dbl(StepData, nrow)
        steps_cases <- data.table(
          Steps = c(
            'Total cases',
            'Cases diagnosized before 1983',
            'Including only first invasive cases',
            'In-situ melanoma cases',
            'Cases that are not histologically verified',
            'Melanoma not verified',
            'Cases with status date before diagnosis date',
            'Cases with basis of death certificate only',
            'Cases with basis of diagnosis as autopsy',
            'Final data used in analysis'
          ),
          N = c(ncases[!grepl("[:alpha:]$", names(ncases))], last(ncases)))
        # N = c(50527, 48901, 45929, 45925, 45921, 45905, 45875, 45839, 45812, 45812))
        steps_cases[, Cases := shift(N) - N]
        steps_cases[, Steps := paste(.I, Steps, sep = ". ")]
        setcolorder(steps_cases, c("Steps", "Cases"))
        saveRDS(steps_cases, str_replace(fpath, "[rR]ds$", "csv"))
        saveRDS(steps_cases, fpath)
        return(steps_cases[])
      }
    )
  }
}, TableData)

## -- Table-1: Charactersitic Table ----
evalq({
  SummaryTable <- function(data = Data(), vars = NULL, group = NULL, ...) {
    fpath <- file.path(TDATA, glue::glue("SummaryTable.rds"))
    Fn$load_or_run(
      rds_path = fpath,
      expression = {
        attach(Results)
        on.exit(detach(Results))
        
        theme_gtsummary_compact()
        theme_gtsummary_journal()
        
        if (is.null(group)) group <- c("Sex", "YearCat")
        
        var_lbls <- c(
          Age = "Age at diagnosis, median (IQR)",
          AgeCat = "Age group, n (%)",
          SurvivalMonth = "Months of survival, median (IQR)",
          HealthRegion = "Residential region, n (%)",
          Season = "Season of dagnosis n (%)",
          AnatomicSite = "Anatomic site, n (%)",
          MelanomaType = "Histopathological subtype, n (%)",
          Ulceration = "Ulceration, n (%)",
          Thickness = "Tumour thickness, median (IQR)",
          Tstage = "T category, n(%)"
        )
        
        if (is.null(vars)) {
          vars <- var_lbls
        } else if (!is.list(vars)) {
          vars <- `names<-`(var_lbls[vars], vars)
        }
        
        table_data <- data %>% 
          tidytable::select(c("Sex", "YearCat", names(var_lbls))) %>%
          tidytable::mutate(
            YearCat = forcats::fct_relabel(YearCat, stringr::str_replace, "-", "\U2013"),
            AgeCat = forcats::fct_relabel(AgeCat, stringr::str_replace, "-", "\U2013")
          )
        
        SummaryTable <- Fn$gt_grouped_summary(
          data = table_data,
          variables = vars,
          group = group,
          ...
        ) %>% modify_table_body(
          filter,
          !(row_type == "missing" & grepl("Tumour thickness", var_label))
        ) 
        
        SummaryTable <- SummaryTable %>%
          modify_table_body(
            mutate,
            stat_label_1 = NA,
            label = CodeMap$get_Tstage_label(label),
          )
        
        if ("Ulceration" %in% names(vars) & length(group) == 2) {
          SummaryTable <- SummaryTable %>%
            modify_table_body(
              mutate,
              stat_1_1 = ifelse(variable == "Ulceration", NA, stat_1_1),
              stat_1_2 = ifelse(variable == "Ulceration", NA, stat_1_2)
            )
        } 
        
        SummaryTableDF <- as_tibble(SummaryTable)
        SummaryTable <- SummaryTable %>%
          as_gt() %>%
          gt::tab_source_note("IQR: interquartile range") %>% 
          gt::tab_source_note("% does not include the unspecified cases") %>% 
          gt::tab_options(table.border.bottom.style = "none")
        
        
        if ("Ulceration" %in% names(vars)) {
          SummaryTable <- SummaryTable %>%
            gt::tab_footnote(
              footnote = "Ulceration reported from the year 2000",
              locations = gt::cells_body(
                columns = label, 
                rows = grepl("Ulceration", label)
              )
            )
        }
        
        fwrite(SummaryTableDF, str_replace(fpath, "[rR]ds$", "csv"))
        saveRDS(SummaryTable, fpath)
        return(invisible(SummaryTable))
      }
    )
  }
}, TableData)

## -- Alternative Table-1 ------------------------------
evalq({
  AltTable <- function(data = Data(), vars = NULL) {
    fpath <- file.path(TDATA, glue::glue("AltTable.rds"))
    Fn$load_or_run(
      rds_path = fpath,
      expression = {
        attach(Results)
        on.exit(detach(Results))
        
        theme_gtsummary_compact()
        theme_gtsummary_journal()
        
        if (is.null(vars)) {
          vars <- c(
            Age = "Age at diagnosis, median (IQR)",
            AgeCat = "Age group, n (%)",
            SurvivalMonth = "Months of survival, median (IQR)",
            HealthRegion = "Residential region, n (%)",
            Season = "Season of dagnosis n (%)",
            AnatomicSite = "Anatomic site, n (%)",
            MelanomaType = "Histopathological subtype, n (%)",
            Ulceration = "Ulceration, n (%)",
            ClinicalStage = "Clinical stage"
          )
        }
        
        data <- data %>% 
          tidytable::mutate(
            MissingTumour = tidytable::if_else(is.na(Thickness), "Missing", "Not missing")
          ) %>% 
          tidytable::select(c("MissingTumour", "YearCat", names(vars))) %>% 
          tidytable::mutate(
            YearCat = forcats::fct_relabel(YearCat, stringr::str_replace, "-", "\U2013"),
            AgeCat = forcats::fct_relabel(AgeCat, stringr::str_replace, "-", "\U2013")
          )
        
        SummaryTable <- Fn$gt_grouped_summary(
          data = data,
          variables = vars,
          group = c("MissingTumour", "YearCat"),
          overall = TRUE
        ) %>% modify_table_body(
          filter,
          !(row_type == "missing" & var_label == "Tumour thickness, median (IQR)")
        ) %>% modify_table_body(
          mutate,
          stat_label_1 = NA,
          label = CodeMap$get_Tstage_label(label),
          stat_1_1 = ifelse(variable == "Ulceration", NA, stat_1_1),
          stat_1_2 = ifelse(variable == "Ulceration", NA, stat_1_2)
        ) %>% 
          modify_table_body(
            mutate, 
            across(
              starts_with("stat"), 
              ~str_replace(.x, ".*NA.*", NA_character_)
            )
          )
        
        SummaryTableDF <- gtsummary::as_tibble(SummaryTable)
        SummaryTable <- SummaryTable %>%
          as_gt() %>%
          gt::tab_source_note("IQR: interquartile range") %>%
          gt::tab_source_note("% does not include the unspecified cases") %>% 
          gt::tab_footnote(
            footnote = "Ulceration reported from the year 2000",
            locations = gt::cells_body(columns = label, rows = 27)
          ) %>% 
          gt::tab_options(table.border.bottom.style = "none")
        
        fwrite(SummaryTableDF, str_replace(fpath, "[rR]ds$", "csv"))
        saveRDS(SummaryTable, fpath)
        return(invisible(SummaryTable))
      }
    )
  }
}, TableData)

## -- Sensetivity Analysis: Count Comparison -------------------------
evalq({
  CountComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    if (!is.null(group)) {
      fpath <- here::here(TDATA, glue::glue("{group}-{by}-CountComparison.rds"))
    } else {
      fpath <- here::here(TDATA, glue::glue("{by}-CountComparison.rds"))
    }
    count_comparison <- Fn$load_or_run(
      rds_path = fpath,
      expression = {
        dataset <- Results$ImpData
        get_count_comparison <- function(dta) {
          out <- list(
            `Model A` = copy(dta[[1]]) %>% 
              modify_at(by, forcats::fct_na_value_to_level, "Unspecified") %>%  
              Fn$get_count_range(var = by),
            `Model B` = dta[[1]][!is.na(get(by))] %>% 
              Fn$get_count_range(var = by),
            `Model C` = map(dta[-1], ~.x[PID %in% dta[[1]][is.na(get(by)), PID]]) %>% 
              Fn$get_count_range(var = by),
            `Model D` = dta[-1] %>% 
              Fn$get_count_range(var = by)
          )
          rbindlist(out, idcol = "Imp", fill = TRUE)
        }
        
        overall <- get_count_comparison(dataset)
        if (is.null(group)) {
          count_comparison <- overall
        } else {
          out <- list(overall)
          names(out) <- overall_label
          out <- append(
            out,
            map(dataset, split, by = group) %>% 
              transpose() %>% 
              map(get_count_comparison)
          )
          count_comparison <- rbindlist(out, idcol = group)
          count_comparison[, c(group) := factor(get(group), unique(get(group))), .SDcols = group]
        }
        count_comparison[, Imp := factor(Imp, unique(Imp))]
        
        model_footnote <- c(
          "Complete with missing defined as unspecified and included in the calculation of percentages", 
          "Complete case dataset (missings are excluded)", 
          "Number of imputed persons",
          "Imputed dataset"
        )
        names(model_footnote) <- count_comparison[, unique(Imp)]
        if (!is.null(group)) {
          model_footnote <- imap(
            model_footnote, 
            ~glue::glue("{.x} (n={count_comparison[get(group) == get(group)[1] & Imp == .y, format(sum(N), big.mark = ',')]})")
          )
        }
        model_footnote <- map_if(
          model_footnote, 
          ~stringr::str_detect(.x, "Imputed"), 
          stringr::str_replace, "\\(", "[Averaged using mean]("
        )
        attr(count_comparison, "footnote") <- model_footnote
        attr(count_comparison, "imp") <- length(dataset[-1])
        
        fwrite(count_comparison, str_replace(fpath, "[rR]ds$", "csv"))
        saveRDS(count_comparison, fpath)
        return(count_comparison)
      }
    )
    
    return(count_comparison)
  }
}, TableData)

## -- Sensetivity Analysis: Hazard Rate Comparison -------------------------
evalq({
  SurvComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    if (!is.null(group)) {
      fpath <- here::here(TDATA, glue::glue("{group}-{by}-SurvComparison.rds"))
    } else {
      fpath <- here::here(TDATA, glue::glue("{by}-SurvComparison.rds"))
    }
    
    hr_comparison <- Fn$load_or_run(
      rds_path = fpath,
      expression = {
        
        dataset <- Results$ImpData
        
        get_hr_comparison <- function(dta) {
          fitted_cox <- list(
            `Model A` = copy(dta)[[1]] %>%
              .[, c(by) := lapply(.SD, forcats::fct_na_value_to_level, "Unspecified"), .SDcols = by],
            `Model B` = dta[[1]][!is.na(get(by))],
            `Model C` = map(dta[-1], ~.x[PID %in% dta[[1]][is.na(get(by)), PID]]),
            `Model D` = dta[-1]
          ) %>% map(Fn$cox_fit, by = by)
          
          hr_comparison <- fitted_cox %>% 
            map(Fn$tidy_fit) %>%
            rbindlist(fill = TRUE, idcol = "Model") %>% 
            .[, HR := Fn$round_transform(2)("HR", .SD)] %>% 
            .[, Term := stringr::str_remove(Term, by)] %>% 
            .[, CI := glue::glue_data(
              .SD, "({Lower}, {Upper})", 
              .transformer = Fn$round_transform(2)
            )]
          ref_level <- na.omit(setdiff(
            dta[[1]][, unique(get(by))], 
            hr_comparison[, unique(Term)]
          ))
          hr_comparison <- hr_comparison %>%
            list(data.table(
              Model = paste0("Model ", LETTERS[1:4]), 
              Term = ref_level, 
              HR = 1
            )) %>% 
            rbindlist(fill = TRUE) %>% 
            .[order(Term)]
          
          event_cases <- map_df(fitted_cox, function(fit) {
            if ("coxph" %in% class(fit)) {
              out <- cbind(cases = fit$n, events = fit$nevent)
            } else {
              out <- cbind(
                cases = unique(map_dbl(fit, "n")),
                events = unique(map_dbl(fit, "nevent"))
              )
            }
            out <- as.data.table(out)
            out[, label := glue::glue_data(
              .SD, 
              "n={format(cases, big.mark = ',')};",
              "Melanoma death={format(events, big.mark = ',')}",
              .sep = " "
            )]
          }, .id = "Model")
          
          return(list(
            fitted_cox = fitted_cox,
            hr_comparison = hr_comparison,
            event_cases = event_cases
          ))
        }
        overall <- get_hr_comparison(dataset)
        
        if (is.null(group)) {
          hr_comparison <- pluck(overall, "hr_comparison")
          fitted_cox <- pluck(overall, "fitted_cox")
        } else {
          out <- list(pluck(overall, "hr_comparison"))
          names(out) <- overall_label
          ## Check here some thing wrong
          ## We get the same model for each split
          by_group <-  map(dataset, split, by = group) %>%
            transpose() %>%
            map(get_hr_comparison)
          out <- append(out, map(by_group, "hr_comparison"))
          hr_comparison <- rbindlist(out, idcol = group)
          hr_comparison[, c(group) := factor(get(group), unique(get(group))), .SDcols = group]
          fitted_cox <- map(by_group, pluck, "fitted_cox")
        }
        
        model_footnote <- c(
          "Complete with missing defined as unspecified and included in the calculation of percentages", 
          "Complete case dataset (missings are excluded)", 
          "Number of imputed persons",
          "Imputed dataset"
        )
        names(model_footnote) <- names(pluck(overall, "fitted_cox"))
        event_cases <- pluck(overall, "event_cases")
        model_footnote <- imap(
          model_footnote, 
          ~glue::glue("{.x} ({event_cases[Model == .y, label]})")
        )
        
        attr(hr_comparison, "fit") <- fitted_cox
        attr(hr_comparison, "event") <- event_cases
        attr(hr_comparison, "footnote") <- model_footnote
        attr(hr_comparison, "imp") <- length(dataset[-1])
        fwrite(hr_comparison, str_replace(fpath, "[rR]ds$", "csv"))
        saveRDS(hr_comparison, fpath)
        hr_comparison
      }
    )
    return(hr_comparison)
  }
}, TableData)

## -- (A)APC Tables -------------------------
evalq({
  AAPC <- new.env()
  
  ## -- Related local functions -------------------------
  evalq({
    Func <- new.env(parent = AAPC)
    evalq({
      get_apc <- function(group = NULL, logY = TRUE, ...) {
        group <- union(group, c("DiagYear", "Age"))
        out <- Results$AgeAdjRate(Results$ImpCases(...), Results$PersonYear, group) %>% 
          Results$Segmented(c("aapc", "psi"), logY = logY) %>% 
          reduce(merge.data.table) %>% 
          .[, c("psi_left", "psi_right") := .(round(psi_left), round(psi_right))]
        return(out[])
      }
      apc_list <- function(apc_data, logY = TRUE) {
        round_transf <- function(name, env) {
          formatC(get(name, env), format = "f", digits = 1)
          # round(get(name, env), 1)
        }
        groups <- names(apc_data)[-seq(
          grep("Estimate", names(apc_data)), 
          grep(last(names(apc_data)), names(apc_data))
        )]
        groups <- setdiff(groups, c("N", "imp"))
        
        cast_form <- reformulate(".", paste(c(groups, "Period"), collapse = " + "))
        
        out <- apc_data %>%
          copy() %>% 
          .[, Label := glue::glue_data(
            .SD, "{Estimate} ({Lower}, {Upper})",
            .transformer = round_transf
          )] %>% 
          .[, Period := glue::glue_data(.SD, "{psi_left}-{psi_right}")] %>% 
          .[, SegmentLab := stringr::str_replace(Segment, "(\\d)", "Trend \\1")] %>% 
          .[, SegmentLab := stringr::str_to_title(SegmentLab)] %>% 
          .[order(SegmentLab)] %>%
          split(by = "SegmentLab") %>%
          map(dcast.data.table, cast_form, value.var = "Label") %>%
          imap(
            function(.x, .y) {
              setnames(
                .x, data.table::last(names(.x), 2), 
                c(glue::glue(.y, "_Period"), 
                  glue::glue(.y, ifelse(logY, "_APC", "_Slope")))) %>% 
                setcolorder(c(
                  setdiff(
                    seq_along(.x), 
                    data.table::last(seq_along(.x), 2)
                  ),  
                  rev(data.table::last(seq_along(.x), 2))
                )) %>% 
                setkeyv(groups) %>% 
                .[, Segment := NULL]
            }
          )
        attr(out, "count") <- apc_data %>% 
          .[, lapply(.SD, unique), by = setdiff(groups, "Segment"), .SDcols = c("N", "imp")]
        return(out)
      }
      merge_apc <- function(apc_list) {
        out <- copy(apc_list)
        out[[1]] <- first(out)[, .SD[, -ncol(.SD), with = FALSE]]
        # out[[length(out)]] <- last(out)[, .SD[, -ncol(.SD), with = FALSE]]
        ret <- reduce(out, merge.data.table, all = TRUE)
        merge.data.table(attr(apc_list, "count"), ret)
      }
      get_parsed <- function(data) {
        data %>%
          tidyr::pivot_longer(
            cols = grep("_", names(.)),
            names_to = c("Trend", ".value"),
            names_sep = "_"
          ) %>% 
          tidyr::extract(
            "APC",
            into = c("APC", "Lower", "Upper"),
            regex = "(.+) \\((.+),(.+)\\)",
            convert = TRUE
          ) %>% 
          as.data.table()
      }
    }, Func)
  }, AAPC)
  
  ## -- By Sex -------------------------
  evalq({
    BySex <- function(..., logY = TRUE) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySex.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc("Sex", logY = logY, case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "HealthRegion"))
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        }
      )
    }
  }, AAPC)
  
  ## -- By Tstage -------------------------
  evalq({
    ByTstage <- function(..., logY = TRUE) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(
        TDATA, "AAPC-ByTstage.Rds"
      ))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc("Tstage", logY = logY)
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        }
      )
    }
  }, AAPC)
  
  ## -- By Sex and T-category ----
  evalq({
    BySexTstage <- function(..., logY = TRUE) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySexTstage.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage"), logY = logY)
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        }
      )
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Anatomic Site ----
  evalq({
    BySexTstageSite <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySexTstageSite.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage", "AnatomicSite"), logY = logY)
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        })
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Melanoma Type ----
  evalq({
    BySexTstageType <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySexTstageType.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage", "MelanomaType"), logY = logY)
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        })
    }
  }, AAPC)
  ## -- By Sex, T-category and Health Region ----
  evalq({
    BySexTstageRegion <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySexTstageRegion.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(
            group = c("Sex", "Tstage", "HealthRegion"), 
            logY = logY, 
            case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "HealthRegion")
          )
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        })
    }
  }, AAPC)
  ## -- By Sex, T-category and Season ----
  evalq({
    BySexTstageSeason <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(TDATA, "AAPC-BySexTstageSeason.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(
            group = c("Sex", "Tstage", "Season"), 
            logY = logY, 
            case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "Season")
          )
          out <- AAPC %>% Func$apc_list(logY = logY)
          ret <- Func$merge_apc(out)
          attr(ret, "get_parsed") <- Func$get_parsed
          fwrite(ret, str_replace(fpath, "[rR]ds$", "csv"))
          saveRDS(ret, file = fpath)
          return(ret)
        })
    }
  }, AAPC)
  
}, TableData)

## -- Count data for table ----------------
evalq({
  PropData <- function() {
    fname <- here::here(TDATA, "Counts.rds")
    PlotData <- Fn$load_or_run(
      rds_path = fname,
      expression = expression({
        data <- Results$ImpCases() %>% 
          rbindlist(
            use.names = TRUE,
            idcol = "Imp"
          )
        
        fwrite(data, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(data, fname) 
      })
    )
  }
}, TableData)
