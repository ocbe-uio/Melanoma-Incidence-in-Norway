## -- Source scripts and functions --------
source(here::here(BASE_PATH, "Scripts/00-CodeMap.R"))
source(here::here(BASE_PATH, "Scripts/00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "data.table", "plotly", "stringr"
))

## -- Get the results ----
source(here::here(BASE_PATH, "Scripts/02-Results.R"))

## -- Table Data ----
TableData <- new.env(parent = Results)

## -- Case Exclusion data ----
evalq({
  CaseExclusion <- function() {
    if (!exists("StepData")) {
      StepData <- readRDS(here::here(CodeMap$DATA_PATH, "StepData.rds"))
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
  }
}, TableData)

## -- Sensetivity Analysis: Count Comparison -------------------------
evalq({
  CountComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    if (!is.null(group)) {
      fpath <- here::here(CodeMap$DATA_PATH, "PlotData", paste(group, by, "CountComparison.rds", sep = "-"))
    } else {
      fpath <- here::here(CodeMap$DATA_PATH, "PlotData", paste(by, "CountComparison.rds", sep = "-"))
    }
    count_comparison <- Fn$load_or_run(
      rds_path = fpath,
      expression = {
        dataset <- Results$ImpData
        get_count_comparison <- function(dta) {
          out <- list(
            `Model A` = Fn$get_count_range(
              data = copy(dta)[[1]][, c(by) := lapply(.SD, forcats::fct_na_value_to_level, "Unspecified"), .SDcols = by], 
              var = by
            ),
            `Model B` = Fn$get_count_range(
              data = dta[[1]][!is.na(get(by))], 
              var = by
            ),
            `Model C` = Fn$get_count_range(
              data = map(dta[-1], ~.x[ID %in% dta[[1]][is.na(get(by)), ID]]),
              var = by
            ),
            `Model D` = Fn$get_count_range(
              data = dta[-1],
              var = by
            )
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
        model_footnote <- imap(
          model_footnote, 
          ~glue::glue("{.x} (n={count_comparison[get(group) == get(group)[1] & Imp == .y, format(sum(N), big.mark = ',')]})")
        )
        model_footnote <- map_if(
          model_footnote, 
          ~stringr::str_detect(.x, "Imputed"), 
          stringr::str_replace, "\\(", "[Averaged using mean]("
        )
        attr(count_comparison, "footnote") <- model_footnote
        attr(count_comparison, "imp") <- length(dataset[-1])
        saveRDS(
          object = count_comparison, 
          file = here::here(CodeMap$DATA_PATH, "PlotData", paste(group, by, "CountComparison.rds", sep = "-"))
        )
        count_comparison
      }
    )
    
    return(count_comparison)
  }
}, TableData)

## -- Sensetivity Analysis: Hazard Rate Comparison -------------------------
evalq({
  SurvComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    fpath <- if (!is.null(group)) {
      here::here(CodeMap$DATA_PATH, "PlotData", paste(by, "SurvComparison.rds", sep = "-"))
    } else {
      here::here(CodeMap$DATA_PATH, "PlotData", paste(group, by, "SurvComparison.rds", sep = "-"))
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
            `Model C` = map(dta[-1], ~.x[ID %in% dta[[1]][is.na(get(by)), ID]]),
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
            names_to = c("Period", ".value"),
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
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySex.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc("Sex", logY = logY, case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "HealthRegion"))
          saveRDS(AAPC, file = fpath)
          AAPC
        }
      )
      
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
    }
  }, AAPC)
  
  ## -- By Tstage -------------------------
  evalq({
    ByTstage <- function(..., logY = TRUE) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(
        CodeMap$DATA_PATH, "PlotData", "AAPC-ByTstage.Rds"
      ))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc("Tstage", logY = logY)
          saveRDS(AAPC, file = fpath)
          AAPC
        }
      )
      
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
    }
  }, AAPC)
  
  ## -- By Sex and T-category ----
  evalq({
    BySexTstage <- function(..., logY = TRUE) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySexTstage.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage"), logY = logY)
          saveRDS(AAPC, file = fpath)
          AAPC
        }
      )
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Anatomic Site ----
  evalq({
    BySexTstageSite <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySexTstageSite.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage", "AnatomicSite"), logY = logY)
          # AAPC <- AgeAdjRate(ImpCases(), PersonYear, c("Year", "Age", "Sex", "Tstage", "AnatomicSite")) %>%
          #   .[AnatomicSite != "Other"] %>%
          #   Segmented(c("aapc", "psi"), logY = logY) %>%
          #   reduce(merge.data.table) %>%
          #   .[, c("psi_left", "psi_right") := .(round(psi_left), round(psi_right))]
          saveRDS(AAPC, file = fpath)
          AAPC
        })
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
      
      # out <- AAPC[!is.na(Tstage) & !is.na(AnatomicSite)] %>%
      #   .[, Label := glue::glue_data(.SD, "{round(Estimate, 2)} ({round(Lower, 1)},{round(Upper, 1)})")] %>%
      #   .[, SegmentLab := fifelse(grepl("^overall", Segment), "Overall", paste("Period", Segment))] %>%
      #   .[order(SegmentLab)] %>% 
      #   split(by = "SegmentLab") %>%
      #   map(dcast.data.table, Imp + Sex + Tstage + AnatomicSite + psi_right ~ ., value.var = "Label") %>%
      #   map(setcolorder, c(1:4, 6, 5)) %>%
      #   imap(~ setnames(.x, 6:5, c(glue::glue(.y, "_Year"), glue::glue(.y, ifelse(logY, "_APC", "_Slope")))))
      # 
      # idx <- seq_along(out)
      # out[c(first(idx), last(idx))] <- map(out[c(first(idx), last(idx))], ~.x[, -ncol(.x), with = FALSE])
      # 
      # out <- reduce(out, ~ merge.data.table(.x, .y, by = c("Imp", "Sex", "Tstage", "AnatomicSite"), all = TRUE))
      # attr(out, "parse") <- function() get_parsed(out)
      # return(out)
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Melanoma Type ----
  evalq({
    BySexTstageType <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySexTstageType.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(c("Sex", "Tstage", "MelanomaType"), logY = logY)
          # AAPC <- AgeAdjRate(ImpCases(), PersonYear, c("Year", "Age", "Sex", "Tstage", "MelanomaType")) %>%
          #   .[MelanomaType != "Other"] %>%
          #   Segmented(c("aapc", "psi"), logY = logY) %>%
          #   reduce(merge.data.table) %>%
          #   .[, c("psi_left", "psi_right") := .(round(psi_left), round(psi_right))]
          saveRDS(AAPC, file = fpath)
          AAPC
        })
      
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
      # out <- AAPC[!is.na(Tstage) & !is.na(MelanomaType)] %>%
      #   .[, Label := glue::glue_data(.SD, "{round(Estimate, 2)} ({round(Lower, 1)},{round(Upper, 1)})")] %>%
      #   .[, SegmentLab := fifelse(grepl("^overall", Segment), "Overall", paste("Period", Segment))] %>%
      #   .[order(SegmentLab)] %>% 
      #   split(by = "SegmentLab") %>%
      #   map(dcast.data.table, Imp + Sex + Tstage + MelanomaType + psi_right ~ ., value.var = "Label") %>%
      #   map(setcolorder, c(1:4, 6, 5)) %>%
      #   imap(~ setnames(.x, 6:5, c(glue::glue(.y, "_Year"), glue::glue(.y, ifelse(logY, "_APC", "_Slope")))))
      # 
      # idx <- seq_along(out)
      # out[c(first(idx), last(idx))] <- map(out[c(first(idx), last(idx))], ~.x[, -ncol(.x), with = FALSE])
      # 
      # out <- reduce(out, ~ merge.data.table(.x, .y, by = c("Imp", "Sex", "Tstage", "MelanomaType"), all = TRUE))
      # attr(out, "parse") <- function() get_parsed(out)
      # return(out)
      
    }
  }, AAPC)
  ## -- By Sex, T-category and Health Region ----
  evalq({
    BySexTstageRegion <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySexTstageRegion.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(
            group = c("Sex", "Tstage", "HealthRegion"), 
            logY = logY, 
            case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "HealthRegion")
          )
          saveRDS(AAPC, file = fpath)
          AAPC
        })
      
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
    }
  }, AAPC)
  ## -- By Sex, T-category and Season ----
  evalq({
    BySexTstageSeason <- function(logY = TRUE, ...) {
      attach(Results)
      on.exit(detach(Results))
      
      fpath <- here::here(file.path(CodeMap$DATA_PATH, "PlotData", "AAPC-BySexTstageSeason.Rds"))
      AAPC <- Fn$load_or_run(
        rds_path = fpath,
        expression = {
          AAPC <- Func$get_apc(
            group = c("Sex", "Tstage", "Season"), 
            logY = logY, 
            case_group = c("DiagYear", "Sex", "AgeGroup5", "Tstage", "Season")
          )
          saveRDS(AAPC, file = fpath)
          AAPC
        })
      
      out <- AAPC %>% Func$apc_list(logY = logY)
      return(Func$merge_apc(out))
    }
  }, AAPC)
  
}, TableData)

## -- Age-adjusted rate and annual percentage -------------------------
evalq({
  
}, TableData)
