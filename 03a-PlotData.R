## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
INC <- Sys.getenv("DataInc")
INC_ROOT <- file.path(ROOT, "01-incidence")

PDATA <- file.path(INC_ROOT, "02-Results", "PlotData")
if (!dir.exists(PDATA)) dir.create(PDATA)

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "tidytable", "data.table", "purrr", "stringr", 
  "plotly", "ggplot2"
))

## -- Get the results ----
Results <- readRDS(here::here(INC, "Results.rds"))

## -- Plots Data ----
PlotData <- new.env(parent = Results)
evalq({
  Spline <- new.env()
  AgeAdjusted <- new.env()
  ModelAPC <- new.env()
}, PlotData)

## -- Spline Plot Data ----
evalq({
  ## -- By: Sex, Tstage --------
  evalq({
    BySexTstage <- function(..., filter = NULL, logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTstage.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTstage.Rds")
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, Thinner Cases ----
  evalq({
    BySexThinStage <- function(..., filter = NULL, logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexThinStage.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexThinStage.Rds")
      }
      expr <- expression({
          
          Fn$quietly_load(c("forcats"))
          ImpData <- get("ImpData", envir = parent.env(environment()))
          
          SubData <- lapply(ImpData, function(dta) {
            dta[Thickness <= 1] %>%
              .[, ThinStage := cut(
                Thickness,
                breaks = c(0, 0.5, 0.8, 1.0),
                include.lowest = TRUE,
                right = TRUE
                # labels = c("[0,0.5]", "(0.5,0.8]", "(0.8,1]")
              )]
          })
          SubCases <- lapply(SubData, function(dta) {
            out <- dta[CJ(DiagYear, Sex, AgeGroup5, ThinStage, unique = TRUE),
                       on = .(DiagYear, Sex, AgeGroup5, ThinStage),
                       .N, by = .(DiagYear, Sex, AgeGroup5, ThinStage)
            ]
            setnames(out, c("ThinStage"), c("Tstage"))
          })
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(adj.rate)]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          
          AgeAdjDataThin <- AgeAdjRate(
            cases = SubCases,
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(adj.rate)]
          
          if (!is.null(filter)) {
            AgeAdjDataThin <- AgeAdjDataThin[eval(parse(text = filter))]
          }
          
          PlotDataAll <- Segmented(AgeAdjData, "spline", logY = logY)
          PlotDataThin <- Segmented(AgeAdjDataThin, "spline", logY = logY)
          
          PlotData <- rbindlist(list(
            `All Cases` = PlotDataAll,
            `Thin Cases` = PlotDataThin
          ), idcol = "Case")
          PlotData[, Tstage := fct_relevel(
            Tstage,
            PlotData[, levels(Tstage)[c(5, 6, 7, 1:4)]]
          )]
          if (!is.null(filter)) {
            PlotData <- PlotData[eval(parse(text = filter))]
          }
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, TSubStage ----
  evalq({
    BySexTSubStage <- function(..., filter = NULL, logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTSubStage.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTSubStage.Rds")
      }
      expr <- expression({
          Cases <- lapply(ImpData, function(dta) {
            out <- dta[!is.na(Thickness)] %>%
              .[CJ(DiagYear, Sex, AgeGroup5, TSubStage, unique = TRUE),
                on = .(DiagYear, Sex, AgeGroup5, TSubStage),
                .N, by = .(DiagYear, Sex, AgeGroup5, TSubStage)
              ] %>%
              .[str_detect(TSubStage, "\\d$", negate = TRUE)]
            setnames(out, "TSubStage", "Tstage")
            out[, Tstage := str_replace(
              Tstage,
              "T([2-4])[a-b]",
              "T\\1"
            )]
          })
          
          AgeAdjData <- AgeAdjRate(
            cases = Cases,
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(adj.rate)]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          
          
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, file = fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Anatomic Site ------------------------------
  evalq({
    BySexTstageSite <- function(..., filter = "Sex != 'Sum'", logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTstageSite.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTstageSite.Rds")
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage", "AnatomicSite"),
            margin = "Sex"
          ) %>%
            .[!is.na(Tstage) & !is.na(AnatomicSite)]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      
      if (check) {
        PlotData <- Fn$load_or_run(rds_path = fname, expression = expr)
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Melanoma Type ------------------------------
  evalq({
    BySexTstageType <- function(..., filter = "Sex != 'Sum'", logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTstageType.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTstageType.Rds")
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage", "MelanomaType"),
            margin = "Sex"
          ) %>%
            .[!is.na(Tstage) & !is.na(MelanomaType)] %>%
            .[adj.rate > 0]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, HealthRegion ------------------------------
  evalq({
    BySexTstageRegion <- function(..., filter = "Sex != 'Sum'", logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTstageRegion.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTstageRegion.Rds")
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(
              case_group = c(
                "DiagYear", "Sex", "AgeGroup5", 
                "Tstage", "HealthRegion"
              )
            ),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage", "HealthRegion"),
            margin = "Sex"
          ) %>%
            .[!is.na(Tstage) & !is.na(HealthRegion)] %>%
            .[adj.rate > 0]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Season ------------------------------
  evalq({
    BySexTstageSeason <- function(..., filter = "Sex != 'Sum'", logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTstageSeason.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTstageSeason.Rds")
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(
              case_group = c(
                "DiagYear", "Sex", "AgeGroup5", 
                "Tstage", "Season"
             )),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage", "Season"),
            margin = "Sex"
          ) %>%
            .[!is.na(Tstage) & !is.na(Season)] %>%
            .[adj.rate > 0]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
  
  ## -- By: Sex, TSubStage after 2008 ----------------
  evalq({
    BySexTSubStageRecent <- function(..., filter = NULL, logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(PDATA, "Spline-logY-BySexTSubStageRecent.Rds")
      } else {
        fname <- here::here(PDATA, "Spline-BySexTSubStageRecent.Rds")
      }
      expr <- expression({
        Cases <- lapply(ImpData, function(dta) {
          out <- copy(dta) %>% 
            filter(DiagYear >= 2008) %>% 
            # mutate(Ulceration = if_else(DiagYear >= 2000 & is.na(Ulceration), "Absent", Ulceration)) %>% 
            mutate(TSubStage = Fn$get_Tsubstage(
              Thickness,
              yes = fifelse(Ulceration == "Present", TRUE, NA),
              no = fifelse(Ulceration == "Absent", TRUE, NA),
              missing = fifelse(is.na(Ulceration), TRUE, NA)
            )) %>% 
            GetCases(c("DiagYear", "Sex", "AgeGroup5", "TSubStage", "Tstage"))
          return(out)
        })
        
        AgeAdjData <- AgeAdjRate(
          cases = Cases,
          person_year = PersonYear,
          group = c("DiagYear", "Sex", "TSubStage", "Tstage")
        )[!is.na(adj.rate)]
        
        if (!is.null(filter)) {
          AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
        }
        
        
        PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
        
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, file = fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, Spline)
}, PlotData)

## -- Age Adjusted Plots ------------------------------
evalq({
  ## -- By: Sex, Tstage ------------------------------
  evalq({
    BySexTstage <- function(..., segmented = FALSE, filter = NULL, logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexTstage.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexTstage.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexTstage.Rds"))
      }
      expr <- expression({
          PlotData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(Tstage)]
          if (!is.null(filter)) {
            PlotData <- PlotData[eval(parse(text = filter))]
          }
          if (segmented) {
            PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
          }
          if (na.rm) PlotData <- na.omit(PlotData)
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
        })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Thinner Cases ----
  evalq({
    BySexThinStage <- function(..., segmented = FALSE, filter = NULL, logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexThinStage.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexThinStage.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexThinStage.Rds"))
      }
      expr <- expression({
          
          Fn$quietly_load(c("forcats"))
          
          SubCases <- lapply(ImpData, function(dta) {
            subData <- dta[Thickness <= 1] %>%
              .[, ThinStage := cut(
                Thickness,
                breaks = c(0, 0.5, 0.8, 1.0),
                include.lowest = TRUE,
                right = TRUE
                # labels = c("[0, 0.5]", "(0.5, 0.8]", "(0.8, 1.0)")
              )]
            out <- subData[
              CJ(DiagYear, Sex, AgeGroup5, ThinStage, unique = TRUE),
              on = .(DiagYear, Sex, AgeGroup5, ThinStage),
              .N, by = .(DiagYear, Sex, AgeGroup5, ThinStage)
            ]
            setnames(out, "ThinStage", "Tstage")
          })
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(adj.rate)]
          if (!is.null(filter)) {
            AgeAdjData <- AgeAdjData[eval(parse(text = filter))]
          }
          
          AgeAdjDataThin <- AgeAdjRate(
            cases = SubCases,
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )[!is.na(adj.rate)]
          if (!is.null(filter)) {
            AgeAdjDataThin <- AgeAdjDataThin[eval(parse(text = filter))]
          }
          
          PlotData <- rbindlist(list(
            `All Cases` = AgeAdjData,
            `Thin Cases` = AgeAdjDataThin
          ), idcol = "Case")
          PlotData[, Tstage := fct_relevel(
            Tstage,
            PlotData[, levels(Tstage)[c(5, 6, 7, 1:4)]]
          )]
          
          if (segmented) {
            PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
          }
          if (na.rm) PlotData <- na.omit(PlotData)
          
          fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
          saveRDS(PlotData, fname)
          PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
      
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Anatomic Site ------------------------------
  evalq({
    BySexTstageSite <- function(..., segmented = FALSE, filter = "Sex != 'Sum'", logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexTstageSite.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexTstageSite.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexTstageSite.Rds"))
      }
      expr <- expression({
        PlotData <- AgeAdjRate(
          cases = ImpCases(...),
          person_year = PersonYear,
          group = c("DiagYear", "Sex", "Tstage", "AnatomicSite"),
          margin = "Sex"
        )[!AnatomicSite %in% c("Other") & !is.na(AnatomicSite) & !is.na(Tstage)]
        if (!is.null(filter)) {
          PlotData <- PlotData[eval(parse(text = filter))]
        }
        if (segmented) {
          PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
        }
        if (na.rm) PlotData <- na.omit(PlotData)
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Melanoma Type ------------------------------
  evalq({
    BySexTstageType <- function(..., segmented = FALSE, filter = "Sex != 'Sum'", logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexTstageType.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexTstageType.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexTstageType.Rds"))
      }
      expr <- expression({
        PlotData <- AgeAdjRate(
          cases = ImpCases(...),
          person_year = PersonYear,
          group = c("DiagYear", "Sex", "Tstage", "MelanomaType"),
          margin = "Sex"
        )[!MelanomaType %in% c("Other") & !is.na(MelanomaType) & !is.na(Tstage)]
        if (!is.null(filter)) {
          PlotData <- PlotData[eval(parse(text = filter))]
        }
        if (segmented) {
          PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
        }
        if (na.rm) PlotData <- na.omit(PlotData)
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Health Region ------------------------------
  evalq({
    BySexTstageRegion <- function(..., segmented = FALSE, filter = "Sex != 'Sum'", logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexTstageRegion.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexTstageRegion.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexTstageRegion.Rds"))
      }
      expr <- expression({
        PlotData <- AgeAdjRate(
          cases = ImpCases(
            case_group = c(
              "DiagYear", "Sex", "AgeGroup5", 
              "Tstage", "HealthRegion"
            )),
          person_year = PersonYear,
          group = c("DiagYear", "Sex", "Tstage", "HealthRegion"),
          margin = "Sex"
        )[!HealthRegion %in% c("Other") & !is.na(HealthRegion) & !is.na(Tstage)]
        if (!is.null(filter)) {
          PlotData <- PlotData[eval(parse(text = filter))]
        }
        if (segmented) {
          PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
        }
        if (na.rm) PlotData <- na.omit(PlotData)
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Season ------------------------------
  evalq({
    BySexTstageSeason <- function(..., segmented = FALSE, filter = "Sex != 'Sum'", logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(PDATA, "Segmented-logY-BySexTstageSeason.Rds"))
        } else {
          fname <- here::here(file.path(PDATA, "Segmented-BySexTstageSeason.Rds"))
        }
      } else {
        fname <- here::here(file.path(PDATA, "AgeAdjusted-BySexTstageSeason.Rds"))
      }
      expr <- expression({
        PlotData <- AgeAdjRate(
          cases = ImpCases(
            case_group = c(
              "DiagYear", "Sex", "AgeGroup5", 
              "Tstage", "Season"
            )),
          person_year = PersonYear,
          group = c("DiagYear", "Sex", "Tstage", "Season"),
          margin = "Sex"
        )[!Season %in% c("Other") & !is.na(Season) & !is.na(Tstage)]
        if (!is.null(filter)) {
          PlotData <- PlotData[eval(parse(text = filter))]
        }
        if (segmented) {
          PlotData <- Segmented(PlotData, c("fitted"), logY = logY)
        }
        if (na.rm) PlotData <- na.omit(PlotData)
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      if (!is.null(filter)) {
        PlotData <- PlotData[eval(parse(text = filter))]
      }
      return(PlotData)
    }
  }, AgeAdjusted)
}, PlotData)

## -- Age Specific Plots ----------------
evalq({
  AgeSpecific <- function(..., BySex = TRUE, Group = NULL, filter = NULL, check = TRUE) {
    attach(Results)
    on.exit(detach(Results))
    
    fname0 <- ifelse(
      BySex & is.null(Group), "Sex",
      ifelse(
        BySex & !is.null(Group), paste0("Sex-", Group),
        ifelse(
          !BySex & !is.null(Group), Group,
          ""
        )
      )
    )
    if (fname0 != "") {
      fname <- here::here(file.path(
        PDATA,
        paste0(paste("ASP-Data", fname0, sep = "-"
        ), ".Rds")))
    } else {
      fname <- here::here(file.path(PDATA, "ASP-Data.Rds"))
    }
    expr <- expression({
      all_group_vars <- c("AgeGroup5")
      if (!is.null(Group)) all_group_vars <- append(all_group_vars, Group)
      if (BySex) all_group_vars <- append(all_group_vars, "Sex")
      
      asp_data <- ImpCases(all_group_vars) %>% 
        map(GetIncDataByGroup, PersonYear, all_group_vars) %>%
        map(tidytable::select, -Imp) %>% 
        map_df(GetIncRate, .id = "imp") %>% 
        tidytable::mutate(
          Imp = if_else(imp == "0", "Complete", "Pooled")
        )
      asp_data <- asp_data %>% 
        .[, .(
          N = sum(N), 
          imp = .N, 
          est = mean(est), 
          lower = mean(lower), 
          upper = mean(upper)
        ), by = setdiff(names(.), c("N", "est", "lower", "upper"))]
      if (!is.null(Group)) asp_data <- asp_data[!is.na(asp_data[[Group]])]
      
      fwrite(asp_data, str_replace(fname, "[rR]ds$", "csv"))
      saveRDS(asp_data, fname)
      asp_data
    })
    if (check) {
      plot_data <- Fn$load_or_run(
        rds_path = fname,
        expression = expr
      )
    } else {
      plot_data <- eval(expr)
    }
    if (!is.null(filter)) plot_data <- plot_data[eval(parse(text = filter))]
    return(plot_data)
  }
}, PlotData)

## -- APC Plots------------------------------
evalq({
  APC <- function(..., BySex = FALSE, Group = NULL, filter = "Age >= 20", check = TRUE) {
    attach(Results)
    on.exit(detach(Results))
    
    fname0 <- ifelse(
      BySex & is.null(Group), "Sex",
      ifelse(
        BySex & !is.null(Group), paste0("Sex-", Group),
        ifelse(
          !BySex & !is.null(Group), Group,
          ""
        )
      )
    )
    if (fname0 != "") {
      fname <- here::here(file.path(
        PDATA,
        paste0(paste("APC", fname0, sep = "-"
        ), ".Rds")))
    } else {
      fname <- here::here(file.path(PDATA, "APC.Rds"))
    }
    expr <- expression({
      all_group_vars <- c("DiagYear", "AgeGroup5")
      if (!is.null(Group)) all_group_vars <- append(all_group_vars, Group)
      if (BySex) all_group_vars <- append(all_group_vars, "Sex")
      apc_data <- GetIncDataByGroup(ImpCases(all_group_vars), PersonYear, all_group_vars) %>%
        Results$APC() %>%
        GetIncRate()
      apc_data[, Imp := fifelse(Imp == "0", "Complete", "Pooled")]
      apc_data <- apc_data %>% 
        .[, .(
          N = sum(N), 
          imp = .N, 
          est = mean(est), 
          lower = mean(lower), 
          upper = mean(upper)
        ), by = setdiff(names(.), c("N", "est", "lower", "upper"))]
      if (!is.null(Group)) apc_data <- apc_data[!is.na(apc_data[[Group]])]
      
      fwrite(apc_data, str_replace(fname, "[rR]ds$", "csv"))
      saveRDS(apc_data, fname)
      apc_data
    })
    if (check) {
      plot_data <- Fn$load_or_run(
        rds_path = fname,
        expression = expr
      )
    } else {
      plot_data <- eval(expr)
    }
    if (!is.null(filter)) plot_data <- plot_data[eval(parse(text = filter))]
    return(plot_data)
  }
  ByAgeSex <- function(row_var = "Tstage", col_var = "AgeGroup", group_var = "Sex", check = TRUE, ...) {
    fname0 <- ifelse(
      row_var & is.null(col_var),row_var,
      ifelse(
        row_var & !is.null(col_var), paste(row_var, col_var, collapse = "-"),
        ifelse(
          !row_var & !is.null(col_var), col_var,
          ""
        )
      )
    )
    if (fname0 != "") {
      fname <- here::here(file.path(
        PDATA,
        paste0(paste("APC-AgeSex", fname0, sep = "-"
        ), ".Rds")))
    } else {
      fname <- here::here(file.path(PDATA, "APC-AgeSex.Rds"))
    }
    expr <- expression({
      attach(Results)
      on.exit(detach(Results))
      
      all_group_vars <- c("DiagYear", group_var)
      if (!is.null(row_var)) all_group_vars <- append(all_group_vars, group_var)
      if (row_var) all_group_vars <- append(all_group_vars, row_var)
      apc_data <- GetIncDataByGroup(ImpCases(), PersonYear, all_group_vars) %>%
        Results$APC() %>%
        GetIncRate()
      apc_data[, Imp := fifelse(Imp == "0", "Complete", "Pooled")]
      
      all_group_vars <- stringr::str_replace(all_group_vars, "DiagYear", "DiagYear")
      apc_data <- apc_data %>% 
        .[, .(
          N = sum(N), 
          imp = .N, 
          est = mean(est), 
          lower = mean(lower), 
          upper = mean(upper)
        ), by = c("Imp", all_group_vars, "BirthYear")]
      
      if (!is.null(Group)) apc_data <- apc_data[!is.na(apc_data[[Group]])]
      
      fwrite(apc_data, str_replace(fname, "[rR]ds$", "csv"))
      saveRDS(apc_data, fname)
      apc_data
    })
    if (check) {
      plot_data <- Fn$load_or_run(
        rds_path = fname,
        expression = expr
      )
    } else {
      plot_data <- eval(expr)
    }
    if (!is.null(filter)) plot_data <- plot_data[eval(parse(text = filter))]
    return(plot_data)
  } 
}, PlotData)

## -- APC Model Plots ----
evalq({
  
  ## -- Effect plot using Epi pacakge ----
  evalq({
    Fit <- function(..., Param = "ACP", Group = NULL, Facet = NULL, 
                    ShowAge = TRUE, filter = NULL, na.rm = TRUE, check = TRUE) {
      fname <- here::here(file.path(PDATA, "APC-Model-Fit.Rds"))
      expr <- expression({
        attach(Results)
        on.exit(detach(Results))
        
        group_vars <- c(Group, Facet)
        all_group_vars <- append(c("DiagYear", "AgeGroup5"), group_vars)
        
        apc_data <- GetIncDataByGroup(Cases, PersonYear, all_group_vars) %>%
          Results$APC() %>%
          ModelAPC(Param = Param)
        apc_data <- apc_data[Imp == "Pooled"]
        
        if (!is.null(Group)) apc_data <- apc_data[!is.na(get(Group))]
        if (!is.null(Facet)) apc_data <- apc_data[!is.na(get(Facet))]
        if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
        if (na.rm) apc_data <- na.omit(apc_data)
        fwrite(apc_data, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(apc_data, fname)
      })
      if (check) {
        apc_data <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        apc_data <- eval(expr)
      }
      return(apc_data)
    }
  }, ModelAPC)
  
  ## -- Effect plot using ggplot2 ----
  evalq({
    Effect <- function(..., Param = "ACP", Group = NULL, Facet = NULL, ShowAge = TRUE, filter = NULL, na.rm = TRUE, check = TRUE) {
      fname <- here::here(file.path(PDATA, "APC-Model-Effect.Rds"))
      expr <- expression({
        attach(Results)
        on.exit(detach(Results))
        
        group_vars <- c(Group, Facet)
        all_group_vars <- append(c("DiagYear", "AgeGroup5"), group_vars)
        apc_data <- GetIncDataByGroup(Cases, PersonYear, all_group_vars) %>%
          Results$APC() %>%
          ModelAPC(Param = Param)
        
        if ("Imp" %in% names(apc_data)) group_vars <- append("Imp", group_vars)
        
        PlotData <- apc_data[, map_df(eff, list), by = group_vars]
        PlotData <- PlotData[Imp == "Pooled"]
        
        if (!ShowAge) PlotData <- PlotData[EffectType != "Age"]
        if (!is.null(Group)) PlotData <- PlotData[!is.na(get(Group))]
        if (!is.null(Facet)) PlotData <- PlotData[!is.na(get(Facet))]
        if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
        if (na.rm) PlotData <- na.omit(PlotData)
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        PlotData
      })
      if (check) {
        PlotData <- Fn$load_or_run(
          rds_path = fname,
          expression = expr
        )
      } else {
        PlotData <- eval(expr)
      }
      return(PlotData)
    }
  }, ModelAPC)
  
}, PlotData)

## -- Missing Breslow and Ulceration ------------------------------
evalq({
  MissingTrend <- function(data = Results$Data(), by = NULL, filter = NULL) {
    fname <- here::here(PDATA, "MissingTrend-Overall.rds")
    PlotData <- Fn$load_or_run(
      rds_path = fname,
      expression = expression({
        Data <- data
        
        formula <- reformulate(union(c("DiagYear", "Sex", by), by))
        
        sex_lbl <- xtabs(~Sex, data = Data) %>% addmargins()
        dimnames(sex_lbl) <- map(dimnames(sex_lbl), str_replace, "Sum", "Overall")
        sex_lbl <- Fn$label_vec(sex_lbl)
        
        
        if (!is.null(by)) {
          if (by %in% c("DiagYear", "Sex")) by <- NULL
        }
        
        if (!is.null(by)) {
          by_lbl <- xtabs(reformulate(by), data = data) %>% addmargins()
          dimnames(by_lbl) <- map(dimnames(by_lbl), str_replace, "Sum", "Overall")
          by_lbl <- Fn$label_vec(by_lbl)
        } else {
          by_lbl <- NULL
        }
        
        PlotData <- rbindlist(list(
          `Tumour thickness` = as.data.table(
            xtabs(formula, data = Data, subset = is.na(Tstage)) %>%
              addmargins(2) / xtabs(formula, data = Data) %>% addmargins(2)
          ),
          Ulceration = as.data.table(
            xtabs(formula, data = Data, subset = is.na(Ulceration)) %>%
              addmargins(2) / xtabs(formula, data = Data) %>% addmargins(2)
          )
        ),
        idcol = "variable"
        )
        PlotData <- PlotData %>% 
          setnames("N", "Prop") %>%
          .[, DiagYear := as.numeric(DiagYear)] %>%
          .[, Sex := CodeMap$factor_map(Sex, "Sex") %>%
              forcats::fct_na_value_to_level("Overall")]
        if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
        
        attr(PlotData, "sex_label") <- sex_lbl
        attr(PlotData, "by_label") <- by_lbl
        
        fwrite(PlotData, str_replace(fname, "[rR]ds$", "csv"))
        saveRDS(PlotData, fname)
        return(PlotData[])
      })
    )
  }
}, PlotData)


## -- Plot-Data for T-histogram ----------------------------------------
evalq({
  thist <- function(data = Results$Data(), group = NULL, max_thickness = 15, breaks = c(0, 0.8, 1, 2, 4, max_thickness), ...) {
    library(ggplot2)
    library(data.table)
    
    plt_df <- data[
      !is.na(Thickness) & Thickness <= max_thickness, 
      .(
        Thickness = round(Thickness, 1),
        Group = if (!is.null(group)) get(group) else NULL
      )
    ][, Cuts := cut(Thickness, breaks = breaks, include.lowest = TRUE, ...),
      by = if (!is.null(group)) Group else NULL]
    
    if (!is.null(group)) {
      setnames(plt_df, "Group", group)
    }
    return(plt_df[])
  }
}, PlotData)

## -- Count data for plotting ----------------
evalq({
  PropData <- function() {
    fname <- here::here(PDATA, "Counts.rds")
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
}, PlotData)
