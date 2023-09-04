## -- Source scripts and functions --------
source(here::here(BASE_PATH, "Scripts/00-CodeMap.R"))
source(here::here(BASE_PATH, "Scripts/00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "data.table", "plotly", "stringr"
))

## -- Get the results ----
if (!exists("Results")) {
  Results <- readRDS(here::here(CodeMap$DATA_PATH, "Composite", "Results.rds"))
} else {
  source(here::here("Scripts/02-Results.R"))
}

## -- Plots Data ----
PlotData <- new.env(parent = Results)
evalq(
  {
    Spline <- new.env()
    AgeAdjusted <- new.env()
    ModelAPC <- new.env()
  },
  PlotData
)

## -- Proportion data ----
evalq({
  Proportion <- function(rate = "incidence", group = NULL) {
    rate <- match.arg(rate)
    if (rate == "incidence") {
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Cases.Rds"
        ))
      data <- Results$Cases
    } else {
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Deaths.Rds"
        ))
      data <- Results$Deaths
    }
    if (is.null(group)) {
      group <- c("DiagYear", "Tstage")
    } else {
      group <- union(group, c("DiagYear", "Tstage"))
    }
    Fn$count_by_group(data, group, margin)
  }
  
}, PlotData) #// TODO: This does not work, need to FIX

## -- Spline Plot Data ----
evalq({
  ## -- By: Sex, Tstage --------
  evalq({
    BySexTstage <- function(..., filter = NULL, logY = TRUE, check = TRUE) {
      if (logY) {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTstage.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTstage.Rds"
        ))
      }
      expr <- expression({
          AgeAdjData <- AgeAdjRate(
            cases = ImpCases(...),
            person_year = PersonYear,
            group = c("DiagYear", "Sex", "Tstage"),
            margin = "Sex"
          )
          PlotData <- Segmented(AgeAdjData, "spline", logY = logY)
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexThinStage.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexThinStage.Rds"
        ))
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTSubStage.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTSubStage.Rds"
        ))
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTstageSite.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTstageSite.Rds"
        ))
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTstageType.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTstageType.Rds"
        ))
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTstageRegion.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTstageRegion.Rds"
        ))
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
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-logY-BySexTstageSeason.Rds"
        ))
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Spline-BySexTstageSeason.Rds"
        ))
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
  
}, PlotData)

## -- Age Adjusted Plots ------------------------------
evalq({
  ## -- By: Sex, Tstage ------------------------------
  evalq({
    BySexTstage <- function(..., segmented = FALSE, filter = NULL, logY = TRUE, na.rm = TRUE, check = TRUE) {
      if (segmented) {
        if (logY) {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexTstage.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexTstage.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexTstage.Rds"
        ))
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
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexThinStage.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexThinStage.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexThinStage.Rds"
        ))
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
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexTstageSite.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexTstageSite.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexTstageSite.Rds"
        ))
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
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexTstageType.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexTstageType.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexTstageType.Rds"
        ))
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
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexTstageRegion.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexTstageRegion.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexTstageRegion.Rds"
        ))
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
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-logY-BySexTstageSeason.Rds"
        ))
        } else {
          fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "Segmented-BySexTstageSeason.Rds"
        ))
        }
      } else {
        fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "AgeAdjusted-BySexTstageSeason.Rds"
        ))
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
          CodeMap$DATA_PATH, 
          "PlotData", 
          paste0(paste("APC-Data", fname0, sep = "-"
        ), ".Rds")))
    } else {
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "APC-Data.Rds"
        ))
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
          CodeMap$DATA_PATH, 
          "PlotData", 
          paste0(paste("APC-AgeSex", fname0, sep = "-"
        ), ".Rds")))
    } else {
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "APC-AgeSex.Rds"
        ))
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
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "APC-Model-Fit.Rds"
        ))
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
      fname <- here::here(file.path(
          CodeMap$DATA_PATH, 
          "PlotData", 
          "APC-Model-Effect.Rds"
        ))
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
    
    return(PlotData[])
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

