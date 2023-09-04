## -- Source scripts and functions --------
source(here::here(BASE_PATH, "Scripts/00-CodeMap.R"))
source(here::here(BASE_PATH, "Scripts/00-Functions.R"))

## ---- Paths ----
res_path <- here::here("Results/Table-Data")
out_path <- here::here("Output", "Tables")

## -- Load packages --------
Fn$quietly_load(c(
  "data.table", "ggplot2", "stringr", "purrr", "ggh4x",
  "gt", "gtsummary", "kableExtra"
))

## -- Options ----------------------------------------
options(knitr.kable.NA = '')

## -- Table environment ----
SelectedTables <- new.env()

## -- Inclusion and Exclusion of cases ----
evalq({
  CaseExclusion <- function() {
    data <- readRDS(here::here(res_path, "ExInclusion.rds"))
    data %>% 
      gt::gt() %>% 
      gt::sub_missing(gt::everything(), missing_text = "-") %>% 
      gt::fmt_integer(2:3, sep_mark = ",") %>% 
      gt::cols_label(
        Cases = "Cases removed",
        N = "Remaining cases"
      ) %>% 
      gt::tab_options(
        column_labels.font.weight = "bold"
      ) %>% 
      gt::tab_header("Exclusion and Inclusing of cases")
  }
}, SelectedTables)

## -- Table-1: Charactersitic Table ----
## NOTE: Need to make this independent of Data. 
evalq(
  {
    SummaryTable <- function() {
      tbl <- readRDS(here::here(res_path, "SummaryTable.rds"))
      tbl[["_data"]][["label"]] <- gsub(
        "Head and neck", "Head/neck", tbl[["_data"]][["label"]]
      )
      return(tbl)
    }
  },
  SelectedTables
)

## -- Alternative Table-1 ------------------------------
## NOTE: Need to make this independent of Data. 
evalq({
  AltTable <- function() {
    tbl <- readRDS(here::here(res_path, "SummaryTable.rds"))
    tbl[["_data"]][["label"]] <- gsub(
      "Head and neck", "Head/neck", tbl[["_data"]][["label"]]
    )
    return(tbl)
  }
}, SelectedTables)

## -- Sensetivity analysis: Count Comparison ------------------------------
evalq({
  CountComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    #// Following data only works for group "YearCat" and by = "Tstage"
    count_comparison <- readRDS(
      here::here(res_path, "SensitivityCount.rds")
    )
    # count_comparison <- TableData$CountComparison(group = group, by = by, overall_label = overall_label)
    model_footnote <- attr(count_comparison, "footnote")
    n_imp <- attr(count_comparison, "imp")
    group_vars <- setdiff(names(count_comparison), c("Imp", "N", "Prop"))
    col_order <- c(
      group_vars,
      CJ(variable = paste("Model", LETTERS[1:4]), value = c("n", "%")) %>% 
        .[, glue::glue_data(.SD, "{variable}_{rev(value)}")]
    )
    
    out <- count_comparison %>% 
      copy() %>% 
      setnames(c("N", "Prop"), c("n", "%")) %>% 
      dcast.data.table(
        reformulate("Imp", paste(group_vars, collapse = " + ")), 
        value.var = c("n", "%")
      ) %>% 
      setnames(names(.), stringr::str_replace(names(.), "(.+)_(.+)", "\\2_\\1")) %>%
      .[, c(group_vars) := lapply(.SD, stringr::str_replace, "-", "\U2013"),
        .SDcols = group_vars] %>% 
      setcolorder(col_order)
    if (by == "Tstage") {
      out[, Tstage := CodeMap$get_Tstage_label(Tstage)]
    }
    if (!is.null(group)) {
      if (group == "YearCat") {
        out <- out %>% 
          .[, YearCat := str_replace(YearCat, "(^1983)(.*)(2019$)", "\\1\\2\\3 (overall)")] %>% 
          .[, YearCat := paste("Year of diagnosis:", YearCat)]
      }
    }
    if (!is.null(group)) {
      source_note <- md(glue::glue(
        "Multiple imputation performed separately for each time-period",
        "({knitr::combine_words(count_comparison[, unique(get(group))][-1])})",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    } else {
      source_note <- md(glue::glue(
        "Multiple imputation",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    }
      ret <- out %>%  
        gt::gt(rowname_col = "Tstage", groupname_col = group) %>% 
        gt::row_group_order(if (is.null(group)) character(0) else out[, unique(get(group))]) %>% 
        gt::sub_missing(everything(), missing_text = "") %>% 
        gt::fmt_number(gt::ends_with("n"), decimals = 0) %>% 
        gt::tab_spanner_delim("_") %>% 
        gt::tab_stubhead("T category") %>% 
        gt::text_transform(
          locations = gt::cells_body(), 
          fn = function(x) str_remove(x, "%")
        ) %>% 
        gt::tab_style(
          style = cell_text(align = "right"),
          locations = gt::cells_stubhead()
        ) %>% 
        gt::tab_style(
          style = cell_text(align = "right"),
          locations = gt::cells_stub()
        ) %>% 
        gt::tab_options(
          data_row.padding = "1px",
          row_group.font.weight = "bold",
          column_labels.font.weight = "bold",
          source_notes.padding = "15px",
          table.border.bottom.style = "none"
        )
      for (m in names(model_footnote)) {
        ret <- gt::tab_footnote(
          ret, model_footnote[[m]], 
          gt::cells_column_spanners(gt::contains(m))
        ) %>% gt::opt_footnote_marks("numbers")
      }
        # gt::tab_footnote(
        #   footnote = model_footnote,
        #   locations = cells_column_spanners()
        # )
        # gt::tab_source_note(source_note)
    
    return(ret)
  }
}, SelectedTables)
## -- Sensetivity analysis: Count Comparison by Type -----------------
evalq({
  CountComparisonByType <- function(group = "YearCat", overall_label = "1983-2019") {
    #// Following data only works for group "YearCat" and by = "Tstage"
    count_comparison <- readRDS(
      here::here(res_path, "SensitivityCountByType.rds")
    )
    # count_comparison <- TableData$CountComparison(group = group, by = by, overall_label = overall_label)
    model_footnote <- attr(count_comparison, "footnote")
    n_imp <- attr(count_comparison, "imp")
    group_vars <- setdiff(names(count_comparison), c("Imp", "N", "Prop"))
    col_order <- c(
      group_vars,
      CJ(variable = paste("Model", LETTERS[1:4]), value = c("n", "%")) %>% 
        .[, glue::glue_data(.SD, "{variable}_{rev(value)}")]
    )
    
    out <- count_comparison %>% 
      copy() %>% 
      setnames(c("N", "Prop"), c("n", "%")) %>% 
      dcast.data.table(
        reformulate("Imp", paste(group_vars, collapse = " + ")), 
        value.var = c("n", "%")
      ) %>% 
      setnames(names(.), stringr::str_replace(names(.), "(.+)_(.+)", "\\2_\\1")) %>%
      .[, c(group_vars) := lapply(.SD, stringr::str_replace, "-", "\U2013"),
        .SDcols = group_vars] %>% 
      setcolorder(col_order)
    if (!is.null(group)) {
      if (group == "YearCat") {
        out <- out %>% 
          .[, YearCat := str_replace(YearCat, "(^1983)(.*)(2019$)", "\\1\\2\\3 (overall)")] %>% 
          .[, YearCat := paste("Year of diagnosis:", YearCat)]
      }
    }
    if (!is.null(group)) {
      source_note <- md(glue::glue(
        "Multiple imputation performed separately for each time-period",
        "({knitr::combine_words(count_comparison[, unique(get(group))][-1])})",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    } else {
      source_note <- md(glue::glue(
        "Multiple imputation",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    }
      ret <- out %>%  
        gt::gt(rowname_col = "MelanomaType", groupname_col = group) %>% 
        gt::row_group_order(if (is.null(group)) character(0) else out[, unique(get(group))]) %>% 
        gt::sub_missing(everything(), missing_text = "") %>% 
        gt::fmt_number(gt::ends_with("n"), decimals = 0) %>% 
        gt::tab_spanner_delim("_") %>% 
        gt::text_transform(
          locations = gt::cells_body(), 
          fn = function(x) str_remove(x, "%")
        ) %>% 
        gt::tab_style(
          style = cell_text(align = "right"),
          locations = gt::cells_stubhead()
        ) %>% 
        gt::tab_style(
          style = cell_text(align = "right"),
          locations = gt::cells_stub()
        ) %>% 
        gt::tab_options(
          data_row.padding = "1px",
          row_group.font.weight = "bold",
          column_labels.font.weight = "bold",
          source_notes.padding = "15px",
          table.border.bottom.style = "none"
        )
      for (m in names(model_footnote)) {
        ret <- gt::tab_footnote(
          ret, model_footnote[[m]], 
          gt::cells_column_spanners(gt::contains(m))
        ) %>% gt::opt_footnote_marks("numbers")
      }
        # gt::tab_footnote(
        #   footnote = model_footnote,
        #   locations = cells_column_spanners()
        # )
        # gt::tab_source_note(source_note)
    
    return(ret)
  }
}, SelectedTables)
## -- Sensetivity analysis: Survival Comparison ------------------------------
evalq({
  SurvComparison <- function(group = "YearCat", by = "Tstage", overall_label = "1983-2019") {
    require(gt)

    #// Following data only works for group "YearCat" and by = "Tstage"
    hr_comparison <- readRDS(
      here::here(res_path, "SensitivityHR.rds")
    )
    # hr_comparison <- TableData$SurvComparison(group = group, by = by, overall_label = overall_label)
    model_footnote <- attr(hr_comparison, "footnote")
    n_imp <- attr(hr_comparison, "imp")
    
    tcat_lbl <- hr_comparison[, unique(Term)]
    
    col_order <- c(
      c(group, "Term"),
      CJ(variable = paste("Model", LETTERS[1:4]), value = c("HR", "95% CI")) %>%
        .[, glue::glue_data(.SD, "{variable}_{rev(value)}")]
    )
    
    if (!is.null(group)) {
      source_note <- md(glue::glue(
        "Multiple imputation performed separately for each time-period",
        "({knitr::combine_words(hr_comparison[, unique(get(group))][-1])})",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    } else {
      source_note <- md(glue::glue(
        "Multiple imputation performed",
        "with {n_imp} imputed datasets, using age at diagnosis, sex,",
        "vital status (per 31 December 2019), survival (<1, 1-5, 5-10 and ≥10 years",
        "from melanoma diagnosis to death, emigration or 31 December 2019)",
        "residential region, anatomic site,",
        "melanoma subtype, and cumulative hazard rate (Nelson-Aalen estimator)",
        .sep = " "
      ))
    }
    
    if (is.null(group)) {
      out <- hr_comparison
    } else {
      if (group == "YearCat") {
        out <- copy(hr_comparison)
        out[, YearCat := str_replace(YearCat, "(^1983)(.*)(2019$)", "\\1\\2\\3 (overall)")]
        out[, YearCat := paste("Year of diagnosis:", YearCat)]
      } else {
        out <- hr_comparison
      }
    }

    if (!is.null(group)) {
      out[, c(group) := lapply(.SD, stringr::str_replace, "-", "\U2013"), .SDcols = c(group)]
    }    

    ret <- out %>% 
      setnames("CI", "95% CI", skip_absent = TRUE) %>% 
      dcast.data.table(
        reformulate("Model", paste(c(group, "Term"), collapse = " + ")), 
        value.var = c("HR", "95% CI")
      ) %>% 
      setnames(names(.), stringr::str_replace(
        names(.), "(.+)_(.+)", "\\2_\\1"
      ))


    ret[, Term := forcats::fct_relevel(
      Term,
      CodeMap$factor_map(key = by, missing_text = "Unspecified")
    )]
    if (by == "Tstage") {
      ret[, Term := forcats::lvls_revalue(Term, CodeMap$get_Tstage_label(levels(Term)))]
    }
    setcolorder(ret, col_order)
    setorderv(ret, col_order[seq(which(col_order == "Term"))])

    ret <- ret %>% 
      gt::gt(rowname_col = "Term",  groupname_col = group) %>% 
      gt::row_group_order(if (is.null(group)) character(0) else out[, unique(get(group))]) %>% 
      gt::sub_missing(everything(), missing_text = "") %>% 
      gt::tab_spanner_delim("_") %>% 
      gt::tab_stubhead(if (by == "Tstage") "T category" else by) %>% 
      gt::tab_style(
        style = cell_text(align = "right"),
        locations = gt::cells_stubhead()
      ) %>% 
      gt::tab_style(
        style = cell_text(align = "right"),
        locations = gt::cells_stub()
      ) %>% 
      gt::cols_align(align = "center", starts_with("Model")) %>% 
      gt::tab_options(
        data_row.padding = "1px",
        row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        source_notes.padding = "15px",
        table.border.bottom.style = "none"
      )
      # gt::tab_source_note(source_note)
      for (m in names(model_footnote)) {
        ret <- gt::tab_footnote(
          ret, model_footnote[[m]], 
          gt::cells_column_spanners(gt::contains(m))
        ) %>% gt::opt_footnote_marks("numbers")
      }
    return(ret)
  }
}, SelectedTables)

## -- (A)APC table ----
evalq({
  AAPC <- new.env()
  evalq({
    TableAPC <- function(data, logY = TRUE, which = c("Pooled", "Complete", "all"),
                         filter = NULL, row_var = NULL, group_var = NULL, ci = TRUE,
                         split_var = NULL, type = c("gt", "kable"), add_group_count = TRUE) {
      which <- match.arg(which)
      type <- match.arg(type)
      if (which != "all") {
        data <- data[Imp == which]
      }
      
      if (!is.null(filter)) {
        data <- data[eval(parse(text = filter))]
      }
      
      rn_col <- row_var
      grp_col <- group_var
      if (which == "all") split_var <- "Imp"
      
      data <- data[, .SD[, which(colSums(is.na(.SD)) < .N), with = FALSE]]
      if ("Tstage" %in% names(data)) {
        data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
      }
      
      fid <- which(map_lgl(data, is.factor))
      data[, c(fid) := lapply(.SD, forcats::fct_na_value_to_level, "Unspecified"), 
           .SDcols = fid]
      
      render_table <- function(tbl, type = c("gt", "kable"), ci = ci) {
        tbl <- copy(tbl)
        type <- match.arg(type)
        all_groups <- intersect(c("Imp", split_var, group_var, row_var), names(tbl))
        if (length(all_groups) > 0) setkeyv(tbl, all_groups)
        tbl[, grep("Period", names(tbl)) := lapply(
          .SD, stringr::str_replace, 
          "(\\d)-(\\d)", "\\1\U2013\\2"
        ), .SDcols = grep("Period", names(tbl))]
        if (type == "gt") {
          if (add_group_count & !is.null(group_var)) {
            lbl <- tbl[, lapply(.SD, unique), by = all_groups, .SDcols = c("N", "imp")] %>% 
              .[, .(N = sum(N), imp = unique(imp)), by = group_var] %>% 
              .[, .(Label = round(mean(N/imp))), by = group_var]
            tbl[, Group := interaction(.SD[, rev(group_var), with = FALSE], sep = " - ")]
            tbl <- merge.data.table(tbl, lbl, by = group_var, all.x = TRUE)
            tbl[, GroupLabel := glue::glue_data(.SD, "{Group} (n={format(Label, big.mark = ',')})")]
            tbl <- tbl[order(Group)]
            tbl[, c(group_var, "Label", "Group", "N", "imp") := NULL]
            group_var <- "GroupLabel"
          } else {
            tbl[, N := round(N/imp)]
          }
          setnames(tbl, "Overall_APC", paste0("1983\U2013", "2019_AAPC"), skip_absent = TRUE)
          
          # Tue May  2 16:24:55 2023 ------------------------------
          #| NOTE: In the last revision, we wanted to write Trend -> Period and Period -> Years
          setnames(tbl, \(x) gsub("Trend", "Period", gsub("_Period", "_Years", x)))
          
          out <-  tbl %>% 
            gt::gt(rowname_col = rn_col, groupname_col = group_var) %>%
            gt::sub_missing(everything(), missing_text = "--") %>%
            gt::tab_spanner_delim("_") %>%
            gt::cols_move_to_end(contains("AAPC"))

    	  if (ci) {
            out <- out %>% 
							gt::text_transform(
                locations = gt::cells_column_labels(), 
                fn = \(x) str_replace(x, "APC", "APC (95% CI)")
              )
						}

    	  out <- out %>%
            gt::tab_options(
              table.width = "100%",
              data_row.padding = "1px",
              row_group.font.weight = "bold",
              column_labels.font.weight = "bold",
              column_labels.padding = "2px",
              row_group.padding = "1px"
            ) %>% 
            gt::tab_source_note(
              "Period refers to the segments separated by the join-points."
            ) %>%
            # gt::tab_footnote(
            #   "APC: Annual percentage change; CI: Confidence interval", 
            #   gt::cells_column_labels(gt::matches("^T.*_APC"))
            # ) %>% 
            # gt::tab_footnote(
            #   "AAPC: Average (Estimated) annual percentage change", 
            #   gt::cells_column_labels(gt::matches("AAPC"))
            # ) %>% 
            # gt::tab_stubhead(label = tbl[, unique(Imp)]) %>% 
            gt::cols_hide(gt::starts_with("Imp"))
          if (tbl[, unique(Imp)] == "Pooled") {
            out <- out %>% gt::tab_footnote(
              glue::glue("Average number of cases over {data[Imp == 'Pooled', unique(imp)]} imputations"),
              gt::cells_row_groups()
            ) %>% gt::tab_options(table.border.bottom.style = "none")
          }
        } else if (type == "kable") {
          if (length(group_var) > 1) {
            tbl_group <- tbl[, xtabs(
              reformulate(group_var[1]), 
              drop.unused.levels = TRUE,
              data = .SD
            )]
            tbl <- tbl[, group_var[1] := NULL]
          }
          if (which == "all" & is.null(split_var)) {
            tbl_group <- tbl[, xtabs(~Imp)]
            tbl <- tbl[, Imp := NULL]
          }
          
          tbl <- tbl %>% 
            setcolorder(intersect(all_groups, names(tbl))) %>% 
            setnames(names(.), str_replace(names(.), "Overall_APC", "AAPC")) %>% 
            dplyr::select(-starts_with("Imp")) %>% 
            dplyr::relocate(AAPC, .after = last_col())
          
          # setnames(
          #   tbl, names(tbl),
          #   str_replace(
          #     names(tbl), "_APC", 
          #     paste0("_APC", footnote_marker_alphabet(1, double_escape = TRUE))
          #   )
          # )
          # setnames(
          #   tbl, names(tbl),
          #   str_replace(
          #     names(tbl), "AAPC", 
          #     paste0("AAPC", footnote_marker_alphabet(2, double_escape = TRUE))
          #   )
          # )
          # 
          
          out <- tbl %>% kbl(align = "c", escape = FALSE)
          
          if (length(group_var) > 1 | which == "all") {
            out <- out %>% group_rows(index = tbl_group)
          }
          
          if (!is.null(group_var)) {
            
            out <- out %>% 
              collapse_rows(which(names(tbl) %in% group_var))
          }
          
          out <- out %>% 
            header_separate("_") %>% 
            kable_paper(html_font = "Arial") %>% 
            footnote(
              alphabet = c(
                "Annual percentage change",
                "Average annual percentage change"
              )
            )
          out_ <- str_replace_all(out, "<th>APC</th>", "<th>APC<sup>a</sup></th>")
          out_ <- str_replace_all(out_, "<th>AAPC</th>", "<th>AAPC<sup>b</sup></th>")
          class(out_) <- class(out)
          out <- out_
        }
        return(out)
      }
      
      if (which == "all" & !is.null(split_var)) {
        out <- data %>% 
          split(by = split_var) %>% 
          map(render_table, type = type, ci = ci)
      } else {
        out <- render_table(data, type = type, ci = ci)
      }
      
      return(out)
    }
  }, AAPC)
  
  ## -- By Sex ----
  evalq({
    BySex <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                      filter = NULL, row_var = NULL, group_var = NULL, 
                      type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)
      out <- readRDS(here::here(res_path, "AAPC-BySex.rds"))
      # out <- TableData$AAPC$BySex(logY = logY, ...)
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, type = type, ...)
    }
  }, AAPC)
  
  ## -- By Tstage ----
  evalq({
    ByTstage <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                         filter = NULL, row_var = NULL, group_var = NULL, 
                         type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)
      out <- readRDS(here::here(res_path, "AAPC-ByTstage.rds"))
      # out <- TableData$AAPC$ByTstage(logY = logY, ...)
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, type = type, ...)
    }
  }, AAPC)
  
  ## -- By Sex and T-category ----
  evalq({
    BySexTstage <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                            filter = NULL, row_var = NULL, group_var = NULL,
                            type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)
      out <- readRDS(here::here(res_path, "AAPC-BySexTstage.rds"))
      # out <- TableData$AAPC$BySexTstage(logY = logY, ...)
      if (is.null(row_var)) row_var <- "Tstage"
      if (is.null(group_var)) group_var <- "Sex"
      
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, type = type, ...)
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Anatomic Site ----
  evalq({
    BySexTstageSite <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                                filter = NULL, row_var = NULL, group_var = NULL, 
                                split_var = NULL, type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)

      out <- readRDS(here::here(res_path, "AAPC-BySexTstageSite.rds"))
      # out <- TableData$AAPC$BySexTstageSite(..., logY = logY)

      if (is.null(row_var)) row_var <- "Tstage"
      if (is.null(group_var)) group_var <- c("Sex", "AnatomicSite")
      if (is.null(split_var)) split_var <- "Imp"
      levels(out$AnatomicSite)[1] <- "Head/neck"
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, 
               split_var = split_var, type = type, ...)
    }
  }, AAPC)
  
  ## -- By Sex, T-category and Melanoma Type ----
  evalq({
    BySexTstageType <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                                filter = NULL, row_var = NULL, group_var = NULL,
                                split_var = NULL, type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)

      out <- readRDS(here::here(res_path, "AAPC-BySexTstageType.rds"))
      # out <- TableData$AAPC$BySexTstageType(..., logY = logY)
      
      if (is.null(row_var)) row_var <- "Tstage"
      if (is.null(group_var)) group_var <- c("Sex", "MelanomaType")
      if (is.null(split_var)) split_var <- "Imp"
      
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, 
               split_var = split_var, type = type, ...)
    }
  }, AAPC)
  ## -- By Sex, T-category and Health Region ----
  evalq({
    BySexTstageRegion <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                                filter = NULL, row_var = NULL, group_var = NULL,
                                split_var = NULL, type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)

      out <- readRDS(here::here(res_path, "AAPC-BySexTstageRegion.rds"))
      # out <- TableData$AAPC$BySexTstageRegion(..., logY = logY)
      out[, HealthRegion := CodeMap$factor_map(HealthRegion, "HealthRegion")]
      
      if (is.null(row_var)) row_var <- "Tstage"
      if (is.null(group_var)) group_var <- c("Sex", "HealthRegion")
      if (is.null(split_var)) split_var <- "Imp"
      
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, 
               split_var = split_var, type = type, ...)
    }
  }, AAPC)
  ## -- By Sex, T-category and Season ----
  evalq({
    BySexTstageSeason <- function(logY = TRUE, which = c("Pooled", "Complete", "all"), 
                                filter = NULL, row_var = NULL, group_var = NULL,
                                split_var = NULL, type = c("gt", "kable"), ...) {
      which <- match.arg(which)
      type <- match.arg(type)
      out <- readRDS(here::here(res_path, "AAPC-BySexTstageSeason.rds"))
      # out <- TableData$AAPC$BySexTstageSeason(..., logY = logY)
      out[, Season := CodeMap$factor_map(Season, "Season")]
      
      if (is.null(row_var)) row_var <- "Tstage"
      if (is.null(group_var)) group_var <- c("Sex", "Season")
      if (is.null(split_var)) split_var <- "Imp"
      
      TableAPC(out, logY = logY, which = which, filter = filter,
               row_var = row_var, group_var = group_var, 
               split_var = split_var, type = type, ...)
    }
  }, AAPC)
  
},
SelectedTables
)

## ---- Proportion and count table ----
evalq({
  PropTableImputed <- function() {
    dta <- readRDS(here::here("Results", "Plot-Data", "Counts.Rds"))

    ## -- Take only imputed cases
    dta <- dta[Imp != "0"]
    ## -- Remove other from Site and Type
    # dta <- dta[AnatomicSite != "Other" & MelanomaType != "Other"]

    ## -- Create year category
    dta[, YearCat := CodeMap$year_cat(DiagYear)]
    
    ## -- Sum cases by group
    dta <- dta[, .(N = sum(N)), by = .(
      Imp, Sex, Tstage, AnatomicSite, MelanomaType, YearCat
    )]

    ## -- Average number of cases in 30 imputed dataset ----
    dta <- dta[, .(N = mean(N)), keyby = .(
      Sex, Tstage, AnatomicSite, MelanomaType, YearCat
    )]
    keys <- key(dta)

    dta <- rbindlist(list(
      dta,
      dta[, .(N = sum(N)), by = setdiff(keys, c("AnatomicSite", "YearCat"))] %>%
      .[, c("AnatomicSite", "YearCat") := .("Overall", "1983-2019")]
    ), use.names = TRUE)


    ## -- Counts: Get margin sums by different categories ---
    # counts <- cube(dta, j = c(list(N = sum(N))), by = keys)

    ## ---- Counts: Label overall ----
    # counts[, c(keys) := lapply(.SD, forcats::fct_na_value_to_level, "Overall"), .SDcols = keys]

    ## -- Prop: Get margin sums by different categories ---
    # props <- cube(dta, j = c(list(Prop = prop.table(N))), by = keys)

    ## ---- Prop: Label overall ----
    # props[, c(keys) := lapply(.SD, forcats::fct_na_value_to_level, "Overall"), .SDcols = keys]
    dta[, Prop := prop.table(N), by = setdiff(keys, "MelanomaType")]

    ## -- Merge both counts and props ------
    # dta <- merge.data.table(counts, props, by = keys)

    ## -- Calculate proportion that sum to one for Tstage
    dta[, label := glue::glue_data(
      .SD, "{N} ({Prop})",
      .transformer = function(data, env) {
        if (data == "Prop") {
          scales::percent(get(data, env), 0.1, suffix = "")
        } else {
          round(get(data, env))
        }
      }
    )]
    dta[, YearCat := gsub("-", "–", YearCat)]
    dta[, Tstage := CodeMap$get_Tstage_label(Tstage)]
    
    ## -- Make the table ready for print
    out <- dcast.data.table(
      dta,
      Sex + Tstage + MelanomaType ~ AnatomicSite + YearCat,
      value.var = "label"
    )
    
    setnames(out, function(x) gsub("Head and neck", "Head/neck", x))
    #levels(out$AnatomicSite)[1] <- "Head/neck"
    
    tbl <- copy(out) %>%
      gt::gt(
        rowname_col = "MelanomaType", 
        groupname_col = c("Sex", "Tstage"),
        row_group.sep = ": "
      ) %>%
      gt::tab_spanner_delim("_") %>%
      gt::summary_rows(
        fns = list(id = "total", label = gt::md("**Overall**, n")) ~ 
          sum(gsub("(\\d*).*", "\\1", .x) %>% as.numeric()),
        side = "top"
      ) %>%
      gt::tab_options(
        column_labels.font.weight = "bold",
        row_group.font.weight = "bold",
        summary_row.background.color = "#f0f0f0",
        data_row.padding = px(0),
        table.font.size = "smaller",
        row_group.padding = px(0),
        summary_row.padding = px(0)
      ) %>%
      gt::text_transform(
        fn = function(x) {
          gsub(".*\\((.*)\\)", "\\1", x)
        }
      ) %>%
      gt::text_transform(
        locations = cells_stub(),
        fn = function(x) {
          fcase(
            x == "Superficial spreading", "SSM, %",
            x == "Nodular", "NM, %",
            x == "Lentigo maligna", "LMM, %",
            x == "Other", "Other, %",
            default = NA_character_
          )
        }
      ) %>%
      gt::tab_style(
        locations = gt::cells_summary(),
        style = cell_text(weight = "bold")
      ) %>%
      gt::tab_style(
        locations = gt::cells_stub(),
        style = cell_text(align = "left", indent = px(15))
      ) %>%
      # gt::tab_stubhead(gt::md("Anatomic site </br> ↳ T category")) %>%
      gt::tab_source_note(md(paste(
        "_SSM_: Superficial spreading melanoma",
        "_NM_: Nodular melanoma",
        "_LMM_: Lentigo malagna melanoma",
        sep = ", "
      ))) %>%
      gt::cols_hide(columns = gt::starts_with("Other"))
    
    
    return(list(data = dta, table = tbl))

  }
  PropTableComplete <- function() {
    dta <- readRDS(here::here("Results", "Plot-Data", "Counts.Rds"))

    ## -- Take only imputed cases
    dta <- dta[Imp == "0"]
    ## -- Remove other from Site and Type
    # dta <- dta[AnatomicSite != "Other" & MelanomaType != "Other"]

    ## -- Remove NA to Unspecified
    dta <- dta[!is.na(Tstage) & !is.na(AnatomicSite) & !is.na(MelanomaType)]

    ## -- Create year category
    dta[, YearCat := CodeMap$year_cat(DiagYear)]
    
    ## -- Sum cases by group
    dta <- dta[, .(N = sum(N)), keyby = .(
      Sex, Tstage, AnatomicSite, MelanomaType, YearCat
    )]
    keys <- key(dta)

    dta <- rbindlist(list(
      dta,
      dta[, .(N = sum(N)), by = setdiff(keys, c("AnatomicSite", "YearCat"))] %>%
      .[, c("AnatomicSite", "YearCat") := .("Overall", "1983-2019")]
    ), use.names = TRUE)


    ## -- Counts: Get margin sums by different categories ---
    # counts <- cube(dta, j = c(list(N = sum(N))), by = keys)

    ## ---- Counts: Label overall ----
    # counts[, c(keys) := lapply(.SD, forcats::fct_na_value_to_level, "Overall"), .SDcols = keys]

    ## -- Prop: Get margin sums by different categories ---
    # props <- cube(dta, j = c(list(Prop = prop.table(N))), by = keys)

    ## ---- Prop: Label overall ----
    # props[, c(keys) := lapply(.SD, forcats::fct_na_value_to_level, "Overall"), .SDcols = keys]
    dta[, Prop := prop.table(N), by = setdiff(keys, "MelanomaType")]

    ## -- Merge both counts and props ------
    # dta <- merge.data.table(counts, props, by = keys)

    ## -- Calculate proportion that sum to one for Tstage
    dta[, label := glue::glue_data(
      .SD, "{N} ({Prop})",
      .transformer = function(data, env) {
        if (data == "Prop") {
          scales::percent(get(data, env), 0.1, suffix = "")
        } else {
          round(get(data, env))
        }
      }
    )]
    dta[, YearCat := gsub("-", "–", YearCat)]
    dta[, Tstage := CodeMap$get_Tstage_label(Tstage)]
    
    ## -- Make the table ready for print
    out <- dcast.data.table(
      dta,
      Sex + Tstage + MelanomaType ~ AnatomicSite + YearCat,
      value.var = "label"
    )
    
    setnames(out, function(x) gsub("Head and neck", "Head/neck", x))
    #levels(out$AnatomicSite)[1] <- "Head/neck"
    
    tbl <- copy(out) %>%
      gt::gt(
        rowname_col = "MelanomaType", 
        groupname_col = c("Sex", "Tstage"),
        row_group.sep = ": "
      ) %>%
      gt::tab_spanner_delim("_") %>%
      gt::summary_rows(
        fns = list(id = "total", label = gt::md("**Overall**, n")) ~ 
          sum(gsub("(\\d*).*", "\\1", .x) %>% as.numeric()),
        side = "top"
      ) %>%
      gt::tab_options(
        column_labels.font.weight = "bold",
        row_group.font.weight = "bold",
        summary_row.background.color = "#f0f0f0",
        data_row.padding = px(0),
        table.font.size = "smaller",
        row_group.padding = px(0),
        summary_row.padding = px(0)
      ) %>%
      gt::text_transform(
        fn = function(x) {
          gsub(".*\\((.*)\\)", "\\1", x)
        }
      ) %>%
      gt::text_transform(
        locations = cells_stub(),
        fn = function(x) {
          fcase(
            x == "Superficial spreading", "SSM, %",
            x == "Nodular", "NM, %",
            x == "Lentigo maligna", "LMM, %",
            x == "Other", "Other, %",
            default = "Unspecified, %"
          )
        }
      ) %>%
      gt::tab_style(
        locations = gt::cells_summary(),
        style = cell_text(weight = "bold")
      ) %>%
      gt::tab_style(
        locations = gt::cells_stub(),
        style = cell_text(align = "left", indent = px(15))
      ) %>%
      # gt::tab_stubhead(gt::md("Anatomic site </br> ↳ T category")) %>%
      gt::tab_source_note(md(paste(
        "_SSM_: Superficial spreading melanoma",
        "_NM_: Nodular melanoma",
        "_LMM_: Lentigo malagna melanoma",
        sep = ", "
      ))) %>%
      gt::cols_hide(columns = gt::starts_with("Other"))
    
    
    return(list(data = dta, table = tbl))

  }


}, SelectedTables)

## ---- Export tables to HTML ----
cat("Saving: case-exclusion.html\n")
case_exclusion <- SelectedTables$CaseExclusion()
gt::gtsave(case_exclusion, "case-exclusion.html", path = out_path)

cat("Saving: count-comparison.html\n")
count_comparison <- SelectedTables$CountComparison()
gt::gtsave(count_comparison, "count-comparison.html", path = out_path)

cat("Saving: count-comparison-by-type.html\n")
count_comparison_by_type <- SelectedTables$CountComparisonByType()
gt::gtsave(count_comparison_by_type, "count-comparison-by-type.html", path = out_path)

cat("Saving: surv-comparison.html\n")
surv_comparison <- SelectedTables$SurvComparison()
gt::gtsave(surv_comparison, "surv-comparison.html", path = out_path)

cat("Saving: summary-table.html\n")
summary_table <- SelectedTables$SummaryTable()
gt::gtsave(summary_table, "summary-table.html", path = out_path)

cat("Saving: alt-table.html\n")
alt_table <- SelectedTables$AltTable()
gt::gtsave(alt_table, "alt-table.html", path = out_path)

fnames <- ls(SelectedTables$AAPC, pattern = "^BySex.+")
for (fname in fnames) {
  cat(glue::glue("Saving: AAPC-{fname}.html"), "\n")
  tbl <- SelectedTables[["AAPC"]][[fname]]
  gt::gtsave(
    tbl(),
    filename = glue::glue("AAPC-{fname}.html"),
    path = out_path
  )
}
cat("Saving: prop-table-complete.html\n")
prop_table_complete <- SelectedTables$PropTableComplete()
gt::gtsave(prop_table_complete$table, "prop-table-complete.html", path = out_path)

cat("Saving: prop-table-imputed.html\n")
prop_table_imputed <- SelectedTables$PropTableImputed()
gt::gtsave(prop_table_imputed$table, "prop-table-imputed.html", path = out_path)