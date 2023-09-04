Fn <- new.env()
Pn <- new.env()

## ---- Utility Functions ----
evalq({
  #' Quietly load packages suppressing any startup messages, 
  #' warnings and other messages
  #'
  #' @param pkgs A character of package name to load
  #'
  #' @return NULL
  #' @export
  #'
  #' @examples
  quietly_load <- function(pkgs) {
    for (pkg in pkgs) {
      suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(
            library(pkg, character.only = TRUE, quietly = TRUE)
          )
        )
      )
    }
    on.exit({
      rm(pkg)
    })
  }
}, Fn) # quietly load the packages suppressing the messages
evalq({
  camel2snake <- function(x) {
    s <- sub("(.)([A-Z][a-z]+)", "\\1_\\2", x)
    s <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", s)
    tolower(s)
  }
}, Fn) # Convert string of camelCase to snake_case
evalq({
  #' Read and return and RDS file if exists
  #' @details Read and return the contents of an RDS file if the file
  #' exits else run an expression. Useful when running time-consuming 
  #' expression
  #'
  #' @param rds_path File path to the Rds file
  #' @param expression Expression to run if the file is not found
  #'
  #' @return the content of the Rds file or the return from the expression
  #' @export
  #'
  #' @examples
  load_or_run <- function(rds_path, expression) {
    if (file.exists(rds_path)) {
      readRDS(rds_path)
    } else {
      eval(expression, envir = parent.frame())
    }
  }
}, Fn) # Load the rds file if exists else run an expression
evalq({
  diff_month <- function(date1, date2) {
    (year(date1) - year(date2)) * 12 + (month(date1) - month(date2))
  }
}, Fn) # Find the difference between two dates in months
evalq({
   diff_month2 <- function(date1, date2, unit = c("days", "weeks", "months", "years")) {
     require(lubridate)
     unit <- match.arg(unit)    
     ufn <- switch (
       unit,
       days = ddays(1),
       weeks = dweeks(1),
       months = dmonths(1),
       years = dyears(1)
     )
     (date1 - date2) / ufn
   }
}, Fn) # Find the difference between two dates in months using lubridate (more exact)
evalq({
  format_percent <- function(x, dec = 2) {
    if (is.numeric(x)) {
      out <- scales::percent(x, accuracy = 1/10^dec)
    } else {
      x <- gsub("[()[:space:]]", "", x)
      out <- stringr::str_split(x, ",|-", simplify = TRUE) %>% 
        apply(1:2, as.numeric) %>% 
        apply(1:2, scales::percent, accuracy = 1 / 10^dec) %>% 
        apply(1, paste, collapse = ", ") %>% 
        paste0("(", ., ")")
    }
    return(out)
  }
}, Fn) # Format a text as percentage (also works for range separated by , or -)
evalq({
  get_count_range <- function(data, var = "Tstage", show_range = FALSE) {
    if (is.data.frame(data)) {
      count <- data[, .N, by = var]
      count[, Prop := prop.table(N)]
    } else {
      count <- map_df(data, ~.x[, .N, by = var], .id = "Imp") %>% copy()
      count[, Prop := prop.table(N), by = Imp]
      count <- count[, .(
        N = round(mean(N)),
        Range = paste(range(N), collapse = "-"),
        PropRange = paste(range(Prop), collapse = "-")
      ), by = var]
      count[, Prop := prop.table(N)]
      if (!show_range) {
        count[, c("Range", "PropRange") := NULL]
      }
    }
    count[, grep("Prop", names(count)) := lapply(.SD, format_percent, dec = 1), 
          .SDcols = grep("Prop", names(count))]
    return(count[order(get(var))][])
  }
}, Fn) # Calculate count and range of count for data or list of data by given variable
evalq({
  count_by_group <- function(count_df, group, margin = NULL) {
    count_prop <- function(df) {
      df %>% 
        .[, .(N = sum(N)), by = group] %>% 
        .[, Prop := prop.table(N), by = margin]
    }
    if (is.data.frame(count_df)) {
      out <- count_prop(count_df)[, Imp := 0]
    } else {
      out <- map_df(count_df, count_prop, .id = "Imp")
    }
    return(out[])
  }
}, Fn) # Aggregate the count for certain set of group using count-data
evalq({
  list2array <- function(list, dnames.name = NULL) {
    l2a <- function(l) {
      do.call(abind::abind, append(l, list(rev.along = 0)))
    }
    while (is.list(list[[1]])) {
      list <- map(list, l2a)
    }
    out <- l2a(list)
    if (!is.null(dnames.name)) names(dimnames(out)) <- dnames.name
    return(out)
  }
}, Fn) # Convert a list to an array (nested list can be used)
evalq({
  tbl2vec <- function(tbl, .dimnames = dimnames(tbl), .dim = dim(tbl), ...) {
    if (is.vector(tbl)) return(tbl)
    vec_names <- expand.grid(.dimnames) %>% interaction(...) %>% as.character()
    vec <- as.vector(tbl)
    names(vec) <- vec_names
    return(vec)
  }
  label_vec <- function(vec, label_template = "{..2} (n={format(..1, big.mark = ',')})") {
    imap_chr(vec, ~glue::glue(label_template))
  }
}, Fn) # Useful function to create label from a vector
evalq({
  median_iqr <- function(x, digits = 1) {
    if (all(is.na(x) | is.null(x))) return(NULL)
    out <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE) %>%
      round(digits = digits)
    glue::glue_data(out, "{`50%`} ({`25%`}–{`75%`})") %>% as.character()
  }
}, Fn) # Gives median (iqr1, iqr2) for a numeric vector x
evalq({
  get_cohort <- function(period_range, age_range) {
    age_gap <- str_split_fixed(age_range[1], "-", 2) %>% as.numeric() %>% diff() %>% abs() %>% sum(1)
    period_mat <- str_split_fixed(period_range, "-", 2) %>% apply(1:2, as.numeric)
    age_mat <- str_split_fixed(age_range, "-|\\+", 2) %>% apply(1:2, as.numeric)
    age_mat[is.na(age_mat)] <- max(age_mat, na.rm = TRUE) + age_gap 
    out <- data.table(
      lower = period_mat[, 1] - age_mat[, 2], 
      upper = period_mat[, 2] - age_mat[, 1]
    )
    out[, range := paste(lower, upper, sep = "-")]
    return(out[])
  }
}, Fn) # Get Cohort range from period and age range
evalq({
  one_hot_encoding <- function(x, addx = TRUE, xlab = NULL) {
    if (is.numeric(x)) {
      message("x must be either character of factor variable")
      return(NULL)
    }
    out <- sapply(levels(x), function(k) as.integer(x == k))
    if ("data.table" %in% rownames(installed.packages())) {
      out <- as.data.table(out)
    }
    if (addx) {
      out <- cbind(x, out)
      if (!is.null(xlab)) {
        setnames(out, 1, xlab)
      }
    }
    return(out)
  }
}, Fn) # Get one-hot encoading from a factor variable

## ---- Summarizing and Aggregating Functions ----
evalq({
  gt_grouped_summary <- function(data, variables, group = NULL, overall = FALSE, pvalue = FALSE, caption = NULL, digits = NULL, ...) {
    require(gtsummary)
    require(purrr)
    vars <- if (!is.null(names(variables))) names(variables) else variables
    if (length(group) < 2 | is.null(group)) {
      tbl <- tbl_summary(
        data[, c(..vars, ..group)],
        by = group,
        label = as.list(variables),
        digits = digits,
        missing_text = "Unspecified"
      ) %>%
        modify_header(
          update = all_stat_cols() ~ ifelse(
            knitr::is_html_output(),
            "**{level}**<br>n={n}",
            "**{level}**\nn={n}"
          )
        )
      if (overall) tbl <- tbl %>% add_overall()
      if (pvalue) tbl <- tbl %>% add_p()
      tbl <- tbl %>%
        modify_footnote(update = everything() ~ NA) %>%
        # modify_footnote(update = label ~ "Median (IQR); n (%)") %>%
        bold_labels()
      return(tbl)
    } else {
      tbls <- lapply(unique(data[[group[1]]]), function(gvar) {
        if (is.null(digits)) {
          digits <- list(
              all_categorical() ~ c(0, 1), 
              any_of("Age") ~ c(0, 0),
              any_of(c("BreslowThickness", "Thickness")) ~ c(1, 1)
            )
        }
        tbl <- data[data[[group[1]]] == gvar, c(..vars, ..group[-1])] %>%
          tbl_summary(
            by = group[2],
            label = as.list(variables),
            digits = digits,
            missing_text = "Unspecified",
	    ...
          ) %>% modify_header(
            update = all_stat_cols() ~ "**{level}**<br>n={format(n, big.mark = ',')}"
          )
        if (overall) tbl <- tbl %>% add_overall(
          col_label = "**Overall**<br>n={format(N, big.mark = ',')}"
        )
        if (pvalue) tbl <- tbl %>% add_p()
        return(tbl)
      })
      names(tbls) <- unique(data[[group[1]]])
      tbl_lbls <- map_dbl(tbls, "N") %>% format(big.mark = ",")
      tbl <- tbl_merge(
        tbls,
        tab_spanner = glue::glue("**{names(tbl_lbls)}** (n={tbl_lbls})")
      ) %>%
        modify_footnote(update = everything() ~ NA) %>%
        # modify_footnote(update = label ~ "Median (IQR); n (%)") %>%
        bold_labels()
      return(tbl)
    }
  }
  add_margin <- function(data, margin_cols, measure_vars, fun_aggregate = sum) {
    dta <- data[, lapply(.SD, fun_aggregate),
                .SDcols = measure_vars,
                by = setdiff(names(data), c(margin_cols, measure_vars))
    ]
    dta[, (margin_cols) := "Sum"]
    setcolorder(dta, names(data))
    rbind(data, dta)
  }
  by_group <- function(data, group = "DiagYear", measure_var = "N") {
    measure_var <- fcase(
      "N" %in% names(data), "N",
      "Population" %in% names(data), "Population",
      default = measure_var
    )
    data[, lapply(.SD, sum), .SDcols = measure_var, by = group]
  }
  get_group_label <- function(data, groups) {
    out <- rbindlist(list(
      `Missing thickness` = data[is.na(Thickness), .N, by = groups],
      Complete = data[!is.na(Thickness), .N, by = groups]
    ), idcol = "State")
    
    na.omit(out) %>% 
      .[, .(Label = glue::glue_data(.SD, "{State}: {N}") %>% 
              glue::glue_collapse(sep = ", ")), 
        by = groups]
  }
  get_facet_label <- function(data, groups, margins = NULL, formatted = TRUE, ...) {
    if (!is.data.frame(data)) {
      out <- map(data, ~xtabs(reformulate(groups), data = .x))
      if (!is.null(margins)) out <- map(out, addmargins, margins)
      out <- map_df(out, as.data.table, .id = "Imp") %>% 
        .[, .(N = round(mean(N))), by = groups]
    } else {
      out <- xtabs(reformulate(groups), data = data)
      if (!is.null(margins)) out <- out %>% addmargins(margins)
      out <- out %>% as.data.table()
    }
    if (formatted) {
      out <- out %>% 
        .[, `names<-`(N, interaction(.SD, ...)), .SDcols = groups] %>% 
        imap_chr(~glue::glue("{..2} (n={..1})"))
      names(out) <- gsub("Sum", "Overall", names(out))
      out <- gsub("Sum", "Overall", out)
    }
    return(out)
  }
  count_cases <- function(case_data, group_by = NULL, margin = NULL, value_name = "Count", 
                          margin_label = "Overall", missing_label = NA) {
    require(purrr)
    case_data <- as.data.table(case_data)
    if (!is.null(group_by)) {
      out <- xtabs(reformulate(group_by), data = case_data, addNA = TRUE)
      if (!is.null(margin)) {
        margin_idx <- match(margin, group_by)
        out <- addmargins(out, margin_idx)
      }
      out <- as.data.table(out) %>% 
        setnames("N", value_name)
      if (!is.null(margin)) {
        out[, c(margin) := map(.SD, ~gsub("Sum", margin_label, .x)), .SDcols = margin]
      }
      if (!is.na(missing_label)) {
        out[, c(group_by) := map(.SD, ~`[<-`(.x, is.na(.x), missing_label)), .SDcols = group_by]
      }
    } else {
      out <- case_data[, .N]
    }
    attr(out, "env") <- mget(names(formals())[-1], sys.frame(sys.nframe()))
    class(out) <- append("CountData", class(out))
    return(out)
  }
  count_prop <- function(data, groups = NULL, prop_margin = NULL, ...) {
    count_data <- Fn$count_cases(data, group_by = groups, ...)
    setkeyv(count_data, groups) 
    if (is.null(groups)) return(count_data)
    prop_data <- copy(count_data)[, Prop := prop.table(Count), by = prop_margin]
    return(prop_data)
  }
}, Fn)

## ---- Breslow and Ulceration Related Functions ----
evalq({
  clean_mean <- function(string, split_pattern) {
    string %>%
      str_split(split_pattern) %>%
      map_dbl(
        ~ .x %>%
          str_remove("[[:alpha:]]+") %>%
          str_trim() %>%
          str_replace(",", ".") %>%
          str_extract(".+") %>%
          na.omit() %>%
          as.numeric() %>%
          mean(na.rm = TRUE)
      )
  }
  parse_range <- function(string) {
    str_vec <- string
    range_idx <- str_which(str_vec, CodeMap$breslow_pattern("range"))
    str_vec[range_idx] <- clean_mean(
      str_vec[range_idx],
      CodeMap$breslow_split_pattern("range")
    )
    return(str_vec)
  }
  parse_ineq <- function(string, x = 0.01) {
    str_vec <- string
    less_idx <- str_which(str_vec, CodeMap$breslow_pattern("less"))
    str_vec[less_idx] <- str_vec[less_idx] %>%
      str_remove_all("<") %>%
      str_replace(",", ".") %>%
      as.numeric() %>%
      `-`(x)
    
    greater_idx <- str_which(str_vec, CodeMap$breslow_pattern("greater"))
    str_vec[greater_idx] <- str_vec[greater_idx] %>%
      str_remove_all(">|[[:alpha:]*\\.]") %>%
      str_replace(",", ".") %>%
      as.numeric() %>%
      `+`(x)
    return(str_vec)
  }
  parse_multiple <- function(string) {
    str_vec <- string
    multiple_idx_1 <- str_which(str_vec, CodeMap$breslow_pattern("multiple"))
    starting_mellom <- str_which(str_vec, "Mellom")
    multiple_idx <- setdiff(multiple_idx_1, starting_mellom)
    str_vec[multiple_idx] <- str_vec[multiple_idx] %>%
      str_split(", |og") %>%
      map(str_remove, "[[:alpha:]\\s]*") %>%
      map(str_replace, ",", ".") %>%
      map(as.numeric) %>%
      map_dbl(max)
    return(str_vec)
  }
  parse_extra <- function(string) {
    str_vec <- string
    numeric_idx <- str_which(str_vec, CodeMap$breslow_pattern("numeric"))
    str_vec[numeric_idx] <- as.numeric(str_vec[numeric_idx])
    
    unknown_idx <- str_which(str_vec, CodeMap$breslow_pattern("unknown"))
    str_vec[unknown_idx] <- NA
    
    extra_idx <- str_which(str_vec, "\\d,,\\d")
    str_vec[extra_idx] <- str_replace(str_vec[extra_idx], ",,", ".")
    
    return(str_vec)
  }
  parse_breslow <- function(str_vec) {
    parsed_string <- str_vec %>%
      parse_range() %>%
      parse_ineq() %>%
      parse_multiple() %>%
      parse_extra() %>%
      as.numeric()
    
    return(parsed_string)
  }
  combine_breslow <- function(data) {
    breslow <- data[, grep("Tumour", names(data)), with = FALSE]
    ulceration <- data[, grep("Ulceration", names(data)), with = FALSE]
    bt1 <- breslow[["TumourThickness1"]]
    bt2 <- breslow[["TumourThickness2"]]
    bt <- breslow[["TumourThickness"]]
    u <- ulceration[["Ulceration"]]
    uy1 <- ulceration[["UlcerationYes1"]]
    uy2 <- ulceration[["UlcerationYes2"]]
    un1 <- ulceration[["UlcerationNO1"]]
    un2 <- ulceration[["UlcerationNO2"]]
    unr1 <- ulceration[["UlcerationNoReport1"]]
    unr2 <- ulceration[["UlcerationNoReport2"]]
    
    ts <- CodeMap$get_Tstage(bt)
    ts1 <- CodeMap$get_Tstage(bt1)
    ts2 <- CodeMap$get_Tstage(bt2)
    tss1 <- get_Tsubstage(bt1, uy1, un1, unr1)
    tss2 <- get_Tsubstage(bt2, uy2, un2, unr2)
    
    fcase(
      !is.na(bt) & (is.na(bt1) | is.na(bt2)), bt,
      is.na(bt) & is.na(bt1) & !is.na(bt2), bt2,
      is.na(bt) & !is.na(bt1) & is.na(bt2), bt1,
      is.na(bt) & !is.na(bt1) & !is.na(bt2),
      fifelse(tss1 > tss2, bt1, bt2),
      default = NA
    )
  }
  parse_ulceration <- function(ulceration) {
    ulceration <- ulceration %>%
      as.character() %>%
      str_replace(".m", "") %>%
      as.numeric() %>%
      as.logical()
    return(ulceration)
  }
  get_Tsubstage <- function(thickness, yes, no, missing) {
    tstage <- CodeMap$get_Tstage(thickness)
    fcase(
      # T1
      tstage == "T1" & thickness <= 0.8 & is.na(yes) & no & is.na(missing), "T1a",
      tstage == "T1" & thickness < 0.8 & yes & is.na(no) & is.na(missing), "T1b",
      tstage == "T1" & thickness %between% c(0.8, 1.0) & is.na(missing), "T1b",
      # tstage == "T1" & is.na(yes) & is.na(no) & missing, "T1",
      tstage == "T1" & missing, "T1",
      # T2
      tstage == "T2" & yes & is.na(no) & is.na(missing), "T2b",
      tstage == "T2" & is.na(yes) & no & is.na(missing), "T2a",
      # tstage == "T2" & is.na(yes) & is.na(no) & missing, "T2",
      tstage == "T2" & missing, "T2",
      # T3
      tstage == "T3" & yes & is.na(no) & is.na(missing), "T3b",
      tstage == "T3" & is.na(yes) & no & is.na(missing), "T3a",
      # tstage == "T3" & is.na(yes) & is.na(no) & missing, "T3",
      tstage == "T3" & missing, "T3",
      # T4
      tstage == "T4" & yes & is.na(no) & is.na(missing), "T4b",
      tstage == "T4" & is.na(yes) & no & is.na(missing), "T4a",
      # tstage == "T4" & is.na(yes) & is.na(no) & missing, "T4",
      tstage == "T4" & missing, "T4",
      default = NA_character_
    )
  }
  combine_ulceration <- function(data) {
    breslow <- data[, grep("Tumour", names(data)), with = FALSE]
    ulceration <- data[, grep("Ulceration", names(data)), with = FALSE]
    bt1 <- breslow[["TumourThickness1"]]
    bt2 <- breslow[["TumourThickness2"]]
    bt <- breslow[["TumourThickness"]]
    u <- ulceration[["Ulceration1"]]
    uy1 <- ulceration[["UlcerationYes1"]]
    uy2 <- ulceration[["UlcerationYes2"]]
    un1 <- ulceration[["UlcerationNO1"]]
    un2 <- ulceration[["UlcerationNO2"]]
    unr1 <- ulceration[["UlcerationNoReport1"]]
    unr2 <- ulceration[["UlcerationNoReport2"]]
    
    ts <- CodeMap$get_Tstage(bt)
    ts1 <- CodeMap$get_Tstage(bt1)
    ts2 <- CodeMap$get_Tstage(bt2)
    tss1 <- get_Tsubstage(bt1, uy1, un1, unr1)
    tss2 <- get_Tsubstage(bt2, uy2, un2, unr2)
    fcase(
      !is.na(u), fifelse(u, "Present", "Absent"),
      !is.na(bt1) & !is.na(bt2), fcase(
        tss1 >= tss2, fcase(
          !is.na(un1) & !is.na(unr1), "Absent",
          !is.na(uy1) & !is.na(unr1), "Present",
          !is.na(un1), "Absent",
          !is.na(uy1), "Present",
          !is.na(unr1), "Missing",
          default = NA_character_
        ),
        tss1 <= tss2, fcase(
          !is.na(un2) & !is.na(unr2), "Absent",
          !is.na(uy2) & !is.na(unr2), "Present",
          !is.na(un2), "Absent",
          !is.na(uy2), "Present",
          !is.na(unr2), "Missing",
          default = NA_character_
        ),
        default = NA_character_
      ),
      !is.na(bt1), fcase(uy1, "Present", un1, "Absent", unr1, "Missing"),
      !is.na(bt2), fcase(uy2, "Present", un2, "Absent", unr2, "Missing"),
      default = NA_character_
    )
  }
}, Fn) 

## ---- Age-Adjustment and Segmented Regression ----
evalq({
  segreg_get <- function(key) {
    get_psi <- function(mdl) {
      if ("segmented" %in% class(mdl)) {
        c(
          min(mdl$model[["DiagYear"]]),
          rep(unname(mdl$psi[, 2]), each = 2),
          max(mdl$model[["DiagYear"]])
        ) %>%
          split(rep(seq(1, length(.) / 2), each = 2)) %>%
          transpose() %>%
          as.data.table() %>%
          setnames(names(.), c("psi_left", "psi_right")) %>%
          cbind(Segment = as.character(1:nrow(.)), .) %>%
          .[, lapply(.SD, unlist), by = Segment] %>%
          rbind(data.table(
            Segment = "overall",
            psi_left = min(mdl$model[["DiagYear"]]),
            psi_right = max(mdl$model[["DiagYear"]])
          )) %>%
          .[, psi_right := ifelse(psi_right == 2019, 2019, psi_right - 1)]
      } else {
        data.table(
          Segment = "overall",
          psi_left = as.numeric(min(mdl$model[["DiagYear"]])),
          psi_right = as.numeric(max(mdl$model[["DiagYear"]]))
        )
      }
    }
    get_coef <- function(mdl) {
      if ("segmented" %in% class(mdl)) {
        cf <- coef(base::summary(mdl$orig_model))
        ci <- as.data.table(confint(mdl$orig_model))
      } else {
        cf <- coef(base::summary(mdl))
        ci <- as.data.table(confint(mdl))
      }
      cf %>%
        as.data.table(keep.rownames = "Type") %>%
        .[, !"Pr(>|t|)"] %>%
        .[, Type := c("Intercept", "Slope")] %>%
        setnames(3:4, c("Std.Error", "Tvalue")) %>%
        .[, c("Lower", "Upper") := ci] %>%
        cbind(Segment = "overall", .)
    }
    get_intercept <- function(mdl) {
      intercept(mdl)[["DiagYear"]] %>%
        as.data.table(keep.rownames = "Segment") %>%
        setnames("Est.", "Estimate") %>%
        .[, Type := "Intercept"] %>%
        .[, Segment := str_remove(Segment, "intercept")] %>%
        setcolorder(c("Segment", "Type"))
    }
    get_slope <- function(mdl) {
      slope(mdl)[["DiagYear"]] %>%
        as.data.table(keep.rownames = "Segment") %>%
        setnames(names(.), c("Segment", "Estimate", "Std.Error", "Tvalue", "Lower", "Upper")) %>%
        .[, Type := "Slope"] %>%
        .[, Segment := str_remove(Segment, "slope")] %>%
        setcolorder(c("Segment", "Type"))
    }
    get_summary <- function(mdl) {
      coef <- get_coef(mdl)
      if ("segmented" %in% class(mdl)) {
        intercept <- get_intercept(mdl)
        slope <- get_slope(mdl)
        psi <- get_psi(mdl)
        coef <- merge.data.table(
          rbindlist(list(intercept, slope, coef), fill = TRUE, use.names = TRUE),
          psi,
          all.x = TRUE,
          by = "Segment"
        )
      } else {
        coef[, c("psi_left", "psi_right") := NA_real_]
      }
      return(coef)
    }
    get_segments <- function(dta, mdl, logY = TRUE) {
      overall <- data.table(
        Segment = "overall",
        x = dta[, min(DiagYear)] %>% as.numeric(),
        xend = dta[, max(DiagYear)] %>% as.numeric()
      )
      overall[, y := predict(mdl, newdata = data.frame(DiagYear = x))]
      if (logY) overall[, y := exp(y)]
      overall[, yend := predict(mdl, newdata = data.frame(DiagYear = xend))]
      if (logY) overall[, yend := exp(yend)]
      if ("segmented" %in% class(mdl)) {
        psi <- get_psi(mdl)
        segments <- setnames(copy(psi), 2:3, c("x", "xend"))
        segments[, y := predict(mdl, newdata = data.frame(DiagYear = x)), by = Segment]
        if (logY) segments[, y := exp(y), by = Segment]
        segments[, yend := predict(mdl, newdata = data.frame(DiagYear = xend)), by = Segment]
        if (logY) segments[, yend := exp(yend), by = Segment]
        out <- rbind(segments, overall)[Segment != "overall"]
      } else {
        out <- overall
      }
      return(out)
    }
    get_aapc <- function(mdl, logY = TRUE) {
      if (is.null(mdl)) return(NULL)
      if ("segmented" %in% class(mdl)) {
        require(segmented)
        sgmt_slope <- slope(mdl, APC = logY)[[1]] %>%
          as.data.table(keep.rownames = "Type")
        if (!logY) setnames(sgmt_slope, 3, "St.Err")
        overall_slope <- as.data.table(t(aapc(mdl, exp.it = logY)))
        if (logY) overall_slope <- overall_slope * 100
        out <- rbind(
          sgmt_slope,
          overall_slope,
          fill = TRUE
        )
        out[, Segment := str_extract(Type, "\\d$")]
        out[, Type := NULL]
        out[is.na(Segment), Segment := "overall"]
        setcolorder(out, "Segment")
        if (!logY) {
          out[, 3:4 := NULL]
        }             
        setnames(out, 2:4, c("Estimate", "Lower", "Upper"))
        return(out)
      } else {
        out <- t(c(coef(mdl)[[2]], confint(mdl, parm = "DiagYear"))) %>%
          as.data.table() %>%
          setnames(1:3, c("Estimate", "Lower", "Upper")) %>%
          cbind(Segment = "overall", .)
        if (logY) {
          out[, 2:4 := lapply(.SD, function(x) (exp(x) - 1) * 100), .SDcols = 2:4]
        }
        return(out)
      }
    }
    get_spline <- function(dta, mdl, logY = TRUE) {
      rate_var <- all.vars(formula(mdl))[1]
      year_var <- all.vars(formula(mdl))[2]
      dta <- dta[dta[[rate_var]] > 0]
      if (is.null(mdl)) return(NULL)
      if ("segmented" %in% class(mdl)) {
        if (logY) {
          spl_mdl <- lm(log(dta[[rate_var]]) ~ splines::ns(dta[[year_var]], knots = mdl$psi[, "Est."]))
        } else {
          spl_mdl <- lm(dta[[rate_var]] ~ splines::ns(dta[[year_var]], knots = mdl$psi[, "Est."]))
        }
        spl_pred <- predict(spl_mdl, `colnames<-`(data.frame(dta[[year_var]]), year_var), interval = "confidence")
      } else {
        spl_pred <- predict(mdl, `colnames<-`(data.frame(dta[[year_var]]), year_var), interval = "confidence")
      }
      colnames(spl_pred) <- paste(setdiff(names(dta), c(year_var, "crude.rate", "N", "imp")), "spl", sep = "_")
      if (logY) spl_pred <- exp(spl_pred)
      return(cbind(dta, spl_pred))
    }
    get_fitted <- function(dta, mdl, logY = TRUE) {
      year_var <- all.vars(formula(mdl))[2]
      if ("segmented" %in% class(mdl)) {
        fit <- broken.line(mdl)
        if (logY) fit <- lapply(fit, exp)
        fit_mat <- do.call(cbind, fit)
        ci_mat <- fit_mat %*% Epi::ci.mat()
        colnames(ci_mat) <- c("fit", "fit_lwr", "fit_upr")
        fit_df <- cbind(fit_mat, ci_mat[, -1]) %>% 
          as.data.table(keep.rownames = "Year") %>%
          .[, c(year_var) := as.numeric(mdl$model[[year_var]])]
      } else {
        fit <- predict(mdl, se.fit = TRUE)[1:2]
        if (logY) fit <- lapply(fit, exp)
        fit_mat <- do.call(cbind, fit)
        ci_mat <- fit_mat %*% Epi::ci.mat()
        colnames(ci_mat) <- c("fit", "fit_lwr", "fit_upr")
        fit_df <- cbind(fit_mat, ci_mat[, -1]) %>% 
          as.data.table(keep.rownames = "Year") %>%
          .[, c(year_var) := as.numeric(mdl$model[[year_var]])]
      }
      out <- merge.data.table(dta, fit_df, by = year_var)
      return(out)
    }
    switch(key,
           psi = get_psi,
           summary = get_summary,
           segments = get_segments,
           aapc = get_aapc,
           spline = get_spline,
           fitted = get_fitted
    )
  }
  get_best_psi <- function(mdl, type = c("score", "davies"), control = seg.control(), logY = TRUE) {
    attach(attr(terms(mdl), ".Environment"))
    on.exit(detach(attr(terms(mdl), ".Environment")))
    
    Kmax <- 2
    alpha <- 0.05
    nomeX <- all.vars(formula(mdl))[2]
    if (length(nomeX) > 1 || any(is.na(nomeX))) {
      stop("I cannot determine the segmented variable")
    }
    seg.Z <- as.formula(paste("~", nomeX))
    
    type <- match.arg(type)
    
    alpha.adj <- alpha/Kmax
    dta <- model.frame(mdl) %>% as.data.table()
    if (logY) {
      dta[[1]] <- exp(dta[[1]])
      setnames(dta, names(dta), gsub("log\\((.*)\\)", "\\1", names(dta)))
    }
    mdl <- update(mdl, data = dta)
    
    p1 <- if (type == "score") {
      pscore.test(mdl, seg.Z, n.break = 2)$p.value
    } else {
      davies.test(mdl)$p.value
    }
    if (p1 > alpha.adj) {
      p2 <- if (type == "score") {
        pscore.test(mdl, seg.Z, n.break = 1)$p.value
      } else {
        p1
      }
      if (p2 > alpha.adj) {
        out <- mdl
      }
      else {
        out <- segmented(mdl, seg.Z, npsi = 1, control = control)
      }
    } else {
      o1 <- segmented(mdl, seg.Z, npsi = 1, control = control)
      
      if (type == "score") {
        p2 <- pscore.test(o1, seg.Z)$p.value
      }
      else {
        p2 <- davies.test(o1, seg.Z)$p.value
      }
      if (p2 > alpha.adj) {
        o1 <- segmented(mdl, seg.Z, npsi = 1, control = control)
        out <- o1
      }
      else {
        o2 <- segmented(mdl, seg.Z, npsi = 2, control = control)
        out <- o2
      }
    }
    n.psi.ok <- length(out$psi[, 2])
    x2 <- -2 * sum(log(c(p1, p2)))
    p <- 1 - pchisq(x2, df = 2 * 2)
    r <- list(pvalues = c(p1 = p1, p2 = p2, p = p), npsi = n.psi.ok)
    return(r)
  }
  check_psi <- function(s_mdl, gap = 3) {
    if ("segmented" %in% class(s_mdl)) {
      psi <- c(1983, s_mdl$psi[, "Est."], 2019)
      diff_psi <- diff(round(psi))
      if (sum(diff_psi <= gap)) {
        return(NULL)
      } else {
        return(s_mdl)
      }
    } else {
      return(NULL)
    }
  }
  fit_segmented <- function(dta, rate_var = "AdjRate", year_var = "DiagYear", logY = TRUE, gap = 5) {
    require(segmented)
    seg_control <- segmented::seg.control(digits = 1, alpha = 0.02)
    rate_var <- fcase(
      "adj.rate" %in% names(dta), "adj.rate",
      "est" %in% names(dta), "est",
      default = rate_var
    )
    year_var <- grep("[yY]ear", names(dta), value = TRUE)
    if (length(year_var) > 1) year_var <- year_var[1]
    dta <- dta[dta[[rate_var]] > 0]
    if (nrow(dta) <= 3) {
      return(NULL)
    }
    if (!logY) {
      frml <- reformulate(year_var, rate_var)
    } else {
      frml <- reformulate(year_var, paste0("log(", rate_var, ")"))
    }
    mdl <- lm(frml, data = dta, na.action = "na.exclude")
    mdl$call$formula <- frml
    
    filter_good_fit <- function(mdls) {
      av <- do.call(anova, unname(mdls)) %>% 
        as.data.table(keep.rownames = "Model")
      selected_models <- av[!(is.na(`Pr(>F)`) & !is.na(Df)), Model]
      if (length(selected_models) > 0) {
        mdls[as.numeric(selected_models)]  
      } else {
        return(NULL)
      }
    }
    get_best_fit <- function(mdls) {
      av <- do.call(anova, unname(mdls)) %>% 
        as.data.table(keep.rownames = "Model")
      n_mdls <- length(mdls)
      if (n_mdls == 3) {
        if (av[Model == 3, `Pr(>F)` < 0.05]) {
          best_model <- mdls[[3]]
        } else if (av[Model == 2, `Pr(>F)` < 0.05]) {
          best_model <- mdls[[2]]
        } else {
          best_model <- mdls[[1]]
        }  
      } else if (n_mdls == 2) {
        if (av[Model == 2, `Pr(>F)` < 0.05]) {
          best_model <- mdls[[2]]
        } else {
          best_model <- mdls[[1]]
        }
      } else {
        best_model <- mdls[[1]]
      }
      return(best_model)
    }
    
    s_mdls <- list(
      s_mdl1 = tryCatch(segmented::segmented(mdl, npsi = 1, control = seg_control), error = function(e) NA),
      s_mdl2 = tryCatch(segmented::segmented(mdl, npsi = 2, control = seg_control), error = function(e) NA),
      s_mdl3 = tryCatch(segmented::segmented(mdl, npsi = 3, control = seg_control), error = function(e) NA)
    ) %>% lapply(Fn$check_psi, gap = 2) %>%
      Filter(Negate(is.null), .)
    
    
    if (length(s_mdls) == 0) {
      best_model <- mdl
    } else if (length(s_mdls) == 1) {
      best_model <- s_mdls[[1]]
    } else {
      best_model <- s_mdls %>% 
        filter_good_fit() %>% 
        get_best_fit()
    }
    
    best_model$orig_model <- mdl
    return(best_model)
  }
}, Fn)

## -- Model Fitting Functions ------------------------------
evalq({
  Fn$cox_fit <- function(data, by = "Tstage") {
    dta <- if (is.data.frame(data)) list(copy(data)) else copy(data)
    dta <- map(dta, function(d) {
      out <- d %>%
        .[, StatusNew := 0] %>% 
        .[grepl("Dead", Status) & DeathCause == "C43", StatusNew := 1] %>% 
        .[, Status := StatusNew] %>% 
        .[, StatusNew := NULL]
      
      out[, c(by) := lapply(.SD, factor, ordered = FALSE), .SDcols = by]
      # Using time and adjust for Age
      # out <- out[, AgeDiag := (DiagYear + yday(DiagDate)/365.25) - (BirthYear + 182/365.25)]
      # .[, AgeEnd := (year(EndDate) + yday(EndDate)/365.25) - (BirthYear + 182/365.25)] %>%
      #   .[AgeEnd - AgeDiag > 0.5]
      return(out)
    })
    fit <- map(dta, function(d) {
      survival::coxph(
        reformulate(by, "survival::Surv(SurvivalMonth, Status)"),
        data = d
      )
    })
    if (length(fit) == 1) fit <- fit[[1]]
    return(fit)
  }
  Fn$tidy_cox_fit <- Fn$tidy_fit <- function(fit) {
    if ("coxph" %in% class(fit)) {
      out <- broom::tidy(fit, conf.int = TRUE, exp = TRUE) %>% 
        .[, c("term", "estimate", "conf.low", "conf.high")]
    } else {
      out <- mice::pool(fit) %>% 
        summary(conf.int = TRUE, exp = TRUE) %>% 
        .[, c("term", "estimate", "2.5 %", "97.5 %")]
    }
    setDT(out)
    setnames(out, names(out), c("Term", "HR", "Lower", "Upper"))
    return(out)
  }
  Fn$round_transform <- function(digits = 2) {
    function(text, envir) {
      formatC(get(text, envir), format = "f", digits = digits)
      # round(eval(parse(text = text), envir), digits = 2)
    }
  }
}, Fn)
## -- Survival functions ---------------------------------------
evalq({
    prepare_surv_data <- function(data, cause_specific = FALSE, remove_date = TRUE, hide_cause = TRUE, group_vars = c("Tstage")) {
      if (!is.data.frame(data)) {
        data <- rbindlist(data)
      } else {
        data[, Imp := 0]
      }
      dta <- data[, .(
        Imp = Imp,
        diag_date = DiagDate,
        status_date = EndDate,
        time = EndDate - DiagDate,
        thickness = Thickness,
        age = Age,
        status = fifelse(grepl("[dD]ead", Status), 1, 0),
        cause = fcase(
          grepl("[Aa]live", Status), "Alive",
          grepl("[Dd]ead", Status) & grepl("C43|172", DeathCause), "Melanoma",
          grepl("[Dd]ead", Status) & !grepl("C43|172", DeathCause), "Other",
          grepl("|[Ll]ost", Status), "Lost",
          default = NA_character_
        ),
        sex = as.factor(fifelse(Sex == "Men", "male", "female")),
        .SD[, group_vars, with = FALSE]
      )][, id := 1:.N, by = .(Imp)]
      if (cause_specific) {
        dta[, status := fifelse(cause == "Melanoma", 1, 0)]
      }
      if (hide_cause) {
        dta[, cause := NULL]
      }
      if (remove_date) {
        dta[, diag_year := year(diag_date)]
        dta[, status_year := year(status_date)]
        dta[, c("diag_date", "status_date") := NULL]
      }
      setnames(dta, Fn$camel2snake(names(dta)))
      return(dta[])
    }
  }, Fn) # Prepare data for survival
evalq({
  #' To tidy up survfit object
  #'
  #' Add various other survival-based point estimates to a tidied survfit object. Like [broom::tidy()] but adds more estimates. Includes ability to expland o include all possible time points, even where no events occurred
  #'
  #' @param survfit Object of class "survfit" from [survival::Surv()]
  #' @param expand Logical. if true then expand survival estimates to include rows for days when no events (or censoring) occurred.
  #' @return A tibble.
  #' @keywords survival
  #' @export

  tidy_surv <- function(survfit, expand = FALSE) {
    if (!expand) {
      output <-
        survfit %>%
        tidy() %>%
        transmute(
          time,
          leadtime = lead(time),
          int = leadtime - time,
          n.risk,
          n.event,
          n.censor,

          # actuarial estimates
          surv = cumprod(1 - n.event / n.risk), # =1-(cml.event/max(n.risk))
          cml.event = cumsum(n.event),
          cml.censor = cumsum(n.censor),
          cml.haz = -log(surv), # =cumsum(haz)
          haz = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * int), # =(cml.haz-lag(cml.haz))/int
          se.haz = (haz * sqrt(1 - (haz * int / 2)^2)) / sqrt(n.event),
          sumerand = n.event / ((n.risk - n.event) * n.risk),
          se.surv = surv * sqrt(cumsum(sumerand)),

          # log(-log()) scale
          llsurv = log(-log(surv)),
          se.llsurv = sqrt((1 / log(surv)^2) * cumsum(sumerand)),

          # kaplan-meier / nelson-aalen estimators
          haz_km = n.event / (n.risk * int), # =-(surv-lag(surv))/lag(surv)
          cml.haz_km = cumsum(haz_km), # =cumsum(haz_km)
          se.haz_km = haz_km * sqrt((n.risk - n.event) / (n.risk * n.event))#,
        )
    }
    if (expand) {
      output <-
        survfit %>%
        tidy() %>%
        complete(
          time = full_seq(time, 1),
          fill = list(n.event = 0, n.censor = 0)
        ) %>%
        fill(n.risk, .direction = c("up")) %>%
        transmute(
          time,
          n.risk,
          n.event,
          n.censor,
          surv = cumprod(1 - n.event / n.risk), # =1-(cml.event/max(n.risk))
          cml.event = cumsum(n.event),
          cml.censor = cumsum(n.censor),
          cml.haz = -log(surv), # =cumsum(haz)
          haz = n.event / ((n.risk - (n.censor / 2) - (n.event / 2))), # =cml.haz-lag(cml.haz)
          se.haz = (haz * sqrt(1 - (haz / 2)^2)) / sqrt(n.event),
          sumerand = n.event / ((n.risk - n.event) * n.risk),
          se.surv = surv * sqrt(cumsum(sumerand)),
          llsurv = log(-log(surv)),
          se.llsurv = sqrt((1 / log(surv)^2) * cumsum(sumerand)),
          # LL.surv=surv^(exp(1.96*se.llsurv)),
          # UL.surv=surv^(exp(-1.96*se.llsurv)),

          # kaplan-meier / nelson-aalen estimators
          haz_km = n.event / (n.risk), # =-(surv-lag(surv))/lag(surv)
          cml.haz_km = cumsum(n.event / n.risk), # =cumsum(haz_km)
          se.haz_km = haz_km * sqrt((n.risk - n.event) / (n.risk * n.event))#,
        )
    }

    return(output)
    }
}, Fn) # Tidy survial fit (github: wjchulme/BCIS-PCI-relative-survival)
evalq({
  #' To tidy survfit object from [relsurv::rs.surv()]
  #'
  #' Adds various useful survival-based point estimates for survfit object
  #'
  #' @param rsurvfit Object of class "survfit" from [relsurv::rs.surv()]
  #' @return A tibble.
  #' @keywords survival, relative survival
  #' @export

  # clean-up and add variables to output from rs.surv model object
  # returns tibble
  tidy_rsurv <- function(rsurvfit) {
    se.fac <- sqrt(qchisq(0.95, 1))

    tidied <- rsurvfit %>% tidy()
    setDT(tidied)
    
    intforevents <- rbind(
      list(time = 0),
      tidied[n.event != 0],
      fill = TRUE
    )[, .(time, leadtime = shift(time, 1, type = "lead"))]
    intforevents[, int := leadtime - time]
    intforevents[, lagint := shift(int)]

    out <- rbind(
      list(
        time = 0,
        n.risk = rsurvfit$n,
        n.event = 0,
        n.censor = 0,
        estimate = 1,
        std.error = 0
      ),
      tidied,
      fill = TRUE
    )

    out <- out %>% merge(intforevents, all.y = TRUE, by = "time")
    
    out <- out %>%
      .[, .(
        time, 
        leadtime, 
        int, 
        lagint, 
        estimate, 
        std.error,
        n.risk, 
        n.event, 
        n.censor
      )] %>%
      .[, leadtime_ := shift(time, 1, type = "lead")] %>%
      .[, int_ := leadtime_ - time] %>%
      .[, sumerand := n.event / ((n.risk - n.event) * n.risk)] %>%
      .[, cml.event := cumsum(n.event)] %>%
      .[, cml.censor := cumsum(n.censor)] %>%
      .[, haz := fifelse(
          n.event == 0, NA, 
          n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * int)
      )] # :=(cml.haz-lag(cml.haz))/int
      
    out <- out %>% 
      .[, se.haz := (haz * sqrt(1 - (haz * int / 2)^2)) / sqrt(n.event)] %>%
      .[, surv := cumprod(1 - n.event / n.risk)] %>%
      .[, se.surv := surv * sqrt(cumsum(sumerand))] # Greenwood's formula

      # log(-log) scale to get standard errors for CIs
    out <- out %>%
      .[, llsurv := log(-log(surv))] %>%
      .[, se.llsurv := sqrt((1 / log(surv)^2) * cumsum(sumerand))] %>%
      .[, surv.ll := surv^(exp(se.fac * se.llsurv))] %>%
      .[, surv.ul := surv^(exp(-se.fac * se.llsurv))] %>%
      .[, cml.haz := -log(surv)] %>% # =cumsum(haz)
      .[, cml.haz.ll := -log(surv.ul)] %>%
      .[, cml.haz.ul := -log(surv.ll)]

      # kaplan-meier / nelson-aalen estimators
    out <- out %>%
      .[, haz_km := n.event / (n.risk * int)] %>% # =-(surv-lag(surv))/lag(surv)
      .[, cml.haz_km := cumsum(ifelse(is.na(haz_km), 0, haz_km))] %>%
      .[, se.haz_km := haz_km * sqrt((n.risk - n.event) / (n.risk * n.event))]

      # relative survival
    out <- out %>%
      .[, rel.surv := estimate] %>%
      .[, se.rel.surv := std.error]

      # expected survival
    out <- out %>%
      .[, exp.surv := surv / rel.surv] %>% # retrieve expected survival by inverting relative survival estimate
      .[, exp.cml.haz := -log(exp.surv)] %>% # =(cml.haz-cml.exs.haz)
      .[, exp.haz := (exp.cml.haz - shift(exp.cml.haz, n = 1, type = "lag")) / 
          shift(int, n = 1, type = "lag")]

      # conf intervals all based on on standard error of log(-log) of surv - ie, assume exp.haz is known without error
    out <- out %>%
      .[, rel.surv.ll := surv.ll / exp.surv] %>%
      .[, rel.surv.ul := surv.ul / exp.surv]

      # cumulative excess hazard
    out <- out %>%
      .[, cml.exs.haz := -log(rel.surv)] %>% # cumulative excess hazard
      .[, cml.exs.haz.ll := -log(rel.surv.ul)] %>%
      .[, cml.exs.haz.ul := -log(rel.surv.ll)]

        # hazard ratio
    out[, rat.haz := haz / exp.haz]

        # transformed survival for testing relative survival differences using log-rank test:
        # these produce very similar CIs so may as well ignore
        # rel.llsurv = log(-log(rel.surv)),
        # se.rel.llsurv = sqrt( (1/log(rel.surv)^2)* cumsum(sumerand) ),
        # rel.surv.ll = rel.surv^(exp( se.fac*se.rel.llsurv)),
        # rel.surv.ul = rel.surv^(exp(-se.fac*se.rel.llsurv)),

    return(out[])
  }
}, Fn) # Tidy relative survial fit (github: wjchulme/BCIS-PCI-relative-survival)

## -- Plotting functions ----
evalq({
  adj_rate_plot <- function(
    data, 
    type = "spline", 
    group_var = "Tstage", 
    size_var = NULL, 
    extra_label = TRUE,
    row_var = NULL, 
    col_var = NULL, 
    se = TRUE, 
    lrow = 1, 
    log = FALSE, 
    subset = NULL, ... 
  ) {
    row_var <- if (is.null(row_var)) "." else row_var
    col_var <- if (is.null(col_var)) "." else col_var
    data[, Size := if (!is.null(size_var)) data[[size_var]] else 1]
    group <- do.call(interaction, data[, ..group_var])
    data[, Group := stringr::str_replace(group, "-", "\U2013")]
    
    if (length(group_var) == 1 & "Tstage" %in% group_var) {
      data[, Group := forcats::fct_relabel(Group, CodeMap$get_Tstage_label)]
    }
    if ("Sex" %in% names(data)) data[Sex == "Sum", Sex := "Overall"]
    
    if (length(group_var) == 1) {
      legend_title <- if (group_var == "Tstage") "T category" else group_var
    } else {
      legend_title <- paste0(group_var, collapse = ":")
    }
    legend_guide <- guide_legend(
      title = legend_title,
      title.position = "left",
      nrow = lrow
    )
    plot_caption <- "Note: Transparent: complete cases, Opaque: after multiple imputation"
    if (extra_label) {
      row_lbl <- col_lbl <- NULL
      .subset <- ifelse("Case" %in% names(data), "Case == 'All Cases'", "")
      summary_df <- data[Imp != "Complete"]
      if (!is.null(subset)) {
        summary_df <- summary_df[eval(parse(text = subset))]
      }
      if (.subset != "") {
        summary_df <- summary_df[eval(parse(text = .subset))]
      }
      sum_var <- c()
      if (row_var != ".") sum_var <- append(sum_var, row_var)
      if (col_var != ".") sum_var <- append(sum_var, col_var)
      sum_expr <- reformulate(sum_var, response = "N / imp")
      
      summary_tbl <- summary_df[, xtabs(
        sum_expr, data = .SD, drop.unused.levels = TRUE
      )] %>% round() %>% addmargins()

      if (length(dim(summary_tbl)) > 1) {
        names(dimnames(summary_tbl)) <- sum_var
        if (row_var != ".") {
          append(dimnames(summary_tbl), list("." = NULL))
          row_lbl <- first(summary_tbl[, "Sum"], -1) %>% 
            Fn$tbl2vec() %>% 
            Fn$label_vec()
        }
        if (col_var != ".") {
          col_lbl <- first(summary_tbl["Sum", ], -1) %>% 
            Fn$tbl2vec() %>% 
            Fn$label_vec()
        }
      } else {
        lbl <- first(summary_tbl, -1) %>%
          Fn$tbl2vec() %>%
          Fn$label_vec()
        if (row_var != ".") row_lbl <- lbl
        if (col_var != ".") col_lbl <- lbl
      }
      
      facet_label <- labeller(.rows = row_lbl, .cols = col_lbl)
      plot_caption <- glue::glue(
        plot_caption, "\n",
        "Note: n is the number of cases averaged",
        "over {data[Imp == 'Pooled', unique(imp)]} imputed datasets",
        sep = " "
      )
    }
    
    if (!is.null(subset)) {
      data <- data[eval(parse(text = subset))]
    }
    
    plot_frame <- expression({
      Plot <- data %>% 
        ggplot(aes(DiagYear, adj.rate, group = Group)) +
        ggh4x::facet_grid2(reformulate(col_var, row_var), labeller = facet_label, ...) +
        ggthemes::scale_color_stata() +
        ggthemes::scale_fill_stata() +
        guides(color = legend_guide, fill = legend_guide) +
        scale_y_continuous(
          trans = if (log) "log" else "identity",
          breaks = scales::breaks_extended(5),
          minor_breaks = scales::breaks_width(1)
        ) +
        scale_x_continuous(
          limits = c(1983, 2020),
          breaks = scales::breaks_width(5),
          guide = guide_axis(n.dodge = 2),
          labels = \(x) ifelse(as.numeric(x) > 2020, "", x)
        ) +
        scale_size_continuous(range = c(0.2, 2), guide = "none") +
        theme(
          plot.caption = element_text(hjust = 0, size = rel(1), lineheight = rel(1.15)),
          legend.position = "bottom",
          axis.text.x = element_text(vjust = 0.5),
          plot.caption.position = "plot"
        ) +
        labs(
          x = "Year of diagnosis",
          y = "Age-standardized incidence rate\nper 100,000 person-years"
          # caption = plot_caption
        )
      if (!log) Plot <- Plot + expand_limits(y = 0)
    })
    points <- expression({
      Plot <- Plot +
        geom_point(
          data = ~subset(.x, Imp %in% c(0, "Complete") & !is.na(Group)),
          aes(color = Group, size = Size),
          alpha = 0.25, 
          fill = "whitesmoke",
          na.rm = TRUE
        ) +
        geom_point(
          data = ~subset(.x, !Imp %in% c(0, "Complete")),
          aes(color = Group, size = Size),
          shape = 21,
          fill = "whitesmoke"
        )
    })
    splines <- expression({
      Plot <- Plot +
        geom_line(
          data = ~subset(.x, Imp %in% c(0, "Complete")),
          aes(y = adj.rate_spl, color = Group, group = Group),
          alpha = 0.25,
          na.rm = TRUE
        ) +
        geom_line(
          data = ~subset(.x, !Imp %in% c(0, "Complete")),
          aes(y = adj.rate_spl, color = Group, group = Group),
          na.rm = TRUE
        )
    })
    segmented <- expression({
      Plot <- Plot +
        geom_line(
          data = ~subset(.x, Imp %in% c(0, "Complete")),
          aes(y = fit, color = Group, group = Group),
          alpha = 0.25,
          na.rm = TRUE
        ) +
        geom_line(
          data = ~subset(.x, !Imp %in% c(0, "Complete")),
          aes(y = fit, color = Group, group = Group)
        )
    })
    adj_lines <- expression({
      Plot <- Plot +
        geom_line(
          data = ~subset(.x, !Imp %in% c(0, "Complete")),
          aes(color = Group)
        )
    })
    lines <- switch(
      type,
      spline = splines,
      segmented = segmented,
      adj_lines
    )
    ribbons <- expression({
      Plot <- Plot +
        geom_ribbon(
          aes(ymax = uci_spl, ymin = lci_spl, fill = Group, group = Group),
          data = ~subset(.x, !Imp %in% c(0, "Complete")),
          alpha = 0.15, 
          color = NA
        )
    })
    
    eval(plot_frame)
    if (type == "segmented") {
      eval(points)
      eval(lines)
    } else if (type == "spline") {
      if (se) eval(ribbons)
      eval(points)
      eval(lines)
    } else {
      eval(lines)
      eval(points)
    }
    return(Plot)
  }
}, Pn)

evalq({
  single_plotly <- function(data, group_var = "Tstage", title = NULL, xtitle = NULL, ytitle = NULL, showlegend = TRUE) {
    plot_data <- copy(data)
    type <- fcase(
      "fit" %in% names(plot_data), "segmented",
      "adj.rate_spl" %in% names(plot_data), "spline",
      default = "adjrate"
    )
    if (type == "spline") {
      plot_data[, errorlabel := glue::glue_data(.SD, "({round(lci_spl, 2)}, {round(uci_spl, 2)})")]
    }
    if (type == "segmented") {
      plot_data[, errorlabel := glue::glue_data(.SD, "({round(fit_lwr, 2)}, {round(fit_upr, 2)})")]
    }
    if (all(c("lci", "uci") %in% names(plot_data))) {
      plot_data[, Size := if (all(c("lci", "uci") %in% names(plot_data))) 
        uci - lci else 1]
      plot_data[, ratelabel := glue::glue_data(
        .SD, "{round(adj.rate, 2)} ({round(lci, 2)}, {round(uci, 2)})"
      )]
    }
    plot_data[, Group := if (!is.null(group_var)) plot_data[[group_var]] else 1]
    Plot <- plot_ly(
      plot_data, 
      x = ~DiagYear, 
      color = if (!is.null(group_var)) reformulate(group_var) else NULL,
      colors = ~ggthemes::stata_pal()(plot_data[, uniqueN(get(..group_var))])
    )
    if (plot_data[, uniqueN(Imp) == 1]) {
      if (type == "spline") {
        Plot <- Plot %>% 
          add_ribbons(
            ymin = ~lci_spl,
            ymax = ~uci_spl,
            line = list(width = 0),
            opacity = 0.25,
            hovertemplate = ~errorlabel,
            legendgroup = ~paste0("spline-ribbon", Group)
          )
      }
      Plot <- Plot %>% 
        add_trace(
          y = ~adj.rate,
          type = "scatter",
          mode = "markers+lines",
          hovertemplate = "%{y:.2f}",
          legendgroup = ~paste0("complete-rate", Group),
          marker = list(
            color = "whitesmoke", 
            line = list(width = 2),
            size = ~Size,
            sizemax = 10,
            sizeref = ~2 * max(Size)/(4 ** 2)
          )
        )
      if (type == "spline") {
        Plot <- Plot %>% 
          add_trace(
            y = ~adj.rate_spl,
            type = "scatter",
            mode = "lines",
            hovertemplate = "%{y:.2f}",
            legendgroup = ~paste0("spline-line", Group)
          )
      }
      if (type == "segmented") {
        Plot <- Plot %>% 
          add_trace(
            y = ~fit,
            type = "scatter",
            mode = "lines",
            legendgroup = ~paste0("segmented", Group),
            hovertemplate = "%{y:.2f}"
          )
      }
    } else {
      if (type == "spline") {
        Plot <- Plot %>% 
          add_ribbons(
            data = plotly_data(Plot)[!Imp %in% c("Complete", 0)],
            ymin = ~lci_spl,
            ymax = ~uci_spl,
            line = list(width = 0),
            opacity = 0.25,
            hovertemplate = ~errorlabel,
            legendgroup = ~paste0("spline-ribbon", Group),
            showlegend = showlegend
          )
      }
      Plot <- Plot %>% 
        add_trace(
          data = plot_data[Imp == "Complete"],
          y = ~adj.rate,
          type = "scatter",
          mode = "markers+lines",
          opacity = 0.2,
          hovertemplate = ~ratelabel,
          legendgroup = ~paste0("complete-rate", Group),
          showlegend = showlegend,
          marker = list(
            color = "whitesmoke", 
            line = list(width = 2),
            size = ~Size,
            sizemax = 10,
            sizeref = ~2 * max(Size)/(4 ** 2)
          ),
          line = list(
            width = ifelse(type %in% c("spline", "segmented"), 0.25, 2)
          )
        ) %>% 
        add_trace(
          data = plot_data[Imp == "Pooled"],
          y = ~adj.rate,
          type = "scatter",
          hovertemplate = ~ratelabel,
          legendgroup = ~paste0("pooled-rate", Group),
          showlegend = showlegend,
          mode = "markers+lines",
          marker = list(
            color = "whitesmoke", 
            line = list(width = 2),
            size = ~Size,
            sizemax = 10,
            sizeref = ~2 * max(Size)/(4 ** 2)
          ),
          line = list(
            width = ifelse(type %in% c("spline", "segmented"), 0.25, 2)
          )
        )
      if (type == "segmented") {
        Plot <- Plot %>% 
          add_trace(
            data = plotly_data(Plot)[!Imp %in% c("Complete", 0)],
            y = ~fit,
            type = "scatter",
            mode = "lines",
            showlegend = showlegend,
            legendgroup = ~paste0("segmented", Group)
          )
      }
      if (type == "spline") {
        Plot <- Plot %>% 
          add_trace(
            data = plotly_data(Plot)[!Imp %in% c("Complete", 0)],
            y = ~adj.rate_spl,
            type = "scatter",
            mode = "lines",
            hovertemplate = "%{y:.2f}",
            showlegend = showlegend,
            legendgroup = ~paste0("spline-line", Group)
          )
      }
    }
    if (!is.null(title)) {
      Plot <- Plot %>% 
        add_annotations(
          text = title,
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15, family = "monospace")
        )
    }
    if (!is.null(ytitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0 ,
          y = 0.5,
          xshift = -80,
          text = ytitle,
          font = list(size = 18),
          textangle = 270,
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    if (!is.null(xtitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0.5 ,
          y = 0,
          yshift = -50,
          text = xtitle,
          font = list(size = 18),
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    Plot <- Plot %>% 
      layout(
        margin = list(b = 50, l = 80, r = 10, t = 30),
        legend = list(
          orientation = "h",
          title = list(text = "<b>T category</b>"),
          traceorder = 'reversed',
          y = -0.14
        ),
        hovermode = "x unified",
        hoverlabel = list(font = list(family = "monospace")),
        xaxis = list(
          showline = TRUE,
          mirror = "all",
          linewidth = 1,
          nticks = 10,
          title = list(text = "")
        ),
        yaxis = list(
          showline = TRUE,
          mirror = "all",
          linewidth = 1,
          zeroline = FALSE,
          nticks = 10,
          title = list(text = "")
        )
      )
    return(Plot)
  }
  
}, Pn)

evalq({
  grouped_plotly <- function(plot_list, row_var = NULL, col_var = NULL, title = NULL, xtitle = NULL, ytitle = NULL,
                             shareX = TRUE, shareY = FALSE) {
    if (is.null(xtitle)) xtitle <- "Year of diagnosis"
    if (is.null(ytitle)) ytitle <- "Age-adjusted incidence rate\nper 100,000 person-year"
    plot_list_customized <- map(plot_list, function(plt) {
      plt %>% 
        layout(
          xaxis = list(
            tickangle = 45,
            range = c(1982, 2020)
          ),
          yaxis = list(
            nticks = 10
          )
        )
    })
    plot_df <- rbindlist(map_depth(plot_list_customized, 1, ~list(Plot = list(.x))), idcol = "Variable")
    if (any(stringr::str_detect(names(plot_list_customized), "\\."))) {
      plot_df <- plot_df %>% tidyr::separate(Variable, c(row_var, col_var), "\\.")
    }
    Plot <- subplot(
      plot_df[["Plot"]], 
      nrows = if (is.null(row_var)) 1 else uniqueN(plot_df[[row_var]]), 
      shareX = shareX, 
      shareY = shareY,
      margin = ifelse(shareY, 0.005, 0.02)
    )
    Plot %>% 
      add_annotations(
        x = 0 ,
        y = 0.5,
        xshift = -80,
        text = ytitle,
        font = list(size = 18),
        textangle = 270,
        showarrow = F,
        xref = 'paper',
        yref = 'paper'
      ) %>% 
      add_annotations(
        x = 0.5 ,
        y = -0.04,
        yshift = -50,
        text = xtitle,
        font = list(size = 18),
        showarrow = F,
        xref = 'paper',
        yref = 'paper'
      )
  }  
  
}, Pn)

evalq({
  apc_plotly <- function(data, x, y, group, showlegend = TRUE, title = NULL, xtitle = NULL, ytitle = NULL, ...) {
    plot_data <- copy(data)
    plot_data <- plot_data[order(get(x))]
    plot_data[, c(group) := lapply(.SD, as.factor), .SDcols = group]
    plot_data[, ratelabel := if (all(c("lower", "upper") %in% names(plot_data)))
      glue::glue_data(
        .SD, "{round(est, 2)} ({round(lower, 2)}, {round(upper, 2)})"
      ) else 1]
    
    Plot <- plot_ly(
      plot_data, 
      x = reformulate(x), 
      y = reformulate(y), 
      color = reformulate(group), 
      colors = viridis::viridis(data[, uniqueN(get(group))])
    ) %>% 
      add_trace(
        type = "scatter",
        hovertemplate = ~ratelabel,
        showlegend = showlegend,
        mode = "markers+lines",
        marker = list(
          color = "whitesmoke", 
          line = list(width = 1.5),
          size = 4
        ),
        line = list(width = 1),
        legendgroup = reformulate(group),
        ...
      ) %>% 
      layout(
        margin = list(b = 50, l = 80, r = 10, t = 20),
        legend = list(
          orientation = "h",
          title = list(text = paste0("<b>", group, "</b>")),
          y = -0.14
        ),
        hovermode = "x unified",
        xaxis = list(
          showline = TRUE,
          mirror = "all",
          linewidth = 1,
          zeroline = FALSE,
          nticks = 10,
          tickangle = 0,
          title = list(text = "")
        ),
        yaxis = list(
          type = "log",
          showline = TRUE,
          mirror = "all",
          linewidth = 1,
          zeroline = FALSE,
          nticks = 5,
          title = list(text = "")
        )
      )
    if (!is.null(title)) {
      Plot <- Plot %>% 
        add_annotations(
          text = title,
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15, family = "monospace")
        )
    }
    if (!is.null(ytitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0 ,
          y = 0.5,
          xshift = -80,
          text = ytitle,
          font = list(size = 18),
          textangle = 270,
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    if (!is.null(xtitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0.5 ,
          y = 0,
          yshift = -50,
          text = xtitle,
          font = list(size = 18),
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    return(Plot)
  }
}, Pn)

evalq({
  lexis_plotly <- function(data, showscale = TRUE, title = NULL, xtitle = NULL, ytitle = NULL, ...) {
    plot_data <- copy(data)
    plot_data[order(Period, Age)]
    plot_data[, ratelabel := glue::glue_data(
      .SD, "Age: {Age}\\nPeriod: {Period}\\nIncidence Rate: {round(est, 2)} ({round(lower, 2)}, {round(upper, 2)})"
    )]
    
    ax <- list(
      showline = TRUE,
      mirror = TRUE,
      linewidth = 1,
      zeroline = FALSE,
      nticks = 10,
      title = list(text = ""),
      scaleratio = 1,
      tickangle = 0,
      # scaleanchor = "y",
      range = plot_data[, range(Period)] + c(-2.5, 2.5),
      constrain = "domain"
    )
    ay <- list(
      showline = TRUE,
      mirror = TRUE,
      linewidth = 1,
      zeroline = FALSE,
      nticks = 15,
      title = list(text = ""),
      scaleratio = 1,
      # scaleanchor = "x",
      range = plot_data[, range(Age)] + c(-2.5, 2.5),
      constrain = "domain"
    )
    
    Plot <-
      plot_ly(
        plot_data, 
        x = ~Period,
        y = ~Age,
        z = ~est,
        type = "heatmap",
        showscale = showscale,
        zmin = 0,
        zmax = plot_data[, max(est)],
        showlegend = FALSE,
        hovertext = ~ratelabel,
        hoverinfo = "x+y+z",
        hovertemplate = "Age: %{y}<br>Period: %{x}<br>Rate: %{z:.2f}<extra></extra>",
        colorbar = list(
          title = list(text = "Rate")
        ),
        ...
      ) %>%
      layout(
        xaxis = ax,
        yaxis = ay
      )
    if (!is.null(title)) {
      Plot <-
        Plot %>% 
        add_annotations(
          text = title,
          x = 0.5,
          y = 0,
          yshift = 15,
          yref = "paper",
          xref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 15, family = "monospace", color = "whitesmoke")
        )
    }
    if (!is.null(ytitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0 ,
          y = 0.5,
          xshift = -80,
          text = ytitle,
          font = list(size = 18),
          textangle = 270,
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    if (!is.null(xtitle)) {
      Plot <- Plot %>% 
        add_annotations(
          x = 0.5 ,
          y = 0,
          yshift = -50,
          text = xtitle,
          font = list(size = 18),
          showarrow = F,
          xref = 'paper',
          yref = 'paper'
        )
    }
    return(Plot)
  }
}, Pn)

evalq({
  grouped_apc_plotly <- 
    function(plot_list, row_var = NULL, col_var = NULL, title = NULL, 
             xtitle = NULL, ytitle = NULL, shareX = TRUE, shareY = TRUE, ...) {
      # if (is.null(xtitle)) xtitle <- "Age at diagnosis"
      if (is.null(ytitle)) ytitle <- "Age-specific incidence rate\nper 100,000 person-year"
      plot_list_customized <- map(plot_list, function(plt) plt)
      
      plot_df <- map_df(plot_list_customized, function(plt) {
        out <- as.data.table(plotly_data(plt))
        if (!is.null(row_var) | !is.null(col_var)) {
          out <- out[, .(Data = list(.SD)), by = c(row_var, col_var)]
        } else {
          out <- out[, .(data = list(.SD))]
        }
        out[, Plot := list(plt)]
      })
      
      nrow <- if (is.null(row_var)) 1 else plot_df[, uniqueN(get(row_var))]
      if (!is.null(col_var) & !is.null(row_var)) {
        plot_df <- plot_df[order(get(col_var))][order(get(row_var))]
      } 
      Plot <- subplot(
        plot_df[["Plot"]], 
        nrows = nrow,
        shareX = shareX, 
        shareY = shareY,
        ...
      )
      if (!is.null(ytitle)) {
        Plot <- Plot %>% 
          add_annotations(
            x = 0 ,
            y = 0.5,
            xshift = -80,
            text = ytitle,
            font = list(size = 18),
            textangle = 270,
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
      }
      if (!is.null(xtitle)) {
        Plot <- Plot %>% 
          add_annotations(
            x = 0.5 ,
            y = -0.04,
            yshift = -35,
            text = xtitle,
            font = list(size = 18),
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
      }
      return(Plot)
    }  
  
}, Pn)

## -- Kaplan-Meier Plot using ggplot2 ----
evalq({
  plot_km <- function(data, group = c("Sex", "Tstage"), row_var = NULL, col_var = NULL, subset = NULL, log_x = TRUE) {
    dta <- copy(data)
    dta[, NewStatus := fcase(
      grepl("Lost", Status), 0, 
      grepl("Alive", Status), 0, 
      grepl("Dead", Status), 1, 
      default = NA
    )]
    surv <- with(dta, Surv(SurvivalMonth, NewStatus))
    all_groups <- c(group, row_var, col_var)
    formula <- reformulate(all_groups, "surv")
    mdl1 <- survfit(
      formula = formula, data = dta, type = "kaplan-meier", 
      subset = if (!is.null(subset)) eval(parse(text = subset)) else NULL
    )
    plot_data <- tidy(mdl1) %>% 
      as.data.table() %>% 
      .[, c(all_groups) := stringr::str_split_fixed(strata, ", ", n = length(all_groups)) %>% 
          apply(1:2, stringr::str_trim) %>% 
          as.data.table()] %>% 
      .[, strata := NULL] %>% 
      .[, c(all_groups) := lapply(.SD, stringr::str_remove, ".*="), .SDcols = all_groups] %>% 
      .[, group := interaction(.SD), .SDcols = group]
    
    plot <- plot_data %>% 
      ggplot(aes(time, estimate, ymax = conf.high, ymin = conf.low)) +
      geom_vline(xintercept = c(12, 24, 60, 120), 
                 color = "#afafaf", 
                 linetype = "dashed") +
      geom_ribbon(color = NA, aes(fill = group), alpha = 0.2) +
      geom_line(aes(color = group)) +
      geom_linerange(
        aes(
          ymax = estimate + 0.01, 
          ymin = estimate - 0.01,
          color = group),
        data = ~subset(.x, n.censor > 0)) +
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1") +
      ggthemes::theme_few() +
      theme(legend.position = "bottom",
            panel.grid = element_line(color = "#f0f0f0")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = "log(Months survived)", y = "Survial probability", 
           fill = "T category", color = "T category")
    
    if (log_x) {
      plot <- plot +
        scale_x_continuous(trans = "log1p", breaks = c(0, 1, 2, 4, 6, 12, 24, 60, 120, 240, 480))
    }
    
    if (is.null(row_var)) row_var <- "."
    if (is.null(col_var)) col_var <- "."
    
    plot <- plot + facet_grid(reformulate(col_var, row_var))
    return(plot)
  }
}, Pn)
## -- Plot histogram of thickness ----------------------------------------
evalq({
  plot_thist <- function(plot_data, type = c("freq", "density"), ...) {
    library(ggplot2)
    type <- match.arg(type)
    
    group_var <- setdiff(names(plot_data), c("Cuts", "Thickness"))
    cuts <- plot_data[, levels(Cuts)] %>% 
      str_remove_all("[\\(\\)\\[\\]]") %>% 
      str_split(",") %>% 
      unlist() %>% 
      unique() %>% 
      as.numeric()
    
    plt <- ggplot(plot_data, aes(Thickness)) +
      geom_histogram(
        aes(fill = Cuts, y = if (type == "freq") ..count.. else ..density..), 
        binwidth = 0.1, 
        color = "black", 
        size = 0.1
      ) +
      geom_vline(xintercept = first(cuts, -1) + 0.05, size = 0.25, lty = 2) +
      scale_y_continuous(
        expand = expansion(c(0, 0.05))
      ) +
      ggthemes::scale_color_stata() +
      ggthemes::scale_fill_stata() +
      labs(
        x = "Tumour thickness", 
        y = "Number of cases",
        fill = "Cut-points",
        color = "Cut-points"
      ) +
      ggthemes::theme_few() +
      theme(
        legend.position = c(1, 1),
        legend.justification = c(1.05, 1.05),
        legend.direction = "horizontal",
        panel.grid = element_line(color = "#f0f0f0")
      )
    if (length(group_var)) {
      plt <- plt + facet_grid(rows = vars(get(group_var)), ...)
    }
    return(plt)
  }
}, Pn)

## -- Some generic function ------------------------------
evalq({
  group_summary <- function(data, group = "DiagYear", FUN = list("N" = "sum"), margin = NULL, margin_label = "Overall") {
    measure_var <- names(FUN)
    fun <- map(FUN, ~eval(parse(text = .x)))
    
    out <- lapply(measure_var, function(var) {
      data[, lapply(.SD, fun[[var]]), .SDcols = var, by = group]
    })
    if (!is.null(margin)) {
      out_margin <- lapply(measure_var, function(var) {
        data[, lapply(.SD, fun[[var]]), .SDcols = var, by = setdiff(group, margin)] %>% 
          .[, c(margin) := margin_label]
      })
      out <- map2(out, out_margin, ~rbind(.x, .y, fill = TRUE, use.names = TRUE))
    }
    ret <- reduce(out, merge.data.table)
    
    attr(ret, "data.env") <- attr(data, "env")
    attr(ret, "env") <- mget(names(formals()), sys.frame(sys.nframe()))[-1]
    class(ret) <- append("CountDataSummary", class(data))
    return(ret)
  }
  create_cat <- function(data, old_var, new_var, ...) {
    dots <- list(...)
    if (length(dots) == 1 & is.function(dots[[1]])) {
      fn <- dots[[1]]
      new_value <- fn(data[[old_var]])
    } else {
      new_value <- cut(data[[old_var]], ...)
    }
    data[, c(new_var) := new_value]
    return(data[])
  }
  llm <- function(data, rate_var, period_var, ...) {
    formula <- reformulate(termlabels = period_var, response = paste0("log(", rate_var, ")"))
    data <- data[data[[rate_var]] > 0, ]
    out <- lm(formula, data = data, ...)
    class(out) <- append("llm", class(out))
    return(out)
  }
  get_aapc <- function(x) UseMethod("get_aapc", x)
  get_aapc.llm <- function(obj, ...) {
    (Epi::ci.exp(obj, subset = variable.names(obj)[2]) - 1) * 100
  }
}, Fn)
## -- Table Theme ------------------------------
evalq({
  gt_theme_espn <- function(data, ...){
    require(gt)
    data %>% 
      opt_table_font(
        font = list(
          google_font("Lato"),
          default_fonts()
        )
      )  %>% 
      opt_row_striping() %>% 
      tab_options(
        row.striping.background_color = "#fafafa",
        table_body.hlines.color = "#f6f7f7",
        source_notes.font.size = 12,
        table.font.size = 16,
        table.width = px(700),
        heading.align = "left",
        heading.title.font.size = 24,
        table.border.top.color = "transparent",
        table.border.top.width = px(3),
        data_row.padding = px(7),
        ...
      ) 
  }
}, Fn) # function to theme gt_table
