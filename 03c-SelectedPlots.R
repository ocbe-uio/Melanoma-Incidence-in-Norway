## -- Setup paths ----------------
ROOT <- Sys.getenv("ROOT")
INC <- Sys.getenv("DataInc")
INC_ROOT <- file.path(ROOT, "01-incidence")

PDATA <- file.path(INC_ROOT, "02-Results", "PlotData")

OUT <- file.path(INC_ROOT, "03-Outputs", "Plots")
if (!dir.exists(OUT)) dir.create(OUT)

## -- Source script files ----
source(file.path(ROOT, "00-common", "00-CodeMap.R"))
source(file.path(ROOT, "00-common", "00-Functions.R"))

## -- Load packages --------
Fn$quietly_load(c(
  "tidytable", "data.table", "stringr", 
  "purrr", "forcats",
  "ggplot2", "ggh4x"
))

## -- Export function --------------------
export_plots <- function(plot_obj, base_filename, width, height, path = OUT, unit = "in", formats = c("pdf", "jpeg", "svg", "png"), ...) {
  walk(formats, function(frmt){
    exp_path <- here::here(path, toupper(frmt))
    if (!dir.exists(exp_path)) dir.create(exp_path, recursive = TRUE)
    
    fname = here::here(exp_path, glue::glue(base_filename, frmt, .sep = "."))
    cat(glue::glue("Saving: {fname}."), "\n")
    ggplot2::ggsave(
      plot_obj,
      filename = fname,
      device = if (frmt == "pdf") cairo_pdf else if (frmt == "svg") svglite::svglite else NULL,
      width = width,
      height = height,
      units = unit,
      dpi = 300,
      ...
    )
  })
}

## -- Plots ----
SelectedPlots <- new.env()
evalq(
  {
    Spline <- new.env()
    AgeAdjusted <- new.env()
    AgeSpecific <- new.env()
    APC <- new.env()
    PropPlot <- new.env()
  },
  SelectedPlots
)

## -- Theme adjustment ------------------------------
ggplot2::theme_set(ggthemes::theme_few())
theme_update(panel.grid = element_line(color = "#f0f0f0"))

## -- Spline Plots ----
evalq({
  ## -- By: Sex, Tstage --------
  evalq({
    BySexTstage <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTstage.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexTstage(filter = filter, logY = logY)
      
      plot <- Pn$adj_rate_plot(
        data = PlotData,
        type = "spline",
        group_var = "Tstage",
        col_var = "Sex",
        log = log,
        scales = "free_y",
        independent = "y",
        se = FALSE,
        ...
      )
      suppressMessages({
        plot <- plot + scale_y_continuous(
          breaks = scales::breaks_width(5),
          limits = c(0, 25)
        )
      })
      return(plot)
    }
  }, Spline)
  
  ## -- By: Sex, Thinner Cases ----
  evalq({
    BySexThinStage <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexThinStage.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexThinStage(filter = filter, logY = logY)
      
      Pn$adj_rate_plot(
        data = PlotData,
        type = "spline",
        group_var = "Tstage",
        col_var = "Sex",
        row_var = "Case",
        lrow = 1,
        scales = "free_y",
        independent = "y",
        se = FALSE,
        log = log,
        ...
      )
    }
  }, Spline)
  
  ## -- By: Sex, TSubStage ----
  evalq({
    BySexTSubStage <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTSubStage.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexTSubStage(filter = filter, logY = logY)
      Pn$adj_rate_plot(
        data = PlotData,
        type = "spline",
        group_var = "Tstage",
        col_var = "Sex",
        log = log,
        se = FALSE,
        ...
      )
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Anatomic Site ------------------------------
  evalq({
    BySexTstageSite <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTstageSite.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      # Change Head and neck to Head/neck
      levels(PlotData$AnatomicSite)[1] <- "Head/neck"
      
      # PlotData <- PlotData$Spline$BySexTstageSite(filter = filter, logY = logY)
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum"],
        type = "spline",
        group_var = "Tstage",
        col_var = "AnatomicSite",
        row_var = "Sex",
        lrow = 1,
        scales = "free_y",
        independent = "y",
        log = log,
        subset = "AnatomicSite != 'Other'",
        se = FALSE,
        ...
      )
      suppressMessages({
        plot <- plot +
          ggh4x::facetted_pos_scales(
            y = list(
              AnatomicSite == "Trunk" ~ scale_y_continuous(
                breaks = scales::breaks_width(5),
                limits = c(0, 15)
              ),
              AnatomicSite == "Head and neck" ~ scale_y_continuous(
                breaks = scales::breaks_width(1),
                limits = c(0, 3)
              ),
              AnatomicSite == "Upper limbs" ~ scale_y_continuous(
                breaks = scales::breaks_width(1),
                limits = c(0, 4)
              ),
              AnatomicSite == "Lower limbs" ~ scale_y_continuous(
                breaks = scales::breaks_width(2),
                limits = c(0, 8)
              )
            )
          )
      })
      return(plot)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Melanoma Type ------------------------------
  evalq({
    BySexTstageType <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTstageType.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexTstageType(filter = filter, logY = logY)
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum"],
        type = "spline",
        group_var = "Tstage",
        col_var = "MelanomaType",
        row_var = "Sex",
        lrow = 1,
        scales = "free_y",
        independent = "y",
        log = log,
        subset = "MelanomaType != 'Other'",
        se = FALSE,
        ...
      )
      suppressMessages({
        plot <- plot +
          ggh4x::facetted_pos_scales(
            y = list(
              MelanomaType == "Superficial spreading" ~ scale_y_continuous(
                breaks = scales::breaks_width(5),
                limits = c(0, 25)
              ),
              MelanomaType == "Nodular" ~ scale_y_continuous(
                breaks = scales::breaks_width(1),
                limits = c(0, 4)
              ),
              MelanomaType == "Lentigo maligna" ~ scale_y_continuous(
                breaks = scales::breaks_width(0.5),
                limits = c(0, 1.5)
              )
            )
          )
      })
      return(plot)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Health Region ------------------------------
  evalq({
    BySexTstageRegion <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTstageRegion.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexTstageRegion(filter = filter, logY = logY)
      PlotData[, HealthRegion := CodeMap$factor_map(HealthRegion, "HealthRegion")]
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum"],
        type = "spline",
        group_var = "Tstage",
        col_var = "HealthRegion",
        row_var = "Sex",
        lrow = 1,
        scales = "free_y",
        independent = "y",
        log = log,
        se = FALSE,
        ...
      )
      
      suppressMessages({
        plot <- plot +
          ggh4x::facetted_pos_scales(
            y = list(
              HealthRegion == "Northern" ~ scale_y_continuous(
                breaks = scales::breaks_width(1),
                limits = c(0, 3)
              )
            )
          )
      })
      return(plot)
    }
  }, Spline)
  
  ## -- By: Sex, Tstage, Season ------------------------------
  evalq({
    BySexTstageSeason <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTstageSeason.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$Spline$BySexTstageSeason(filter = filter, logY = logY)
      PlotData[, Season := CodeMap$factor_map(Season, "Season")]
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum"],
        type = "spline",
        group_var = "Tstage",
        col_var = "Season",
        row_var = "Sex",
        lrow = 1,
        log = log,
        se = FALSE,
        ...
      )
      suppressMessages({
        plot <- plot + scale_y_continuous(breaks = scales::breaks_width(2), limits = c(0, 8))
      })
      return(plot)
    }
  }, Spline)
  
  ## -- By: Sex, TSubStage for recent periods ----------------
  evalq({
    BySexTSubStageRecent <- function(filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "Spline-BySexTSubStageRecent.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      browser()
      tstage_lbl <- PlotData %>% 
        filter(Imp == "Pooled") %>% 
        summarize(
          N = round(sum(N) / max(imp)),
          .by = "Tstage"
        ) %>% mutate(Tstage = CodeMap$get_Tstage_label(Tstage)) %>% 
        xtabs(N ~ Tstage, data = .) %>% 
        Fn$label_vec()
      
      sex_lbl <- PlotData %>% 
        filter(Imp == "Pooled") %>% 
        summarize(
          N = round(sum(N) / max(imp)),
          .by = "Sex"
        ) %>% 
        xtabs(N ~ Sex, data = .) %>% 
        Fn$label_vec()
      
      # PlotData <- PlotData$Spline$BySexTSubStage(filter = filter, logY = logY)
      PlotData[!is.na(Tstage)] %>% 
        mutate(TSubStageCode = case_when(
          grepl("a", TSubStage) ~ "a",
          grepl("b", TSubStage) ~ "b",
          TRUE ~ "–"
        )) %>% 
        ggplot(aes(DiagYear)) +
        facet_grid(
          cols = vars(Tstage), rows = vars(Sex),
          labeller = labeller(
            Tstage = \(x) tstage_lbl[CodeMap$get_Tstage_label(x)],
            Sex = sex_lbl
          )
        ) +
        geom_ribbon(
          data = ~subset(.x, Imp == "Pooled"),
          aes(ymin = lci_spl, ymax = uci_spl, fill = TSubStage, group = TSubStage),
          alpha = 0.15
        ) +
        geom_line(
          data = ~subset(.x, Imp == "Complete" & !is.na(TSubStage)),
          alpha = 0.05,
          aes(y = adj.rate, color = TSubStage, group = TSubStage, linetype = TSubStage),
          show.legend = FALSE
        ) +
        geom_point(
          data = ~subset(.x, Imp == "Complete" & !is.na(TSubStage)),
          shape = 21,
          fill = "whitesmoke",
          alpha = 0.05,
          aes(y = adj.rate, color = TSubStage),
          show.legend = FALSE
        ) +
        geom_line(
          data = ~subset(.x, Imp == "Pooled"),
          aes(y = adj.rate, color = TSubStage, group = TSubStage, linetype = TSubStage),
          alpha = 0.15
        ) +
        geom_line(
          data = ~subset(.x, Imp == "Pooled"),
          aes(y = adj.rate_spl, color = TSubStage, group = TSubStage, linetype = TSubStage)
        ) +
        geom_point(
          data = ~subset(.x, Imp == "Pooled"),
          shape = 21,
          fill = "whitesmoke",
          aes(y = adj.rate, color = TSubStage),
        ) +
        scale_x_continuous(
          breaks = seq(2008, 2020, 2)
        ) +
        scale_y_continuous(
          breaks = scales::breaks_extended(10)
        ) +
        scale_color_manual(
          aesthetics = c("color", "fill"),
          values = rep(ggthemes::stata_pal()(4), each = 3),
          breaks = apply(CJ(paste0("T", 1:4), c("", "a", "b")), 1, paste, collapse = "")
        ) +
        scale_linetype_manual(
          values = rep(c(3, 1, 2), 4),
          breaks = apply(CJ(paste0("T", 1:4), c("", "a", "b")), 1, paste, collapse = "")
        ) +
        ggthemes::theme_few() +
        theme(
          legend.position = "bottom",
          legend.justification = "left",
          legend.key.width = unit(1, "cm"),
          panel.grid = element_line(color = "#f0f0f0")
        ) +
        guides(
          color = guide_legend(nrow = 1, title.position = "top"),
          fill = guide_legend(nrow = 1, title.position = "top")
        ) +
        labs(
          x = "Year of diagnosis",
          y = "Age-standardized incidence rate\n per 100,000 person-years",
          color = "Thickness sub-category",
          fill = "Thickness sub-category",
          linetype = "Thickness sub-category"
          # caption = "NOTE: Ulceration missing were coded as no ulceration"
        )
    }
  }, Spline)
}, SelectedPlots)

## -- Export Spline Plots ----------------
spl_by_sex_tstage <- SelectedPlots$Spline$BySexTstage()
# export_plots(spl_by_sex_tstage, "Spline-BySexTstage", 11, 4.5)
export_plots(spl_by_sex_tstage, "Spline-BySexTstage", 7, 3, scale = 1.5)

spl_by_sex_tstage_site <- SelectedPlots$Spline$BySexTstageSite()
export_plots(spl_by_sex_tstage_site, "Spline-BySexTstageSite", 7, 4, scale = 1.5)

spl_by_sex_tstage_type <- SelectedPlots$Spline$BySexTstageType()
export_plots(spl_by_sex_tstage_type, "Spline-BySexTstageType", 6, 4, scale = 1.5)

spl_by_sex_tstage_region <- SelectedPlots$Spline$BySexTstageRegion()
export_plots(spl_by_sex_tstage_region, "Spline-BySexTstageRegion", 7, 4, scale = 1.5)

spl_by_sex_tstage_season <- SelectedPlots$Spline$BySexTstageSeason()
export_plots(spl_by_sex_tstage_season, "Spline-BySexTstageSeason", 7, 4, scale = 1.5)

spl_by_sex_tsubstage <- SelectedPlots$Spline$BySexTSubStageRecent()
export_plots(spl_by_sex_tsubstage, "Spline-BySexTsubStage", 7, 4, scale = 1.5)

## -- AgeAdjusted Plots ------------------------------
evalq({
  ## -- By: Sex, Tstage ------------------------------
  evalq({
    BySexTstage <- function(segmented = FALSE, filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexTstage.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      type <- if (segmented) "segmented" else "age-adjusted"
      # PlotData <- PlotData$AgeAdjusted$BySexTstage(
      #   segmented = segmented,
      #   filter = filter,
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData %>% 
        Pn$adj_rate_plot(
          type = type,
          col_var = "Sex",
          # size_var = "Size",
          log = log,
          ...
        )
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Thinner Cases ----
  evalq({
    BySexThinStage <- function(segmented = FALSE, filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      type <- if (segmented) "segmented" else "age-adjusted"
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexThinStage.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$AgeAdjusted$BySexThinStage(
      #   segmented = segmented,
      #   filter = filter,
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData %>% Pn$adj_rate_plot(
        type = type,
        col_var = "Sex",
        row_var = "Case",
        # size_var = "Size",
        log = log,
        scales = "free_y",
        independent = "y",
        ...
      )
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Anatomic Site ------------------------------
  evalq({
    BySexTstageSite <- function(segmented = FALSE, filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      type <- if (segmented) "segmented" else "age-adjusted"
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexTstageSite.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # Change Head and neck to Head/neck
      levels(PlotData$AnatomicSite)[1] <- "Head/neck"
      
      # PlotData <- PlotData$AgeAdjusted$BySexTstageSite(
      #   segmented = segmented,
      #   filter = filter,
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData[!is.na(Tstage) & Sex != "Sum"] %>% 
        Pn$adj_rate_plot(
          type = type,
          col_var = "AnatomicSite",
          row_var = "Sex",
          # size_var = "Size",
          log = log,
          scales = "free_y",
          independent = "y",
          ...
        )
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Melanoma Type ------------------------------
  evalq({
    BySexTstageType <- function(segmented = FALSE, filter = NULL, log = FALSE, na.rm = TRUE, ...) {
      type <- if (segmented) "segmented" else "age-adjusted"
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexTstageType.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$AgeAdjusted$BySexTstageType(
      #   segmented = segmented,
      #   filter = filter,
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData[!is.na(Tstage) & Sex != "Sum "] %>% 
        Pn$adj_rate_plot(
          type = type,
          col_var = "MelanomaType",
          row_var = "Sex",
          # size_var = "Size",
          log = log,
          scales = "free_y",
          independent = "y",
          ...
        )
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Health Region ------------------------------
  evalq({
    BySexTstageRegion <- function(segmented = FALSE, filter = NULL, na.rm = TRUE, log = FALSE, ...) {
      type <- if (segmented) "segmented" else "age-adjusted"
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexTstageRegion.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$AgeAdjusted$BySexTstageRegion(
      #   segmented = segmented,
      #   filter = filter, 
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData[, HealthRegion := CodeMap$factor_map(HealthRegion, "HealthRegion")]
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum" & !is.na(Tstage)],
        type = type,
        group_var = "Tstage",
        col_var = "HealthRegion",
        row_var = "Sex",
        # size_var = "Size",
        scales = "free_y",
        independent = "y",
        log = log,
        ...
      )
      
      suppressMessages({
        plot <- plot +
          ggh4x::facetted_pos_scales(
            y = list(
              HealthRegion == "Northern" ~ scale_y_continuous(
                breaks = scales::breaks_width(1),
                limits = c(0, 3)
              )
            )
          )
      })
      return(plot)
    }
  }, AgeAdjusted)
  
  ## -- By: Sex, Tstage, Season ------------------------------
  evalq({
    BySexTstageSeason <- function(segmented = FALSE, filter = NULL, na.rm = TRUE, log = FALSE, ...) {
      type <- if (segmented) "segmented" else "age-adjusted"
      PlotData <- readRDS(here::here(PDATA, "AgeAdj-BySexTstageSeason.rds"))
      if (!is.null(filter)) PlotData <- PlotData[eval(parse(text = filter))]
      if (na.rm) PlotData <- na.omit(PlotData)
      
      # PlotData <- PlotData$AgeAdjusted$BySexTstageSeason(
      #   segmented = segmented,
      #   filter = filter, 
      #   logY = logY
      # )
      PlotData[, Size := uci - lci]
      PlotData[, Season := CodeMap$factor_map(Season, "Season")]
      plot <- Pn$adj_rate_plot(
        data = PlotData[Sex != "Sum" & !is.na(Tstage)],
        type = type,
        group_var = "Tstage",
        col_var = "Season",
        row_var = "Sex",
        # size_var = "Size",
        log = log,
        ...
      )
      
      return(plot)
    }
  }, AgeAdjusted)
  
}, SelectedPlots)

## -- AgeSpecific Plots ----------------
evalq({
  ## -- By: Age, Sex and Tstage ----------------
  evalq({
    ByAgeSexTstage <- function() {
      PlotData <- readRDS(here::here(PDATA, "ASP-Data-Sex-Tstage.rds"))
      plot_data <- PlotData %>% 
        tidytable::select(-7) %>% 
        mutate(imp = as.numeric(imp)) %>% 
        filter(imp != 0)
      
      sex_lbl <- plot_data %>%
        summarize(N = round(sum(N) / max(imp)), .by = "Sex") %>% 
        xtabs(N ~ Sex, data = .) %>% 
        Fn$label_vec()
      
      plot_data <- plot_data %>%
        summarize(
          N = round(sum(N) / max(imp)),
          est = mean(est),
          lower = mean(lower),
          upper = mean(upper),
          .by = c(AgeGroup5, Sex, Tstage)
        )
      
      ggplot(plot_data, aes(AgeGroup5, est, ymin = lower, ymax = upper)) +
        geom_ribbon(
          aes(group = Tstage, fill = Tstage), alpha = 0.15
        ) +
        geom_line(
          aes(group = Tstage, color = Tstage)
        ) +
        geom_point(
          shape = 21, fill = "whitesmoke", aes(color = Tstage)
        ) +
        facet_grid(cols = vars(Sex), labeller = labeller(Sex = sex_lbl)) +
        scale_x_discrete(
          labels = \(x) gsub("-", "–", x)
        ) +
        scale_y_continuous(
          breaks = scales::breaks_width(5)
        ) +
        ggthemes::scale_color_stata(
          name = "T category",
          label = CodeMap$get_Tstage_label
        ) +
        ggthemes::scale_fill_stata(
          name = "T category",
          label = CodeMap$get_Tstage_label
        ) +
        ggthemes::theme_few() +
        theme(
          legend.position = "bottom",
          legend.justification = "left",
          panel.grid = element_line(color = "#f0f0f0"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
          x = "Age at diagnosis",
          y = "Age-specific incidence rate\n per 100,000 person-year"
        )
    }
  }, AgeSpecific)
}, SelectedPlots)

## -- Export AgeSpecific Plots ----------------
asp_by_sex_tstage <- SelectedPlots$AgeSpecific$ByAgeSexTstage()
export_plots(asp_by_sex_tstage, "ASP-BySexTstage", 7, 4, scale = 1.5)


## -- APC Plots------------------------------
evalq({
  
  ## -- APC: Cohort by Age ----
  evalq({
    CohortByAge <- function(BySex = FALSE, Group = NULL, filter = "Age > 20", logY = TRUE, age_var = "Age", show_label = FALSE, na.rm = TRUE, ...) {
      fname <- glue::glue("APC")
      if (BySex) fname <- glue::glue(fname, "-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = BySex,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage") {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      apc_data[, c(age_var) := paste(
        get(age_var), get(age_var) + 4, 
        sep = "-"
      )]
      Plot <- apc_data %>%
        ggplot(aes(
          x = Cohort, 
          y = est, 
          group = get(age_var), 
          color = as.factor(get(age_var))
        )) +
        geom_line(na.rm = TRUE) +
        geom_point(
          size = rel(0.8),
          shape = 21, 
          fill = "whitesmoke", 
          na.rm = TRUE
        )
      
      if (show_label) {
        Plot <- Plot + 
          ggh4x::geom_text_aimed(
            aes(label = AgeCat),
            data = ~subset(.x, Period == max(Period)),
            hjust = -0.2,
            check_overlap = TRUE,
            color = "grey40",
            size = if (!is.null(Group)) rel(3) else rel(4)
          )
      }
      
      Plot <- Plot + 
        scale_size_continuous(range = c(0.01, 2), guide = "none") +
        scale_x_continuous(
          breaks = scales::breaks_extended(ifelse(!is.null(Group), 5, 10))
        )
      
      Plot <- Plot + scale_color_viridis_d(
        "Age group",
        na.value = "grey40",
        direction = -1,
        end = 0.9,
        labels = \(x) gsub("^85.*", "85+", x), # c(first(x, -1), gsub("-\\d+", "+", last(x))),
        guide = guide_legend(
          reverse = TRUE,
          title.position = "top",
          title.hjust = 0.5,
          ncol =  if (BySex) 1 else NULL,
          nrow = if (BySex) NULL else 2
        )
      )
      
      Plot <- Plot +
        theme(
          legend.position = ifelse(BySex, "right", "bottom"),
          legend.justification = if (is.null(Group) & !BySex) NULL else c(1, 0),
          plot.caption = element_text(hjust = 0, size = rel(1)),
          plot.caption.position = "plot"
        ) +
        labs(
          x = "Birth year",
          y = "Age-specific incidence rate\nper 100,000 person-years"
        )
      if (logY) {
        Plot <- Plot + scale_y_log10(breaks = scales::breaks_log(10))
      } else {
        Plot <- Plot + scale_y_continuous(breaks = scales::breaks_extended(10))
      }
      
      if (BySex | !is.null(Group)) {
        row_label <- col_label <- NULL
        if (BySex) {
          row_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        if (!is.null(Group)) {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ get(Group))/unique(imp)))]
        } else {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        facet_label <- labeller(
          .rows = row_label,
          .cols = col_label
        )
      }
      
      if (!is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), rows = vars(Sex), labeller = facet_label, ...)
      } else if (is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(Sex), labeller = facet_label, ...)
      } else if (!is.null(Group) & !BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), labeller = facet_label, ...)
      }
      return(Plot)
    }
  }, APC)
  
  ## -- APC: Period by Age ------------------------------
  evalq({
    PeriodByAge <- function(BySex = FALSE, Group = NULL, filter = "Age > 20", logY = TRUE, age_var = "Age", show_label = FALSE, na.rm = TRUE, ...) {
      fname <- glue::glue("APC")
      if (BySex) fname <- glue::glue(fname, "-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = BySex,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage") {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      apc_data[, c(age_var) := paste(
        get(age_var), get(age_var) + 4, 
        sep = "-"
      )]
      
      Plot <- apc_data %>%
        ggplot(aes(Period, est, group = get(age_var), color = get(age_var))) +
        geom_line(na.rm = TRUE) +
        geom_point(
          shape = 21, 
          fill = "whitesmoke", 
          na.rm = TRUE,
          size = rel(0.8)
        )
      if (show_label) {
        Plot <- Plot + 
          ggh4x::geom_text_aimed(
            aes(label = AgeCat),
            data = ~subset(.x, Period == max(Period)),
            hjust = -0.2,
            check_overlap = TRUE,
            color = "grey40",
            size = if (!is.null(Group)) rel(3) else rel(4)
          )
      }
      Plot <- Plot + 
        scale_x_continuous(
          breaks = scales::breaks_extended(ifelse(!is.null(Group), 5, 10)),
          limits = c(1983, 2020)
        )
      
      Plot <- Plot + scale_color_viridis_d(
        "Age group",
        na.value = "grey40",
        direction = -1,
        labels = \(x) gsub("^85.*", "85+", x), # c(first(x, -1), gsub("-\\d+", "+", last(x))),
        end = 0.9,
        guide = guide_legend(
          reverse = TRUE,
          title.position = "top",
          title.hjust = 0.5,
          ncol =  if (BySex) 1 else NULL,
          nrow = if (BySex) NULL else 2
        )
      )
      Plot <- Plot + 
        theme(
          legend.position = ifelse(BySex, "right", "bottom"),
          legend.justification = c(1, 0),
          plot.caption = element_text(hjust = 0, size = rel(1)),
          plot.caption.position = "plot"
        ) +
        labs(
          x = "Calendar year at diagnosis",
          y = "Age-specific incidence rate\nper 100,000 person-years"
        )
      if (logY) {
        Plot <- Plot + scale_y_log10(breaks = scales::breaks_log(10))
      } else {
        Plot <- Plot + scale_y_continuous(breaks = scales::breaks_extended(10))
      }
      
      if (BySex | !is.null(Group)) {
        row_label <- col_label <- NULL
        if (BySex) {
          row_label <- apc_data[, Fn$label_vec(
            round(xtabs(N ~ Sex)/unique(imp))
          )]
        }
        if (!is.null(Group)) {
          col_label <- apc_data[, Fn$label_vec(
            round(xtabs(N ~ get(Group))/unique(imp))
          )]
        } else {
          col_label <- apc_data[, Fn$label_vec(
            round(xtabs(N ~ Sex)/unique(imp))
          )]
        }
        facet_label <- labeller(
          .rows = row_label,
          .cols = col_label
        )
      }
      
      if (!is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(
          cols = vars(get(Group)), 
          rows = vars(Sex), 
          labeller = facet_label, 
          ...
        )
      } else if (is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(
          cols = vars(Sex), 
          labeller = facet_label, 
          ...
        )
      } else if (!is.null(Group) & !BySex) {
        Plot <- Plot + facet_grid(
          cols = vars(get(Group)), 
          labeller = facet_label, 
          ...
        )
      }
      return(Plot)
    }
  }, APC)
  
  ## -- APC: Age by Cohort ------------------------------
  evalq({
    AgeByCohort <- function(BySex = FALSE, Group = NULL, filter = "Age > 20", logY = TRUE, cohort_var = "Cohort", show_label = FALSE, na.rm = TRUE, ...) {
      fname <- glue::glue("APC")
      if (BySex) fname <- glue::glue(fname, "-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = BySex,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage" & !is.null(Group)) {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      
      Plot <- apc_data %>%
        ggplot(aes(Age, est, group = get(cohort_var), color = get(cohort_var))) +
        geom_line(na.rm = TRUE) +
        geom_point(
          shape = 21, 
          fill = "whitesmoke", 
          size = rel(0.8),
          na.rm = TRUE
        )
      
      if (show_label) {
        Plot <- Plot + 
          ggh4x::geom_text_aimed(
            aes(label = CohortCat),
            data = ~subset(.x, Age == max(Age)),
            hjust = -0.2,
            check_overlap = TRUE,
            color = "grey40",
            size = if (!is.null(Group)) rel(3) else rel(4)
          )
      }
      
      Plot <- Plot + 
        scale_size_continuous(range = c(0.2, 2), guide = "none") +
        scale_x_continuous(
          breaks = scales::breaks_extended(ifelse(!is.null(Group), 5, 10)),
          limits = c(apc_data[, min(Age)], apc_data[, max(Age) + if (show_label) 10 else 0])
        )
      
      if (is.numeric(apc_data[[cohort_var]])) {
        Plot <- Plot + scale_color_viridis_b(
          "Birth year",
          aesthetics = c("color", "fill"),
          na.value = "grey40",
          breaks = scales::breaks_extended(n = 10),
          direction = 1,
          end = 0.9,
          guide = guide_colorbar(
            barwidth = if (BySex) unit(0.02, "npc") else unit(0.7, "npc"),
            barheight = if (BySex) unit(0.7, "npc") else unit(0.02, "npc"),
            title.position = "top",
            title.hjust = 0.5
          ),
          limits = apc_data[, c(min(get(cohort_var)), max(get(cohort_var)) + 5)]
        )
      } else {
        Plot <- Plot + scale_color_viridis_d(
          "Birth year",
          na.value = "grey40",
          direction = 1,
          end = 0.9,
          guide = guide_legend(
            title.position = "top",
            title.hjust = 0.5,
            ncol =  if (BySex) 1 else NULL,
            nrow = if (BySex) NULL else 2
          )
        )
      }
      
      Plot <- Plot + 
        theme(
          legend.position = ifelse(BySex, "right", "bottom"),
          legend.justification = c(1, 0),
          plot.caption = element_text(hjust = 0, size = rel(1)),
          plot.caption.position = "plot"
        ) +
        labs(
          x = "Age at diagnosis",
          y = "Age-specific incidence rate\nper 100,000 person-years"
          # caption = glue::glue(
        )
      
      if (logY) {
        Plot <- Plot + scale_y_log10(breaks = scales::breaks_log(10))
      } else {
        Plot <- Plot + scale_y_continuous(breaks = scales::breaks_extended(10))
      }
      
      if (BySex | !is.null(Group)) {
        row_label <- col_label <- NULL
        if (BySex) {
          row_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        if (!is.null(Group)) {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ get(Group))/unique(imp)))]
        } else {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        facet_label <- labeller(
          .rows = row_label,
          .cols = col_label
        )
      }
      
      if (!is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), rows = vars(Sex), labeller = facet_label, ...)
      } else if (is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(Sex), labeller = facet_label, ...)
      } else if (!is.null(Group) & !BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), labeller = facet_label, ...)
      }
      return(Plot)
    }
  }, APC)
  
  ## -- APC: Age by Period ------------------------------
  evalq({
    AgeByPeriod <- function(BySex = FALSE, Group = NULL, filter = "Age > 20", logY = TRUE, period_var = "Period", show_label = FALSE, na.rm = TRUE, ...) {
      fname <- glue::glue("APC")
      if (BySex) fname <- glue::glue(fname, "-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = BySex,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage") {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      apc_data[, Period := paste(Period, Period + 5, sep = "-")]
      
      Plot <- apc_data %>%
        ggplot(aes(
          x = Age, 
          y = est, 
          group = get(period_var), 
          color = as.factor(get(period_var))
        )) +
        geom_line(na.rm = TRUE) +
        geom_point(
          shape = 21, 
          fill = "whitesmoke", 
          size = rel(0.8),
          na.rm = TRUE
        )
      
      if (show_label) {
        Plot <- Plot + 
          ggh4x::geom_text_aimed(
            aes(label = PeriodCat),
            data = ~subset(.x, Age == max(Age)),
            hjust = -0.2,
            check_overlap = TRUE,
            color = "grey40",
            size = if (!is.null(Group)) rel(3) else rel(4)
          )
      }
      
      Plot <- Plot +
        scale_size_continuous(range = c(0.2, 2), guide = "none") +
        scale_x_continuous(
          breaks = scales::breaks_extended(ifelse(!is.null(Group), 5, 10)),
          limits = c(apc_data[, min(Age)], apc_data[, max(Age) + if (show_label) 10 else 0])
        )
      
      # if (apc_data[, is.numeric(get(period_var))]) {
      #   Plot <- Plot + scale_color_viridis_b(
      #     "Period",
      #     aesthetics = c("color", "fill"),
      #     breaks = scales::breaks_extended(10),
      #     na.value = "grey40",
      #     direction = 1,
      #     end = 0.9,
      #     guide = guide_colorbar(
      #       barwidth = if (BySex) unit(0.02, "npc") else unit(0.7, "npc"),
      #       barheight = if (BySex) unit(0.7, "npc") else unit(0.02, "npc"),
      #       label.vjust = "inward",
      #       title.position = "top",
      #       title.hjust = 0.5,
      #       title.vjust = 5
      #     ),
      #     limits = apc_data[, c(
      #       min(get(period_var)), 
      #       max(get(period_var)) + 5
      #     )]
      #   )
      # } else {
      Plot <- Plot + scale_color_viridis_d(
        "Period",
        na.value = "grey40",
        direction = 1,
        end = 0.9,
        guide = guide_legend(
          title.position = "top",
          title.hjust = 1,
          reverse = TRUE,
          ncol =  if (BySex) 1 else NULL,
          nrow = if (BySex) NULL else 2
        )
      )
      # }
      
      Plot <- Plot + theme(
        legend.position = ifelse(BySex, "right", "bottom"),
        legend.justification = c(1, 0),
        plot.caption = element_text(hjust = 0, size = rel(1)),
        plot.caption.position = "plot"
      ) +
        labs(
          x = "Age at diagnosis",
          y = "Age-specific incidence rate\nper 100,000 person-years"
        )
      if (logY) {
        Plot <- Plot + scale_y_log10(breaks = scales::breaks_log(10))
      } else {
        Plot <- Plot + scale_y_continuous(breaks = scales::breaks_extended(10))
      }
      
      if (BySex | !is.null(Group)) {
        row_label <- col_label <- NULL
        if (BySex) {
          row_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        if (!is.null(Group)) {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ get(Group))/unique(imp)))]
        } else {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        facet_label <- labeller(
          .rows = row_label,
          .cols = col_label
        )
      }
      
      if (!is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), rows = vars(Sex), labeller = facet_label, ...)
      } else if (is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(Sex), labeller = facet_label, ...)
      } else if (!is.null(Group) & !BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), labeller = facet_label, ...)
      }
      return(Plot)
    }
  }, APC)
  
  ## -- APC: Lexis plot by Tstage (and Sex) ------------------------------
  evalq({
    LexisPlot <- function(BySex = FALSE, Group = NULL, filter = NULL, na.rm = TRUE) {
      fname <- glue::glue("APC")
      if (BySex) fname <- glue::glue(fname, "-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = BySex,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage") {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      
      Plot <- apc_data %>%
        ggplot(aes(Period, Age, fill = est)) +
        geom_tile(color = "whitesmoke", linewidth = 0.1, na.rm = TRUE) +
        geom_segment(aes(xend = Period + 5, yend = Age + 5),
                     color = "whitesmoke", na.rm = TRUE,
                     linetype = "dashed", linewidth = 0.5, alpha = 0.25
        ) +
        coord_equal(xlim = c(1982.5, 2017.5), ylim = c(22.5, 87.5)) +
        scale_x_continuous(
          breaks = scales::breaks_width(5), expand = expansion(add = 0),
          sec.axis = sec_axis(
            name = "Birth year",
            transform = \(x) x - 90,
            labels = \(x) paste0("-", x),
            breaks = scales::breaks_width(5)
          )
        ) +
        scale_y_continuous(
          breaks = scales::breaks_width(5),
          expand = expansion(add = 0),
          sec.axis = sec_axis(
            name = "Birth year",
            transform = \(x) x - 2020,
            labels = \(x) ifelse(as.numeric(x) > 1990, NA, paste0("-", x)),
            breaks = scales::breaks_width(5)
          )
        ) +
        scale_fill_gradientn(
          "Incidence\nRate",
          breaks = \(x) Epi::nice(x, log = TRUE, lpos = c(1, 3, 5)),
          labels = scales::label_number(0.1),
          colours = c("#fde725", "#22a884", "#440154"),
          values = c(0, 0.6, 0.8, 1),
          transform = "log",
          na.value = "grey40",
          guide = guide_colorbar(
            barwidth = if (BySex | !is.null(Group)) unit(0.02, "npc") else unit(0.6, "npc"),
            barheight = if (BySex | !is.null(Group)) unit(0.6, "npc") else unit(0.02, "npc"),
            title.position = "top",
            title.vjust = 2
          )
        ) +
        theme(
          legend.position = ifelse(BySex | !is.null(Group), "right", "bottom"),
          panel.spacing = unit(2, "pt"),
          strip.placement = "outside",
          axis.ticks.x.top = element_blank(),
          axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.x.top = element_text(angle = 45, hjust = -1, vjust = -1),
          axis.ticks.y.right = element_blank(),
          axis.text.y.right = element_text(angle = 45, hjust = 0.25, vjust = 0.25),
          plot.caption = element_text(hjust = 0, size = rel(1)),
          plot.caption.position = "plot"
        )
      
      if (BySex | !is.null(Group)) {
        row_label <- col_label <- NULL
        if (BySex) {
          row_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        if (!is.null(Group)) {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ get(Group))/unique(imp)))]
        } else {
          col_label <- apc_data[, Fn$label_vec(round(xtabs(N ~ Sex)/unique(imp)))]
        }
        facet_label <- labeller(
          .rows = row_label,
          .cols = col_label
        )
      }
      
      if (!is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), rows = vars(Sex), labeller = facet_label)
      } else if (is.null(Group) & BySex) {
        Plot <- Plot + facet_grid(cols = vars(Sex), labeller = facet_label)
      } else if (!is.null(Group) & !BySex) {
        Plot <- Plot + facet_grid(cols = vars(get(Group)), labeller = facet_label)
      }
      
      Plot <- Plot + labs(
        x = "Calendar year at diagnosis",
        y = "Age at diagnosis"
        # caption = glue::glue(
        #   "Note: n is the number of cases averaged",
        #   "over {apc_data[, unique(imp)]} imputed datasets",
        #   .sep = " "
        # )
      )
      
      return(Plot)
    }
  }, APC)
  
  ## -- APC: By Sex and AgeGroup ------------------------------
  evalq({
    BySexAgeGroup <- function(Group = NULL, filter = NULL, logY = FALSE, na.rm = TRUE, ...) {
      fname <- glue::glue("APC-Sex")
      if (!is.null(Group)) fname <- glue::glue(fname, "-{Group}")
      apc_data <- readRDS(here::here(PDATA, glue::glue("{fname}.rds")))
      if (!is.null(filter)) apc_data <- apc_data[eval(parse(text = filter))]
      if (na.rm) apc_data <- na.omit(apc_data)
      
      # apc_data <- PlotData$APC(
      #   BySex = TRUE,
      #   Group = Group,
      #   filter = filter
      # )
      if (!missing(Group)) {
        if (Group == "Tstage") {
          apc_data[, Tstage := forcats::fct_relabel(Tstage, CodeMap$get_Tstage_label)]
        }
      }
      apc_data <- apc_data[est > 0 & Imp == "Pooled"]
      apc_data[, AgeGroup := CodeMap$age_map(Age)]
      apc_data[, Age := glue::glue("{Age}–{Age + 4}")]
      apc_data[grepl("^85", Age), Age := "85+"]
      
      
      Plot <- apc_data %>%
        ggplot(aes(Period, est, group = Sex, color = Sex)) +
        geom_line(na.rm = TRUE) +
        geom_point(
          shape = 21, 
          fill = "whitesmoke", 
          size = rel(0.5),
          na.rm = TRUE
        ) +
        # scale_size_continuous(range = c(0.2, 2), guide = "none") +
        scale_x_continuous(
          breaks = scales::breaks_extended()
        ) +
        scale_color_manual(
          values = rev(ggthemes::stata_pal()(2)),
          breaks = c("Women", "Men")
        ) +
        theme(
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = rel(1)),
          plot.caption.position = "plot",
          axis.text.x = element_text(angle = 90, vjust = 0.5)
        ) +
        labs(
          x = "Year of diagnosis",
          y = "Age-specific incidence rate\nper 100,000 person-years"
        )
      if (logY) {
        Plot <- Plot + scale_y_log10(breaks = scales::breaks_log(10))
      } else {
        Plot <- Plot + scale_y_continuous(breaks = scales::breaks_extended(10))
      }
      
      if (!is.null(Group)) {
        row_label <- apc_data[, Fn$label_vec(
          round(xtabs(N ~ get(Group))/unique(imp)),
          label_template = "{..2}\n(n={format(..1, big.mark = ',')})"
        )]
      } else {
        row_label <- NULL
      }
      col_label_1 <- apc_data[, Fn$label_vec(
        round(xtabs(N ~ Age)/unique(imp)),
        label_template = "Age: {..2}\n(n={format(..1, big.mark = ',')})"
      )]
      col_label_2 <- apc_data[, Fn$label_vec(
        round(xtabs(N ~ AgeGroup)/unique(imp)),
        label_template = "{..2}\n(n={format(..1, big.mark = ',')})"
      )]
      
      facet_label <- labeller(
        .rows = row_label,
        Age = col_label_1,
        AgeGroup = col_label_2
      )
      
      if (!is.null(Group)) {
        Plot <- Plot + facet_grid(
          cols = vars(Age), 
          rows = vars(get(Group)), 
          labeller = facet_label,
          scales = "free_y",
          ...
        )
      } else {
        Plot <- Plot + facet_grid(
          cols = vars(Age), 
          labeller = facet_label,
          ...
        )
      }
      return(Plot)
      
    }
  }, APC)
  
}, SelectedPlots)

## -- Export APC Plots ----------------
apc_cohort_age <- SelectedPlots$APC$CohortByAge(
  BySex = TRUE, 
  Group = "Tstage",
  filter = "Age > 20"
)
export_plots(apc_cohort_age, "APC-ByCohortAge", 7, 3.5, scale = 1.5)

apc_age_cohort <- SelectedPlots$APC$AgeByCohort(
  BySex = TRUE, 
  Group = "Tstage",
  filter = "Age > 20"
)
export_plots(apc_age_cohort, "APC-ByAgeCohort", 7, 3.5, scale = 1.5)

apc_period_age <- SelectedPlots$APC$PeriodByAge(
  BySex = TRUE, 
  Group = "Tstage",
  filter = "Age > 20"
)
export_plots(apc_period_age, "APC-ByPeriodAge", 7, 3.5, scale = 1.5)

apc_lexis_plot <- SelectedPlots$APC$LexisPlot(
  BySex = FALSE,
  Group = "Tstage",
  filter = "Age > 20"
)
export_plots(apc_lexis_plot, "APC-LexisPlot", 7, 3.5, scale = 1.5)

apc_age_tstage <- SelectedPlots$APC$BySexAgeGroup(
  Group = "Tstage",
  filter = "Age > 20"
)
export_plots(apc_age_tstage, "APC-ByAgeTstage", 8.5, 5, scale = 1.5)


## -- Missing Breslow and Ulceration ------------------------------
evalq({
  MissingTrend <- function(by = NULL, filter = "Sex != 'Overall'", na.rm = TRUE) {
    if (!is.null(by)) {
      fname <- glue::glue("MissingTrend-By{by}")
    } else {
      fname <- glue::glue("MissingTrend-Overall")
    }
    plot_data <- readRDS(
      here::here(PDATA, glue::glue("{fname}.rds"))
    )
    if (!is.null(filter)) plot_data <- plot_data[eval(parse(text = filter))]
    if (na.rm) plot_data <- na.omit(plot_data)
    
    # data <- Results$Data()
    # plot_data <- PlotData$MissingTrend(by = by, filter = filter, data = data)
    sex_lbl <- attr(plot_data, "sex_label")
    by_lbl <- attr(plot_data, "by_label")
    
    Plot <- plot_data[!(variable == "Ulceration" & DiagYear < 2000)] %>%
      ggplot(aes(DiagYear, Prop, color = variable)) +
      geom_line(na.rm = TRUE) +
      geom_point(shape = 21, fill = "whitesmoke", na.rm = TRUE, size = rel(0.8)) +
      geom_vline(xintercept = 2008, linetype = "dashed", color = "grey50") +
      facet_grid(reformulate("Sex", by), labeller = labeller(.rows = by_lbl, .cols = sex_lbl)) +
      theme(legend.position = "bottom") +
      labs(
        x = "Year of diagnosis",
        y = "Percentage of missing cases",
        color = "Missing"
      ) +
      scale_y_continuous(
        labels = function(x) round(x * 100), 
        limits = c(0, 0.85),
        breaks = scales::breaks_width(0.1),
        minor_breaks = scales::breaks_width(0.05)
      ) +
      scale_x_continuous(
        breaks = scales::breaks_width(5),
        minor_breaks = scales::breaks_width(1)
      ) +
      ggthemes::scale_color_stata()
    
    return(Plot)
  }
}, SelectedPlots)

## -- Export Missing Breslow and Ulceration ----------------
missing_trend <- SelectedPlots$MissingTrend()
export_plots(missing_trend, "Missing-Trend", 5, 3.5, scale = 1.5)


## -- Proportion plot ----
evalq({
  prop_plot <- function(x_var = "year", y_var = c("count", "prop"), group_var, type = "pooled", col_var = NULL, row_var = NULL, page_var = NULL, margins = FALSE) {
    y_var <- match.arg(y_var)
    group_vars <- c(x_var, group_var, col_var, row_var, page_var)
    dta <- readRDS(here::here(PDATA, "Counts.Rds"))
    
    if (type == "pooled") {
      plt_dta <- copy(dta)[Imp != "0"]
      plt_dta <- plt_dta[, .(count = sum(count, na.rm = TRUE)), by = c(group_vars, "Imp")]
      plt_data <- plt_dta[, .(count = sum(count) / .SD[, uniqueN(Imp)]), by = c(group_vars)]
      plt_dta[, Imp := NULL]
    } else {
      plt_dta <- copy(dta)[Imp == "0"][, Imp := NULL]
      plt_dta <- plt_dta[, .(count = sum(count, na.rm = TRUE)), by = group_vars]
    }
    
    plt_dta <- plt_dta[!is.na(get(group_var))]
    
    if (!is.null(row_var)) plt_dta <- plt_dta[!is.na(get(row_var))]
    if (!is.null(col_var)) plt_dta <- plt_dta[!is.na(get(col_var))]
    
    if (is.null(row_var)) row_var <- "."
    if (is.null(col_var)) col_var <- "."
    
    if (!is.null(page_var)) {
      plt_dta <- plt_dta %>% split(by = page_var)
    } else {
      plt_dta <- list(plt_dta)
    }
    
    plots <- plt_dta %>% map(function(dta) {
      dta %>%
        ggplot(aes(
          x = get(x_var), 
          y = count,
          color = get(group_var),
          fill = get(group_var)
        )) +
        geom_area(position = if (y_var == "prop") "fill" else "stack") +
        facet_grid(
          reformulate(col_var, response = row_var),
          margins = margins
        ) +
        scale_x_continuous(
          breaks = scales::breaks_width(5),
          expand = expansion(0)
        ) +
        scale_y_continuous(
          breaks = scales::breaks_width(0.2),
          labels = scales::label_percent(suffix = ""),
          expand = expansion()
        )
    })
    return(plots)
  }
}, SelectedPlots) ## Need to test and validate the correctness

evalq({
  evalq({
    BySexTstage <- function() {
      dta <- readRDS(here::here(PDATA, "Counts.Rds"))
      
      setnames(
        dta,
        c("DiagYear", "Sex", "Tstage", "AnatomicSite", "MelanomaType", "N"),
        c("year", "sex", "tstage", "site", "type", "count")
      )
      
      plot_data <- dta[, .(count = sum(count)), by = .(Imp, year, sex, tstage)]
      margin <- plot_data[, .(count = sum(count)), by = .(Imp, year, tstage)]
      margin[, sex := "Overall"]
      plot_data <- rbind(plot_data, margin, use.names = TRUE)
      
      plot_data <- plot_data[, .(count = mean(count)), by = .(
        Imp = fifelse(Imp == "0", "Complete", "Pooled"),
        year, sex, tstage
      )][Imp == "Pooled"]
      
      ggplot(
        data = plot_data,
        aes(x = year, count, fill = tstage, color = tstage)
      ) +
        geom_area(alpha = 0.75, position = "fill") +
        geom_line(position = "fill") +
        facet_grid(
          cols = vars(sex)
        ) +
        scale_x_continuous(
          breaks = scales::breaks_width(5),
          expand = expansion(0)
        ) +
        scale_y_continuous(
          breaks = scales::breaks_width(0.2),
          labels = scales::label_percent(suffix = ""),
          expand = expansion(0)
        ) +
        scale_color_manual(
          "T category",
          values = ggthemes::stata_pal()(4),
          labels = CodeMap$get_Tstage_label,
          aesthetics = c("color", "fill")
        ) +
        theme(
          legend.position = "bottom",
          legend.justification = "left"
        ) +
        labs(
          x = "Year of diagnosis",
          y = "Percentage of cases"
        )
    }
    BySexTstageSiteType <- function(margins = FALSE) {
      dta <- readRDS(here::here(PDATA, "Counts.Rds"))
      
      setnames(
        dta,
        c("DiagYear", "Sex", "Tstage", "AnatomicSite", "MelanomaType", "N"),
        c("year", "sex", "tstage", "site", "type", "count")
      )
      
      if (margins) {
        margin_sex <- dta[, .(count = sum(count)), by = .(
          Imp, year, tstage, site, type
        )][, sex := "Overall"]
        
        margin_site <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage, type
        )][, site := "Overall"]
        
        margin_type <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage, site
        )][, type := "Overall"]
        
        margin_site_type <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage
        )][, c("site", "type") := "Overall"]
        
        plot_data <- rbind( 
          dta, margin_sex,
          margin_site, margin_type,
          margin_site_type,
          use.names = TRUE
        )
      } else {
        plot_data <- dta
      }
      
      plot_data <- plot_data[type != "Other" & site != "Other"]
      
      plot_data <- plot_data[, .(count = mean(count)), by = .(
        Imp = fifelse(Imp == "0", "Complete", "Pooled"),
        year, sex, tstage, site, type
      )][Imp == "Pooled"]
      
      plot_data %>% split(by = "sex") %>% map(function(dt) {
        ggplot(
          data = dt,
          aes(
            x = year, y = count, 
            fill  = tstage, 
            color = tstage,
            group = tstage
          )
        ) +
          geom_area(alpha = 0.75, position = "fill") +
          geom_line(position = "fill") +
          facet_grid(
            cols = vars(site),
            rows = vars(type)
          ) +
          scale_x_continuous(
            breaks = scales::breaks_width(5),
            expand = expansion(0),
            guide = guide_axis(n.dodge = 2)
          ) +
          scale_y_continuous(
            breaks = scales::breaks_width(0.2),
            labels = scales::label_percent(suffix = ""),
            expand = expansion(0)
          ) +
          scale_color_manual(
            "T category",
            values = ggthemes::stata_pal()(4),
            labels = CodeMap$get_Tstage_label,
            aesthetics = c("color", "fill")
          ) +
          theme(
            legend.position = "bottom",
            legend.justification = "left"
          ) +
          labs(
            x = "Year of diagnosis",
            y = "Percentage of cases",
            subtitle = glue::glue("Sex: {dt[, unique(sex)]}")
          )
      })
    }
    BySexTstageSiteType2 <- function(margins = FALSE) {
      dta <- readRDS(here::here(PDATA, "Counts.Rds"))
      setnames(
        dta,
        c("DiagYear", "Sex", "Tstage", "AnatomicSite", "MelanomaType", "N"),
        c("year", "sex", "tstage", "site", "type", "count")
      )
      if (margins) {
        margin_sex <- dta[, .(count = sum(count)), by = .(
          Imp, year, tstage, site, type
        )][, sex := "Overall"]
        
        margin_site <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage, type
        )][, site := "Overall"]
        
        margin_type <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage, site
        )][, type := "Overall"]
        
        margin_site_type <- dta[, .(count = sum(count)), by = .(
          Imp, year, sex, tstage
        )][, c("site", "type") := "Overall"]
        
        plot_data <- rbind( 
          dta, margin_sex,
          margin_site, margin_type,
          margin_site_type,
          use.names = TRUE
        )
      } else {
        plot_data <- dta
      }
      
      plot_data <- plot_data[type != "Other" & site != "Other"]
      plot_data <- plot_data[!type == "Lentigo maligna"]
      
      plot_data <- plot_data[, .(count = mean(count)), by = .(
        Imp = fifelse(Imp == "0", "Complete", "Pooled"),
        year, sex, tstage, site, type
      )][Imp == "Pooled"]
      
      plot_data %>% 
        ggplot(
          aes(
            x = year, y = count, 
            fill  = tstage, 
            color = tstage,
            group = tstage
          )
        ) +
        geom_area(position = "fill", alpha = 0.75) +
        geom_line(position = "fill") +
        ggh4x::facet_nested(
          cols = vars(site),
          rows = vars(type, sex)
        ) +
        scale_x_continuous(
          breaks = scales::breaks_width(5),
          expand = expansion(0),
          guide = guide_axis(n.dodge = 2)
        ) +
        scale_y_continuous(
          breaks = scales::breaks_width(0.2),
          labels = scales::label_percent(suffix = ""),
          expand = expansion(0)
        ) +
        scale_color_manual(
          "T category",
          values = ggthemes::stata_pal()(4),
          labels = CodeMap$get_Tstage_label,
          aesthetics = c("color", "fill")
        ) +
        theme(
          legend.position = "bottom",
          legend.justification = "left"
        ) +
        labs(
          color = "T category",
          fill = "T category",
          x = "Year of diagnosis",
          y = "Percentage of cases"
        )
    }
  }, PropPlot)
}, SelectedPlots)


## -- Export Proportion plots ---------------------------------------------
prop_by_sex_tstage_site_type <- SelectedPlots$PropPlot$BySexTstageSiteType()
export_plots(
  prop_by_sex_tstage_site_type$Women,
  "Prop-Women-ByTstageSiteType",
  8, 4.5,
  scale = 1.2
)
export_plots(
  prop_by_sex_tstage_site_type$Men,
  "Prop-Men-ByTstageSiteType",
  8, 4.5,
  scale = 1.2
)

prop_by_sex_tstage_site_type2 <- SelectedPlots$PropPlot$BySexTstageSiteType2()
export_plots(
  prop_by_sex_tstage_site_type2,
  "Prop-BySexTstageSiteType",
  8, 5,
  scale = 1.2
)
