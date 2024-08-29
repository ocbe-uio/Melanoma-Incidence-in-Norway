## -- PATHS ----------------
ROOT <- Sys.getenv("ROOT")
DATA <- Sys.getenv("Data")
RAW <- file.path(DATA, "00-raw")
PROCESSED <- file.path(DATA, "00-processed")

## -- Source Script ----------------
source(file.path(ROOT, "00-common/00-CodeMap.R"))
source(file.path(ROOT, "00-common/00-Functions.R"))

## -- Load Packages ----
Fn$quietly_load(c(
  "tidytable", "data.table", "purrr", "stringr", 
  "lubridate", "readxl", "here", "haven"
))

## -- Population Norway ----------------
CountyPop <- foreign::read.dta(
  here(RAW, "CountyPop-CRN.dta")
) %>% as_tidytable()

setnames(CountyPop, 1:5, c("Year", "CountyCode", "Sex", "AgeGroup", "Population"))
CountyPop <- CountyPop[Year >= 1983]
CountyPop[, Sex := fifelse(Sex == "Females", "Women", "Men", na = NA_character_)]
CountyPop[, AgeGroup := CodeMap$age_map2(idx = AgeGroup)]

AvgPop <- copy(CountyPop) %>% 
  rename(AgeGroup5 = AgeGroup) %>% 
  group_by(Year, Sex, AgeGroup5) %>% 
  summarize(Population = sum(Population))

## -- Save Population Data ----------------
iwalk(
  list(CountyPop = CountyPop, Population = AvgPop), 
  function(dta, .name) {
    if (!file.exists(here(PROCESSED, glue::glue("{.name}.csv")))) {
      fwrite(
        file = here(PROCESSED, glue::glue("{.name}.csv")),
        x = dta, sep = ";", dec = ","
      )
    }
    
    if (!file.exists(here(PROCESSED, glue::glue("{.name}.rds")))) {
      saveRDS(dta, here(PROCESSED, glue::glue("{.name}.rds")))
    }
    
    if (!file.exists(here(PROCESSED, glue::glue("{.name}.dta")))) {
      write_dta(dta, here(PROCESSED, glue::glue("{.name}.dta")))
    }
  }
) 

## -- Ratetable (Life-Table) of mortality rate in Norway ----------------
## -- Copy original ratetable (dta file) to processed folder ----------------
norpop_raw <- haven::read_dta(here::here(RAW, "lifetable_norway_2021.dta")) %>% 
  rename(rate = qx) %>% 
  mutate(sex = sex + 1) %>% 
  mutate(sex = set_value_labels(sex, c(female = 1, male = 2))) %>% 
  arrange(`_year`, sex, `_age`)
haven::write_dta(norpop_raw, here::here(PROCESSED, "LifeTable.dta"))

norpop_df <- norpop_raw %>% 
  as_tidytable() %>% 
  modify_if(labelled::is.labelled, labelled::to_factor) %>% 
  modify_at("sex", stringr::str_remove, "s$") %>% 
  modify_at("sex", factor, levels = c("male", "female")) %>% 
  rename_with(~str_remove(.x, "^_"))

norpop <- popEpi::long_dt_to_ratetable(
  norpop_df,
  stratum.col.nms = c("age", "year", "sex"),
  value.col.nm = "rate",
  dim.types = c(2, 3, 1),
  cut.points = list(
    norpop_df[["age"]] %>% unique() %>% map_dbl(prod, 365.241),
    norpop_df[["year"]] %>% unique() %>% 
      map_chr(paste0, "-01-15") %>% 
      date::as.date(order = "ymd"),
    NULL
  )
)
attr(norpop, "dimid") <- names(dimnames(norpop)) <- c("age", "year", "sex")
attr(norpop, "factor") <- c(0, 0, 1)
dimnames(norpop)[["sex"]] <- c("female", "male")

## -- Save Ratetable and Ratetable data frame ----------------
saveRDS(norpop, here::here(PROCESSED, "RateTable.rds"))
saveRDS(norpop_df, here::here(PROCESSED, "LifeTable.rds"))
fwrite(norpop_df, here::here(PROCESSED, "LifeTable.csv"))



