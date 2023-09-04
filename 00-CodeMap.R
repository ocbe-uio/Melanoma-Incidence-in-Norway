CodeMap <- new.env()

evalq({
  BASE_PATH <- ifelse(
    Sys.getenv("ANALYSIS_ROOT") != "",
    Sys.getenv("ANALYSIS_ROOT"),
    dirname(file.path(getwd(), ".Rprofile"))
  )
  DATA_PATH <- ifelse(
    .Platform$OS.type == "windows", 
    "N:\\durable\\shared\\dataset", 
    file.path(BASE_PATH, "Data")
  )
  var_class <- function(key, value) {
    if (length(key) > 1) key <- deparse(substitute(key))
    
    ## Replace "" with NA in the value
    value[value == ""] <- NA
    
    fct_vars <- c(
      "Sex", "DiagSafety", "Localization", "Topography", "TopographyICDO3", "Histology",
      "MorphologyICDO3", "DiagBasis", "Metastasis", "Surgery", "Radiotherapy", "Side",
      "DeathCause", "TopographyICD10", "HealthRegion", "Status", "County", "Ulceration"
    )
    out <- fcase(
      key == "PID", list(as.integer),
      key == "SID", list(as.integer),
      key == "BirthYear", list(as.integer),
      key == "LymphNodeCount", list(as.numeric),
      key == "LymphNodeCountMet", list(as.numeric),
      key == "Mitosis", list(as.numeric),
      grepl(".*Date", key), list(function(x) as.Date(x, format = "%d.%m.%Y")),
      grepl("UlcerationNoReport.", key), list(as.integer),
      grepl("ULcerationYes.", key), list(as.integer),
      grepl("UlcerationNo.", key), list(as.integer),
      grepl("BreslowMissing.", key), list(as.integer),
      grepl("DiagYear.", key), list(as.integer),
      key %in% fct_vars, list(as.factor),
      default = list(function(x) x)
    )
    return(out[[1]](value))
  }
  factor_map <- function(value = NULL, key, missing_text = NA) {
    uord_fct <- list(
      "Sex" = c("Women", "Men"),
      "AnatomicSite" = c("Head and neck", "Upper limbs", "Trunk", "Lower limbs", "Other"),
      "Season" = c("Spring", "Summer", "Autumn", "Winter"),
      "MelanomaType" = c("Superficial spreading", "Nodular", "Lentigo maligna", "Other"),
      "Ulceration" = c("Absent", "Present"),
      "ClinicalStage" = c("Local", "Regional metastasis", "Distant metastasis"),
      "HealthRegion" = c("South-Eastern", "Western", "Central", "Northern")
    )
    ord_fct <- list(
      "AJCC" = c("I", "II", "III", "IV"),
      "Tstage0" = c(
        "is", "X", "0", "1", "1a", "1b", "2", "2a", "2b",
        "3", "3a", "3b", "4", "4a", "4b"
      ),
      "Nstage" = c("X", "0", "1", "1b", "2", "2a", "2b", "2c", "3"),
      "Mstage" = c("X", "0", "1", "1a", "1b", "1c"),
      "AgeGroup" = c("<20", "20-39", "40-59", "60-84", "85+"),
      "AgeGroup5" = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
        "60-64", "65-69", "70-74", "75-79", "80-84", "85+"
      ),
      "BreslowTstage" = c("T1", "T2", "T3", "T4"),
      "Tstage" = c("T1", "T2", "T3", "T4"),
      "BreslowTstage2" = c("[0,0.5]", "(0.5,0.8]", "(0.8,1]", "T2", "T3", "T4"),
      "Tstage2" = c("[0,0.5]", "(0.5,0.8]", "(0.8,1]", "T2", "T3", "T4"),
      "BreslowTSubStage" = c(
        "T1", "T1a", "T1b", "T2", "T2a", "T2b",
        "T3", "T3a", "T3b", "T4", "T4a", "T4b"
      ),
      "TSubStage" = c(
        "T1", "T1a", "T1b", "T2", "T2a", "T2b",
        "T3", "T3a", "T3b", "T4", "T4a", "T4b"
      ),
      "YearCat" = c("1983-1999", "2000-2007", "2008-2019"),
      "DiagYear10" = c("1983-1989", "1990-1999", "2000-2009", "2010-2019"),
      "DiagYear5" = c(
        "1983-1984", "1985-1989", "1990-1994", "1995-1999",
        "2000-2004", "2005-2009", "2010-2014", "2015-2019"
      )
    )
    if (missing(value)) {
      out <- append(uord_fct, ord_fct)[key]
      out <- unname(unlist(out))
      if (!is.na(missing_text)) out <- c(out, missing_text)
      return(out)
    }
    if (!is.null(ord_fct[[key]])) {
      if (is.null(value)) {
        return(ord_fct[[key]])
      }
      out <- factor(value, levels = ord_fct[[key]], ordered = TRUE)
    } else if (!is.null(uord_fct[[key]])) {
      if (is.null(value)) {
        return(uord_fct[[key]])
      }
      out <- factor(value, levels = uord_fct[[key]])
    } else {
      if (is.null(value)) {
        return(unique(value))
      }
      out <- factor(value)
    }
    if (!is.na(missing_text)) out <- forcats::fct_na_value_to_level(out, missing_text)
    return(out)
  }
  factor_map2 <- function(key, missing_label) {
    function(value, missing_text = missing_label) {
      factor_map(value, key, missing_text)
    }
  }
  sex_map <- function(x) {
    fcase(
      x %in% c("K", "Female"), "Women",
      x %in% c("M", "Male"), "Men",
      default = NA_character_
    )
  } # Sex Map
  morphology_map <- function(x) {
    # https://metadata.kreftregisteret.no/variables/detail/13?tabIndex=1
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "80003", "Neoplasm, malignant",
      x == "80103", "Carcinoma, NOS",
      x == "87203", "Malignant melanoma, NOS",
      x == "87213", "Nodular melanoma",
      x == "87223", "Balloon cell melanoma",
      x == "87233", "Malignant Melanoma, Regressing",
      x == "87270", "Dysplastic nevus",
      x == "87303", "Amelanotic melanoma",
      x == "87403", "Malignant melanoma in junctional nevus",
      x == "87413", "Malignant melanoma in precancerous melanosis",
      x == "87423", "Lentigo maligna melanoma",
      x == "87433", "Low cumulative sun damage melanoma",
      x == "87443", "Acral melanoma",
      x == "87453", "Desmoplastic melanoma, NOS",
      x == "87613", "Malignant melanoma arising in giant congenital nevus",
      x == "87703", "Malignant Spitz tumor",
      x == "87713", "Epithelioid cell melanoma",
      x == "87723", "Spindle cell melanoma, NOS",
      x == "87803", "Blue nevus, malignant",
      default = NA_character_
    )
  } # Morphology Map
  morphology_icdo3_map <- function(x) {
    # https://metadata.kreftregisteret.no/variables/detail/22?tabIndex=4
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    morphology_map(substr(x, 0, 5))
  }
  histology_map <- function(x) {
    map <- c(
      "80003" = "Unspecified", # Neoplasm, malignant
      "80103" = "Unspecified", # Carcinoma, NOS
      "87203" = "Unspecified", # Malignant melanoma, NOS
      "87213" = "Nodular", # Nodular melanoma
      "87223" = "Other", # Balloon cell melanoma
      "87233" = "Other", # Malignant Melanoma, Regressing
      "87303" = "Other", # Amelanotic melanoma
      "87403" = "Other", # Malignant melanoma in junctional nevus
      "87423" = "Lentigo maligna", # Lentigo maligna melanoma
      "87433" = "Superficial spreading", # Low cumulative sun damage melanoma
      "87443" = "Other", # Acral melanoma
      "87453" = "Other", # Desmoplastic melanoma, NOS
      "87613" = "Other", # Malignant melanoma arising in giant congenital nevus
      "87703" = "Other", # Malignant Spitz tumor
      "87713" = "Other", # Epithelioid cell melanoma
      "87723" = "Other", # Spindle cell melanoma, NOS
      "87803" = "Other" # Blue nevus, malignant
    )
    if (missing(x)) {
      # message("Argument required:\t", appendLF = FALSE)
      out <- as.data.table(map, keep.rownames = "code")
      out <- out[, .(codes = list(code)), by = map]
      out <- `names<-`(out[, codes], out[, map])
      return(out)
    }
    return(map[as.character(x)])
    # fcase(
    #   x == "80003", "Unspecified", # Neoplasm, malignant
    #   x == "80103", "Unspecified", # Carcinoma, NOS
    #   x == "87203", "Unspecified", # Malignant melanoma, NOS
    #   x == "87213", "Nodular", # Nodular melanoma
    #   x == "87223", "Other", # Balloon cell melanoma
    #   x == "87233", "Other", # Malignant Melanoma, Regressing
    #   x == "87303", "Other", # Amelanotic melanoma
    #   x == "87403", "Other", # Malignant melanoma in junctional nevus
    #   x == "87423", "Lentigo maligna", # Lentigo maligna melanoma
    #   x == "87433", "Superficial spreading", # Low cumulative sun damage melanoma
    #   x == "87443", "Other", # Acral melanoma
    #   x == "87453", "Other", # Desmoplastic melanoma, NOS
    #   x == "87613", "Other", # Malignant melanoma arising in giant congenital nevus
    #   x == "87703", "Other", # Malignant Spitz tumor
    #   x == "87713", "Other", # Epithelioid cell melanoma
    #   x == "87723", "Other", # Spindle cell melanoma, NOS
    #   x == "87803", "Other", # Blue nevus, malignant
    #   default = NA_character_
    # )
  } # Histology Map
  localization_map <- function(x) {
    # https://metadata.kreftregisteret.no/variables/detail/12?tabIndex=1
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "1900", "lip, eyelid, outer ear, face, scalp, neck, behind ear",
      x == "1901", "truncus, shoulder, hip",
      x == "1902", "upper limb",
      x == "1903", "foot below ankle",
      x == "1904", "lower limb on or above ankle",
      x == "1905", "perianal",
      x == "1906", "scrotum",
      x == "1907", "breast (women), aerola (men)",
      x == "1908", "under nail",
      x == "1909", "unspecified",
      default = NA_character_
    )
  } # Localization Map
  localization_class <- function(x) {
    # https://metadata.kreftregisteret.no/variables/detail/12?tabIndex=1
    map <- c(
      "1900" = "Head and neck", # lip, eyelid, outer ear, face, scalp, neck, behind ear
      "1901" = "Trunk", # truncus, shoulder, hip
      "1902" = "Upper limbs", # upper limb
      "1903" = "Lower limbs", # foot below ankle
      "1904" = "Lower limbs", # lower limb on or above ankle
      "1905" = "Other", # perianal
      "1906" = "Other", # scrotum
      "1907" = "Trunk", # breast (women), aerola (men)
      "1908" = "Other", # under nail
      "1909" = NA_character_ # unspecified
    )
    if (missing(x)) {
      # message("Argument required:\t", appendLF = FALSE)
      out <- as.data.table(map, keep.rownames = "code")
      out <- out[, .(codes = list(code)), by = map]
      out <- `names<-`(out[, codes], out[, map])
      return(out)
    }
    return(map[as.character(x)])
    # fcase(
    #   x == "1900", "Head and neck", # lip, eyelid, outer ear, face, scalp, neck, behind ear
    #   x == "1901", "Trunk", # truncus, shoulder, hip
    #   x == "1902", "Upper limbs", # upper limb
    #   x == "1903", "Lower limbs", # foot below ankle
    #   x == "1904", "Lower limbs", # lower limb on or above ankle
    #   x == "1905", "Other", # perianal
    #   x == "1906", "Other", # scrotum
    #   x == "1907", "Trunk", # breast (women), aerola (men)
    #   x == "1908", "Other", # under nail
    #   x == "1909", NA_character_, # unspecified
    #   default = NA_character_
    # )
  } # Localization Major Class
  topo_icdo3_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "440", "Skin lip, NOS",
      x == "441", "Eyelid NOS",
      x == "442", "External ear",
      x == "443", "Skin face",
      x == "444", "Skin scalp, neck",
      x == "445", "Skin trunk",
      x == "446", "Skin limb, upper",
      x == "447", "Skin limb, lower",
      x == "448", "Overlapping lesion of skin",
      x == "449", "Skin, NOS",
      x == "632", "Scrotum, NOS",
      x == "809", "Unknown primary site",
      default = NA_character_
    )
  } # TopographyICDO3
  diag_basis_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "0", "Clinical examination (outside of hospital) without additional examinations",
      x == "10", "Clinical examination in hospital without additional examiniations",
      x == "20", "Diagnostic imaging (X-ray, CT, MR, ultrasound)",
      x == "22", "Clinical notification about cytological examination",
      x == "23", "Radiotherapy and patient administrative data and/o…eath certificate and patient administrative data.",
      x == "29", "Prostate-spesific antigen (PSA) test",
      x == "30", "Biochemical analysis, electrophoresis",
      x == "31", "Endoscopy (including ERCP)",
      x == "32", "Cytological examination of primary tumor",
      x == "33", "Blood smear",
      x == "34", "Bone marrow aspiration (sternal puncture)",
      x == "35", "Spinal liquid examination",
      x == "36", "Cytology from metastasis",
      x == "37", "Cytology from local recurrence of primary tumor",
      x == "38", "Cytology with immunophenotyping, immunocytochemistry or cytogenetics",
      x == "39", "Cytology (unknown from primary tumor or metastasis)",
      x == "40", "Surgery without histology",
      x == "41", "Autopsy without histology",
      x == "45", "Ploidy analysis, flow cytometry or diagnostic imaging",
      x == "46", "Hormonreceptor analysis of primary tumor",
      x == "47", "Molecular genetic analysis of primary tumor",
      x == "57", "Histology from locoregional recurrence of primary tumor",
      x == "60", "Histology from metastasis",
      x == "68", "Histology from metastasis + autopsy",
      x == "70", "Histology from primary tumor",
      x == "71", "Automatic aggregated value",
      x == "72", "Clinical notification about histological examination",
      x == "74", "Histological examination using electron microscope",
      x == "75", "Histological examination with immunophenotyping",
      x == "76", "Histological examination with cytogenetics, molecular genetic examination",
      x == "78", "Histology from primary tumor and autopsy. Automatic aggregated value",
      x == "79", "Histology, unknown from primary tumor or metastasis",
      x == "80", "Autopsy with histological examination",
      x == "81", "Incidental finding at autopsy with histology",
      x == "82", "Autopsy, partial",
      x == "83", "Clinical notification about autopsy",
      x == "84", "Autopsy without cancer tissue",
      x == "90", "Death certificate",
      x == "98", "Histology or cytology without cancer tissue",
      x == "99", "Unknown basis of diagnostics",
      default = NA_character_
    )
  } # Basis of Diagnosis
  ClinicalStageMap <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "0", "Local", # No metastasis. Metastasis within same organ as primary tumor
      x == "1", "Regional metastasis", # Metastasis to regional lymph nodes
      x == "2", "Distant metastasis", # Metastasis to distant lymph nodes
      x == "3", "Regional metastasis", # Metastasis to organ in the same body region as primary tumor
      x == "4", "Distant metastasis", # Metastasis to organ to another body region than primary tumor
      x == "5", "Regional metastasis", # Microscopic tumor growth into adjacent tissue
      x == "6", "Regional metastasis", # Macroscopic tumor growth into adjacent tissue
      x == "7", NA_character_, # Metastasis found, primary tumor unknown
      x == "8", "Local", # Microscopically infiltrating tumor
      x == "9", NA_character_, # Unknown metastasis at time of diagnosis
      x == "A", "Regional metastasis", # Regional lymph node metastasis (clinically or histologically)
      x == "B", "Distant metastasis", # Distant lymph node metastasis or organ metastasis
      x == "C", NA_character_, # Metastasis found, primary tumor unknown
      x == "D", "Regional metastasis", # Direct growth into adjacent tissue or organ
      default = NA_character_
    )
  } # Clinical Stage Map
  MetastasisMap <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "0", "No metastasis. Metastasis within same organ as primary tumor",
      x == "1", "Metastasis to regional lymph nodes",
      x == "2", "Metastasis to distant lymph nodes",
      x == "3", "Metastasis to organ in the same body region as primary tumor",
      x == "4", "Metastasis to organ to another body region than primary tumor",
      x == "5", "Microscopic tumor growth into adjacent tissue",
      x == "6", "Macroscopic tumor growth into adjacent tissue",
      x == "7", "Metastasis found, primary tumor unknown",
      x == "8", "Microscopically infiltrating tumor",
      x == "9", "Unknown metastasis at time of diagnosis",
      x == "A", "Regional lymph node metastasis (clinically or histologically)",
      x == "B", "Distant lymph node metastasis or organ metastasis",
      x == "C", "Metastasis found, primary tumor unknown",
      x == "D", "Direct growth into adjacent tissue or organ",
      default = NA_character_
    )
  } # Metastasis
  surgery_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "0", "No surgery",
      x == "1", "Biopsy",
      x == "2", "Surgical exploration with or without biopsy",
      x == "3", "Excision biopsy",
      x == "7", "Sentinel node",
      x == "9", "Local ablative treatment with or without biopsy",
      x == "10", "Surgical removal of tumor and parts of or the entire organ(s)",
      x == "11", "Surgical removal of only tumor",
      x == "12", "Surgical removal of lymph nodes",
      x == "13", "Transvesical resection",
      x == "14", "Larger specialised surgical procedures",
      x == "15", "Mastectomy with removal of lymph nodes",
      x == "16", "Mastectomy - unknown if lymph nodes are removed",
      x == "17", "Breast-conserving surgery without removal of lymph nodes",
      x == "18", "Breast-conserving surgery with removal of lymph nodes",
      x == "19", "Breast-conserving surgery, unknown if lymph nodes are removed",
      x == "20", "Specialised surgical procedures",
      x == "21", "Therapeutic intervention directed at metastasis",
      x == "25", "Mastectomy with removal of sentinel lymph node",
      x == "26", "Mastectomy with removal of sentinel lymph node\nand removal of lymph nodes",
      x == "28", "Breast-conserving surgery with removal of sentinel lymph node",
      x == "29", "Breast-conserving surgery with removal of sentinel lymph node and removal of lymph nodes",
      x == "30", "Anastomosis, drainage procedure; ventriculostomy, thracheostomy, gastrostomy, gastro-enterostomy, colostomy, coecostomy, transversostomy, sigmoideostomy, cholecystostomy, cystostomy, nephrostomy ",
      x == "35", "Re-excision",
      x == "40", "Palliative procedure",
      x == "43", "Prostate cancer primarily treated with hormonal treatment and then recessed transvesically",
      x == "50", "Transurethral resection of prostate",
      x == "95", "Biopsy from metastasis, local recurrence or non-classified tumor",
      x == "96", "Cytology",
      x == "97", "Therapeutic intervention directed at metastasis/palliative care",
      x == "98", "Surgical procedure NOS",
      x == "99", "No information about surgical procedure",
      default = NA_character_
    )
  } # Surgery
  radiotherapy_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "0", "Not treated with radiotherapy",
      x == "1", "Received radiotherapy",
      x == "2", "Treated with gamma knife",
      x == "3", "Treated with radioactive iodine",
      x == "9", "Unknown",
      x == "12", "Received radiotherapy. The Cancer Registry of Norw…otification specifying treatment with gamma knife",
      x == "13", "Received radiotherapy. The Cancer Registry of Norw…tion specifying treatment with radioactive iodine",
      default = NA_character_
    )
  } # Radiotherapy
  topo_icd10_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "C430", "Malignant melanoma of lip",
      x == "C431", "Malignant melanoma of eyelid, including canthus",
      x == "C432", "Malignant melanoma of ear and external auricular canal",
      x == "C433", "Malignant melanoma of other and unspecified parts of face",
      x == "C434", "Malignant melanoma of scalp and neck",
      x == "C435", "Malignant melanoma of trunk",
      x == "C436", "Malignant melanoma of upper limb, including shoulder",
      x == "C437", "Malignant melanoma of lower limb, including hip",
      x == "C438", "Overlapping malignant melanoma of skin",
      x == "C439", "Malignant melanoma of skin, unspecified",
      x == "C440", "Malignant neoplasm of skin of lip",
      x == "C441", "Malignant neoplasm of skin of eyelid, including canthus",
      x == "C442", "Malignant neoplasm of skin of ear and external auricular canal",
      x == "C443", "Malignant neoplasm of skin of other and unspecified parts of face",
      x == "C444", "Malignant neoplasm of skin of scalp and neck",
      x == "C445", "Malignant neoplasm of skin of trunk",
      x == "C446", "Malignant neoplasm of skin of upper limb, including shoulder",
      x == "C447", "Malignant neoplasm of skin of lower limb, including hip",
      x == "C448", "Malignant neoplasm of overlapping sites of skin",
      x == "C449", "Malignant neoplasm of skin, unspecified"
    )
  } # TopographyICD10
  status_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "1", "Alive",
      x == "2", "Dead",
      x == "3", "Lost to follow-up",
      x == "4", "Unknown",
      default = NA_character_
    )
  } # Persons Status
  status_map2 <- function(status, death_cause) {
    if (missing(status) | missing(death_cause)) {
      message("Both status and casue of status (death) is required.")
      return(NULL)
    }
    fcase(
      grepl("[dD]ead", status) & grepl("172|[cC]43", death_cause), "Melanoma Death",
      grepl("[dD]ead", status) & !grepl("172|[cC]43", death_cause), "Other Death",
      grepl("[lL]ost", status), "Lost to follow-up",
      grepl("Alive", status), "Alive",
      default = NA_character_
    )
  } # status with melanoma specific death and other death
  county_map <- function(x) {
    x <- formatC(as.numeric(as.character(x)), flag = "0", width = 2)
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "01", "Østfold",
      x == "02", "Akershus",
      x == "03", "Oslo",
      x == "04", "Hedmark",
      x == "05", "Oppland",
      x == "06", "Buskerud",
      x == "07", "Vestfold",
      x == "08", "Telemark",
      x == "09", "Aust-Agder",
      x == "10", "Vest-Agder",
      x == "11", "Rogaland",
      x == "12", "Hordaland",
      x == "13", "Bergen",
      x == "14", "Sogn og Fjordane",
      x == "15", "Møre og Romsdal",
      x == "16", "Sør-Trøndelag",
      x == "17", "Nord-Trøndelag",
      x == "18", "Nordland",
      x == "19", "Troms",
      x == "20", "Finnmark",
      x == "30", "Viken",
      x == "34", "Innlandet",
      x == "38", "Vestfold og Telemark",
      x == "42", "Agder",
      x == "46", "Vestland",
      x == "50", "Trøndelag",
      x == "54", "Troms og Finnmark",
      x == "99", "Unknown",
      default = NA_character_
    )
  } # County
  diag_safety_map <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "3", "Malignant cancer, confirmed localization",
      x == "6", "Malignant solid tumor with uncertain origin",
      x == "4", "Malignant cancer, confirmed localization for patie…4 months before the malignant cancer is diagnosed",
      x == "1", "Benign/pre-malignant tumor, confirmed localization",
      x == "0", "Tumor of uncertain malignity and uncertain localization",
      x == "5", "Clinically malignant cancer, not histologically verified",
      x == "2", "Malignant cancer, unconfirmed localization",
      x == "7", "Histology/cytology/death report about tumor with u…localization before clinical report is registered",
      default = NA_character_
    )
  } # Diagnosis Safety
  HealthRegion <- function(x) {
    if (is.null(x)) {
      message("HealthRegion not found:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "East", "South-Eastern",
      x == "Middle", "Western",
      x == "North", "Northern",
      x == "West", "Central",
      default = NA_character_
    )
  } # Health Region
  age_map <- function(age, from = c("numeric", "5years")) {
    if (missing(age)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    from <- match.arg(from)
    if (from == "numeric") {
      return(
        fcase(
          age %between% c(85, Inf), "85+",
          age %between% c(60, 84), "60-84",
          age %between% c(40, 59), "40-59",
          age %between% c(20, 39), "20-39",
          age %between% c(0, 19), "<20",
          default = NA_character_
        )
      )
    }
    if (from == "5years") {
      return(fcase(
        age == "85+", "85+",
        age %in% c("60-64", "65-69", "70-74", "75-79", "80-84"), "60-84",
        age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59",
        age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39",
        age %in% c("0-4", "5-9", "10-14", "15-19"), "<20",
        default = NA_character_
      ))
    }
  } # Age
  age_map2 <- function(age = NULL, idx = NULL, step = 5) {
    if (is.null(age) & is.null(idx)) {
      message("Either Age or age group index is required:\t", appendLF = FALSE)
      return(NULL)
    }
    splits <- seq(0, ifelse(step %% 10 == 0, 80, 85), by = step)
    groups <- c(paste(head(splits, -1), tail(splits, -1) - 1, sep = "-"), paste0(max(splits), "+"))
    if (!is.null(idx)) {
      out <- factor(groups[idx], levels = groups, ordered = TRUE)
      return(out)
    } else {
      out <- cut(age, c(splits, Inf), include.lowest = TRUE, right = FALSE, ordered_result = TRUE, labels = groups)
      return(out)
    }
  } # 5 Years AgeGroup
  age_map3 <- function(age, from = c("numeric", "5years")) {
    if (missing(age)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    from <- match.arg(from)
    if (from == "5years") {
      out <- fcase(
        age %in% c("75-79", "80-84", "85+"), "75+",
        age %in% c("65-69", "70-74"), "65-74",
        age %in% c("55-59", "60-64"), "55-64",
        age %in% c("45-49", "50-54"), "45-54",
        age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44"), "15-44",
        age %in% c("0-4", "5-9", "10-14"), "0-14",
        default = NA_character_
      )
    }
    if (from == "numeric") {
      out <- fcase(
        age < 15, "0-14",
        age %between% c(15, 44), "15-44",
        age %between% c(45, 54), "45-54",
        age %between% c(55, 64), "55-64",
        age %between% c(65, 74), "65-74",
        age > 74, "75+",
        default = NA_character_
      )
    }
    out <- factor(
      x = out, 
      levels = c("0-14", "15-44", "45-54", "55-64", "65-74", "75+"),
      ordered = TRUE
    )
    return(out)
  } # icss age-group
  age5210 <- function(age5) {
    out <- age5 %>%
      str_replace("4$", "9") %>%
      str_replace("5-", "0-") %>%
      str_replace("80-89", "80+") %>%
      str_replace("85", "80")
    return(out)
  }
  season <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    months <- format(x, "%m")
    fcase(
      months %in% c("12", "01", "02"), "Winter",
      months %in% c("03", "04", "05"), "Spring",
      months %in% c("06", "07", "08"), "Summer",
      months %in% c("09", "10", "11"), "Autumn",
      default = NA_character_
    )
  } # Season Map
  year_range <- function(start, end, step, include.lowest = TRUE, right = FALSE, ordered_result = FALSE, ...) {
    function(year) {
      year <- as.numeric(as.character(year))
      cut_points <- seq(start, end, step)
      # if (min(year) < min(cut_points)) cut_points <- c(min(year), cut_points)
      # if (min(year) > min(cut_points)) cut_points[1] <- year[1]
      # if (max(year) > max(cut_points)) cut_points <- c(cut_points, max(year))
      if (min(year) < min(cut_points)) cut_points <- c(min(cut_points) - step, cut_points)
      if (max(year) > max(cut_points)) cut_points <- c(cut_points, max(cut_points) + step)
      ret <- base::cut(year, cut_points, dig.lab = 10, include.lowest = include.lowest, right = right, ...)
      cp_df <- data.frame(
        lower = as.numeric(sub("[\\[\\(](.*),.*", "\\1", ret)),
        upper = as.numeric(sub(".*,(.+)[])]", "\\1", ret))
      )
      if (right) {
        cp_df$lower <- cp_df$lower + 1
        if (include.lowest) {
          cp_df$lower[cp_df$lower == min(cp_df$lower)] <- cp_df$lower[cp_df$lower == min(cp_df$lower)] - 1
        }
      } else {
        cp_df[cp_df$upper != max(cp_df$upper), "upper"] <- cp_df[cp_df$upper != max(cp_df$upper), "upper"] - 1
      }
      out <- with(cp_df, paste(lower, upper, sep = "-"))
      out <- factor(out, ordered = ordered_result)
      return(out)
    }
  }
  year_10 <- year_range(1980, 2020, 10, include.lowest = TRUE, right = FALSE, ordered_result = TRUE) # 10 years period
  year_5 <- year_range(1980, 2020, 5, include.lowest = TRUE, right = FALSE, ordered_result = TRUE) # 5 years period
  year_cat <- function(DiagYear) {
    if (missing(DiagYear)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    out <- fcase(
      DiagYear < 2000, "1983-1999",
      DiagYear >= 2000 & DiagYear < 2008, "2000-2007",
      DiagYear >= 2008, "2008-2019",
      default = NA_character_
    )
    factor(out, c("1983-1999", "2000-2007", "2008-2019"), ordered = TRUE)
  } # 3 categories for diag year
  uvr_map <- function(CountyCode) {
    if (missing(CountyCode)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      CountyCode == 50, "Central (Medium-low)",
      CountyCode >= 18, "Northern (Low)",
      CountyCode >= 15 & CountyCode < 18, "Central (Medium-low)",
      CountyCode >= 11 & CountyCode < 15, "South-West (Medium)",
      CountyCode < 11, "South-East (Highest)",
      default = NA_character_
    )
  } # UVR Region
  breslow_pattern <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "range", ".+til.+|[mM]ellom|-|.\\>.+\\>.|.\\<.+\\<.",
      x == "less", "^\\<",
      x == "greater", "^\\>|over|^min",
      x == "unknown", "[xX]|^,$|\\.m|\\.v",
      x == "missing", NA_character_,
      x == "multiple", ".+og.+|\\d\\d?, \\d,?\\d?",
      x == "numeric", "^\\d+$|^\\d\\.\\d$|^\\d\\,\\d$"
    )
  } # Breslow pattern
  breslow_split_pattern <- function(x) {
    if (missing(x)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      x == "range", "\\s*til\\s*|-|[Mm]ellom|og|\\<.+\\<",
      x == "multiple", ", |og"
    )
  } # Splitting pattern for breslow
  get_Tstage <- function(Breslow) {
    if (missing(Breslow)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      Breslow <= 1.0, "T1",
      Breslow > 1.0 & Breslow <= 2.0, "T2",
      Breslow > 2.0 & Breslow <= 4.0, "T3",
      Breslow > 4.0, "T4",
      default = NA_character_
    )
  }
  get_Tstage2 <- function(Breslow) {
    if (missing(Breslow)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    fcase(
      Breslow <= 0.5, "[0,0.5]",
      Breslow > 0.5 & Breslow <= 0.8, "(0.5,0.8]",
      Breslow > 0.8 & Breslow <= 1.0, "(0.8,1]",
      Breslow > 1.0 & Breslow <= 2.0, "T2",
      Breslow > 2.0 & Breslow <= 4.0, "T3",
      Breslow > 4.0, "T4",
      default = NA_character_
    )
  }
  get_Tstage_label <- function(Tstage) {
    if (missing(Tstage)) {
      message("Argument required:\t", appendLF = FALSE)
      return(NULL)
    }
    new_label <- c(
      "[0,0.5]" = paste0("0.0", "\U2013", "0.5 mm"),
      "(0.5,0.8]" = paste0(">0.5", "\U2013", "0.8 mm"),
      "(0.8,1]" = paste0(">0.8", "\U2013", "1 mm"),
      "(0.8,1)" = paste0(">0.8", "\U2013", "<1 mm"),
      "T1" = paste0("T1 (\U2264", "1.0 mm)"),
      "T1a" = "T1a",
      "T1b" = "T1b",
      "T2" = paste0("T2 (>1.0", "\U2013", "2.0 mm)"),
      "T3" = paste0("T3 (>2.0", "\U2013", "4.0 mm)"),
      "T4" = paste0("T4 (>4.0 mm)"),
      "Unspecified" = "Unspecified"
    )
    get_new_label <- function(x, map = new_label) {
      if (is.factor(x)) {
        y <- levels(x)
        y[y %in% names(map)] <- map[y[y %in% names(map)]]
        levels(x) <- y
      } else {
        x[x %in% names(map)] <- map[x[x %in% names(map)]]
      }
      return(x)
    }
    
    if (is.factor(Tstage)) {
      out <-  factor(
        get_new_label(Tstage),
        levels = get_new_label(levels(Tstage)),
        ordered = is.ordered(Tstage)
      )
    } else {
      out <- get_new_label(Tstage)
    }
    return(out)
  }
  std_pop <- function(x) {
    std_pop <- popEpi::stdpop18 %>%
      data.table() %>%
      setnames(names(.), c("AgeGroup5", "World", "Europe", "Nordic")) %>%
      .[, AgeGroup5 := age_map2(idx = 1:18)] %>%
      .[, Norway := c(
        6039, 6102, 5993, 6349, 6681, 6770, 6690, 6670, 7296,
        7207, 6492, 6108, 5575, 5369, 3702, 2663, 2063, 2231
      )] %>%
      .[, US2000 := c(
        6913, 7253, 7303, 7217, 6648, 6453, 7105, 8076, 8185,
        7212, 6272, 4846, 3879, 3426, 3177, 2700, 1784, 1551
      )]
    if (missing(x)) {
      return(std_pop[])
    } else {
      x <- match.arg(x, c("world", "europe", "nordic", "norway", "us2000"))
      out <- std_pop[, c(
        "AgeGroup5",
        fifelse(x == "us2000", toupper(x), stringr::str_to_title(x))
      ), with = FALSE]
      setnames(out, 2, "StdPop")
      attr(out, "StdPop") <- stringr::str_to_title(x)
      return(out[])
    }
  }
  icss_wt <- function(age_group = NULL, which = 2, type = c("default", "5-year"), prop = TRUE) {
    type <- match.arg(type)
    if (type == "5-year") {
      out <- popEpi::ICSS
      out[, AgeGroup := paste0(age, "-", shift(age, -1) - 1)]
      out[, AgeGroup := gsub("-NA", "+", AgeGroup)]
      setcolorder(out, "AgeGroup")
    } else {
      out <- data.table(
        AgeGroup = c("0-14", "15-44", "45-54", "55-64", "65-74", "75+"),
        ICSS1 = c(0, 7000, 12000, 23000, 29000, 29000), 
        ICSS2 = c(0, 28000, 17000, 21000, 20000, 14000), 
        ICSS3 = c(0, 60000, 10000, 10000, 10000, 10000)
      )
    }
    if (prop) {
      out[, c(2:4) := lapply(.SD, `/`, 100000), .SDcols = c(2:4)]
    }
    if (!is.null(which)) {
      out <- out[, c(1, which + 1), with = FALSE]
    }
    if (!is.null(age_group)) {
      out <- out[match(age_group, AgeGroup)]
      if (length(age_group) == 1) {
        out <- out[[-1]]
      }
    }
    return(out)
  }
}, envir = CodeMap)
