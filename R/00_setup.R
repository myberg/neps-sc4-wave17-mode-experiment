# ===
# Analysis setup
# Loads packages, paths, functions, and the prepared analysis data.
# This file is called automatically by 00_run_analysis.R.
# ===

# repository paths ---------------------------------------------------------

path.exp <- normalizePath(".", winslash = "/", mustWork = TRUE)
path.save <- file.path("output")
prepared_file <- file.path("data", "derived", "AuswertungB160.RData")

dir.create(path.save, recursive = TRUE, showWarnings = FALSE)

# packages ----------------------------------------------------------------

packages <- c(
  "tidyverse",
  "readstata13",
  "janitor",
  "knitr",
  "kableExtra",
  "modelsummary",
  "sandwich",
  "marginaleffects",
  "lmtest",
  "broom",
  "nnet",
  "car",
  "scales"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing R packages: ",
    paste(missing_packages, collapse = ", "),
    "."
  )
}

suppressPackageStartupMessages(
  invisible(lapply(packages, library, character.only = TRUE))
)

# functions ---------------------------------------------------------------

source(file.path("R", "00_functions.R"))

# Colours used in manuscript figures
col <- list(
  magenta = "#e6007e",
  lightblue = "#009fe3",
  green = "#157536",
  orange = "#D55E00"
)

# load prepared analysis data ---------------------------------------------

if (!file.exists(prepared_file)) {
  stop(
    "Prepared analysis data not found: ", prepared_file, "\n",
    "Individual-level data are not included in this repository. ",
    "See data/README.md."
  )
}

load(prepared_file)

if (!exists("AuswertungB160")) {
  stop("The prepared data file must contain an object named `AuswertungB160`.")
}

# variables used across analyses ------------------------------------------

AuswertungB160.deskriptiv <- AuswertungB160 %>%
  mutate(
    female = if_else(Gender_suf %in% "weiblich", 1, 0, missing = NA_real_),
    male = if_else(Gender_suf %in% "männlich", 1, 0, missing = NA_real_),

    age_lower = if_else(Alter_suf %in% "1994 and earlier", 1, 0, missing = NA_real_),
    age = if_else(Alter_suf %in% "1995", 1, 0, missing = NA_real_),
    age_higher = if_else(Alter_suf %in% "1996 and later", 1, 0, missing = NA_real_),
    age_unknown = if_else(Alter_suf %in% "unbekannt", 1, 0, missing = NA_real_),

    east = if_else(Region_suf %in% "Ostdeutschland inkl. Berlin", 1, 0, missing = NA_real_),
    west = if_else(Region_suf %in% "Westdeutschland", 1, 0, missing = NA_real_),

    migration_background_no = if_else(migBG_suf %in% "kein migBG", 1, 0, missing = NA_real_),
    migration_background_yes = if_else(migBG_suf %in% "migBG", 1, 0, missing = NA_real_),
    migration_background_unknown = if_else(migBG_suf %in% "unbekannt", 1, 0, missing = NA_real_),

    CASMIN_lower = if_else(CASMIN_suf %in% "1abc2ab", 1, 0, missing = NA_real_),
    CASMIN_middle = if_else(CASMIN_suf %in% "2c", 1, 0, missing = NA_real_),
    CASMIN_higher = if_else(CASMIN_suf %in% "3ab", 1, 0, missing = NA_real_),
    CASMIN_unknown = if_else(CASMIN_suf %in% "unbekannt", 1, 0, missing = NA_real_),

    ISCED_lower = if_else(ISCED_suf %in% "0-2", 1, 0, missing = NA_real_),
    ISCED_middle = if_else(ISCED_suf %in% "3-4", 1, 0, missing = NA_real_),
    ISCED_higher = if_else(ISCED_suf %in% "5-6", 1, 0, missing = NA_real_),
    ISCED_unknown = if_else(ISCED_suf %in% "unbekannt", 1, 0, missing = NA_real_),

    valid_comp_w10_valid = if_else(valid_comp_w10 %in% 1, 1, 0, missing = NA_real_),
    valid_comp_w10_novalid = if_else(valid_comp_w10 %in% 0, 1, 0, missing = NA_real_),
    valid_comp_w14_valid = if_else(valid_comp_w14 %in% 1, 1, 0, missing = NA_real_),
    valid_comp_w14_novalid = if_else(valid_comp_w14 %in% 0, 1, 0, missing = NA_real_),

    temporary_dropout = if_else(status.16 %in% "TDO", 1, 0, missing = NA_real_),
    participate = if_else(status.16 %in% "Part", 1, 0, missing = NA_real_),

    prior_participation_low = if_else(part_hist %in% "12 or less", 1, 0, missing = NA_real_),
    prior_participation_middle = if_else(part_hist %in% "13-14", 1, 0, missing = NA_real_),
    prior_participation_high = if_else(part_hist %in% "15-16", 1, 0, missing = NA_real_)
  )

vars <- tibble::tribble(
  ~var, ~label,
  "female", "Female",
  "male", "Male",
  "age_lower", "Older age (1994 and earlier)",
  "age", "Birth cohort 1995",
  "age_higher", "Younger age (1996 and later)",
  "age_unknown", "Age (unknown)",
  "east", "East Germany",
  "west", "West Germany",
  "migration_background_no", "Migration background (no)",
  "migration_background_yes", "Migration background (yes)",
  "migration_background_unknown", "Migration background (unknown)",
  "CASMIN_lower", "Lower education (CASMIN 1a--2b)",
  "CASMIN_middle", "Higher secondary education (CASMIN 2c)",
  "CASMIN_higher", "Tertiary education (CASMIN 3a--3b)",
  "CASMIN_unknown", "CASMIN unknown",
  "ISCED_lower", "Lower education (ISCED 0--2)",
  "ISCED_middle", "Medium education (ISCED 3--4)",
  "ISCED_higher", "Higher education (ISCED 5--6)",
  "ISCED_unknown", "ISCED unknown",
  "valid_comp_w10_valid", "Valid competence data in Wave 10",
  "valid_comp_w10_novalid", "No valid competence data in Wave 10",
  "valid_comp_w14_valid", "Valid competence data in Wave 14",
  "valid_comp_w14_novalid", "No valid competence data in Wave 14",
  "temporary_dropout", "Temporary dropout",
  "participate", "Repeat respondent",
  "prior_participation_low", "12 or fewer prior participations",
  "prior_participation_middle", "13--14 prior participations",
  "prior_participation_high", "15--16 prior participations"
)
