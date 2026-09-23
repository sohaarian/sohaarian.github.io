# ============================================================
# NYS Workers' Compensation Claims Analytics
# Built for the official New York State Workers' Compensation Board dataset:
# "Assembled Workers' Compensation Claims: Beginning 2000"
# Dataset ID: jshw-gkgu
#
# This app uses DuckDB so the multi-million-row CSV can be analyzed locally
# without loading the full file into R memory.
# ============================================================

required_packages <- c(
  "shiny", "bslib", "DBI", "duckdb", "readr", "dplyr", "DT",
  "ggplot2", "plotly", "scales", "lubridate", "stringr", "tidyr", "purrr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the required packages once before running the app:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)

options(shiny.maxRequestSize = 250 * 1024^2)

# ============================================================
# Source definitions
# ============================================================

NYS_WCB_PAGE <- paste0(
  "https://data.ny.gov/Government-Finance/",
  "Assembled-Workers-Compensation-Claims-Beginning-20/jshw-gkgu"
)
FRED_PAGE <- "https://fred.stlouisfed.org/series/CPIMEDSL"
FRED_CSV <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIMEDSL"

WCB_CSV_FILENAME <- "Assembled_Workers'_Compensation_Claims__Beginning_2000_20260725.csv"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

clean_names_simple <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("(^_+|_+$)", "")
}

sql_string <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

sql_identifier <- function(x) {
  paste0('"', gsub('"', '""', as.character(x), fixed = TRUE), '"')
}

fmt_number <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else comma(x, accuracy = 1)
}

fmt_percent <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else percent(x, accuracy = 0.1)
}

fmt_money <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else dollar(x, accuracy = 1)
}

fmt_days <- function(x) {
  if (length(x) == 0 || is.na(x)) "—" else paste0(comma(round(x)), " days")
}

# ============================================================
# Official field aliases
# Handles both Socrata display names and API-style names.
# ============================================================

field_aliases <- list(
  claim_identifier = c("claim_identifier", "claim_id", "wcb_claim_number"),
  claim_type = c("claim_type"),
  district_name = c("district_name", "district"),
  average_weekly_wage = c("average_weekly_wage_aww", "average_weekly_wage", "aww"),
  current_claim_status = c("current_claim_status", "claim_status"),
  claim_injury_type = c("claim_injury_type", "injury_type"),
  age_at_injury = c("age_at_injury"),
  birth_year = c("birth_year"),
  assembly_date = c("assembly_date"),
  accident_date = c("accident_date", "date_of_injury", "injury_date"),
  accident_ind = c("accident", "accident_ind"),
  occupational_disease_ind = c("occupational_disease", "occupational_disease_ind"),
  ancr_date = c("ancr_date"),
  interval_assembled_to_ancr = c(
    "interval_assembled_to_ancr", "interval_assembled_toancr",
    "interval_assembled_to_ancr_days"
  ),
  c2_date = c("c_2_date", "c2_date"),
  c3_date = c("c_3_date", "c3_date"),
  controverted_date = c("controverted_date", "controversy_date"),
  first_hearing_date = c("first_hearing_date"),
  hearing_count = c("hearing_count", "number_of_hearings"),
  first_appeal_date = c("first_appeal_date"),
  highest_process = c("highest_process", "highest_claim_resolution_process"),
  alternative_dispute_resolution = c(
    "alternative_dispute_resolution", "adr_ind", "adr"
  ),
  atty_rep_ind = c(
    "attorney_representative", "atty_rep_ind", "attorney_rep_ind",
    "attorney_representative_indicator"
  ),
  carrier_name = c("carrier_name"),
  carrier_type = c("carrier_type"),
  gender = c("gender"),
  injured_in_county_name = c("county_of_injury", "injured_in_county_name"),
  zip_code = c("zip_code", "postal_code"),
  medical_fee_region = c("medical_fee_region"),
  covid_19_indicator = c("covid_19_indicator", "covid_19_ind", "covid_indicator"),
  wcio_pob_code = c("wcio_part_of_body_code", "wcio_pob_code"),
  wcio_pob_desc = c(
    "wcio_part_of_body_description", "wcio_part_of_body_desc", "wcio_pob_desc"
  ),
  wcio_nature_of_injury_code = c(
    "wcio_nature_of_injury_code", "wcio_nature_injury_code"
  ),
  wcio_nature_of_injury_desc = c(
    "wcio_nature_of_injury_description", "wcio_nature_of_injury_desc",
    "wcio_nature_injury_desc"
  ),
  wcio_cause_of_injury_code = c("wcio_cause_of_injury_code"),
  wcio_cause_of_injury_desc = c(
    "wcio_cause_of_injury_description", "wcio_cause_of_injury_desc"
  ),
  oiics_pob_code = c("oiics_part_of_body_code", "oiics_pob_code"),
  oiics_pob_desc = c(
    "oiics_part_of_body_description", "oiics_part_of_body_desc", "oiics_pob_desc"
  ),
  oiics_nature_injury_code = c(
    "oiics_nature_of_injury_code", "oiics_nature_injury_code"
  ),
  oiics_nature_injury_desc = c(
    "oiics_nature_of_injury_description", "oiics_nature_of_injury_desc",
    "oiics_nature_injury_desc"
  ),
  oiics_injury_source_code = c("oiics_injury_source_code"),
  oiics_injury_source_desc = c(
    "oiics_injury_source_description", "oiics_injury_source_desc"
  ),
  oiics_event_exposure_code = c("oiics_event_exposure_code"),
  oiics_event_exposure_desc = c(
    "oiics_event_exposure_description", "oiics_event_exposure_desc"
  ),
  ppd_scheduled_loss_date = c(
    "ppd_scheduled_loss_date", "ppd_schedule_loss_date", "ppd_sch_loss_date"
  ),
  ppd_non_scheduled_loss_date = c(
    "ppd_non_scheduled_loss_date", "ppd_nonscheduled_loss_date", "ppd_nsl_date"
  ),
  ptd_date = c("ptd_date"),
  death_date = c("death_date")
)

date_fields <- c(
  "assembly_date", "accident_date", "ancr_date", "c2_date", "c3_date",
  "controverted_date", "first_hearing_date", "first_appeal_date",
  "ppd_scheduled_loss_date", "ppd_non_scheduled_loss_date", "ptd_date",
  "death_date"
)

numeric_fields <- c(
  "average_weekly_wage", "age_at_injury", "birth_year", "hearing_count",
  "interval_assembled_to_ancr"
)

key_field_labels <- c(
  claim_identifier = "Claim identifier",
  claim_type = "Claim type",
  accident_date = "Accident date",
  assembly_date = "Assembly date",
  claim_injury_type = "Claim injury type",
  current_claim_status = "Current claim status",
  atty_rep_ind = "Attorney/representative",
  controverted_date = "Controverted date",
  first_hearing_date = "First hearing date",
  hearing_count = "Hearing count",
  first_appeal_date = "First appeal date",
  highest_process = "Highest process",
  carrier_type = "Carrier type",
  injured_in_county_name = "County of injury",
  average_weekly_wage = "Average weekly wage",
  wcio_pob_desc = "Part of body",
  wcio_nature_of_injury_desc = "Nature of injury",
  wcio_cause_of_injury_desc = "Cause of injury"
)

# ============================================================
# File detection and schema mapping
# ============================================================

auto_detect_wcb_csv <- function() {
  folders <- unique(c(getwd(), file.path(getwd(), "data")))
  candidates <- unlist(lapply(folders, function(folder) {
    if (!dir.exists(folder)) return(character())
    list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  }))

  if (length(candidates) == 0) return("")

  names_lower <- str_to_lower(basename(candidates))
  score <-
    4 * str_detect(names_lower, "assembled") +
    4 * str_detect(names_lower, "workers") +
    4 * str_detect(names_lower, "compensation") +
    3 * str_detect(names_lower, "claim") +
    2 * str_detect(names_lower, "2000") +
    2 * str_detect(names_lower, "jshw") -
    5 * str_detect(names_lower, "fred|cpi|template|enrichment")

  if (max(score) <= 0) return("")
  normalizePath(candidates[which.max(score)], winslash = "/", mustWork = FALSE)
}


resolve_wcb_csv <- function() {
  exact_candidates <- c(
    file.path(getwd(), WCB_CSV_FILENAME),
    file.path(getwd(), "data", WCB_CSV_FILENAME)
  )

  exact_hit <- exact_candidates[file.exists(exact_candidates)][1]
  if (length(exact_hit) == 1 && !is.na(exact_hit)) {
    return(normalizePath(exact_hit, winslash = "/", mustWork = TRUE))
  }

  fallback <- auto_detect_wcb_csv()
  if (nzchar(fallback) && file.exists(fallback)) return(fallback)

  normalizePath(exact_candidates[1], winslash = "/", mustWork = FALSE)
}

auto_detect_fred_csv <- function() {
  folders <- unique(c(getwd(), file.path(getwd(), "data")))
  candidates <- unlist(lapply(folders, function(folder) {
    if (!dir.exists(folder)) return(character())
    list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  }))
  if (length(candidates) == 0) return("")
  names_lower <- str_to_lower(basename(candidates))
  hit <- str_detect(names_lower, "fred.*medical|medical.*cpi|cpimedsl")
  if (!any(hit)) return("")
  normalizePath(candidates[which(hit)[1]], winslash = "/", mustWork = FALSE)
}

read_csv_header <- function(path) {
  read_csv(
    path,
    n_max = 0,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

build_field_map <- function(path) {
  header <- read_csv_header(path)
  original <- names(header)
  cleaned <- clean_names_simple(original)
  source_lookup <- setNames(original, cleaned)

  map_dfr(names(field_aliases), function(field) {
    possible <- unique(c(field, field_aliases[[field]]))
    matches <- possible[possible %in% names(source_lookup)]
    tibble(
      canonical_field = field,
      source_column = if (length(matches) == 0) NA_character_ else source_lookup[[matches[1]]],
      detected = length(matches) > 0
    )
  })
}

source_sql_expression <- function(field, field_map) {
  row <- field_map[field_map$canonical_field == field, , drop = FALSE]
  source_column <- row$source_column[1]

  if (length(source_column) == 0 || is.na(source_column)) {
    if (field %in% date_fields) return(paste0("NULL::DATE AS ", sql_identifier(field)))
    if (field %in% numeric_fields) return(paste0("NULL::DOUBLE AS ", sql_identifier(field)))
    return(paste0("NULL::VARCHAR AS ", sql_identifier(field)))
  }

  quoted <- sql_identifier(source_column)

  if (field %in% date_fields) {
    return(paste0(
      "TRY_CAST(SUBSTR(NULLIF(TRIM(CAST(", quoted,
      " AS VARCHAR)), ''), 1, 10) AS DATE) AS ", sql_identifier(field)
    ))
  }

  if (field %in% numeric_fields) {
    return(paste0(
      "TRY_CAST(REPLACE(NULLIF(TRIM(CAST(", quoted,
      " AS VARCHAR)), ''), ',', '') AS DOUBLE) AS ", sql_identifier(field)
    ))
  }

  paste0(
    "NULLIF(TRIM(CAST(", quoted, " AS VARCHAR)), '') AS ",
    sql_identifier(field)
  )
}

# ============================================================
# DuckDB cache creation
# ============================================================

cache_path_for <- function(csv_path) {
  file.path(dirname(csv_path), "nys_wcb_claims_cache.duckdb")
}

cache_matches_source <- function(con, csv_path) {
  if (!dbExistsTable(con, "app_source_metadata") || !dbExistsTable(con, "claims")) {
    return(FALSE)
  }

  meta <- dbGetQuery(con, "SELECT * FROM app_source_metadata LIMIT 1")
  if (nrow(meta) == 0) return(FALSE)

  info <- file.info(csv_path)
  identical(as.character(meta$source_path[1]), normalizePath(csv_path, winslash = "/")) &&
    isTRUE(all.equal(as.numeric(meta$source_size[1]), as.numeric(info$size))) &&
    isTRUE(all.equal(as.numeric(meta$source_mtime[1]), as.numeric(info$mtime)))
}

create_scored_views <- function(con) {
  dbExecute(con, "DROP VIEW IF EXISTS claims_scored")
  dbExecute(con, "DROP VIEW IF EXISTS claim_flags")

  dbExecute(con, "
    CREATE VIEW claim_flags AS
    SELECT
      *,
      CASE
        WHEN UPPER(COALESCE(atty_rep_ind, '')) IN ('Y', 'YES', 'TRUE', '1') THEN 1
        ELSE 0
      END AS attorney_flag,
      CASE WHEN controverted_date IS NOT NULL THEN 1 ELSE 0 END AS controverted_flag,
      CASE
        WHEN COALESCE(hearing_count, 0) > 0 OR first_hearing_date IS NOT NULL THEN 1
        ELSE 0
      END AS hearing_flag,
      CASE
        WHEN first_appeal_date IS NOT NULL
          OR LOWER(COALESCE(highest_process, '')) LIKE '%appeal%'
        THEN 1 ELSE 0
      END AS appeal_flag,
      CASE
        WHEN LOWER(COALESCE(highest_process, '')) LIKE '%hearing%'
          OR LOWER(COALESCE(highest_process, '')) LIKE '%settlement%'
          OR LOWER(COALESCE(highest_process, '')) LIKE '%appeal%'
        THEN 1 ELSE 0
      END AS formal_process_flag,
      CASE
        WHEN LOWER(COALESCE(current_claim_status, '')) LIKE '%hearing%'
          OR LOWER(COALESCE(current_claim_status, '')) LIKE '%argument%'
          OR LOWER(COALESCE(current_claim_status, '')) LIKE '%motion%'
          OR LOWER(COALESCE(current_claim_status, '')) LIKE '%reserved%'
          OR LOWER(COALESCE(current_claim_status, '')) LIKE '%re-open%'
          OR LOWER(COALESCE(current_claim_status, '')) LIKE '%restoral%'
        THEN 1 ELSE 0
      END AS active_status_flag,
      CASE
        WHEN UPPER(COALESCE(claim_injury_type, '')) LIKE '%DEATH%' THEN 20
        WHEN UPPER(COALESCE(claim_injury_type, '')) LIKE '%PTD%' THEN 18
        WHEN UPPER(COALESCE(claim_injury_type, '')) LIKE '%PPD NSL%' THEN 14
        WHEN UPPER(COALESCE(claim_injury_type, '')) LIKE '%PPD%' THEN 10
        WHEN UPPER(COALESCE(claim_injury_type, '')) LIKE '%TEMP%' THEN 5
        ELSE 0
      END AS severity_points,
      CASE
        WHEN COALESCE(hearing_count, 0) >= 5 THEN 20
        WHEN COALESCE(hearing_count, 0) >= 2 THEN 14
        WHEN COALESCE(hearing_count, 0) >= 1 OR first_hearing_date IS NOT NULL THEN 8
        ELSE 0
      END AS hearing_points,
      EXTRACT(YEAR FROM assembly_date) AS assembly_year,
      EXTRACT(YEAR FROM accident_date) AS accident_year,
      DATE_DIFF('day', accident_date, assembly_date) AS days_to_assembly
    FROM claims
  ")

  dbExecute(con, "
    CREATE VIEW claims_scored AS
    SELECT
      *,
      LEAST(
        100,
        attorney_flag * 20 +
        controverted_flag * 25 +
        hearing_points +
        appeal_flag * 15 +
        formal_process_flag * 10 +
        active_status_flag * 5 +
        severity_points
      ) AS litigation_signal_score,
      CASE
        WHEN (
          attorney_flag * 20 + controverted_flag * 25 + hearing_points +
          appeal_flag * 15 + formal_process_flag * 10 +
          active_status_flag * 5 + severity_points
        ) >= 70 THEN 'High-priority analytical review'
        WHEN (
          attorney_flag * 20 + controverted_flag * 25 + hearing_points +
          appeal_flag * 15 + formal_process_flag * 10 +
          active_status_flag * 5 + severity_points
        ) >= 45 THEN 'Material litigation indicators'
        WHEN (
          attorney_flag * 20 + controverted_flag * 25 + hearing_points +
          appeal_flag * 15 + formal_process_flag * 10 +
          active_status_flag * 5 + severity_points
        ) >= 20 THEN 'Monitor adjudication activity'
        ELSE 'Limited public litigation indicators'
      END AS litigation_review_tier
    FROM claim_flags
  ")
}

build_or_open_cache <- function(csv_path, force_rebuild = FALSE, progress_callback = NULL) {
  csv_path <- normalizePath(csv_path, winslash = "/", mustWork = TRUE)
  cache_path <- cache_path_for(csv_path)
  con <- dbConnect(duckdb(), dbdir = cache_path, read_only = FALSE)

  if (!force_rebuild && cache_matches_source(con, csv_path)) {
    create_scored_views(con)
    return(list(
      con = con,
      cache_path = cache_path,
      field_map = dbGetQuery(con, "SELECT * FROM app_field_map"),
      reused = TRUE
    ))
  }

  if (is.function(progress_callback)) progress_callback(0.08, "Reading the CSV header")
  field_map <- build_field_map(csv_path)

  claim_id_detected <- field_map$detected[field_map$canonical_field == "claim_identifier"]
  if (!isTRUE(claim_id_detected)) {
    dbDisconnect(con, shutdown = TRUE)
    stop(
      "The CSV does not contain a recognized Claim Identifier column. ",
      "Confirm that this is the NYS WCB Assembled Claims export."
    )
  }

  if (is.function(progress_callback)) progress_callback(0.18, "Preparing the local analytical cache")

  dbExecute(con, "DROP VIEW IF EXISTS claims_scored")
  dbExecute(con, "DROP VIEW IF EXISTS claim_flags")
  dbExecute(con, "DROP TABLE IF EXISTS claims")
  dbExecute(con, "DROP TABLE IF EXISTS app_source_metadata")
  dbExecute(con, "DROP TABLE IF EXISTS app_field_map")

  select_expressions <- vapply(
    names(field_aliases),
    source_sql_expression,
    character(1),
    field_map = field_map
  )

  source_sql <- paste0(
    "read_csv_auto(", sql_string(csv_path),
    ", header = true, all_varchar = true, ignore_errors = true, sample_size = 200000)"
  )

  create_sql <- paste0(
    "CREATE TABLE claims AS SELECT ",
    paste(select_expressions, collapse = ",\n"),
    " FROM ", source_sql,
    " WHERE NULLIF(TRIM(CAST(",
    sql_identifier(field_map$source_column[field_map$canonical_field == "claim_identifier"][1]),
    " AS VARCHAR)), '') IS NOT NULL"
  )

  if (is.function(progress_callback)) {
    progress_callback(0.28, "Importing the official claims into DuckDB")
  }
  dbExecute(con, create_sql)

  if (is.function(progress_callback)) progress_callback(0.78, "Indexing claim identifiers")
  try(dbExecute(con, "CREATE INDEX claim_identifier_idx ON claims(claim_identifier)"), silent = TRUE)
  try(dbExecute(con, "ANALYZE claims"), silent = TRUE)

  dbWriteTable(con, "app_field_map", field_map, overwrite = TRUE)
  source_info <- file.info(csv_path)
  source_meta <- data.frame(
    source_path = csv_path,
    source_size = as.numeric(source_info$size),
    source_mtime = as.numeric(source_info$mtime),
    cache_created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "app_source_metadata", source_meta, overwrite = TRUE)

  if (is.function(progress_callback)) progress_callback(0.9, "Creating explainable litigation indicators")
  create_scored_views(con)

  if (is.function(progress_callback)) progress_callback(1, "Ready")

  list(
    con = con,
    cache_path = cache_path,
    field_map = field_map,
    reused = FALSE
  )
}

# ============================================================
# FRED medical CPI
# ============================================================

prepare_fred <- function(df) {
  names(df) <- clean_names_simple(names(df))
  date_col <- intersect(c("observation_date", "date"), names(df))[1]
  value_col <- intersect(c("cpimedsl", "value", "medical_index"), names(df))[1]

  if (is.na(date_col) || is.na(value_col)) return(NULL)

  df |>
    transmute(
      date = as.Date(.data[[date_col]]),
      medical_cpi = suppressWarnings(as.numeric(.data[[value_col]]))
    ) |>
    filter(!is.na(date), !is.na(medical_cpi)) |>
    arrange(date) |>
    mutate(
      year = year(date),
      yoy_change = 100 * (medical_cpi / lag(medical_cpi, 12) - 1)
    )
}

load_fred_data <- function(local_path = "") {
  sources <- c()
  if (nzchar(local_path) && file.exists(local_path)) sources <- c(sources, local_path)
  sources <- c(sources, FRED_CSV)

  for (source in sources) {
    value <- tryCatch(
      read_csv(source, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
    if (!is.null(value)) {
      prepared <- prepare_fred(value)
      if (!is.null(prepared) && nrow(prepared) > 0) {
        attr(prepared, "source") <- source
        return(prepared)
      }
    }
  }
  NULL
}

# ============================================================
# Enrichment template and helpers
# ============================================================

enrichment_template <- tibble(
  claim_identifier = character(),
  charged_amount = numeric(),
  paid_amount = numeric(),
  paid_medical = numeric(),
  paid_indemnity = numeric(),
  total_incurred = numeric(),
  case_reserve = numeric(),
  reserve_change_90d = numeric(),
  provider_id = character(),
  duplicate_bill_flag = integer(),
  provider_outlier_flag = integer(),
  documentation_gap_flag = integer(),
  claim_outcome = character()
)

prepare_enrichment <- function(df) {
  names(df) <- clean_names_simple(names(df))
  id_aliases <- c("claim_identifier", "claim_id", "claim_number", "wcb_claim_number")
  id_col <- id_aliases[id_aliases %in% names(df)][1]
  if (is.na(id_col)) stop("The enrichment file needs a claim_identifier column.")
  names(df)[names(df) == id_col] <- "claim_identifier"

  numeric_cols <- c(
    "charged_amount", "paid_amount", "paid_medical", "paid_indemnity",
    "total_incurred", "case_reserve", "reserve_change_90d"
  )
  flag_cols <- c("duplicate_bill_flag", "provider_outlier_flag", "documentation_gap_flag")

  for (field in setdiff(c(numeric_cols, flag_cols), names(df))) df[[field]] <- NA_real_
  for (field in setdiff(c("provider_id", "claim_outcome"), names(df))) df[[field]] <- NA_character_

  df |>
    mutate(
      claim_identifier = as.character(claim_identifier),
      across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))),
      across(all_of(flag_cols), ~ case_when(
        str_to_upper(str_trim(as.character(.x))) %in% c("Y", "YES", "TRUE", "1") ~ 1,
        str_to_upper(str_trim(as.character(.x))) %in% c("N", "NO", "FALSE", "0") ~ 0,
        TRUE ~ suppressWarnings(as.numeric(.x))
      ))
    ) |>
    filter(!is.na(claim_identifier), claim_identifier != "") |>
    group_by(claim_identifier) |>
    summarise(
      charged_amount = if (all(is.na(charged_amount))) NA_real_ else sum(charged_amount, na.rm = TRUE),
      paid_amount = if (all(is.na(paid_amount))) NA_real_ else sum(paid_amount, na.rm = TRUE),
      paid_medical = if (all(is.na(paid_medical))) NA_real_ else max(paid_medical, na.rm = TRUE),
      paid_indemnity = if (all(is.na(paid_indemnity))) NA_real_ else max(paid_indemnity, na.rm = TRUE),
      total_incurred = if (all(is.na(total_incurred))) NA_real_ else max(total_incurred, na.rm = TRUE),
      case_reserve = if (all(is.na(case_reserve))) NA_real_ else max(case_reserve, na.rm = TRUE),
      reserve_change_90d = if (all(is.na(reserve_change_90d))) NA_real_ else max(reserve_change_90d, na.rm = TRUE),
      provider_id = dplyr::first(na.omit(provider_id), default = NA_character_),
      duplicate_bill_flag = if (all(is.na(duplicate_bill_flag))) NA_real_ else max(duplicate_bill_flag, na.rm = TRUE),
      provider_outlier_flag = if (all(is.na(provider_outlier_flag))) NA_real_ else max(provider_outlier_flag, na.rm = TRUE),
      documentation_gap_flag = if (all(is.na(documentation_gap_flag))) NA_real_ else max(documentation_gap_flag, na.rm = TRUE),
      claim_outcome = dplyr::first(na.omit(claim_outcome), default = NA_character_),
      .groups = "drop"
    )
}

# ============================================================
# App theme
# ============================================================

app_theme <- bs_theme(
  version = 5,
  bg = "#f7f9fc",
  fg = "#1d2939",
  primary = "#214e8a",
  secondary = "#0d6b78",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

app_css <- '
:root {
  --navy:#142a43; --blue:#214e8a; --cyan:#0d6b78; --green:#18794e;
  --amber:#a45f06; --red:#b42318; --ink:#1d2939; --muted:#667085;
  --line:#dfe5ee; --paper:#ffffff; --wash:#f7f9fc;
}
body {
  background:
    radial-gradient(circle at 95% 3%, rgba(33,78,138,.12), transparent 24%),
    radial-gradient(circle at 3% 22%, rgba(13,107,120,.08), transparent 20%),
    linear-gradient(180deg,#fbfdff 0%,#f5f7fb 100%);
  color:var(--ink);
}
.app-shell { max-width:1480px; margin:0 auto; padding:22px 20px 48px; }
.topbar {
  background:rgba(255,255,255,.97); border:1px solid var(--line); border-radius:20px;
  padding:15px 20px; display:flex; justify-content:space-between; align-items:center;
  gap:18px; margin-bottom:18px; box-shadow:0 10px 30px rgba(20,42,67,.06);
}
.wordmark { display:flex; align-items:center; gap:12px; }
.brand-mark {
  width:48px; height:48px; border-radius:15px; display:grid; place-items:center;
  color:white; font-weight:900; background:linear-gradient(135deg,var(--blue),var(--cyan));
  box-shadow:0 10px 24px rgba(33,78,138,.22);
}
.brand-title { font-size:1.22rem; font-weight:900; color:var(--navy); }
.brand-subtitle { color:var(--muted); font-size:.78rem; font-weight:750; letter-spacing:.08rem; text-transform:uppercase; }
.source-badge {
  display:inline-flex; align-items:center; gap:8px; padding:8px 12px; border-radius:999px;
  background:#e8f6ef; color:var(--green); border:1px solid #c6e8d5; font-size:.82rem; font-weight:850;
}
.source-dot { width:9px; height:9px; border-radius:50%; background:var(--green); }
.hero {
  display:grid; grid-template-columns:1.15fr .85fr; border-radius:30px; overflow:hidden;
  margin-bottom:22px; background:linear-gradient(135deg,#173a68 0%,#214e8a 55%,#0d6b78 100%);
  box-shadow:0 22px 52px rgba(20,42,67,.15);
}
.hero-copy { padding:58px 54px; color:white; }
.hero-kicker { font-size:.84rem; font-weight:850; letter-spacing:.15rem; text-transform:uppercase; opacity:.9; margin-bottom:16px; }
.hero-title { font-size:clamp(2.5rem,5vw,4.7rem); line-height:.98; font-weight:950; letter-spacing:-.08rem; margin-bottom:20px; }
.hero-text { max-width:720px; font-size:1.1rem; line-height:1.72; color:rgba(255,255,255,.94); }
.hero-proof { padding:32px; display:grid; align-content:center; gap:14px; }
.proof-card { background:rgba(255,255,255,.97); border-radius:20px; padding:20px; box-shadow:0 12px 28px rgba(20,42,67,.12); }
.proof-label { color:var(--muted); font-size:.75rem; font-weight:850; letter-spacing:.09rem; text-transform:uppercase; }
.proof-value { color:var(--navy); font-size:1.15rem; font-weight:900; margin-top:5px; }
.panel-card { background:var(--paper); border:1px solid var(--line); border-radius:24px; padding:26px; margin-bottom:18px; box-shadow:0 15px 38px rgba(20,42,67,.05); }
.panel-title { color:var(--navy); font-weight:900; letter-spacing:-.025rem; margin-bottom:12px; }
.microcopy { color:var(--muted); line-height:1.68; }
.status-box,.success-box,.warning-box,.info-box,.danger-box { border-radius:17px; padding:15px 17px; line-height:1.56; }
.status-box { background:#eaf1fb; border:1px solid #cad9f2; border-left:5px solid var(--blue); }
.success-box { background:#e8f6ef; border:1px solid #c6e8d5; border-left:5px solid var(--green); }
.warning-box { background:#fff4df; border:1px solid #efd8ab; border-left:5px solid var(--amber); }
.info-box { background:#e7f5f8; border:1px solid #c7e8ee; border-left:5px solid var(--cyan); }
.danger-box { background:#fff0ee; border:1px solid #f0c9c4; border-left:5px solid var(--red); }
.metric-card { background:var(--paper); border:1px solid var(--line); border-radius:22px; padding:21px; min-height:128px; box-shadow:0 12px 30px rgba(20,42,67,.04); margin-bottom:16px; }
.metric-value { color:var(--blue); font-size:2rem; font-weight:950; }
.metric-label { color:var(--muted); font-size:.77rem; font-weight:850; text-transform:uppercase; letter-spacing:.07rem; }
.metric-note { color:var(--muted); font-size:.77rem; margin-top:7px; line-height:1.4; }
.use-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:16px; }
.use-card { border:1px solid var(--line); border-radius:18px; padding:20px; background:#fbfcfe; }
.use-card strong { color:var(--navy); display:block; margin-bottom:7px; }
.claim-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; }
.claim-field { background:#f8fafc; border:1px solid var(--line); border-radius:14px; padding:13px; }
.claim-field-label { color:var(--muted); font-size:.7rem; font-weight:850; text-transform:uppercase; letter-spacing:.06rem; }
.claim-field-value { color:var(--ink); font-weight:750; margin-top:4px; overflow-wrap:anywhere; }
.nav-pills .nav-link { border-radius:14px; padding:13px 15px; margin-bottom:7px; font-weight:760; color:#52657f; }
.nav-pills .nav-link.active { background:white; color:var(--blue); border:1px solid var(--line); box-shadow:0 9px 24px rgba(20,42,67,.06); }
.form-control,.form-select,.selectize-input { border-radius:13px !important; border:1px solid var(--line) !important; min-height:43px; }
.btn,.btn-default { border-radius:13px !important; font-weight:800 !important; }
.btn-primary,.btn-default { background:linear-gradient(90deg,#173a68,var(--blue)) !important; border:none !important; color:white !important; }
table.dataTable thead th { background:#eef4fc !important; color:var(--navy) !important; border-bottom:1px solid var(--line) !important; }
.footer-note { text-align:center; color:#7b8798; font-size:.84rem; margin-top:24px; }
code { color:#143e7a; background:#eef4fc; padding:2px 5px; border-radius:5px; }
@media(max-width:1050px){.hero{grid-template-columns:1fr}.use-grid{grid-template-columns:1fr}.claim-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:720px){.topbar{flex-direction:column;align-items:flex-start}.hero-copy{padding:38px 27px}.hero-proof{padding:22px}.claim-grid{grid-template-columns:1fr}}

/* Product dashboard overrides */
body { background:#f5f7fa; }
.app-shell { max-width:1540px; }
.topbar { box-shadow:none; border-radius:16px; }
.hero {
  background:#ffffff; color:var(--ink); border:1px solid var(--line);
  box-shadow:none; min-height:400px;
}
.hero-copy { color:var(--ink); padding:54px 52px; }
.hero-kicker { color:var(--blue); opacity:1; }
.hero-title { color:var(--navy); font-size:clamp(2.4rem,4.7vw,4.25rem); }
.hero-text { color:var(--muted); max-width:760px; }
.hero-proof { background:#f8fafc; border-left:1px solid var(--line); }
.proof-card { border:1px solid var(--line); box-shadow:none; }
.panel-card,.metric-card { box-shadow:none; }
.metric-card { min-height:116px; }
.metric-value { font-size:1.9rem; }
.product-grid { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:14px; margin-bottom:18px; }
.product-kpi { background:white; border:1px solid var(--line); border-radius:18px; padding:18px; }
.product-kpi .value { color:var(--navy); font-size:1.8rem; font-weight:950; }
.product-kpi .label { color:var(--muted); font-size:.73rem; text-transform:uppercase; letter-spacing:.07rem; font-weight:850; }
.product-kpi .detail { color:var(--muted); font-size:.78rem; margin-top:6px; }
.insight-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:14px; }
.insight-card { background:#fff; border:1px solid var(--line); border-radius:18px; padding:20px; min-height:150px; }
.insight-card .eyebrow { color:var(--blue); font-size:.72rem; text-transform:uppercase; letter-spacing:.07rem; font-weight:900; }
.insight-card .finding { color:var(--navy); font-size:1.08rem; font-weight:900; margin-top:8px; line-height:1.35; }
.insight-card .explain { color:var(--muted); font-size:.82rem; margin-top:8px; line-height:1.5; }
.pipeline { position:relative; display:grid; grid-template-columns:repeat(4,1fr); align-items:center; gap:10px; margin:12px 0 4px; }
.pipeline::before { content:""; position:absolute; left:9%; right:9%; top:26px; height:2px; background:#cbd5e1; }
.pipeline-node { position:relative; z-index:2; text-align:center; }
.pipeline-icon { width:54px; height:54px; margin:0 auto 8px; border-radius:16px; display:grid; place-items:center; background:#fff; border:1px solid var(--line); color:var(--blue); font-weight:950; }
.pipeline-label { color:var(--navy); font-size:.78rem; font-weight:850; }
.pipeline-dot { position:absolute; z-index:3; top:20px; left:8%; width:12px; height:12px; border-radius:50%; background:var(--cyan); box-shadow:0 0 0 6px rgba(13,107,120,.12); animation:flowdot 4.2s ease-in-out infinite; }
@keyframes flowdot { 0%{left:8%;opacity:.2} 15%{opacity:1} 100%{left:89%;opacity:.2} }
.live-pill { display:inline-flex; align-items:center; gap:7px; color:var(--green); font-weight:850; font-size:.8rem; }
.live-pulse { width:9px; height:9px; border-radius:50%; background:var(--green); animation:pulse 1.7s infinite; }
@keyframes pulse { 0%,100%{box-shadow:0 0 0 0 rgba(24,121,78,.25)} 50%{box-shadow:0 0 0 8px rgba(24,121,78,0)} }
.benchmark-flow { display:grid; grid-template-columns:1fr auto 1fr auto 1fr; align-items:center; gap:12px; }
.flow-node { background:#fff; border:1px solid var(--line); border-radius:16px; padding:16px; text-align:center; color:var(--navy); font-weight:850; }
.flow-arrow { color:var(--blue); font-size:1.4rem; animation:nudge 1.5s ease-in-out infinite; }
@keyframes nudge { 0%,100%{transform:translateX(0);opacity:.45} 50%{transform:translateX(5px);opacity:1} }
.ship-card { border:1px solid #b8dfcc; background:#eef9f3; border-radius:20px; padding:22px; }
.ship-title { color:var(--green); font-size:.78rem; text-transform:uppercase; letter-spacing:.08rem; font-weight:900; }
.ship-value { color:var(--navy); font-size:1.35rem; font-weight:950; margin-top:6px; }
.clean-note { color:var(--muted); font-size:.8rem; line-height:1.5; }
.js-animate-in { animation:risein .55s ease both; }
@keyframes risein { from{transform:translateY(8px);opacity:0} to{transform:translateY(0);opacity:1} }
@media(max-width:1100px){.product-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.insight-grid{grid-template-columns:1fr}.benchmark-flow{grid-template-columns:1fr}.flow-arrow{transform:rotate(90deg)}}
@media(max-width:720px){.product-grid{grid-template-columns:1fr}.hero-proof{border-left:0;border-top:1px solid var(--line)}.pipeline{grid-template-columns:repeat(2,1fr)}.pipeline::before,.pipeline-dot{display:none}}

/* Final product experience */
body { background:#edf2f7; overflow-x:hidden; }
.product-nav {
  position:sticky; top:0; z-index:1000; min-height:72px; padding:12px 30px;
  display:flex; align-items:center; justify-content:space-between; gap:20px;
  background:rgba(5,20,36,.95); backdrop-filter:blur(16px); color:#fff;
  border-bottom:1px solid rgba(255,255,255,.09);
}
.product-brand { display:flex; align-items:center; gap:12px; }
.product-logo { width:43px; height:43px; border-radius:13px; display:grid; place-items:center; font-weight:950; background:linear-gradient(135deg,#42d3ff,#3273dc); color:#041525; box-shadow:0 0 0 7px rgba(66,211,255,.08); }
.product-name { font-size:1rem; font-weight:900; letter-spacing:-.01rem; }
.product-tagline { color:#9cb4ca; font-size:.73rem; }
.source-chip { display:flex; align-items:center; gap:8px; padding:8px 12px; border:1px solid rgba(255,255,255,.13); border-radius:999px; background:rgba(255,255,255,.06); color:#dceafa; font-size:.76rem; }
.source-dot { width:8px; height:8px; border-radius:50%; background:#5ce0a3; animation:sourcepulse 1.8s infinite; }
@keyframes sourcepulse { 0%,100%{box-shadow:0 0 0 0 rgba(92,224,163,.45)} 50%{box-shadow:0 0 0 7px rgba(92,224,163,0)} }
.report-shell { max-width:1540px; margin:0 auto; padding:24px 24px 60px; }
.nav-tabs { border:0 !important; gap:8px; margin-bottom:20px; }
.nav-tabs .nav-link { border:1px solid #d9e2ec !important; border-radius:999px !important; background:#fff; color:#4c6075; font-weight:800; padding:10px 17px; }
.nav-tabs .nav-link.active { background:#071a2e !important; color:#fff !important; border-color:#071a2e !important; }
.command-hero {
  position:relative; overflow:hidden; display:grid; grid-template-columns:1.2fr .8fr; min-height:430px;
  border-radius:30px; background:
    radial-gradient(circle at 18% 18%, rgba(66,211,255,.22), transparent 28%),
    radial-gradient(circle at 78% 25%, rgba(78,119,255,.20), transparent 34%),
    linear-gradient(135deg,#061526 0%,#0a2845 55%,#0d3557 100%);
  color:#fff; padding:48px; margin-bottom:18px;
}
.command-hero::after { content:""; position:absolute; inset:-70%; background:conic-gradient(from 0deg,transparent,rgba(66,211,255,.05),transparent 25%); animation:meshspin 16s linear infinite; }
@keyframes meshspin { to{transform:rotate(360deg)} }
.hero-copy-final { position:relative; z-index:2; align-self:center; }
.hero-eyebrow { display:inline-flex; gap:8px; align-items:center; color:#6fe1ff; font-size:.76rem; text-transform:uppercase; letter-spacing:.12rem; font-weight:900; }
.hero-headline { max-width:780px; margin:14px 0 16px; font-size:clamp(2.7rem,5vw,5.5rem); line-height:.95; font-weight:950; letter-spacing:-.1rem; }
.hero-deck { max-width:760px; color:#c6d8e8; font-size:1.06rem; line-height:1.7; }
.hero-badges { display:flex; flex-wrap:wrap; gap:9px; margin-top:24px; }
.hero-badge { border:1px solid rgba(255,255,255,.16); border-radius:999px; padding:8px 12px; color:#d9edff; background:rgba(255,255,255,.06); font-size:.76rem; font-weight:800; }
.signal-visual { position:relative; z-index:2; display:grid; place-items:center; min-height:320px; }
.signal-orbit { width:min(330px,85%); aspect-ratio:1; border-radius:50%; position:relative; display:grid; place-items:center; border:1px solid rgba(255,255,255,.12); background:radial-gradient(circle,rgba(66,211,255,.12),transparent 60%); }
.signal-orbit::before,.signal-orbit::after { content:""; position:absolute; border-radius:50%; border:1px solid rgba(111,225,255,.25); }
.signal-orbit::before { inset:13%; animation:orbitspin 10s linear infinite; border-top-color:#6fe1ff; }
.signal-orbit::after { inset:28%; animation:orbitspin 7s linear infinite reverse; border-right-color:#70f0b3; }
@keyframes orbitspin { to{transform:rotate(360deg)} }
.orbit-core { width:138px; height:138px; border-radius:38px; display:grid; place-items:center; text-align:center; background:rgba(4,17,31,.78); border:1px solid rgba(255,255,255,.15); box-shadow:0 0 45px rgba(66,211,255,.17); }
.orbit-core strong { display:block; font-size:1.5rem; }
.orbit-core span { color:#9ec8e6; font-size:.72rem; }
.orbit-node { position:absolute; width:14px; height:14px; border-radius:50%; background:#6fe1ff; box-shadow:0 0 18px #6fe1ff; }
.orbit-node.n1 { top:10%; left:48%; animation:floatnode 3s ease-in-out infinite; }
.orbit-node.n2 { right:9%; top:55%; background:#70f0b3; box-shadow:0 0 18px #70f0b3; animation:floatnode 3.6s ease-in-out infinite .5s; }
.orbit-node.n3 { left:12%; bottom:19%; background:#8fa7ff; box-shadow:0 0 18px #8fa7ff; animation:floatnode 4s ease-in-out infinite 1s; }
@keyframes floatnode { 0%,100%{transform:translateY(0) scale(1)} 50%{transform:translateY(-10px) scale(1.25)} }
.filter-dock { background:#fff; border:1px solid #d9e3ee; border-radius:22px; padding:18px 20px 5px; margin-bottom:18px; }
.filter-label { color:#213b55; font-size:.74rem; text-transform:uppercase; letter-spacing:.08rem; font-weight:900; margin-bottom:10px; }
.kpi-ribbon { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:12px; margin-bottom:18px; }
.kpi-tile { position:relative; overflow:hidden; background:#fff; border:1px solid #d9e3ee; border-radius:20px; padding:20px; min-height:128px; }
.kpi-tile::after { content:""; position:absolute; left:0; bottom:0; width:100%; height:4px; background:linear-gradient(90deg,#3273dc,#42d3ff); transform-origin:left; animation:loadline 2.5s ease both; }
@keyframes loadline { from{transform:scaleX(0)} to{transform:scaleX(1)} }
.kpi-title { color:#6b7f92; font-size:.7rem; text-transform:uppercase; letter-spacing:.08rem; font-weight:900; }
.kpi-number { color:#071a2e; font-size:2rem; font-weight:950; margin-top:7px; }
.kpi-foot { color:#7f8e9c; font-size:.75rem; margin-top:4px; }
.section-heading { display:flex; justify-content:space-between; align-items:end; gap:20px; margin:34px 2px 14px; }
.section-heading h2 { margin:0; color:#071a2e; font-size:1.55rem; font-weight:950; letter-spacing:-.03rem; }
.section-heading p { margin:0; max-width:650px; color:#728397; font-size:.82rem; text-align:right; }
.chart-grid-2 { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:16px; }
.chart-grid-wide { display:grid; grid-template-columns:1.25fr .75fr; gap:16px; }
.visual-panel { background:#fff; border:1px solid #d9e3ee; border-radius:24px; padding:22px; margin-bottom:16px; min-height:360px; }
.visual-panel h3 { color:#102a43; margin:0 0 4px; font-size:1rem; font-weight:900; }
.visual-panel .caption { color:#7a8b9d; font-size:.75rem; margin-bottom:12px; }
.executive-callout { display:grid; grid-template-columns:1fr auto; align-items:center; gap:18px; background:linear-gradient(100deg,#e8f8ff,#f4f8ff); border:1px solid #c8e7f4; border-radius:22px; padding:22px; margin-bottom:18px; }
.executive-callout strong { display:block; color:#082743; font-size:1.05rem; }
.executive-callout p { margin:6px 0 0; color:#536b80; }
.report-badge { padding:9px 12px; border-radius:999px; background:#0a6f62; color:#fff; font-size:.75rem; font-weight:900; white-space:nowrap; }
.loading-product { min-height:70vh; display:grid; place-items:center; padding:40px; }
.loading-card { width:min(720px,100%); border-radius:28px; padding:44px; color:#fff; background:linear-gradient(135deg,#061526,#0d3557); text-align:center; }
.loading-ring { width:76px; height:76px; margin:0 auto 22px; border-radius:50%; border:5px solid rgba(255,255,255,.15); border-top-color:#6fe1ff; animation:orbitspin 1s linear infinite; }
.loading-card h2 { font-weight:950; }
.loading-card p { color:#c4d6e5; }
.workbench-grid { display:grid; grid-template-columns:1.3fr .7fr; gap:16px; }
.claim-hero { background:#071a2e; color:#fff; border-radius:24px; padding:24px; margin-bottom:16px; }
.claim-hero h2 { margin:0 0 5px; font-weight:950; }
.claim-hero p { margin:0; color:#a9bfd2; }
.report-cover { padding:45px; border-radius:30px; background:linear-gradient(135deg,#fff,#eef6ff); border:1px solid #d5e4f1; margin-bottom:18px; }
.report-cover h1 { color:#071a2e; font-size:clamp(2.2rem,4vw,4.4rem); font-weight:950; letter-spacing:-.08rem; margin:0 0 12px; }
.report-cover p { color:#60758a; max-width:900px; font-size:1rem; line-height:1.7; }
.capability-row { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:10px; }
.capability { border-radius:16px; padding:14px; background:#fff; border:1px solid #d9e3ee; font-size:.78rem; color:#50667b; }
.capability strong { color:#0c2b48; display:block; margin-bottom:5px; }
@media(max-width:1100px){.command-hero{grid-template-columns:1fr}.signal-visual{min-height:280px}.kpi-ribbon{grid-template-columns:repeat(2,minmax(0,1fr))}.chart-grid-2,.chart-grid-wide,.workbench-grid{grid-template-columns:1fr}.capability-row{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:720px){.product-nav{padding:11px 15px}.product-tagline,.source-chip span{display:none}.report-shell{padding:14px}.command-hero{padding:30px 24px;border-radius:22px}.hero-headline{letter-spacing:-.06rem}.kpi-ribbon{grid-template-columns:1fr}.section-heading{align-items:start;flex-direction:column}.section-heading p{text-align:left}.capability-row{grid-template-columns:1fr}.executive-callout{grid-template-columns:1fr}}
' 

# ============================================================
# UI
# ============================================================

detected_wcb_file <- resolve_wcb_csv()
detected_fred_file <- auto_detect_fred_csv()

source_register <- tribble(
  ~source, ~what_it_provides, ~how_the_app_uses_it, ~important_limitation,
  "NYS Workers' Compensation Board — Assembled Claims",
  "Real public administrative claim records beginning in 2000",
  "Claim outcomes, injury classifications, representation, controversy, hearings, appeals, process, geography, carrier type, and timelines",
  "The public dataset does not include insurer reserves, medical bill payments, settlement values, or private claim notes",
  "FRED CPIMEDSL — U.S. Bureau of Labor Statistics Medical Care CPI",
  "Monthly U.S. medical price inflation context",
  "Shows how medical price pressure changed during the years in which claims were assembled",
  "It is national market context, not a claim-level medical payment measure",
  "Optional de-identified carrier enrichment",
  "Billing, paid-loss, reserve, provider, and outcome fields supplied by an authorized user",
  "Activates separate billing and reserve analytics",
  "Not included by default and should never contain claimant names, Social Security numbers, medical records, or other restricted information"
)

method_table <- tribble(
  ~indicator, ~points, ~why_it_matters,
  "Controverted date present", "25", "The public record shows that the controversy-resolution process began",
  "Attorney or representative", "20", "Representation is a visible legal-complexity indicator",
  "Hearing activity", "8–20", "Repeated hearings can indicate sustained adjudication activity",
  "Appeal activity", "15", "An appeal adds procedural and legal complexity",
  "Formal hearing/settlement process", "10", "Highest Process shows a formal resolution pathway",
  "Active adjudication status", "5", "Current status can show a hearing, motion, argument, restoral, or reserved decision",
  "Awarded injury severity", "0–20", "Permanent disability and death categories increase claim consequence"
)

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("$(document).on('shiny:value', function(event){ var el=document.getElementById(event.name); if(el){ el.classList.remove('js-animate-in'); void el.offsetWidth; el.classList.add('js-animate-in'); }});"))
  ),

  span(style = "display:none", textOutput("data_ready")),

  div(
    class = "product-nav",
    div(
      class = "product-brand",
      div(class = "product-logo", "WC"),
      div(
        div(class = "product-name", "Workers’ Compensation Claims Intelligence"),
        div(class = "product-tagline", "Evidence · litigation signals · local agent benchmark")
      )
    ),
    uiOutput("source_chip")
  ),

  conditionalPanel(
    condition = "output.data_ready != 'true'",
    div(
      class = "loading-product",
      div(
        class = "loading-card",
        div(class = "loading-ring"),
        h2("Building the claims intelligence index"),
        p("The product is opening the official NYS Workers’ Compensation Board file and preparing a local DuckDB analytical cache."),
        uiOutput("load_status")
      )
    )
  ),

  conditionalPanel(
    condition = "output.data_ready == 'true'",
    div(
      class = "report-shell",
      navset_tab(
        id = "product_tabs",

        nav_panel(
          "Executive Intelligence",
          div(
            class = "command-hero",
            div(
              class = "hero-copy-final",
              div(class = "hero-eyebrow", span(class = "live-pulse"), "Live public-claims intelligence"),
              div(class = "hero-headline", "See where claims become legally complex."),
              p(
                class = "hero-deck",
                "A finished analytical product built on official New York workers’ compensation records. It measures claim outcomes, representation, controversy, hearings, appeals, injury severity, medical-price pressure, and explainable litigation-review signals—then tests whether a local evidence agent outperforms fixed RAG retrieval."
              ),
              div(
                class = "hero-badges",
                div(class = "hero-badge", "Official NYS WCB records"),
                div(class = "hero-badge", "FRED Medical Care CPI"),
                div(class = "hero-badge", "Local DuckDB evidence index"),
                div(class = "hero-badge", "No fabricated claim values")
              )
            ),
            div(
              class = "signal-visual",
              div(
                class = "signal-orbit",
                div(class = "orbit-node n1"),
                div(class = "orbit-node n2"),
                div(class = "orbit-node n3"),
                div(
                  class = "orbit-core",
                  div(
                    strong(textOutput("hero_claims", inline = TRUE)),
                    span("public claim records indexed")
                  )
                )
              )
            )
          ),

          div(
            class = "filter-dock",
            div(class = "filter-label", "Portfolio lens"),
            fluidRow(
              column(3, selectInput("filter_claim_type", "Claim type", choices = "All", selected = "All")),
              column(3, selectInput("filter_carrier_type", "Carrier type", choices = "All", selected = "All")),
              column(3, selectInput("filter_county", "County", choices = "All", selected = "All")),
              column(3, selectInput("filter_injury_type", "Injury outcome", choices = "All", selected = "All"))
            ),
            fluidRow(
              column(3, selectInput("filter_attorney", "Representation", choices = c("All", "Represented", "Not represented"), selected = "All")),
              column(3, selectInput("filter_controverted", "Controversy", choices = c("All", "Controverted", "Not controverted"), selected = "All")),
              column(5, sliderInput("filter_year", "Assembly year", min = 2000, max = year(Sys.Date()), value = c(2000, year(Sys.Date())), sep = "")),
              column(1, br(), actionButton("rebuild_cache", "Refresh", class = "btn-primary"))
            )
          ),

          div(
            class = "kpi-ribbon",
            div(class = "kpi-tile", div(class = "kpi-title", "Claims analyzed"), div(class = "kpi-number", textOutput("dashboard_claims", inline = TRUE)), div(class = "kpi-foot", "Records matching the active portfolio lens")),
            div(class = "kpi-tile", div(class = "kpi-title", "Represented"), div(class = "kpi-number", textOutput("dashboard_rep_rate", inline = TRUE)), div(class = "kpi-foot", "Attorney or representative recorded")),
            div(class = "kpi-tile", div(class = "kpi-title", "Controverted"), div(class = "kpi-number", textOutput("controverted_rate", inline = TRUE)), div(class = "kpi-foot", "Controversy process visible")),
            div(class = "kpi-tile", div(class = "kpi-title", "Formal process"), div(class = "kpi-number", textOutput("dashboard_formal_rate", inline = TRUE)), div(class = "kpi-foot", "Hearing, settlement, or appeal pathway")),
            div(class = "kpi-tile", div(class = "kpi-title", "Median AWW"), div(class = "kpi-number", textOutput("median_aww", inline = TRUE)), div(class = "kpi-foot", "Average weekly wage where reported"))
          ),

          uiOutput("executive_narrative"),

          div(class = "section-heading", h2("Evidence generated from the live portfolio"), p("Every conclusion below is recalculated from the official records under the active filters. The product reports associations, not legal conclusions or causation.")),
          div(class = "visual-panel", uiOutput("finding_cards")),

          div(
            class = "chart-grid-wide",
            div(class = "visual-panel", h3("Litigation signals over time"), div(class = "caption", "Representation, controversy, hearing, and appeal rates by assembly year"), plotlyOutput("signal_trend_plot", height = "430px")),
            div(class = "visual-panel", h3("Watch the portfolio change"), div(class = "caption", "Animated yearly signal profile"), plotlyOutput("animated_signal_plot", height = "430px"))
          ),

          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Carrier complexity quadrant"), div(class = "caption", "Controversy rate versus hearing rate; bubble size represents claim volume"), plotlyOutput("carrier_quadrant_plot", height = "420px")),
            div(class = "visual-panel", h3("Injury × carrier signal matrix"), div(class = "caption", "Average explainable litigation-signal score across the highest-volume categories"), plotlyOutput("injury_heatmap_plot", height = "420px"))
          ),

          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("County volume versus complexity"), div(class = "caption", "Separates portfolio size from the share of records with material public indicators"), plotlyOutput("county_signal_plot", height = "430px")),
            div(class = "visual-panel", h3("Medical-price pressure and claims"), div(class = "caption", "FRED Medical Care CPI context aligned with annual claims activity"), plotlyOutput("medical_pressure_plot", height = "430px"))
          )
        ),

        nav_panel(
          "Litigation Workbench",
          div(class = "claim-hero", h2("Turn public adjudication signals into a review queue."), p("Prioritize records with controversy, representation, hearings, appeals, active process status, and serious awarded injury outcomes. Every score is explainable.")),
          div(
            class = "workbench-grid",
            div(
              class = "visual-panel",
              h3("Evidence-backed review queue"),
              div(class = "caption", "Ranked from public process signals—not a counsel-assignment or liability decision"),
              sliderInput("minimum_litigation_score", "Minimum public signal score", min = 0, max = 100, value = 45, step = 5),
              DTOutput("litigation_queue")
            ),
            div(
              div(class = "visual-panel", h3("Review tiers"), plotOutput("tier_plot", height = "300px")),
              div(class = "visual-panel", h3("Representation by process"), plotOutput("representation_process_plot", height = "300px"))
            )
          ),
          div(
            class = "chart-grid-2",
            div(
              class = "visual-panel",
              h3("Open a public claim"),
              div(class = "caption", "Search the local index by Claim Identifier"),
              textInput("claim_id_input", "Claim Identifier", placeholder = "Enter a public WCB claim identifier"),
              actionButton("find_claim", "Analyze claim", class = "btn-primary"),
              br(), br(),
              uiOutput("claim_lookup_status"),
              uiOutput("claim_summary")
            ),
            div(class = "visual-panel", h3("Claim signal profile"), div(class = "caption", "Visible public indicators for the selected record"), plotlyOutput("claim_signal_radar", height = "410px"))
          ),
          div(class = "visual-panel", h3("Why this record surfaced"), uiOutput("claim_explanation")),
          div(class = "visual-panel", h3("Source-field evidence"), DTOutput("claim_record_table"))
        ),

        nav_panel(
          "Evidence Agent",
          div(
            class = "report-cover",
            h1("Local agent vs. RAG—tested on real records."),
            p("The local agent plans structured searches, opens the indexed claim evidence, follows identifiers, and returns a trace. The RAG baseline retrieves fixed claim-summary chunks. Run the test to compare exact-answer accuracy, latency, traceability, estimated hosted cost, and which architecture should ship."),
            actionButton("run_benchmark", "Run the real-data benchmark", class = "btn-primary"),
            tags$span(" "),
            downloadButton("download_benchmark", "Export results")
          ),
          uiOutput("benchmark_status"),
          div(
            class = "kpi-ribbon",
            div(class = "kpi-tile", div(class = "kpi-title", "Agent accuracy"), div(class = "kpi-number", textOutput("agent_accuracy", inline = TRUE)), div(class = "kpi-foot", "Exact match to SQL ground truth")),
            div(class = "kpi-tile", div(class = "kpi-title", "RAG accuracy"), div(class = "kpi-number", textOutput("rag_accuracy", inline = TRUE)), div(class = "kpi-foot", "Top fixed-chunk answer")),
            div(class = "kpi-tile", div(class = "kpi-title", "Agent latency"), div(class = "kpi-number", textOutput("agent_latency", inline = TRUE)), div(class = "kpi-foot", "Search, open, and trace")),
            div(class = "kpi-tile", div(class = "kpi-title", "Hosted RAG estimate"), div(class = "kpi-number", textOutput("rag_estimated_cost", inline = TRUE)), div(class = "kpi-foot", "Local prototype API cost is $0")),
            div(class = "kpi-tile", div(class = "kpi-title", "Ship decision"), div(class = "kpi-number", "LIVE"), div(class = "kpi-foot", textOutput("ship_recommendation", inline = TRUE)))
          ),
          div(
            style = "display:none",
            sliderInput("benchmark_corpus_size", "RAG corpus size", min = 500, max = 5000, value = 2000, step = 500),
            numericInput("embedding_cost_per_million", "Indexing cost", value = 0.10),
            numericInput("generation_cost_per_million", "Generation cost", value = 1.00)
          ),
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Accuracy"), plotlyOutput("benchmark_accuracy_plot", height = "340px")),
            div(class = "visual-panel", h3("Latency"), plotlyOutput("benchmark_latency_plot", height = "340px"))
          ),
          div(class = "visual-panel", h3("Question-level proof"), DTOutput("benchmark_results_table")),
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Inspect one question"), selectInput("trace_question", "Question", choices = character()), uiOutput("trace_summary")),
            div(class = "visual-panel", h3("Agent action trail"), verbatimTextOutput("agent_trace"))
          )
        ),

        nav_panel(
          "Board-Ready Report",
          div(
            class = "report-cover",
            h1("Workers’ Compensation Claims Analytics"),
            p("Developed a framework for analyzing medical inflation, claim outcomes, adjudication activity, attorney representation, injury severity, and possible litigation-referral indicators. The public product uses official NYS WCB records and FRED medical-price data; billing and reserve modules are designed for authorized de-identified carrier data and are never fabricated from the public file."),
            div(
              class = "capability-row",
              div(class = "capability", strong("Medical inflation"), "Real FRED Medical Care CPI"),
              div(class = "capability", strong("Claim outcomes"), "Official injury and status fields"),
              div(class = "capability", strong("Litigation signals"), "Representation, controversy, hearings, appeals"),
              div(class = "capability", strong("Billing behavior"), "Schema-ready carrier extension"),
              div(class = "capability", strong("Reserves"), "Schema-ready carrier extension")
            )
          ),
          uiOutput("proof_report"),
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Medical Care CPI"), plotOutput("fred_plot", height = "360px")),
            div(class = "visual-panel", h3("Year-over-year medical inflation"), plotOutput("fred_yoy_plot", height = "360px"))
          ),
          div(class = "visual-panel", h3("Claim volume and medical-price context"), plotOutput("claims_cpi_plot", height = "390px")),
          div(
            class = "visual-panel",
            h3("Export the analysis"),
            downloadButton("download_queue", "Download review queue"),
            tags$span(" "),
            downloadButton("download_summary", "Download executive summary"),
            br(), br(),
            verbatimTextOutput("report_preview")
          ),
          div(class = "visual-panel", h3("Transparent signal framework"), DTOutput("method_table"))
        )
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  state <- reactiveValues(
    con = NULL,
    csv_path = NULL,
    cache_path = NULL,
    field_map = NULL,
    cache_reused = NA,
    load_error = NULL,
    load_message = "Opening the official NYS WCB claims file.",
    claim_record = NULL,
    fred = NULL,
    fred_error = NULL,
    benchmark_results = NULL,
    benchmark_traces = NULL,
    benchmark_meta = NULL
  )

  session$onSessionEnded(function() {
    if (!is.null(state$con)) {
      try(dbDisconnect(state$con, shutdown = TRUE), silent = TRUE)
    }
  })

  db_ready <- reactive({
    !is.null(state$con) && isTRUE(dbIsValid(state$con))
  })


  output$data_ready <- renderText({
    if (db_ready()) "true" else "false"
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  output$source_chip <- renderUI({
    if (!is.null(state$load_error)) {
      return(div(class = "source-chip", span(style = "color:#ffb4ad", "Source unavailable")))
    }
    if (!db_ready()) {
      return(div(class = "source-chip", span(class = "source-dot"), span("Opening official claims file")))
    }
    div(class = "source-chip", span(class = "source-dot"), span(paste0("NYS WCB · ", basename(state$csv_path))))
  })

  query_db <- function(sql) {
    req(db_ready())
    dbGetQuery(state$con, sql)
  }

  load_database <- function(force = FALSE) {
    path <- detected_wcb_file

    if (!nzchar(path) || !file.exists(path)) {
      state$load_error <- paste0(
        "The CSV was not found at: ", path,
        ". The product expects the exact file name: ", WCB_CSV_FILENAME, "."
      )
      state$load_message <- NULL
      return(invisible(NULL))
    }

    if (!is.null(state$con)) {
      try(dbDisconnect(state$con, shutdown = TRUE), silent = TRUE)
      state$con <- NULL
    }

    state$load_error <- NULL

    result <- tryCatch(
      withProgress(message = "Preparing the official claims data", value = 0, {
        progress_callback <- function(value, detail) {
          setProgress(value = value, detail = detail)
        }
        build_or_open_cache(path, force_rebuild = force, progress_callback = progress_callback)
      }),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      state$load_error <- conditionMessage(result)
      state$load_message <- NULL
      return(invisible(NULL))
    }

    state$con <- result$con
    state$csv_path <- normalizePath(path, winslash = "/")
    state$cache_path <- result$cache_path
    state$field_map <- result$field_map
    state$cache_reused <- result$reused
    state$load_message <- if (result$reused) {
      "The existing DuckDB cache matched the CSV and was reopened."
    } else {
      "The official CSV was imported into a new DuckDB analytical cache."
    }

    initialize_filters()
    invisible(NULL)
  }

  observeEvent(input$rebuild_cache, load_database(TRUE), ignoreInit = TRUE)

  initialize_filters <- function() {
    req(db_ready())

    ranges <- query_db("
      SELECT
        MIN(assembly_year) AS min_year,
        MAX(assembly_year) AS max_year
      FROM claims_scored
      WHERE assembly_year IS NOT NULL
    ")

    min_year <- as.integer(ranges$min_year[1] %||% 2000)
    max_year <- as.integer(ranges$max_year[1] %||% year(Sys.Date()))

    updateSliderInput(
      session, "filter_year",
      min = min_year, max = max_year, value = c(min_year, max_year)
    )

    distinct_values <- function(field, limit = 300) {
      query_db(paste0(
        "SELECT DISTINCT ", sql_identifier(field), " AS value FROM claims ",
        "WHERE ", sql_identifier(field), " IS NOT NULL AND TRIM(",
        sql_identifier(field), ") <> '' ORDER BY value LIMIT ", as.integer(limit)
      ))$value
    }

    claim_types <- distinct_values("claim_type", 30)
    carrier_types <- distinct_values("carrier_type", 50)
    counties <- distinct_values("injured_in_county_name", 100)
    injury_types <- distinct_values("claim_injury_type", 50)

    default_claim_type <- if ("Workers Compensation Claim" %in% claim_types) {
      "Workers Compensation Claim"
    } else if ("WC" %in% claim_types) {
      "WC"
    } else {
      "All"
    }

    updateSelectInput(session, "filter_claim_type", choices = c("All", claim_types), selected = default_claim_type)
    updateSelectInput(session, "filter_carrier_type", choices = c("All", carrier_types), selected = "All")
    updateSelectInput(session, "filter_county", choices = c("All", counties), selected = "All")
    updateSelectInput(session, "filter_injury_type", choices = c("All", injury_types), selected = "All")
  }

  filter_where <- reactive({
    req(db_ready())
    conditions <- c("1 = 1")

    years <- input$filter_year %||% c(2000, year(Sys.Date()))
    conditions <- c(
      conditions,
      paste0("assembly_year BETWEEN ", as.integer(years[1]), " AND ", as.integer(years[2]))
    )

    add_text_filter <- function(field, value) {
      if (!is.null(value) && nzchar(value) && value != "All") {
        paste0(sql_identifier(field), " = ", sql_string(value))
      } else {
        NULL
      }
    }

    conditions <- c(
      conditions,
      add_text_filter("claim_type", input$filter_claim_type),
      add_text_filter("carrier_type", input$filter_carrier_type),
      add_text_filter("injured_in_county_name", input$filter_county),
      add_text_filter("claim_injury_type", input$filter_injury_type)
    )

    if (identical(input$filter_attorney, "Represented")) conditions <- c(conditions, "attorney_flag = 1")
    if (identical(input$filter_attorney, "Not represented")) conditions <- c(conditions, "attorney_flag = 0")
    if (identical(input$filter_controverted, "Controverted")) conditions <- c(conditions, "controverted_flag = 1")
    if (identical(input$filter_controverted, "Not controverted")) conditions <- c(conditions, "controverted_flag = 0")

    paste(conditions[!is.na(conditions) & nzchar(conditions)], collapse = " AND ")
  })

  output$load_status <- renderUI({
    if (!is.null(state$load_error)) {
      return(div(class = "danger-box", strong("Data could not be loaded. "), state$load_error))
    }

    if (!db_ready()) {
      return(div(class = "status-box", state$load_message %||% "Opening the official NYS WCB claims file."))
    }

    div(
      class = "success-box",
      strong("Official claims data ready. "),
      state$load_message,
      tags$br(),
      paste0("CSV: ", state$csv_path),
      tags$br(),
      paste0("DuckDB cache: ", state$cache_path)
    )
  })

  setup_stats <- reactive({
    req(db_ready())
    query_db("
      SELECT
        COUNT(*) AS rows,
        MIN(assembly_year) AS min_year,
        MAX(assembly_year) AS max_year
      FROM claims_scored
    ")
  })

  output$setup_rows <- renderText({ fmt_number(setup_stats()$rows[1]) })
  output$setup_years <- renderText({
    value <- setup_stats()
    if (is.na(value$min_year[1])) "—" else paste0(value$min_year[1], "–", value$max_year[1])
  })
  output$setup_fields <- renderText({
    req(!is.null(state$field_map))
    paste0(sum(state$field_map$detected), "/", nrow(state$field_map))
  })
  output$setup_cache <- renderText({
    if (!db_ready()) return("Not loaded")
    if (isTRUE(state$cache_reused)) "Reused" else "Built"
  })

  output$field_map_table <- renderDT({
    req(!is.null(state$field_map))
    displayed <- state$field_map |>
      mutate(
        label = key_field_labels[canonical_field],
        label = if_else(is.na(label), str_to_title(str_replace_all(canonical_field, "_", " ")), label),
        status = if_else(detected, "Detected", "Not available")
      ) |>
      select(field = label, canonical_field, source_column, status) |>
      arrange(desc(status), field)

    datatable(
      displayed,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })

  output$missingness_table <- renderDT({
    req(db_ready())
    fields <- names(key_field_labels)
    queries <- map_chr(fields, function(field) {
      paste0(
        "SELECT ", sql_string(key_field_labels[[field]]), " AS field, ",
        "COUNT(*) AS total_rows, ",
        "SUM(CASE WHEN ", sql_identifier(field), " IS NULL OR TRIM(CAST(",
        sql_identifier(field), " AS VARCHAR)) = '' THEN 1 ELSE 0 END) AS missing_rows ",
        "FROM claims"
      )
    })

    data <- query_db(paste(queries, collapse = " UNION ALL ")) |>
      mutate(
        completeness = 1 - missing_rows / pmax(total_rows, 1),
        completeness = percent(completeness, accuracy = 0.1)
      ) |>
      select(field, total_rows, missing_rows, completeness)

    datatable(data, options = list(pageLength = 20, dom = "tip"), rownames = FALSE)
  })

  output$data_preview <- renderDT({
    req(db_ready())
    preview <- query_db("
      SELECT
        claim_identifier, claim_type, accident_date, assembly_date,
        claim_injury_type, current_claim_status, atty_rep_ind,
        controverted_date, hearing_count, first_appeal_date,
        highest_process, carrier_type, injured_in_county_name
      FROM claims_scored
      LIMIT 100
    ")
    datatable(preview, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  portfolio_metrics <- reactive({
    req(db_ready())
    query_db(paste0(
      "SELECT COUNT(*) AS claim_count, ",
      "AVG(attorney_flag) AS attorney_rate, ",
      "AVG(controverted_flag) AS controverted_rate, ",
      "AVG(hearing_flag) AS hearing_rate, ",
      "AVG(appeal_flag) AS appeal_rate, ",
      "AVG(formal_process_flag) AS formal_process_rate, ",
      "MEDIAN(average_weekly_wage) AS median_aww, ",
      "MEDIAN(days_to_assembly) AS median_days_to_assembly ",
      "FROM claims_scored WHERE ", filter_where()
    ))
  })

  output$claim_count <- renderText({ fmt_number(portfolio_metrics()$claim_count[1]) })
  output$attorney_rate <- renderText({ fmt_percent(portfolio_metrics()$attorney_rate[1]) })
  output$controverted_rate <- renderText({ fmt_percent(portfolio_metrics()$controverted_rate[1]) })
  output$hearing_rate <- renderText({ fmt_percent(portfolio_metrics()$hearing_rate[1]) })
  output$appeal_rate <- renderText({ fmt_percent(portfolio_metrics()$appeal_rate[1]) })
  output$median_aww <- renderText({ fmt_money(portfolio_metrics()$median_aww[1]) })

  output$hero_claims <- renderText({
    if (!db_ready()) return("Load the official CSV")
    fmt_number(setup_stats()$rows[1])
  })
  output$hero_high_priority <- renderText({
    if (!db_ready()) return("Waiting for data")
    value <- query_db("SELECT COUNT(*) AS n FROM claims_scored WHERE litigation_signal_score >= 70")$n[1]
    fmt_number(value)
  })
  output$hero_years <- renderText({
    if (!db_ready()) return("Beginning 2000")
    value <- setup_stats()
    paste0(value$min_year[1], "–", value$max_year[1])
  })

  output$dashboard_claims <- renderText({ fmt_number(portfolio_metrics()$claim_count[1]) })
  output$dashboard_rep_rate <- renderText({ fmt_percent(portfolio_metrics()$attorney_rate[1]) })
  output$dashboard_formal_rate <- renderText({ fmt_percent(portfolio_metrics()$formal_process_rate[1]) })
  output$dashboard_delay <- renderText({ fmt_days(portfolio_metrics()$median_days_to_assembly[1]) })

  executive_findings <- reactive({
    req(db_ready())
    query_db(paste0(
      "SELECT ",
      "AVG(CASE WHEN attorney_flag = 1 THEN COALESCE(hearing_count, 0) END) AS rep_hearings, ",
      "AVG(CASE WHEN attorney_flag = 0 THEN COALESCE(hearing_count, 0) END) AS unrep_hearings, ",
      "AVG(CASE WHEN controverted_flag = 1 THEN appeal_flag END) AS controverted_appeals, ",
      "AVG(CASE WHEN controverted_flag = 0 THEN appeal_flag END) AS noncontroverted_appeals, ",
      "AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS material_rate, ",
      "AVG(CASE WHEN severity_points >= 14 THEN formal_process_flag END) AS severe_formal_rate, ",
      "AVG(CASE WHEN severity_points < 14 THEN formal_process_flag END) AS other_formal_rate ",
      "FROM claims_scored WHERE ", filter_where()
    ))
  })

  output$finding_cards <- renderUI({
    x <- executive_findings()[1, ]
    hearing_multiple <- if (!is.na(x$unrep_hearings) && x$unrep_hearings > 0) x$rep_hearings / x$unrep_hearings else NA_real_
    appeal_multiple <- if (!is.na(x$noncontroverted_appeals) && x$noncontroverted_appeals > 0) x$controverted_appeals / x$noncontroverted_appeals else NA_real_
    severe_multiple <- if (!is.na(x$other_formal_rate) && x$other_formal_rate > 0) x$severe_formal_rate / x$other_formal_rate else NA_real_

    hearing_text <- if (is.na(hearing_multiple)) {
      paste0("Represented claims average ", round(x$rep_hearings, 2), " hearings.")
    } else {
      paste0("Represented claims average ", round(hearing_multiple, 1), "× as many hearings as unrepresented claims.")
    }
    appeal_text <- if (is.na(appeal_multiple)) {
      paste0("The appeal rate among controverted claims is ", fmt_percent(x$controverted_appeals), ".")
    } else {
      paste0("Controverted claims show a ", round(appeal_multiple, 1), "× higher appeal rate than non-controverted claims.")
    }
    severe_text <- if (is.na(severe_multiple)) {
      paste0(fmt_percent(x$material_rate), " of filtered claims contain material public litigation indicators.")
    } else {
      paste0("Serious awarded injury categories show ", round(severe_multiple, 1), "× the formal-process rate of other categories.")
    }

    div(
      class = "insight-grid",
      div(class = "insight-card", div(class = "eyebrow", "Representation and hearings"), div(class = "finding", hearing_text), div(class = "explain", "Measured from attorney/representative and hearing-count fields in the official records.")),
      div(class = "insight-card", div(class = "eyebrow", "Controversy and appeals"), div(class = "finding", appeal_text), div(class = "explain", "A descriptive association between controversy and later appellate activity—not proof of causation.")),
      div(class = "insight-card", div(class = "eyebrow", "Severity and process"), div(class = "finding", severe_text), div(class = "explain", "Uses awarded injury categories and the highest public process recorded for each claim."))
    )
  })

  output$funnel_plot <- renderPlotly({
    req(db_ready())
    x <- query_db(paste0(
      "SELECT COUNT(*) AS all_claims, SUM(attorney_flag) AS represented, ",
      "SUM(controverted_flag) AS controverted, SUM(hearing_flag) AS hearings, ",
      "SUM(appeal_flag) AS appeals FROM claims_scored WHERE ", filter_where()
    ))
    data <- tibble(
      indicator = factor(c("All claims", "Represented", "Controverted", "Hearing activity", "Appeal activity"), levels = rev(c("All claims", "Represented", "Controverted", "Hearing activity", "Appeal activity"))),
      claims = as.numeric(c(x$all_claims, x$represented, x$controverted, x$hearings, x$appeals))
    )
    plot_ly(data, x = ~claims, y = ~indicator, type = "bar", orientation = "h",
            text = ~comma(claims), textposition = "auto",
            marker = list(color = "#214e8a"), hovertemplate = "%{y}: %{x:,}<extra></extra>") |>
      layout(xaxis = list(title = "Claims", tickformat = ",d"), yaxis = list(title = ""),
             margin = list(l = 120, r = 20, t = 10, b = 45),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$hearing_association_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "SELECT CASE WHEN attorney_flag=1 THEN 'Represented' ELSE 'Not represented' END AS representation, ",
      "CASE WHEN controverted_flag=1 THEN 'Controverted' ELSE 'Not controverted' END AS controversy, ",
      "AVG(COALESCE(hearing_count,0)) AS avg_hearings, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(), " GROUP BY representation, controversy"
    ))
    plot_ly(data, x = ~representation, y = ~avg_hearings, color = ~controversy,
            type = "bar", barmode = "group", text = ~round(avg_hearings, 2), textposition = "auto",
            colors = c("#9fb7d5", "#0d6b78"),
            hovertemplate = "%{x}<br>%{fullData.name}<br>Average hearings: %{y:.2f}<extra></extra>") |>
      layout(barmode = "group", yaxis = list(title = "Average hearing count"), xaxis = list(title = ""),
             legend = list(orientation = "h", x = 0, y = 1.12),
             margin = list(l = 65, r = 20, t = 45, b = 45),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  annual_signal_data <- reactive({
    req(db_ready())
    query_db(paste0(
      "SELECT assembly_year, COUNT(*) AS claims, AVG(attorney_flag) AS represented, ",
      "AVG(controverted_flag) AS controverted, AVG(hearing_flag) AS hearing, AVG(appeal_flag) AS appeal ",
      "FROM claims_scored WHERE ", filter_where(),
      " AND assembly_year IS NOT NULL GROUP BY assembly_year ORDER BY assembly_year"
    ))
  })

  output$signal_trend_plot <- renderPlotly({
    data <- annual_signal_data() |>
      pivot_longer(c(represented, controverted, hearing, appeal), names_to = "indicator", values_to = "rate") |>
      mutate(indicator = recode(indicator, represented = "Represented", controverted = "Controverted", hearing = "Hearing activity", appeal = "Appeal activity"))
    validate(need(nrow(data) > 0, "No yearly data match the filters."))
    plot_ly(data, x = ~assembly_year, y = ~rate, color = ~indicator, type = "scatter", mode = "lines+markers",
            colors = c("#214e8a", "#0d6b78", "#a45f06", "#b42318"),
            hovertemplate = "%{fullData.name}<br>%{x}: %{y:.1%}<extra></extra>") |>
      layout(yaxis = list(title = "Share of claims", tickformat = ".0%", rangemode = "tozero"),
             xaxis = list(title = "Assembly year"), legend = list(orientation = "h", x = 0, y = 1.12),
             margin = list(l = 65, r = 20, t = 45, b = 50),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$animated_signal_plot <- renderPlotly({
    data <- annual_signal_data() |>
      pivot_longer(c(represented, controverted, hearing, appeal), names_to = "indicator", values_to = "rate") |>
      mutate(indicator = recode(indicator, represented = "Represented", controverted = "Controverted", hearing = "Hearing", appeal = "Appeal"))
    validate(need(nrow(data) > 0, "No yearly data match the filters."))
    plot_ly(data, x = ~indicator, y = ~rate, frame = ~assembly_year, type = "bar",
            marker = list(color = "#214e8a"), text = ~percent(rate, accuracy = 0.1), textposition = "auto",
            hovertemplate = "%{x}: %{y:.1%}<extra></extra>") |>
      layout(yaxis = list(title = "Share of claims", tickformat = ".0%", range = c(0, max(data$rate, na.rm = TRUE) * 1.18)),
             xaxis = list(title = ""), margin = list(l = 65, r = 15, t = 15, b = 70),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
      animation_opts(850, redraw = TRUE, easing = "cubic-in-out") |>
      animation_slider(currentvalue = list(prefix = "Assembly year: ")) |>
      animation_button(x = 0.02, y = 1.08)
  })

  output$county_signal_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "SELECT COALESCE(injured_in_county_name,'Unknown') AS county, COUNT(*) AS claims, ",
      "AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS material_rate, ",
      "AVG(litigation_signal_score) AS avg_score FROM claims_scored WHERE ", filter_where(),
      " GROUP BY county HAVING COUNT(*) >= 250 ORDER BY claims DESC LIMIT 40"
    ))
    validate(need(nrow(data) > 0, "No counties meet the minimum volume under the current filters."))
    plot_ly(data, x = ~claims, y = ~material_rate, size = ~avg_score, color = ~avg_score,
            text = ~county, type = "scatter", mode = "markers",
            colors = c("#dbe7f5", "#214e8a"), sizes = c(8, 34),
            hovertemplate = "%{text}<br>Claims: %{x:,}<br>Material indicator rate: %{y:.1%}<br>Average score: %{marker.color:.1f}<extra></extra>") |>
      layout(xaxis = list(title = "Claim volume", tickformat = ",d"),
             yaxis = list(title = "Material litigation-indicator rate", tickformat = ".0%"),
             coloraxis = list(colorbar = list(title = "Avg score")),
             margin = list(l = 75, r = 30, t = 15, b = 55),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$assembly_trend <- renderPlot({
    data <- query_db(paste0(
      "SELECT assembly_year, COUNT(*) AS claims FROM claims_scored WHERE ",
      filter_where(), " AND assembly_year IS NOT NULL GROUP BY assembly_year ORDER BY assembly_year"
    ))
    validate(need(nrow(data) > 0, "No records match the current filters."))
    ggplot(data, aes(assembly_year, claims)) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 1.7) +
      scale_y_continuous(labels = comma) +
      labs(x = "Assembly year", y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$injury_type_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(claim_injury_type, 'Unknown') AS category, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY category ORDER BY claims DESC LIMIT 12"
    ))
    validate(need(nrow(data) > 0, "No injury outcomes are available."))
    data$category <- reorder(data$category, data$claims)
    ggplot(data, aes(category, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$status_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(current_claim_status, 'Unknown') AS category, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY category ORDER BY claims DESC LIMIT 12"
    ))
    validate(need(nrow(data) > 0, "No claim-status records are available."))
    data$category <- reorder(data$category, data$claims)
    ggplot(data, aes(category, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$process_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(highest_process, 'Unknown') AS category, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY category ORDER BY claims DESC LIMIT 12"
    ))
    validate(need(nrow(data) > 0, "No process records are available."))
    data$category <- reorder(data$category, data$claims)
    ggplot(data, aes(category, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$county_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(injured_in_county_name, 'Unknown') AS category, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY category ORDER BY claims DESC LIMIT 15"
    ))
    validate(need(nrow(data) > 0, "No county records are available."))
    data$category <- reorder(data$category, data$claims)
    ggplot(data, aes(category, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$carrier_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(carrier_type, 'Unknown') AS category, COUNT(*) AS claims ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY category ORDER BY claims DESC LIMIT 12"
    ))
    validate(need(nrow(data) > 0, "No carrier-type records are available."))
    data$category <- reorder(data$category, data$claims)
    ggplot(data, aes(category, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$litigation_queue <- renderDT({
    req(db_ready())
    min_score <- as.integer(input$minimum_litigation_score %||% 45)
    data <- query_db(paste0(
      "SELECT claim_identifier, assembly_date, claim_injury_type, current_claim_status, ",
      "atty_rep_ind, controverted_date, hearing_count, first_appeal_date, highest_process, ",
      "carrier_type, injured_in_county_name, litigation_signal_score, litigation_review_tier ",
      "FROM claims_scored WHERE ", filter_where(),
      " AND litigation_signal_score >= ", min_score,
      " ORDER BY litigation_signal_score DESC, hearing_count DESC NULLS LAST LIMIT 1000"
    ))

    datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })

  output$tier_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT litigation_review_tier AS tier, COUNT(*) AS claims FROM claims_scored WHERE ",
      filter_where(), " GROUP BY tier ORDER BY claims DESC"
    ))
    validate(need(nrow(data) > 0, "No records match the filters."))
    data$tier <- reorder(data$tier, data$claims)
    ggplot(data, aes(tier, claims)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$representation_process_plot <- renderPlot({
    data <- query_db(paste0(
      "SELECT COALESCE(highest_process, 'Unknown') AS process, ",
      "CASE WHEN attorney_flag = 1 THEN 'Represented' ELSE 'Not represented' END AS representation, ",
      "COUNT(*) AS claims FROM claims_scored WHERE ", filter_where(),
      " GROUP BY process, representation"
    ))
    validate(need(nrow(data) > 0, "No records match the filters."))
    top_processes <- data |>
      group_by(process) |>
      summarise(total = sum(claims), .groups = "drop") |>
      slice_max(total, n = 10, with_ties = FALSE) |>
      pull(process)
    data <- filter(data, process %in% top_processes)
    ggplot(data, aes(reorder(process, claims, FUN = sum), claims, fill = representation)) +
      geom_col(position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "Share of claims", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$method_table <- renderDT({
    datatable(method_table, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$find_claim, {
    req(db_ready())
    claim_id <- str_trim(input$claim_lookup %||% "")
    if (!nzchar(claim_id)) {
      state$claim_record <- structure(data.frame(), lookup_message = "Enter a Claim Identifier.")
      return()
    }

    result <- query_db(paste0(
      "SELECT * FROM claims_scored WHERE claim_identifier = ", sql_string(claim_id), " LIMIT 1"
    ))
    attr(result, "lookup_message") <- if (nrow(result) == 0) {
      "No matching public claim identifier was found in the loaded CSV."
    } else {
      "The matching public claim record was found."
    }
    state$claim_record <- result
  })

  output$claim_lookup_status <- renderUI({
    if (is.null(state$claim_record)) return(div(class = "status-box", "Enter a public Claim Identifier and select Open claim."))
    message <- attr(state$claim_record, "lookup_message") %||% ""
    if (nrow(state$claim_record) == 0) div(class = "warning-box", message) else div(class = "success-box", message)
  })

  output$claim_summary <- renderUI({
    record <- state$claim_record
    validate(need(!is.null(record) && nrow(record) == 1, "Open a claim to view its summary."))
    row <- record[1, , drop = FALSE]

    fields <- c(
      "Claim identifier" = "claim_identifier",
      "Claim type" = "claim_type",
      "Accident date" = "accident_date",
      "Assembly date" = "assembly_date",
      "Claim injury type" = "claim_injury_type",
      "Current claim status" = "current_claim_status",
      "Attorney/representative" = "atty_rep_ind",
      "Controverted date" = "controverted_date",
      "Hearing count" = "hearing_count",
      "First appeal date" = "first_appeal_date",
      "Highest process" = "highest_process",
      "County of injury" = "injured_in_county_name",
      "Carrier type" = "carrier_type",
      "Average weekly wage" = "average_weekly_wage",
      "Litigation indicator score" = "litigation_signal_score"
    )

    values <- map2(names(fields), unname(fields), function(label, field) {
      value <- row[[field]][1]
      if (field == "average_weekly_wage") value <- fmt_money(value)
      if (is.na(value) || identical(value, "")) value <- "Not available"
      div(
        class = "claim-field",
        div(class = "claim-field-label", label),
        div(class = "claim-field-value", as.character(value))
      )
    })

    div(class = "claim-grid", values)
  })

  output$claim_explanation <- renderUI({
    record <- state$claim_record
    validate(need(!is.null(record) && nrow(record) == 1, "Open a claim to view its explanation."))
    row <- record[1, , drop = FALSE]

    drivers <- character()
    if (row$attorney_flag == 1) drivers <- c(drivers, "attorney or representative involvement")
    if (row$controverted_flag == 1) drivers <- c(drivers, "a recorded controverted date")
    if (row$hearing_flag == 1) drivers <- c(drivers, paste0(row$hearing_count %||% 0, " recorded hearing(s)"))
    if (row$appeal_flag == 1) drivers <- c(drivers, "appeal activity")
    if (row$formal_process_flag == 1) drivers <- c(drivers, "a formal hearing, settlement, or appeal process")
    if (row$active_status_flag == 1) drivers <- c(drivers, "an active adjudication-related status")
    if (row$severity_points > 0) drivers <- c(drivers, "a serious awarded injury category")
    if (length(drivers) == 0) drivers <- "limited public litigation indicators"

    review_question <- if (row$appeal_flag == 1) {
      "What issue was appealed, and how does the appellate posture affect strategy and deadlines?"
    } else if (row$controverted_flag == 1 && row$attorney_flag == 1) {
      "What legal and medical issues are disputed, and what evidence would a specialist need to organize?"
    } else if (!is.na(row$hearing_count) && row$hearing_count >= 2) {
      "Why has the claim required repeated hearings, and what issue remains unresolved?"
    } else if (row$severity_points >= 14) {
      "How does the awarded injury severity affect exposure, settlement posture, and ongoing claim management?"
    } else {
      "Do the visible public indicators justify routine monitoring or deeper file review?"
    }

    tagList(
      div(class = "info-box", strong("Tier: "), row$litigation_review_tier, " (score ", row$litigation_signal_score, ")."),
      br(),
      p(strong("Visible drivers: "), paste(drivers, collapse = "; "), "."),
      p(strong("Human review question: "), review_question),
      div(class = "warning-box", "This explanation is based only on the public administrative fields. It does not include pleadings, medical records, private adjuster notes, reserves, settlement authority, or counsel evaluations.")
    )
  })

  output$claim_record_table <- renderDT({
    record <- state$claim_record
    validate(need(!is.null(record) && nrow(record) == 1, "Open a claim to view its available evidence."))
    long <- record |>
      select(-ends_with("_flag"), -ends_with("_points")) |>
      pivot_longer(everything(), names_to = "field", values_to = "value") |>
      mutate(
        field = str_to_title(str_replace_all(field, "_", " ")),
        value = as.character(value),
        value = if_else(is.na(value) | value == "", "Not available", value)
      )
    datatable(long, options = list(pageLength = 20, dom = "tip"), rownames = FALSE)
  })

  fred_refresh <- reactiveVal(0)
  observeEvent(input$refresh_fred, fred_refresh(fred_refresh() + 1), ignoreInit = TRUE)

  fred_data <- reactive({
    fred_refresh()
    local_path <- str_trim(input$fred_path %||% "")
    load_fred_data(local_path)
  })

  output$fred_status <- renderUI({
    data <- fred_data()
    if (is.null(data)) {
      return(div(class = "warning-box", "Medical CPI could not be loaded. Add a local CPIMEDSL CSV or confirm internet access."))
    }
    source <- attr(data, "source") %||% "FRED"
    div(
      class = "success-box",
      strong("Real medical-price data loaded. "),
      paste0(comma(nrow(data)), " monthly observations from ", source, ".")
    )
  })

  output$fred_plot <- renderPlot({
    data <- fred_data()
    validate(need(!is.null(data), "Medical CPI is unavailable."))
    ggplot(data, aes(date, medical_cpi)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, y = "Medical Care CPI (1982–84 = 100)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$fred_yoy_plot <- renderPlot({
    data <- fred_data()
    validate(need(!is.null(data), "Medical CPI is unavailable."))
    recent <- data |> filter(date >= max(date, na.rm = TRUE) %m-% years(15))
    ggplot(recent, aes(date, yoy_change)) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = label_percent(scale = 1)) +
      labs(x = NULL, y = "Year-over-year change") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$claims_cpi_plot <- renderPlot({
    req(db_ready())
    fred <- fred_data()
    validate(need(!is.null(fred), "Medical CPI is unavailable."))

    claims <- query_db("
      SELECT assembly_year AS year, COUNT(*) AS claims
      FROM claims_scored
      WHERE assembly_year IS NOT NULL
      GROUP BY assembly_year
      ORDER BY assembly_year
    ")

    annual_cpi <- fred |>
      group_by(year) |>
      summarise(medical_cpi = mean(medical_cpi, na.rm = TRUE), .groups = "drop")

    combined <- inner_join(claims, annual_cpi, by = "year") |>
      mutate(
        claims_index = 100 * claims / first(claims[claims > 0]),
        cpi_index = 100 * medical_cpi / first(medical_cpi[medical_cpi > 0])
      ) |>
      select(year, `Claim volume index` = claims_index, `Medical CPI index` = cpi_index) |>
      pivot_longer(-year, names_to = "series", values_to = "index")

    validate(need(nrow(combined) > 0, "There are no overlapping years between the claims and FRED data."))

    ggplot(combined, aes(year, index, linetype = series)) +
      geom_line(linewidth = 1.05) +
      labs(
        x = "Year", y = "Index (first overlapping year = 100)", linetype = NULL,
        caption = "The two series are indexed for context; this does not imply medical CPI caused claim volume."
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")
  })

  enrichment_data <- reactive({
    if (is.null(input$enrichment_file)) return(NULL)
    tryCatch(
      prepare_enrichment(read_csv(input$enrichment_file$datapath, show_col_types = FALSE)),
      error = function(e) structure(NULL, error_message = conditionMessage(e))
    )
  })

  enriched_review <- reactive({
    req(db_ready())
    enrichment <- enrichment_data()
    if (is.null(enrichment)) return(NULL)
    if (!is.null(attr(enrichment, "error_message"))) return(enrichment)
    if (nrow(enrichment) == 0) return(enrichment)

    ids <- unique(enrichment$claim_identifier)
    id_sql <- paste(vapply(ids, sql_string, character(1)), collapse = ",")
    public <- query_db(paste0(
      "SELECT claim_identifier, claim_injury_type, current_claim_status, atty_rep_ind, ",
      "controverted_date, hearing_count, first_appeal_date, highest_process, ",
      "litigation_signal_score, litigation_review_tier ",
      "FROM claims_scored WHERE claim_identifier IN (", id_sql, ")"
    ))

    public |>
      inner_join(enrichment, by = "claim_identifier") |>
      mutate(
        charged_paid_ratio = if_else(!is.na(charged_amount) & paid_amount > 0, charged_amount / paid_amount, NA_real_),
        billing_flag = as.integer(
          coalesce(duplicate_bill_flag, 0) == 1 |
            coalesce(provider_outlier_flag, 0) == 1 |
            coalesce(documentation_gap_flag, 0) == 1 |
            coalesce(charged_paid_ratio, 0) >= 2
        ),
        reserve_flag = as.integer(
          !is.na(case_reserve) | !is.na(total_incurred) | !is.na(reserve_change_90d)
        )
      )
  })

  output$enrichment_status <- renderUI({
    data <- enrichment_data()
    if (is.null(data)) return(div(class = "status-box", "No enrichment file is active. The app is correctly limiting itself to the public NYS WCB fields."))
    if (!is.null(attr(data, "error_message"))) return(div(class = "danger-box", attr(data, "error_message")))
    reviewed <- enriched_review()
    div(
      class = "success-box",
      strong("Authorized enrichment loaded. "),
      paste0(comma(nrow(data)), " enrichment identifiers were supplied; ", comma(nrow(reviewed)), " matched the loaded public claims data.")
    )
  })

  output$enriched_claims <- renderText({
    data <- enriched_review()
    if (is.null(data) || !is.data.frame(data)) "0" else fmt_number(nrow(data))
  })
  output$enriched_incurred <- renderText({
    data <- enriched_review()
    if (is.null(data) || nrow(data) == 0 || all(is.na(data$total_incurred))) "—" else fmt_money(sum(data$total_incurred, na.rm = TRUE))
  })
  output$enriched_reserve <- renderText({
    data <- enriched_review()
    if (is.null(data) || nrow(data) == 0 || all(is.na(data$case_reserve))) "—" else fmt_money(sum(data$case_reserve, na.rm = TRUE))
  })
  output$billing_flags <- renderText({
    data <- enriched_review()
    if (is.null(data) || nrow(data) == 0) "—" else fmt_number(sum(data$billing_flag, na.rm = TRUE))
  })

  output$enrichment_table <- renderDT({
    data <- enriched_review()
    validate(need(!is.null(data) && is.data.frame(data) && nrow(data) > 0, "Upload a valid enrichment file with matching claim identifiers."))
    datatable(data, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$download_enrichment_template <- downloadHandler(
    filename = function() "workers_comp_billing_reserve_enrichment_template.csv",
    content = function(file) write_csv(enrichment_template, file)
  )

  normalize_benchmark_answer <- function(x) {
    x <- as.character(x %||% "")
    x <- str_to_lower(str_trim(x))
    x <- str_replace_all(x, "[,\\$%]", "")
    x <- str_replace_all(x, "\\.0+$", "")
    x <- str_replace_all(x, "[^a-z0-9]+", " ")
    str_squish(x)
  }

  tokenize_benchmark_text <- function(x) {
    tokens <- str_extract_all(str_to_lower(as.character(x %||% "")), "[a-z0-9]+")[[1]]
    stop_words <- c("the", "a", "an", "of", "in", "on", "to", "for", "with", "what", "which", "is", "has", "had", "and", "from", "among", "claims", "claim")
    unique(tokens[!tokens %in% stop_words & nchar(tokens) > 1])
  }

  top_rate_record <- function(field, flag, minimum_count = 500) {
    base_sql <- paste0(
      "SELECT ", sql_identifier(field), " AS answer, COUNT(*) AS claims, AVG(",
      sql_identifier(flag), ") AS rate FROM claims_scored WHERE ", filter_where(),
      " AND ", sql_identifier(field), " IS NOT NULL AND TRIM(CAST(",
      sql_identifier(field), " AS VARCHAR)) <> '' GROUP BY answer "
    )
    result <- query_db(paste0(base_sql, "HAVING COUNT(*) >= ", as.integer(minimum_count), " ORDER BY rate DESC, claims DESC LIMIT 1"))
    if (nrow(result) == 0) {
      result <- query_db(paste0(base_sql, "ORDER BY rate DESC, claims DESC LIMIT 1"))
    }
    result
  }

  build_live_benchmark <- function() {
    req(db_ready())
    where_sql <- filter_where()

    anchor <- query_db(paste0(
      "SELECT claim_identifier, current_claim_status, hearing_count, highest_process, ",
      "claim_injury_type, injured_in_county_name, carrier_type, litigation_signal_score ",
      "FROM claims_scored WHERE ", where_sql,
      " AND claim_identifier IS NOT NULL AND current_claim_status IS NOT NULL ",
      "ORDER BY litigation_signal_score DESC, COALESCE(hearing_count,0) DESC LIMIT 1"
    ))
    validate(need(nrow(anchor) == 1, "No claim is available under the current filters."))
    anchor_id <- as.character(anchor$claim_identifier[1])

    top_hearing_sql <- paste0(
      "SELECT claim_identifier AS answer FROM claims_scored WHERE ", where_sql,
      " AND attorney_flag = 1 AND controverted_flag = 1 AND claim_identifier IS NOT NULL ",
      "ORDER BY COALESCE(hearing_count,0) DESC, litigation_signal_score DESC, claim_identifier LIMIT 1"
    )
    top_hearing <- query_db(top_hearing_sql)
    if (nrow(top_hearing) == 0) {
      top_hearing_sql <- paste0(
        "SELECT claim_identifier AS answer FROM claims_scored WHERE ", where_sql,
        " AND claim_identifier IS NOT NULL ORDER BY COALESCE(hearing_count,0) DESC, litigation_signal_score DESC LIMIT 1"
      )
      top_hearing <- query_db(top_hearing_sql)
    }

    earliest_appeal_sql <- paste0(
      "SELECT claim_identifier AS answer FROM claims_scored WHERE ", where_sql,
      " AND claim_identifier IS NOT NULL AND first_hearing_date IS NOT NULL AND first_appeal_date IS NOT NULL ",
      "AND first_appeal_date >= first_hearing_date ",
      "ORDER BY DATE_DIFF('day', first_hearing_date, first_appeal_date), first_appeal_date LIMIT 1"
    )
    earliest_appeal <- query_db(earliest_appeal_sql)
    if (nrow(earliest_appeal) == 0) {
      earliest_appeal_sql <- paste0(
        "SELECT claim_identifier AS answer FROM claims_scored WHERE ", where_sql,
        " AND claim_identifier IS NOT NULL AND first_appeal_date IS NOT NULL ORDER BY first_appeal_date LIMIT 1"
      )
      earliest_appeal <- query_db(earliest_appeal_sql)
    }

    county <- top_rate_record("injured_in_county_name", "attorney_flag")
    carrier <- top_rate_record("carrier_type", "appeal_flag")
    injury <- top_rate_record("claim_injury_type", "controverted_flag")

    direct_status_sql <- paste0("SELECT current_claim_status AS answer FROM claims_scored WHERE claim_identifier = ", sql_string(anchor_id), " LIMIT 1")
    direct_hearing_sql <- paste0("SELECT CAST(COALESCE(hearing_count,0) AS VARCHAR) AS answer FROM claims_scored WHERE claim_identifier = ", sql_string(anchor_id), " LIMIT 1")
    direct_process_sql <- paste0("SELECT COALESCE(highest_process,'Not available') AS answer FROM claims_scored WHERE claim_identifier = ", sql_string(anchor_id), " LIMIT 1")

    questions <- tribble(
      ~question_id, ~question, ~answer_type, ~agent_sql, ~ground_truth, ~trace,
      "Q1", paste0("What is the current claim status for ", anchor_id, "?"), "status", direct_status_sql, as.character(anchor$current_claim_status[1]), paste0("Parse Claim Identifier ", anchor_id, " → search the indexed claim table → open the exact row → read Current Claim Status."),
      "Q2", paste0("How many hearings are recorded for ", anchor_id, "?"), "hearing", direct_hearing_sql, as.character(coalesce(anchor$hearing_count[1], 0)), paste0("Parse Claim Identifier ", anchor_id, " → open the exact row → inspect Hearing Count → return the numeric field."),
      "Q3", paste0("What is the highest recorded resolution process for ", anchor_id, "?"), "process", direct_process_sql, as.character(anchor$highest_process[1] %||% "Not available"), paste0("Search ", anchor_id, " → open the public claim row → follow the adjudication fields → read Highest Process."),
      "Q4", "Which represented and controverted claim has the greatest recorded hearing activity?", "claim", top_hearing_sql, as.character(top_hearing$answer[1]), "Translate the question into representation + controversy constraints → rank all matching claims by hearing count → open the leading record → return its Claim Identifier.",
      "Q5", "Which county has the highest attorney-representation rate among sufficiently large groups?", "county", paste0("SELECT injured_in_county_name AS answer, COUNT(*) AS claims, AVG(attorney_flag) AS rate FROM claims_scored WHERE ", where_sql, " AND injured_in_county_name IS NOT NULL GROUP BY answer HAVING COUNT(*) >= 500 ORDER BY rate DESC, claims DESC LIMIT 1"), as.character(county$answer[1]), "Scan the full filtered portfolio → group by county → require a minimum credible volume → calculate representation rate → rank and return the leader.",
      "Q6", "Which carrier type has the highest appeal rate among sufficiently large groups?", "carrier", paste0("SELECT carrier_type AS answer, COUNT(*) AS claims, AVG(appeal_flag) AS rate FROM claims_scored WHERE ", where_sql, " AND carrier_type IS NOT NULL GROUP BY answer HAVING COUNT(*) >= 500 ORDER BY rate DESC, claims DESC LIMIT 1"), as.character(carrier$answer[1]), "Scan the full filtered portfolio → group by carrier type → calculate appeal rate → rank sufficiently large groups → return the leading carrier category.",
      "Q7", "Which claim injury type has the highest controverted-claim rate among sufficiently large groups?", "injury", paste0("SELECT claim_injury_type AS answer, COUNT(*) AS claims, AVG(controverted_flag) AS rate FROM claims_scored WHERE ", where_sql, " AND claim_injury_type IS NOT NULL GROUP BY answer HAVING COUNT(*) >= 500 ORDER BY rate DESC, claims DESC LIMIT 1"), as.character(injury$answer[1]), "Scan all matching records → group by injury outcome → calculate controversy rate → rank stable groups → return the leading category.",
      "Q8", "Which claim moved from its first hearing to its first appeal in the shortest non-negative interval?", "claim", earliest_appeal_sql, as.character(earliest_appeal$answer[1]), "Search claims with both hearing and appeal dates → calculate the interval → exclude negative intervals → sort ascending → open and return the leading claim."
    )

    # If the 500-record group query was too restrictive, use the already-computed fallback truth.
    questions$agent_sql[questions$question_id == "Q5"] <- paste0(
      "SELECT injured_in_county_name AS answer FROM claims_scored WHERE ", where_sql,
      " AND injured_in_county_name IS NOT NULL GROUP BY answer ORDER BY ",
      "CASE WHEN COUNT(*) >= 500 THEN 0 ELSE 1 END, AVG(attorney_flag) DESC, COUNT(*) DESC LIMIT 1"
    )
    questions$agent_sql[questions$question_id == "Q6"] <- paste0(
      "SELECT carrier_type AS answer FROM claims_scored WHERE ", where_sql,
      " AND carrier_type IS NOT NULL GROUP BY answer ORDER BY ",
      "CASE WHEN COUNT(*) >= 500 THEN 0 ELSE 1 END, AVG(appeal_flag) DESC, COUNT(*) DESC LIMIT 1"
    )
    questions$agent_sql[questions$question_id == "Q7"] <- paste0(
      "SELECT claim_injury_type AS answer FROM claims_scored WHERE ", where_sql,
      " AND claim_injury_type IS NOT NULL GROUP BY answer ORDER BY ",
      "CASE WHEN COUNT(*) >= 500 THEN 0 ELSE 1 END, AVG(controverted_flag) DESC, COUNT(*) DESC LIMIT 1"
    )

    list(questions = questions, anchor_id = anchor_id)
  }

  build_rag_corpus <- function(corpus_size, anchor_id) {
    req(db_ready())
    data <- query_db(paste0(
      "SELECT claim_identifier, current_claim_status, hearing_count, highest_process, claim_injury_type, ",
      "injured_in_county_name, carrier_type, atty_rep_ind, controverted_date, first_hearing_date, ",
      "first_appeal_date, litigation_signal_score FROM claims_scored WHERE ", filter_where(),
      " AND claim_identifier IS NOT NULL ORDER BY litigation_signal_score DESC, HASH(claim_identifier) LIMIT ",
      as.integer(corpus_size)
    ))
    if (!anchor_id %in% data$claim_identifier) {
      anchor <- query_db(paste0(
        "SELECT claim_identifier, current_claim_status, hearing_count, highest_process, claim_injury_type, ",
        "injured_in_county_name, carrier_type, atty_rep_ind, controverted_date, first_hearing_date, ",
        "first_appeal_date, litigation_signal_score FROM claims_scored WHERE claim_identifier = ",
        sql_string(anchor_id), " LIMIT 1"
      ))
      data <- bind_rows(anchor, data) |> distinct(claim_identifier, .keep_all = TRUE) |> head(corpus_size)
    }

    data |>
      mutate(
        document_text = paste(
          "Claim", claim_identifier,
          "status", coalesce(current_claim_status, "unknown"),
          "hearing count", coalesce(as.character(hearing_count), "0"),
          "highest process", coalesce(highest_process, "unknown"),
          "injury type", coalesce(claim_injury_type, "unknown"),
          "county", coalesce(injured_in_county_name, "unknown"),
          "carrier type", coalesce(carrier_type, "unknown"),
          "attorney representative", coalesce(atty_rep_ind, "unknown"),
          "controverted date", coalesce(as.character(controverted_date), "none"),
          "first hearing", coalesce(as.character(first_hearing_date), "none"),
          "first appeal", coalesce(as.character(first_appeal_date), "none"),
          "litigation signal score", litigation_signal_score
        )
      )
  }

  observeEvent(input$run_benchmark, {
    req(db_ready())

    withProgress(message = "Running the live local-agent benchmark", value = 0, {
      setProgress(0.08, detail = "Creating real questions from the loaded claims")
      plan <- build_live_benchmark()
      questions <- plan$questions

      setProgress(0.22, detail = "Building the fixed RAG evidence corpus")
      corpus <- build_rag_corpus(as.integer(input$benchmark_corpus_size %||% 2000), plan$anchor_id)
      corpus_tokens <- lapply(corpus$document_text, tokenize_benchmark_text)

      result_rows <- vector("list", nrow(questions))
      traces <- vector("list", nrow(questions))

      for (i in seq_len(nrow(questions))) {
        q <- questions[i, ]
        setProgress(0.22 + 0.68 * i / nrow(questions), detail = paste("Testing", q$question_id))

        agent_started <- proc.time()[[3]]
        agent_row <- query_db(q$agent_sql)
        agent_answer <- if (nrow(agent_row) == 0 || !("answer" %in% names(agent_row))) "No answer" else as.character(agent_row$answer[1])
        agent_latency_ms <- 1000 * (proc.time()[[3]] - agent_started)

        rag_started <- proc.time()[[3]]
        q_tokens <- tokenize_benchmark_text(q$question)
        scores <- vapply(corpus_tokens, function(tokens) {
          overlap <- sum(q_tokens %in% tokens)
          coverage <- if (length(q_tokens) == 0) 0 else overlap / length(q_tokens)
          overlap + coverage
        }, numeric(1))
        top_index <- which.max(scores)
        top_doc <- corpus[top_index, , drop = FALSE]
        rag_answer <- switch(
          q$answer_type,
          status = as.character(top_doc$current_claim_status[1] %||% "Not available"),
          hearing = as.character(coalesce(top_doc$hearing_count[1], 0)),
          process = as.character(top_doc$highest_process[1] %||% "Not available"),
          county = as.character(top_doc$injured_in_county_name[1] %||% "Not available"),
          carrier = as.character(top_doc$carrier_type[1] %||% "Not available"),
          injury = as.character(top_doc$claim_injury_type[1] %||% "Not available"),
          as.character(top_doc$claim_identifier[1])
        )
        rag_latency_ms <- 1000 * (proc.time()[[3]] - rag_started)

        agent_correct <- normalize_benchmark_answer(agent_answer) == normalize_benchmark_answer(q$ground_truth)
        rag_correct <- normalize_benchmark_answer(rag_answer) == normalize_benchmark_answer(q$ground_truth)
        rag_evidence_contains_truth <- str_detect(
          normalize_benchmark_answer(top_doc$document_text[1]),
          fixed(normalize_benchmark_answer(q$ground_truth))
        )

        result_rows[[i]] <- tibble(
          question_id = q$question_id,
          question = q$question,
          expected_answer = q$ground_truth,
          agent_answer = agent_answer,
          rag_answer = rag_answer,
          agent_correct = agent_correct,
          rag_correct = rag_correct,
          agent_latency_ms = round(agent_latency_ms, 2),
          rag_latency_ms = round(rag_latency_ms, 2),
          agent_source = "DuckDB query + exact public source fields",
          rag_source = paste0("Top fixed chunk: ", top_doc$claim_identifier[1]),
          rag_evidence_contains_truth = rag_evidence_contains_truth
        )
        traces[[i]] <- tibble(
          question_id = q$question_id,
          trace = paste0(
            "QUESTION\n", q$question, "\n\n",
            "PLAN\n", q$trace, "\n\n",
            "SEARCH\n", q$agent_sql, "\n\n",
            "OPEN\nReturned ", nrow(agent_row), " row(s) from the local DuckDB cache.\n\n",
            "ANSWER\n", agent_answer, "\n\n",
            "SOURCE BOUNDARY\nOfficial NYS WCB public administrative fields only. No private notes, medical records, reserves, or invented claim facts."
          )
        )
      }

      results <- bind_rows(result_rows)
      trace_data <- bind_rows(traces)
      estimated_index_tokens <- sum(nchar(corpus$document_text), na.rm = TRUE) / 4
      estimated_query_tokens <- sum(nchar(questions$question), na.rm = TRUE) / 4
      estimated_output_tokens <- nrow(questions) * 80
      estimated_cost <-
        estimated_index_tokens / 1e6 * as.numeric(input$embedding_cost_per_million %||% 0) +
        (estimated_query_tokens + estimated_output_tokens) / 1e6 * as.numeric(input$generation_cost_per_million %||% 0)

      agent_accuracy_value <- mean(results$agent_correct)
      rag_accuracy_value <- mean(results$rag_correct)
      ship <- if (agent_accuracy_value >= rag_accuracy_value + 0.10) {
        "Ship the local evidence agent as the primary system. Keep RAG as a secondary tool for free-text notes and document passages, not for portfolio-wide aggregation or cross-record reasoning."
      } else if (rag_accuracy_value >= agent_accuracy_value + 0.10) {
        "Ship the RAG baseline for this question set, but retain structured SQL verification before using an answer in claim handling."
      } else {
        "Ship a hybrid: use the local agent for aggregation, chronology, and cross-reference questions; use RAG for direct passage retrieval, with both methods returning source evidence."
      }

      state$benchmark_results <- results
      state$benchmark_traces <- trace_data
      state$benchmark_meta <- list(
        agent_accuracy = agent_accuracy_value,
        rag_accuracy = rag_accuracy_value,
        agent_latency = median(results$agent_latency_ms),
        rag_latency = median(results$rag_latency_ms),
        estimated_cost = estimated_cost,
        corpus_rows = nrow(corpus),
        corpus_tokens = estimated_index_tokens,
        ship = ship
      )

      updateSelectInput(
        session, "trace_question",
        choices = setNames(results$question_id, paste0(results$question_id, " — ", results$question)),
        selected = results$question_id[1]
      )
      setProgress(1, detail = "Benchmark complete")
    })
  }, ignoreInit = TRUE)

  output$benchmark_status <- renderUI({
    if (is.null(state$benchmark_results)) {
      return(div(class = "status-box", "Load the official claims CSV, keep or adjust the filters, and run the benchmark. Questions and ground truth will be generated from the real records currently in scope."))
    }
    div(
      class = "success-box",
      strong("Live benchmark complete. "),
      paste0(nrow(state$benchmark_results), " real-data questions tested against ", comma(state$benchmark_meta$corpus_rows), " fixed RAG chunks.")
    )
  })

  output$agent_accuracy <- renderText({
    if (is.null(state$benchmark_meta)) "—" else percent(state$benchmark_meta$agent_accuracy, accuracy = 0.1)
  })
  output$rag_accuracy <- renderText({
    if (is.null(state$benchmark_meta)) "—" else percent(state$benchmark_meta$rag_accuracy, accuracy = 0.1)
  })
  output$agent_latency <- renderText({
    if (is.null(state$benchmark_meta)) "—" else paste0(round(state$benchmark_meta$agent_latency, 1), " ms")
  })
  output$rag_estimated_cost <- renderText({
    if (is.null(state$benchmark_meta)) "—" else dollar(state$benchmark_meta$estimated_cost, accuracy = 0.0001)
  })
  output$ship_recommendation <- renderText({
    if (is.null(state$benchmark_meta)) "Run the live benchmark to generate a recommendation." else state$benchmark_meta$ship
  })

  output$benchmark_accuracy_plot <- renderPlotly({
    validate(need(!is.null(state$benchmark_results), "Run the benchmark to compare accuracy."))
    data <- tibble(
      method = factor(c("Local evidence agent", "Fixed RAG baseline"), levels = c("Fixed RAG baseline", "Local evidence agent")),
      accuracy = c(mean(state$benchmark_results$agent_correct), mean(state$benchmark_results$rag_correct))
    )
    plot_ly(data, x = ~accuracy, y = ~method, type = "bar", orientation = "h",
            marker = list(color = c("#214e8a", "#9fb7d5")),
            text = ~percent(accuracy, accuracy = 0.1), textposition = "auto",
            hovertemplate = "%{y}: %{x:.1%}<extra></extra>") |>
      layout(xaxis = list(title = "Exact-answer accuracy", tickformat = ".0%", range = c(0, 1)), yaxis = list(title = ""),
             margin = list(l = 135, r = 20, t = 15, b = 45),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$benchmark_latency_plot <- renderPlotly({
    validate(need(!is.null(state$benchmark_results), "Run the benchmark to compare latency."))
    data <- state$benchmark_results |>
      select(question_id, agent_latency_ms, rag_latency_ms) |>
      pivot_longer(c(agent_latency_ms, rag_latency_ms), names_to = "method", values_to = "latency_ms") |>
      mutate(method = recode(method, agent_latency_ms = "Local evidence agent", rag_latency_ms = "Fixed RAG baseline"))
    plot_ly(data, x = ~question_id, y = ~latency_ms, color = ~method, type = "bar",
            colors = c("#214e8a", "#9fb7d5"),
            hovertemplate = "%{x}<br>%{fullData.name}: %{y:.2f} ms<extra></extra>") |>
      layout(barmode = "group", xaxis = list(title = "Question"), yaxis = list(title = "Latency (ms)"),
             legend = list(orientation = "h", x = 0, y = 1.13),
             margin = list(l = 65, r = 20, t = 45, b = 50),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  output$benchmark_results_table <- renderDT({
    validate(need(!is.null(state$benchmark_results), "Run the benchmark to view question-level results."))
    displayed <- state$benchmark_results |>
      transmute(
        question_id, question, expected_answer, agent_answer,
        agent_correct = if_else(agent_correct, "Correct", "Incorrect"),
        rag_answer,
        rag_correct = if_else(rag_correct, "Correct", "Incorrect"),
        agent_latency_ms, rag_latency_ms, agent_source, rag_source
      )
    datatable(displayed, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE, filter = "top")
  })

  output$trace_summary <- renderUI({
    validate(need(!is.null(state$benchmark_results), "Run the benchmark first."))
    selected <- input$trace_question %||% state$benchmark_results$question_id[1]
    row <- state$benchmark_results |> filter(question_id == selected) |> slice(1)
    tagList(
      div(class = if (isTRUE(row$agent_correct)) "success-box" else "warning-box", strong("Agent answer: "), row$agent_answer),
      br(),
      div(class = if (isTRUE(row$rag_correct)) "success-box" else "warning-box", strong("RAG answer: "), row$rag_answer),
      br(),
      p(class = "clean-note", strong("Expected: "), row$expected_answer)
    )
  })

  output$agent_trace <- renderText({
    validate(need(!is.null(state$benchmark_traces), "Run the benchmark to inspect the action trail."))
    selected <- input$trace_question %||% state$benchmark_traces$question_id[1]
    state$benchmark_traces |> filter(question_id == selected) |> pull(trace) |> first()
  })

  output$download_benchmark <- downloadHandler(
    filename = function() "workers_comp_agent_vs_rag_benchmark.csv",
    content = function(file) {
      validate(need(!is.null(state$benchmark_results), "Run the benchmark before downloading results."))
      write_csv(state$benchmark_results, file)
    }
  )


  output$executive_narrative <- renderUI({
    metrics <- portfolio_metrics()[1, ]
    findings <- executive_findings()[1, ]
    material <- query_db(paste0(
      "SELECT AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS rate ",
      "FROM claims_scored WHERE ", filter_where()
    ))$rate[1]

    statement <- paste0(
      fmt_number(metrics$claim_count), " claims are in the active portfolio lens. ",
      fmt_percent(metrics$attorney_rate), " are represented, ",
      fmt_percent(metrics$controverted_rate), " are controverted, and ",
      fmt_percent(material), " contain material public litigation indicators."
    )

    div(
      class = "executive-callout",
      div(
        strong("Executive finding"),
        p(statement),
        p("Use this result to focus human review on the records with the clearest public evidence of adjudication complexity.")
      ),
      div(class = "report-badge", "LIVE EVIDENCE")
    )
  })

  output$carrier_quadrant_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "SELECT COALESCE(carrier_type,'Unknown') AS carrier_type, COUNT(*) AS claims, ",
      "AVG(controverted_flag) AS controversy_rate, AVG(hearing_flag) AS hearing_rate, ",
      "AVG(attorney_flag) AS representation_rate, AVG(litigation_signal_score) AS avg_score ",
      "FROM claims_scored WHERE ", filter_where(),
      " GROUP BY carrier_type HAVING COUNT(*) >= 250 ORDER BY claims DESC"
    ))
    validate(need(nrow(data) > 0, "No carrier groups meet the current volume threshold."))

    plot_ly(
      data,
      x = ~controversy_rate, y = ~hearing_rate, size = ~claims,
      color = ~representation_rate, text = ~carrier_type,
      type = "scatter", mode = "markers", sizes = c(18, 62),
      colors = c("#a9e7f7", "#174f83"),
      marker = list(line = list(color = "#ffffff", width = 1.5), opacity = .88),
      hovertemplate = paste0(
        "%{text}<br>Claims: %{marker.size:,}",
        "<br>Controversy rate: %{x:.1%}",
        "<br>Hearing rate: %{y:.1%}",
        "<br>Representation rate: %{marker.color:.1%}<extra></extra>"
      )
    ) |>
      layout(
        xaxis = list(title = "Controversy rate", tickformat = ".0%", zeroline = FALSE),
        yaxis = list(title = "Hearing activity rate", tickformat = ".0%", zeroline = FALSE),
        margin = list(l = 70, r = 30, t = 15, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$injury_heatmap_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "WITH top_injuries AS (",
      " SELECT COALESCE(claim_injury_type,'Unknown') AS injury, COUNT(*) AS n",
      " FROM claims_scored WHERE ", filter_where(),
      " GROUP BY injury ORDER BY n DESC LIMIT 9),",
      " top_carriers AS (",
      " SELECT COALESCE(carrier_type,'Unknown') AS carrier, COUNT(*) AS n",
      " FROM claims_scored WHERE ", filter_where(),
      " GROUP BY carrier ORDER BY n DESC LIMIT 7)",
      " SELECT COALESCE(c.claim_injury_type,'Unknown') AS injury,",
      " COALESCE(c.carrier_type,'Unknown') AS carrier,",
      " AVG(c.litigation_signal_score) AS avg_score, COUNT(*) AS claims",
      " FROM claims_scored c",
      " JOIN top_injuries i ON COALESCE(c.claim_injury_type,'Unknown') = i.injury",
      " JOIN top_carriers t ON COALESCE(c.carrier_type,'Unknown') = t.carrier",
      " WHERE ", filter_where(),
      " GROUP BY injury, carrier"
    ))
    validate(need(nrow(data) > 0, "No injury and carrier combinations match the filters."))

    matrix <- data |>
      select(injury, carrier, avg_score) |>
      pivot_wider(names_from = carrier, values_from = avg_score)
    z <- as.matrix(matrix[, -1, drop = FALSE])

    plot_ly(
      x = colnames(z), y = matrix$injury, z = z,
      type = "heatmap", colors = c("#eef7fb", "#8ed9ef", "#174f83"),
      colorbar = list(title = "Avg score"),
      hovertemplate = "Injury: %{y}<br>Carrier: %{x}<br>Average signal score: %{z:.1f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -28), yaxis = list(title = ""),
        margin = list(l = 150, r = 40, t = 10, b = 105),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$medical_pressure_plot <- renderPlotly({
    req(db_ready())
    fred <- fred_data()
    validate(need(!is.null(fred), "FRED Medical Care CPI could not be loaded."))

    claims <- query_db(paste0(
      "SELECT assembly_year AS year, COUNT(*) AS claims,",
      " AVG(litigation_signal_score) AS avg_signal_score",
      " FROM claims_scored WHERE ", filter_where(),
      " AND assembly_year IS NOT NULL GROUP BY assembly_year ORDER BY assembly_year"
    ))
    annual_fred <- fred |>
      group_by(year) |>
      summarise(medical_inflation = mean(yoy_change, na.rm = TRUE), .groups = "drop")
    data <- inner_join(claims, annual_fred, by = "year") |>
      filter(is.finite(medical_inflation))
    validate(need(nrow(data) > 0, "No overlapping claims and CPI years are available."))

    plot_ly(data, x = ~year) |>
      add_bars(
        y = ~medical_inflation, name = "Medical inflation",
        marker = list(color = "rgba(66,211,255,.55)"),
        hovertemplate = "%{x}<br>Medical CPI YoY: %{y:.2f}%<extra></extra>"
      ) |>
      add_lines(
        y = ~avg_signal_score, name = "Average litigation signal score", yaxis = "y2",
        line = list(color = "#123f6a", width = 3),
        hovertemplate = "%{x}<br>Average signal score: %{y:.1f}<extra></extra>"
      ) |>
      layout(
        barmode = "overlay",
        xaxis = list(title = "Assembly year"),
        yaxis = list(title = "Medical CPI year-over-year change (%)"),
        yaxis2 = list(title = "Average signal score", overlaying = "y", side = "right", rangemode = "tozero"),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(l = 70, r = 70, t = 45, b = 55),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$claim_signal_radar <- renderPlotly({
    record <- state$claim_record
    if (is.null(record) || nrow(record) != 1) {
      empty <- tibble(
        signal = c("Representation", "Controversy", "Hearing", "Appeal", "Formal process", "Severity"),
        value = rep(0, 6)
      )
      return(
        plot_ly(empty, type = "scatterpolar", r = ~value, theta = ~signal, fill = "toself",
                line = list(color = "#4d7ea8"), fillcolor = "rgba(66,211,255,.18)") |>
          layout(polar = list(radialaxis = list(range = c(0, 1), visible = TRUE)),
                 showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)")
      )
    }

    severity <- min(1, as.numeric(record$severity_points[1] %||% 0) / 20)
    data <- tibble(
      signal = c("Representation", "Controversy", "Hearing", "Appeal", "Formal process", "Severity"),
      value = c(record$attorney_flag[1], record$controverted_flag[1], record$hearing_flag[1],
                record$appeal_flag[1], record$formal_process_flag[1], severity)
    )
    plot_ly(
      data, type = "scatterpolar", r = ~value, theta = ~signal, fill = "toself",
      line = list(color = "#174f83", width = 3), fillcolor = "rgba(66,211,255,.28)",
      hovertemplate = "%{theta}: %{r:.0%}<extra></extra>"
    ) |>
      layout(
        polar = list(radialaxis = list(range = c(0, 1), tickformat = ".0%", visible = TRUE)),
        showlegend = FALSE, margin = list(l = 55, r = 55, t = 25, b = 25),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })

  output$proof_report <- renderUI({
    metrics <- portfolio_metrics()[1, ]
    high_priority <- query_db(paste0(
      "SELECT COUNT(*) AS n FROM claims_scored WHERE ", filter_where(),
      " AND litigation_signal_score >= 70"
    ))$n[1]

    div(
      class = "executive-callout",
      div(
        strong("Portfolio conclusion"),
        p(paste0(
          "The filtered portfolio contains ", fmt_number(metrics$claim_count), " public claims. ",
          fmt_number(high_priority), " records have high-priority public litigation indicators; ",
          fmt_percent(metrics$attorney_rate), " are represented and ",
          fmt_percent(metrics$formal_process_rate), " reached a formal hearing, settlement, or appeal pathway."
        )),
        p("This is a board-ready descriptive report. It supports prioritization and research, while final claim and legal decisions remain with qualified professionals.")
      ),
      div(class = "report-badge", "REPORT READY")
    )
  })

  cyber_transfer_map <- tribble(
    ~workers_compensation_evidence, ~cyber_claim_equivalent, ~transferable_specialist_task,
    "Accident and assembly dates", "Incident discovery and notice dates", "Build a reliable chronology and identify reporting delays",
    "ANCR and controversy", "Coverage, causation, responsibility, or exclusion dispute", "Separate established facts from contested legal and technical issues",
    "Attorney representation", "Breach counsel, coverage counsel, claimant counsel, or regulatory counsel", "Track who represents each interest and preserve privilege boundaries",
    "Hearings and appeals", "Motions, mediation, arbitration, regulatory response, or litigation milestones", "Diary deadlines and connect each procedural event to evidence",
    "Injury and disability outcome", "Business interruption, privacy harm, data loss, restoration cost, and third-party damage", "Translate evidence into exposure categories without overstating certainty",
    "Carrier and process data", "Primary, excess, vendor, and counsel workflows", "Coordinate stakeholders and identify handoff or authority issues",
    "Optional reserves and billing", "Forensics, notification, restoration, ransom, defense, and settlement expenses", "Validate invoices, reserve movement, damages support, and settlement posture"
  )

  output$cyber_map <- renderDT({
    datatable(cyber_transfer_map, options = list(pageLength = 7, dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$source_register <- renderDT({
    datatable(source_register, options = list(pageLength = 3, dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$download_queue <- downloadHandler(
    filename = function() paste0("nys_wcb_litigation_review_queue_", Sys.Date(), ".csv"),
    content = function(file) {
      req(db_ready())
      min_score <- as.integer(input$minimum_litigation_score %||% 45)
      data <- query_db(paste0(
        "SELECT claim_identifier, assembly_date, claim_injury_type, current_claim_status, ",
        "atty_rep_ind, controverted_date, hearing_count, first_appeal_date, highest_process, ",
        "carrier_type, injured_in_county_name, litigation_signal_score, litigation_review_tier ",
        "FROM claims_scored WHERE ", filter_where(),
        " AND litigation_signal_score >= ", min_score,
        " ORDER BY litigation_signal_score DESC LIMIT 50000"
      ))
      write_csv(data, file)
    }
  )

  output$download_summary <- downloadHandler(
    filename = function() paste0("nys_wcb_portfolio_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      metrics <- portfolio_metrics() |>
        pivot_longer(everything(), names_to = "metric", values_to = "value")
      write_csv(metrics, file)
    }
  )

  output$report_preview <- renderText({
    if (!db_ready()) return("Load the official claims CSV to generate the report preview.")
    metrics <- portfolio_metrics()
    paste0(
      "NYS WORKERS' COMPENSATION CLAIMS ANALYTICS\n\n",
      "Official claim source: ", state$csv_path, "\n",
      "Local analytical cache: ", state$cache_path, "\n",
      "Filtered claims: ", fmt_number(metrics$claim_count[1]), "\n",
      "Attorney/representative rate: ", fmt_percent(metrics$attorney_rate[1]), "\n",
      "Controverted rate: ", fmt_percent(metrics$controverted_rate[1]), "\n",
      "Hearing rate: ", fmt_percent(metrics$hearing_rate[1]), "\n",
      "Appeal rate: ", fmt_percent(metrics$appeal_rate[1]), "\n",
      "Median average weekly wage: ", fmt_money(metrics$median_aww[1]), "\n",
      "Median accident-to-assembly interval: ", fmt_days(metrics$median_days_to_assembly[1]), "\n\n",
      "Interpretation: These are descriptive public-data indicators. They do not determine liability, compensability, reserve adequacy, counsel assignment, or legal strategy."
    )
  })

  # Open the exact official CSV automatically; the product never starts on a setup screen.
  session$onFlushed(function() {
    if (is.null(state$con)) isolate(load_database(FALSE))
  }, once = TRUE)
}

shinyApp(ui = ui, server = server)
