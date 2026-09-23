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
  "ggplot2", "scales", "lubridate", "stringr", "tidyr", "purrr"
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
'

# ============================================================
# UI
# ============================================================

detected_wcb_file <- auto_detect_wcb_csv()
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
  tags$head(tags$style(HTML(app_css))),

  div(
    class = "app-shell",

    div(
      class = "topbar",
      div(
        class = "wordmark",
        div(class = "brand-mark", "WC"),
        div(
          div(class = "brand-title", "NYS Workers’ Compensation Claims Analytics"),
          div(class = "brand-subtitle", "Public claims · adjudication · litigation indicators")
        )
      ),
      div(class = "source-badge", div(class = "source-dot"), "Official public claims data")
    ),

    div(
      class = "hero",
      div(
        class = "hero-copy",
        div(class = "hero-kicker", "Evidence-first claims intelligence"),
        div(class = "hero-title", "REAL CLAIMS. CLEAR PROCESS. EXPLAINABLE REVIEW."),
        div(
          class = "hero-text",
          "A local analytics application built for the New York State Workers’ Compensation Board’s Assembled Workers’ Compensation Claims dataset. It helps claims and litigation professionals understand injury outcomes, claim timelines, attorney representation, controversy, hearings, appeals, carrier patterns, and possible litigation-review indicators—without inventing claim facts."
        )
      ),
      div(
        class = "hero-proof",
        div(class = "proof-card", div(class = "proof-label", "Claims source"), div(class = "proof-value", "New York State Workers’ Compensation Board")),
        div(class = "proof-card", div(class = "proof-label", "Local engine"), div(class = "proof-value", "DuckDB for multi-million-row analysis")),
        div(class = "proof-card", div(class = "proof-label", "Medical context"), div(class = "proof-value", "BLS Medical Care CPI through FRED"))
      )
    ),

    navset_pill_list(
      widths = c(3, 9),

      nav_panel(
        "Overview",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Workers’ Compensation Claims Analytics"),
          p(
            class = "microcopy",
            "Developed a public-data framework for analyzing medical inflation, claim outcomes, injury severity, attorney representation, controversy, hearings, appeals, process milestones, and possible litigation-review indicators. Billing behavior and reserve development are supported only through a separate authorized enrichment file because those fields are not published in the official public claims dataset."
          ),
          div(
            class = "success-box",
            strong("Credibility standard: "),
            "The default app uses the official NYS WCB CSV and real FRED medical CPI. It contains no fictitious workers’ compensation claims and does not silently generate missing billing or reserve values."
          )
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Who can use it"),
          div(
            class = "use-grid",
            div(class = "use-card", strong("Claims specialist"), "Review portfolio trends, claim outcomes, injury patterns, status, representation, and adjudication activity."),
            div(class = "use-card", strong("Litigation specialist"), "Prioritize records with visible controversy, hearings, appeals, representation, and serious awarded injury outcomes."),
            div(class = "use-card", strong("Cyber litigation specialist"), "Transfer the same evidence discipline to incident chronology, counsel activity, forensic causation, vendor invoices, reserves, damages, and procedural deadlines.")
          )
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "What the public data can prove"),
          DTOutput("source_register")
        )
      ),

      nav_panel(
        "Data Setup",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Connect the official CSV in your RStudio project"),
          p(
            class = "microcopy",
            "Place the downloaded NYS WCB CSV in the same folder as app.R. The app tries to find it automatically. DuckDB creates a local cache beside the CSV so the entire file is scanned once instead of being loaded into R memory for every chart."
          ),
          textInput(
            "claims_path",
            "Path to the NYS WCB Assembled Claims CSV",
            value = detected_wcb_file,
            placeholder = "Example: Assembled_Workers_Compensation_Claims_Beginning_2000.csv"
          ),
          fluidRow(
            column(6, actionButton("load_claims", "Load / open claims cache", class = "btn-primary")),
            column(6, actionButton("rebuild_cache", "Rebuild cache from CSV"))
          ),
          br(),
          uiOutput("load_status")
        ),
        fluidRow(
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("setup_rows")), div(class = "metric-label", "Claim rows"), div(class = "metric-note", "Rows in the local DuckDB claims table"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("setup_years")), div(class = "metric-label", "Assembly years"), div(class = "metric-note", "Earliest through latest available year"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("setup_fields")), div(class = "metric-label", "Fields detected"), div(class = "metric-note", "Recognized official dataset columns"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("setup_cache")), div(class = "metric-label", "Cache status"), div(class = "metric-note", "Reused or rebuilt during this session")))
        ),
        div(class = "panel-card", h3(class = "panel-title", "Detected field map"), DTOutput("field_map_table")),
        div(class = "panel-card", h3(class = "panel-title", "Key-field completeness"), DTOutput("missingness_table")),
        div(class = "panel-card", h3(class = "panel-title", "Real data preview"), DTOutput("data_preview"))
      ),

      nav_panel(
        "Claims Portfolio",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Portfolio filters"),
          fluidRow(
            column(4, selectInput("filter_claim_type", "Claim type", choices = "All", selected = "All")),
            column(4, selectInput("filter_carrier_type", "Carrier type", choices = "All", selected = "All")),
            column(4, selectInput("filter_county", "County of injury", choices = "All", selected = "All"))
          ),
          fluidRow(
            column(4, selectInput("filter_injury_type", "Claim injury type", choices = "All", selected = "All")),
            column(4, selectInput("filter_attorney", "Attorney/representative", choices = c("All", "Represented", "Not represented"), selected = "All")),
            column(4, selectInput("filter_controverted", "Controverted claim", choices = c("All", "Controverted", "Not controverted"), selected = "All"))
          ),
          sliderInput("filter_year", "Assembly year", min = 2000, max = year(Sys.Date()), value = c(2000, year(Sys.Date())), sep = "")
        ),
        fluidRow(
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("claim_count")), div(class = "metric-label", "Claims"), div(class = "metric-note", "Filtered public claim records"))),
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("attorney_rate")), div(class = "metric-label", "Represented"), div(class = "metric-note", "Attorney/representative indicator"))),
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("controverted_rate")), div(class = "metric-label", "Controverted"), div(class = "metric-note", "Controverted date present"))),
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("hearing_rate")), div(class = "metric-label", "With hearings"), div(class = "metric-note", "Hearing count or first hearing date"))),
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("appeal_rate")), div(class = "metric-label", "With appeals"), div(class = "metric-note", "Appeal date or appeal process"))),
          column(2, div(class = "metric-card", div(class = "metric-value", textOutput("median_aww")), div(class = "metric-label", "Median AWW"), div(class = "metric-note", "Average weekly wage where available")))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3(class = "panel-title", "Claims assembled by year"), plotOutput("assembly_trend", height = "380px"))),
          column(6, div(class = "panel-card", h3(class = "panel-title", "Claim injury outcomes"), plotOutput("injury_type_plot", height = "380px")))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3(class = "panel-title", "Current claim status"), plotOutput("status_plot", height = "390px"))),
          column(6, div(class = "panel-card", h3(class = "panel-title", "Highest resolution process"), plotOutput("process_plot", height = "390px")))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3(class = "panel-title", "County profile"), plotOutput("county_plot", height = "390px"))),
          column(6, div(class = "panel-card", h3(class = "panel-title", "Carrier-type profile"), plotOutput("carrier_plot", height = "390px")))
        )
      ),

      nav_panel(
        "Litigation Indicators",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Explainable public litigation-review queue"),
          p(
            class = "microcopy",
            "The score ranks records using only fields published by the NYS WCB: controversy, representation, hearings, appeals, highest process, current adjudication status, and awarded injury severity. It is a research triage aid—not a legal conclusion, counsel-assignment decision, reserve recommendation, or prediction of lawsuit outcomes."
          ),
          sliderInput("minimum_litigation_score", "Minimum indicator score", min = 0, max = 100, value = 45, step = 5),
          DTOutput("litigation_queue")
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3(class = "panel-title", "Litigation-review tiers"), plotOutput("tier_plot", height = "380px"))),
          column(6, div(class = "panel-card", h3(class = "panel-title", "Representation by highest process"), plotOutput("representation_process_plot", height = "380px")))
        ),
        div(class = "panel-card", h3(class = "panel-title", "Transparent scoring framework"), DTOutput("method_table"))
      ),

      nav_panel(
        "Claim Explorer",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Open a real public claim record"),
          p(class = "microcopy", "Enter a Claim Identifier from the public dataset. The app queries the indexed DuckDB cache directly instead of loading millions of identifiers into a dropdown."),
          fluidRow(
            column(8, textInput("claim_lookup", "Claim Identifier", placeholder = "Enter the public claim identifier")),
            column(4, br(), actionButton("find_claim", "Open claim", class = "btn-primary"))
          ),
          uiOutput("claim_lookup_status")
        ),
        div(class = "panel-card", h3(class = "panel-title", "Claim summary"), uiOutput("claim_summary")),
        div(class = "panel-card", h3(class = "panel-title", "Why the record surfaced"), uiOutput("claim_explanation")),
        div(class = "panel-card", h3(class = "panel-title", "Available public evidence"), DTOutput("claim_record_table"))
      ),

      nav_panel(
        "Medical Inflation",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Real medical-price context from FRED"),
          p(class = "microcopy", "The Medical Care CPI is used as market context. It does not represent medical payments on any specific workers’ compensation claim."),
          textInput("fred_path", "Optional local FRED CPIMEDSL CSV", value = detected_fred_file, placeholder = "Leave blank to try the live FRED CSV"),
          actionButton("refresh_fred", "Refresh medical CPI", class = "btn-primary"),
          br(), br(),
          uiOutput("fred_status")
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3(class = "panel-title", "Medical Care CPI"), plotOutput("fred_plot", height = "390px"))),
          column(6, div(class = "panel-card", h3(class = "panel-title", "Year-over-year medical inflation"), plotOutput("fred_yoy_plot", height = "390px")))
        ),
        div(class = "panel-card", h3(class = "panel-title", "Claims volume and medical-price context"), plotOutput("claims_cpi_plot", height = "420px"))
      ),

      nav_panel(
        "Billing & Reserves",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Optional authorized carrier-data extension"),
          div(
            class = "warning-box",
            strong("Why this is separate: "),
            "The official NYS WCB public dataset does not publish case reserves, paid medical, indemnity paid, submitted provider charges, duplicate bills, settlement amounts, or private claim notes. The app will not invent them. Upload only properly de-identified data you are authorized to use."
          ),
          br(),
          fileInput("enrichment_file", "De-identified billing / reserve CSV", accept = ".csv"),
          downloadButton("download_enrichment_template", "Download blank enrichment template"),
          br(), br(),
          uiOutput("enrichment_status")
        ),
        fluidRow(
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("enriched_claims")), div(class = "metric-label", "Matched claims"), div(class = "metric-note", "Public identifiers matched to enrichment"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("enriched_incurred")), div(class = "metric-label", "Total incurred"), div(class = "metric-note", "Authorized enrichment only"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("enriched_reserve")), div(class = "metric-label", "Case reserves"), div(class = "metric-note", "Authorized enrichment only"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("billing_flags")), div(class = "metric-label", "Billing flags"), div(class = "metric-note", "Duplicate, outlier, or documentation indicators")))
        ),
        div(class = "panel-card", h3(class = "panel-title", "Enriched claim review"), DTOutput("enrichment_table"))
      ),

      nav_panel(
        "Cyber Litigation Transfer",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Why this matters for cyber litigation claims"),
          p(
            class = "microcopy",
            "Workers’ compensation law and cyber coverage law are different. The transferable skill is evidence-centered claim analysis: organizing chronology, identifying contested issues, separating facts from assumptions, tracking representation and procedural milestones, connecting financial exposure to source records, and preserving a human decision-maker."
          )
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Transferable evidence map"),
          DTOutput("cyber_map")
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Portfolio statement"),
          div(
            class = "status-box",
            "Built a local workers’ compensation claims analytics application using official NYS WCB administrative records and FRED medical-price data. The application analyzes claim outcomes, injury severity, attorney representation, controversy, hearings, appeals, carrier patterns, and explainable litigation-review indicators, while preserving optional modules for authorized billing and reserve data."
          )
        )
      ),

      nav_panel(
        "Export & Limitations",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Export research outputs"),
          downloadButton("download_queue", "Download litigation-review queue"),
          tags$span(" "),
          downloadButton("download_summary", "Download portfolio summary"),
          br(), br(),
          verbatimTextOutput("report_preview")
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Limitations"),
          tags$ul(
            tags$li("The dataset contains public administrative claim records, not complete insurer claim files."),
            tags$li("The score describes visible public adjudication indicators; it is not a trained predictive model."),
            tags$li("A controversy, hearing, appeal, or attorney indicator does not by itself prove misconduct, liability, or the need for litigation."),
            tags$li("FRED Medical Care CPI is national price context and cannot measure a specific workers’ compensation medical bill."),
            tags$li("Billing and reserve conclusions require authorized, properly de-identified insurer data and professional file review."),
            tags$li("Public records should still be handled responsibly and should not be used to identify or contact injured workers.")
          )
        )
      )
    ),

    div(class = "footer-note", "NYS Workers’ Compensation Claims Analytics | Official Public Data | Explainable Research Prototype")
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
    load_message = "Choose the official CSV and load the cache.",
    claim_record = NULL,
    fred = NULL,
    fred_error = NULL
  )

  session$onSessionEnded(function() {
    if (!is.null(state$con)) {
      try(dbDisconnect(state$con, shutdown = TRUE), silent = TRUE)
    }
  })

  db_ready <- reactive({
    !is.null(state$con) && isTRUE(dbIsValid(state$con))
  })

  query_db <- function(sql) {
    req(db_ready())
    dbGetQuery(state$con, sql)
  }

  load_database <- function(force = FALSE) {
    path <- str_trim(input$claims_path %||% "")

    if (!nzchar(path) || !file.exists(path)) {
      state$load_error <- paste0(
        "The CSV was not found at: ", path,
        ". Put the official CSV in the project folder or paste its full path."
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

  observeEvent(input$load_claims, load_database(FALSE), ignoreInit = TRUE)
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
      return(div(class = "status-box", state$load_message %||% "Choose the official CSV and load the cache."))
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

  # Automatically load when a likely official CSV was found in the project folder.
  observe({
    if (nzchar(detected_wcb_file) && is.null(state$con) && identical(input$claims_path, detected_wcb_file)) {
      isolate(load_database(FALSE))
    }
  })
}

shinyApp(ui = ui, server = server)
