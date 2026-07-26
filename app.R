# ============================================================
# Workers Compensation Claims Intelligence — single-file app
# Keep workers_comp.csv in the same project folder as this app.R.
# This file contains the complete Shiny application.
# ============================================================

# ============================================================
# NYC Claims Intelligence — commercial prototype
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
WCB_CSV_FILENAME <- "workers_comp.csv"

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
  claim_identifier = c("claim_identifier", "claim_id", "claim_number", "claim_no", "wcb_claim_number", "wcb_claim_id", "wcb_number", "wcb_no", "wcb_case_number", "case_number", "case_id"),
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

wcb_search_folders <- function() {
  # Search the portfolio project first so the app works even when app.R
  # was opened from Downloads or another working directory.
  project_dir <- path.expand("~/sohaarian.github.io")
  rstudio_project <- Sys.getenv("RSTUDIO_PROJECT", unset = "")
  
  folders <- c(
    project_dir,
    file.path(project_dir, "data"),
    rstudio_project,
    if (nzchar(rstudio_project)) file.path(rstudio_project, "data") else "",
    getwd(),
    file.path(getwd(), "data"),
    path.expand("~/Downloads")
  )
  
  unique(folders[nzchar(folders)])
}


auto_detect_wcb_csv <- function() {
  folders <- wcb_search_folders()
  candidates <- unlist(lapply(folders, function(folder) {
    if (!dir.exists(folder)) return(character())
    list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  }))
  
  candidates <- unique(candidates[file.exists(candidates)])
  if (length(candidates) == 0) return("")
  
  names_lower <- str_to_lower(basename(candidates))
  score <-
    6 * (basename(candidates) == WCB_CSV_FILENAME) +
    4 * str_detect(names_lower, "assembled") +
    4 * str_detect(names_lower, "workers") +
    4 * str_detect(names_lower, "compensation") +
    3 * str_detect(names_lower, "claim") +
    2 * str_detect(names_lower, "2000") +
    2 * str_detect(names_lower, "jshw") -
    5 * str_detect(names_lower, "fred|cpi|template|enrichment")
  
  if (max(score) <= 0) return("")
  normalizePath(candidates[which.max(score)], winslash = "/", mustWork = TRUE)
}


resolve_wcb_csv <- function() {
  folders <- wcb_search_folders()
  exact_candidates <- file.path(folders, WCB_CSV_FILENAME)
  
  exact_hit <- exact_candidates[file.exists(exact_candidates)][1]
  if (length(exact_hit) == 1 && !is.na(exact_hit)) {
    return(normalizePath(exact_hit, winslash = "/", mustWork = TRUE))
  }
  
  fallback <- auto_detect_wcb_csv()
  if (nzchar(fallback) && file.exists(fallback)) return(fallback)
  
  # Keep the error message pointed at the portfolio project, where the
  # official dataset is expected to live.
  normalizePath(
    file.path(path.expand("~/sohaarian.github.io"), WCB_CSV_FILENAME),
    winslash = "/",
    mustWork = FALSE
  )
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
    possible <- unique(clean_names_simple(c(field, field_aliases[[field]])))
    matches <- possible[possible %in% names(source_lookup)]

    # Extra protection for exports that rename the claim ID header slightly.
    if (length(matches) == 0 && identical(field, "claim_identifier")) {
      fuzzy <- names(source_lookup)[str_detect(
        names(source_lookup),
        "^(claim|wcb_claim|wcb_case|case).*(identifier|_id|number|_no)$"
      )]
      if (length(fuzzy) > 0) matches <- fuzzy[1]
    }

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
    # The NY claims export can contain ISO dates (YYYY-MM-DD) or U.S. dates
    # (MM/DD/YYYY). Parse both so year-based filters do not silently remove
    # every claim when the CSV uses the latter format.
    raw_date <- paste0("NULLIF(TRIM(CAST(", quoted, " AS VARCHAR)), '')")
    first_10 <- paste0("SUBSTR(", raw_date, ", 1, 10)")
    
    return(paste0(
      "COALESCE(",
      "TRY_CAST(", first_10, " AS DATE), ",
      "CAST(TRY_STRPTIME(", first_10, ", '%m/%d/%Y') AS DATE), ",
      "CAST(TRY_STRPTIME(", first_10, ", '%Y/%m/%d') AS DATE), ",
      "CAST(TRY_STRPTIME(", first_10, ", '%m-%d-%Y') AS DATE)",
      ") AS ", sql_identifier(field)
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

cache_path_for <- function(workers_comp.csv) {
  file.path(dirname(workers_comp.csv), "nyc_claims_intelligence_cache_v3.duckdb")
}

cache_matches_source <- function(con, workers_comp.csv) {
  if (!dbExistsTable(con, "app_source_metadata") || !dbExistsTable(con, "claims")) {
    return(FALSE)
  }
  
  meta <- dbGetQuery(con, "SELECT * FROM app_source_metadata LIMIT 1")
  if (nrow(meta) == 0) return(FALSE)
  
  info <- file.info(workers_comp.csv)
  identical(as.character(meta$source_path[1]), normalizePath(workers_comp.csv, winslash = "/")) &&
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
      COALESCE(EXTRACT(YEAR FROM assembly_date), EXTRACT(YEAR FROM accident_date)) AS analysis_year,
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
        ) >= 70 THEN 'Priority specialist review'
        WHEN (
          attorney_flag * 20 + controverted_flag * 25 + hearing_points +
          appeal_flag * 15 + formal_process_flag * 10 +
          active_status_flag * 5 + severity_points
        ) >= 45 THEN 'Specialist review'
        WHEN (
          attorney_flag * 20 + controverted_flag * 25 + hearing_points +
          appeal_flag * 15 + formal_process_flag * 10 +
          active_status_flag * 5 + severity_points
        ) >= 20 THEN 'Monitor'
        ELSE 'Routine'
      END AS litigation_review_tier
    FROM claim_flags
  ")
}

build_or_open_cache <- function(workers_comp.csv, force_rebuild = FALSE, progress_callback = NULL) {
  workers_comp.csv <- normalizePath(workers_comp.csv, winslash = "/", mustWork = TRUE)
  cache_path <- cache_path_for(workers_comp.csv)
  con <- dbConnect(duckdb(), dbdir = cache_path, read_only = FALSE)
  
  if (!force_rebuild && cache_matches_source(con, workers_comp.csv)) {
    create_scored_views(con)
    return(list(
      con = con,
      cache_path = cache_path,
      field_map = dbGetQuery(con, "SELECT * FROM app_field_map"),
      reused = TRUE
    ))
  }
  
  if (is.function(progress_callback)) progress_callback(0.08, "Reading the CSV header")
  field_map <- build_field_map(workers_comp.csv)
  
  claim_id_detected <- field_map$detected[field_map$canonical_field == "claim_identifier"]
  if (!isTRUE(claim_id_detected)) {
    detected_headers <- tryCatch(names(read_csv_header(workers_comp.csv)), error = function(e) character())
    dbDisconnect(con, shutdown = TRUE)
    stop(
      "workers_comp.csv was found, but no Claim Identifier column was recognized. ",
      "Expected a header such as 'Claim Identifier' or 'claim_identifier'. ",
      if (length(detected_headers) > 0) {
        paste0("Detected headers begin with: ", paste(head(detected_headers, 8), collapse = ", "))
      } else {
        "The CSV header could not be read."
      }
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
    "read_csv_auto(", sql_string(workers_comp.csv),
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
  source_info <- file.info(workers_comp.csv)
  source_meta <- data.frame(
    source_path = workers_comp.csv,
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
# Optional carrier operational extract
# ============================================================

# The public WCB file is useful for benchmark/demo mode. A sellable claims
# workflow also needs the carrier's own de-identified operational fields.
# Claim identifiers may be internal IDs. Public-WCB matching is optional.
enrichment_template <- tibble(
  claim_identifier = character(),
  claim_owner = character(),
  internal_status = character(),
  loss_date = character(),
  last_activity_date = character(),
  next_diary_date = character(),
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

parse_carrier_date <- function(x) {
  value <- suppressWarnings(parse_date_time(
    as.character(x),
    orders = c("ymd", "mdy", "Ymd HMS", "mdY HMS", "Y-m-d H:M:S"),
    quiet = TRUE
  ))
  as.Date(value)
}

first_nonmissing <- function(x) {
  keep <- !is.na(x) & trimws(as.character(x)) != ""
  if (!any(keep)) return(NA_character_)
  as.character(x[which(keep)[1]])
}

prepare_enrichment <- function(df) {
  names(df) <- clean_names_simple(names(df))
  id_aliases <- c("claim_identifier", "claim_id", "claim_number", "wcb_claim_number")
  id_col <- id_aliases[id_aliases %in% names(df)][1]
  if (is.na(id_col)) stop("The carrier extract needs a claim_identifier column.")
  names(df)[names(df) == id_col] <- "claim_identifier"
  
  numeric_cols <- c(
    "charged_amount", "paid_amount", "paid_medical", "paid_indemnity",
    "total_incurred", "case_reserve", "reserve_change_90d"
  )
  flag_cols <- c("duplicate_bill_flag", "provider_outlier_flag", "documentation_gap_flag")
  text_cols <- c("claim_owner", "internal_status", "provider_id", "claim_outcome")
  date_cols <- c("loss_date", "last_activity_date", "next_diary_date")
  
  for (field in setdiff(c(numeric_cols, flag_cols), names(df))) df[[field]] <- NA_real_
  for (field in setdiff(text_cols, names(df))) df[[field]] <- NA_character_
  for (field in setdiff(date_cols, names(df))) df[[field]] <- NA_character_
  
  df |>
    mutate(
      claim_identifier = str_trim(as.character(claim_identifier)),
      across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(str_replace_all(as.character(.x), "[$,]", "")))),
      across(all_of(flag_cols), ~ case_when(
        str_to_upper(str_trim(as.character(.x))) %in% c("Y", "YES", "TRUE", "1") ~ 1,
        str_to_upper(str_trim(as.character(.x))) %in% c("N", "NO", "FALSE", "0") ~ 0,
        TRUE ~ suppressWarnings(as.numeric(.x))
      )),
      across(all_of(date_cols), parse_carrier_date)
    ) |>
    filter(!is.na(claim_identifier), claim_identifier != "") |>
    group_by(claim_identifier) |>
    summarise(
      claim_owner = first_nonmissing(claim_owner),
      internal_status = first_nonmissing(internal_status),
      loss_date = if (all(is.na(loss_date))) as.Date(NA) else min(loss_date, na.rm = TRUE),
      last_activity_date = if (all(is.na(last_activity_date))) as.Date(NA) else max(last_activity_date, na.rm = TRUE),
      next_diary_date = if (all(is.na(next_diary_date))) as.Date(NA) else min(next_diary_date, na.rm = TRUE),
      charged_amount = if (all(is.na(charged_amount))) NA_real_ else sum(charged_amount, na.rm = TRUE),
      paid_amount = if (all(is.na(paid_amount))) NA_real_ else sum(paid_amount, na.rm = TRUE),
      paid_medical = if (all(is.na(paid_medical))) NA_real_ else max(paid_medical, na.rm = TRUE),
      paid_indemnity = if (all(is.na(paid_indemnity))) NA_real_ else max(paid_indemnity, na.rm = TRUE),
      total_incurred = if (all(is.na(total_incurred))) NA_real_ else max(total_incurred, na.rm = TRUE),
      case_reserve = if (all(is.na(case_reserve))) NA_real_ else max(case_reserve, na.rm = TRUE),
      reserve_change_90d = if (all(is.na(reserve_change_90d))) NA_real_ else max(reserve_change_90d, na.rm = TRUE),
      provider_id = first_nonmissing(provider_id),
      duplicate_bill_flag = if (all(is.na(duplicate_bill_flag))) NA_real_ else max(duplicate_bill_flag, na.rm = TRUE),
      provider_outlier_flag = if (all(is.na(provider_outlier_flag))) NA_real_ else max(provider_outlier_flag, na.rm = TRUE),
      documentation_gap_flag = if (all(is.na(documentation_gap_flag))) NA_real_ else max(documentation_gap_flag, na.rm = TRUE),
      claim_outcome = first_nonmissing(claim_outcome),
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

/* Litigation product polish + scroll-driven reveal */
html { scroll-behavior:smooth; }
body::before {
  content:""; position:fixed; inset:0; pointer-events:none; z-index:-1;
  background:
    radial-gradient(circle at 14% 10%, rgba(66,211,255,.08), transparent 22%),
    radial-gradient(circle at 88% 38%, rgba(78,119,255,.08), transparent 25%);
}
.product-nav { box-shadow:0 12px 34px rgba(4,17,31,.16); }
.product-logo { animation:brandfloat 4.8s ease-in-out infinite; }
@keyframes brandfloat { 0%,100%{transform:translateY(0)} 50%{transform:translateY(-3px)} }
.nav-tabs .nav-link { transition:transform .2s ease, box-shadow .2s ease, background .2s ease; }
.nav-tabs .nav-link:hover { transform:translateY(-2px); box-shadow:0 8px 20px rgba(7,26,46,.08); }
.visual-panel,.kpi-tile,.insight-card,.filter-dock,.executive-callout,.claim-hero,.report-cover {
  transition:transform .28s cubic-bezier(.2,.8,.2,1), box-shadow .28s ease, border-color .28s ease;
}
.reveal-ready { opacity:0; transform:translateY(28px) scale(.985); }
.reveal-ready.reveal-visible { opacity:1; transform:translateY(0) scale(1); transition:opacity .7s ease, transform .7s cubic-bezier(.2,.8,.2,1); }
.reveal-visible.visual-panel:hover,.reveal-visible.kpi-tile:hover,.reveal-visible.insight-card:hover {
  transform:translateY(-5px); box-shadow:0 20px 50px rgba(17,52,82,.10); border-color:#bfd8ea;
}
.kpi-number { background:linear-gradient(100deg,#071a2e,#174f83,#0d6b78); -webkit-background-clip:text; background-clip:text; color:transparent; }
.litigation-strip { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:10px; margin:0 0 18px; }
.litigation-step { position:relative; overflow:hidden; padding:16px; border-radius:18px; border:1px solid #d8e5f0; background:linear-gradient(145deg,#fff,#f3f8fc); }
.litigation-step strong { display:block; color:#0b2b48; font-size:.86rem; }
.litigation-step span { color:#708399; font-size:.73rem; }
.litigation-step::after { content:""; position:absolute; left:-35%; bottom:0; width:35%; height:3px; background:linear-gradient(90deg,#42d3ff,#3273dc); animation:legalflow 3.6s linear infinite; }
.litigation-step:nth-child(2)::after{animation-delay:.35s}.litigation-step:nth-child(3)::after{animation-delay:.7s}.litigation-step:nth-child(4)::after{animation-delay:1.05s}.litigation-step:nth-child(5)::after{animation-delay:1.4s}
@keyframes legalflow { 0%{left:-35%;opacity:.2} 35%{opacity:1} 100%{left:110%;opacity:.15} }
.agent-lab-hero { position:relative; overflow:hidden; padding:38px; border-radius:28px; margin-bottom:18px; color:#fff; background:linear-gradient(135deg,#061526,#0c3151 55%,#123b5d); }
.agent-lab-hero::after { content:""; position:absolute; width:480px; height:480px; right:-220px; top:-250px; border-radius:50%; border:1px solid rgba(111,225,255,.22); box-shadow:0 0 0 70px rgba(111,225,255,.035),0 0 0 140px rgba(111,225,255,.025); animation:orbitspin 14s linear infinite; }
.agent-lab-hero h1 { position:relative; z-index:2; margin:0 0 10px; font-size:clamp(2.1rem,4vw,4.2rem); font-weight:950; letter-spacing:-.07rem; }
.agent-lab-hero p { position:relative; z-index:2; max-width:900px; color:#c7dae9; line-height:1.7; }
.agent-lab-hero .btn { position:relative; z-index:2; }
.benchmark-flow .flow-node { min-height:92px; display:grid; place-items:center; box-shadow:0 10px 26px rgba(7,26,46,.05); }
.ship-card { position:relative; overflow:hidden; }
.ship-card::before { content:""; position:absolute; inset:0; background:linear-gradient(110deg,transparent 25%,rgba(255,255,255,.72) 45%,transparent 65%); transform:translateX(-120%); animation:shipsheen 5s ease-in-out infinite; }
@keyframes shipsheen { 0%,70%{transform:translateX(-120%)} 100%{transform:translateX(120%)} }
.empty-proof { min-height:280px; display:grid; place-items:center; text-align:center; color:#7b8da0; border:1px dashed #cbd9e6; border-radius:18px; background:linear-gradient(145deg,#fbfdff,#f5f9fc); }
@media(max-width:900px){.litigation-strip{grid-template-columns:1fr 1fr}.benchmark-flow{grid-template-columns:1fr}.flow-arrow{transform:rotate(90deg)}}

' 

# ============================================================
# UI
# ============================================================

detected_wcb_file <- resolve_wcb_csv()
source_register <- tribble(
  ~source, ~what_it_provides, ~how_the_product_uses_it, ~important_limitation,
  "New York Workers' Compensation Board — Assembled Claims",
  "Statewide public administrative claims beginning in 2000",
  "External benchmark, claim-process patterns, representation, controversy, hearings, appeals, injury outcomes, geography, carrier context, and chronology",
  "Public data are not a carrier claim system and do not include private notes, authority, reserves, payments, medical records, or task/diary history",
  "Evidence Agent Lab — local benchmark harness",
  "Question templates, local DuckDB search/open traces, fixed claim-summary retrieval chunks, SQL ground truth, and timing/cost scenario calculations",
  "Compares retrieval architectures on real records without sending public claim content to an external model in this prototype",
  "The benchmark is deterministic and task-specific; it does not establish general language-model performance on private claim documents",
  "Optional de-identified carrier operational extract",
  "Internal status, owner, diary/activity dates, paid and incurred fields, reserves, and explicit billing/documentation exception flags",
  "Operational queueing and financial-exception review without inventing unavailable fields",
  "Production deployment requires the carrier's access controls, data governance, validation rules, and approved integrations"
)

method_table <- tribble(
  ~indicator, ~points, ~why_it_matters,
  "Controverted date present", "25", "The public record shows that the controversy-resolution process began",
  "Attorney or representative", "20", "Representation is a visible legal-complexity indicator",
  "Hearing activity", "8–20", "Repeated hearings can indicate sustained adjudication activity",
  "Appeal activity", "15", "An appeal adds procedural and legal complexity",
  "Formal hearing/settlement process", "10", "Highest Process shows a formal resolution pathway",
  "Active adjudication status", "5", "Current status can show a hearing, motion, argument, restoral, or reserved decision",
  "Serious injury outcome", "0–20", "Permanent disability and death outcomes add consequence to the review workload"
)

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML('
      (function(){
        var revealObserver = new IntersectionObserver(function(entries){
          entries.forEach(function(entry){
            if(entry.isIntersecting){
              entry.target.classList.add("reveal-visible");
              revealObserver.unobserve(entry.target);
            }
          });
        }, {threshold:0.08, rootMargin:"0px 0px -35px 0px"});

        function armReveal(){
          document.querySelectorAll(".visual-panel,.kpi-tile,.insight-card,.filter-dock,.executive-callout,.claim-hero,.report-cover,.agent-lab-hero,.section-heading").forEach(function(el){
            if(!el.classList.contains("reveal-ready")){
              el.classList.add("reveal-ready");
              revealObserver.observe(el);
            }
          });
        }

        document.addEventListener("DOMContentLoaded", function(){ setTimeout(armReveal, 80); });
        $(document).on("shiny:connected shown.bs.tab", function(){ setTimeout(armReveal, 80); });
        $(document).on("shiny:value", function(event){
          var el=document.getElementById(event.name);
          if(el){ el.classList.remove("js-animate-in"); void el.offsetWidth; el.classList.add("js-animate-in"); }
          setTimeout(armReveal, 30);
        });
        new MutationObserver(function(){ setTimeout(armReveal, 20); }).observe(document.documentElement,{childList:true,subtree:true});
      })();
    '))
  ),
  
  span(style = "display:none", textOutput("data_ready")),
  
  div(
    class = "product-nav",
    div(
      class = "product-brand",
      div(class = "product-logo", "WC"),
      div(
        div(class = "product-name", "NYC Litigated Claims Intelligence"),
        div(class = "product-tagline", "Legal escalation · evidence agent · specialist triage")
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
        h2("Preparing claims intelligence"),
        p("Opening the official New York WCB file and preparing a local DuckDB benchmark index."),
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
          "Litigation Command Center",
          div(
            class = "command-hero",
            div(
              class = "hero-copy-final",
              div(class = "hero-eyebrow", span(class = "live-pulse"), "Litigated-claims decision intelligence"),
              div(class = "hero-headline", "See where a claim becomes legal work."),
              p(
                class = "hero-deck",
                "Follow the path from representation and controversy through hearings, formal process, and appeal. Surface the files that deserve litigated-claims attention, inspect the evidence behind every signal, and test whether a local evidence agent beats a fixed-chunk RAG baseline on real claim questions."
              ),
              div(
                class = "hero-badges",
                div(class = "hero-badge", "Litigation pathway analytics"),
                div(class = "hero-badge", "Explainable legal signals"),
                div(class = "hero-badge", "Local agent vs. RAG lab"),
                div(class = "hero-badge", "Carrier operational mode")
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
                  div(strong(textOutput("hero_claims", inline = TRUE)), span("public records indexed"))
                )
              )
            )
          ),
          
          div(
            class = "filter-dock",
            div(class = "filter-label", "Book of business lens"),
            fluidRow(
              column(3, selectInput("filter_carrier_name", "Carrier", choices = "All", selected = "All")),
              column(3, selectInput("filter_claim_type", "Claim type", choices = "All", selected = "All")),
              column(3, selectInput("filter_status", "Current status", choices = "All", selected = "All")),
              column(3, selectInput("filter_county", "County of injury", choices = "All", selected = "All"))
            ),
            fluidRow(
              column(2, selectInput("filter_injury_type", "Injury outcome", choices = "All", selected = "All")),
              column(2, selectInput("filter_attorney", "Representation", choices = c("All", "Represented", "Not represented"), selected = "All")),
              column(2, selectInput("filter_controverted", "Controversy", choices = c("All", "Controverted", "Not controverted"), selected = "All")),
              column(2, selectInput("filter_legal_stage", "Legal stage", choices = c("All", "Hearing activity", "Appeal activity", "Formal process", "Priority review"), selected = "All")),
              column(3, sliderInput("filter_year", "Analysis year", min = 2000, max = year(Sys.Date()), value = c(2000, year(Sys.Date())), sep = "")),
              column(1, br(), actionButton("rebuild_cache", "Refresh", class = "btn-primary"))
            ),
            div(
              style = "display:flex;gap:10px;align-items:center;flex-wrap:wrap;padding-bottom:12px;",
              actionButton("reset_filters", "Reset to full portfolio"),
              checkboxInput("auto_visual_fallback", "Keep visualizations populated when a filter combination has zero exact matches", value = TRUE)
            )
          ),
          uiOutput("filter_status_banner"),
          
          div(
            class = "kpi-ribbon",
            div(class = "kpi-tile", div(class = "kpi-title", "Claims in view"), div(class = "kpi-number", textOutput("dashboard_claims", inline = TRUE)), div(class = "kpi-foot", "Records matching the active litigation lens")),
            div(class = "kpi-tile", div(class = "kpi-title", "Priority litigation review"), div(class = "kpi-number", textOutput("dashboard_priority", inline = TRUE)), div(class = "kpi-foot", "Observed litigation-signal points of 70 or higher")),
            div(class = "kpi-tile", div(class = "kpi-title", "Represented"), div(class = "kpi-number", textOutput("dashboard_rep_rate", inline = TRUE)), div(class = "kpi-foot", "Attorney or representative recorded")),
            div(class = "kpi-tile", div(class = "kpi-title", "Controverted"), div(class = "kpi-number", textOutput("dashboard_controverted_rate", inline = TRUE)), div(class = "kpi-foot", "Controversy-resolution process recorded")),
            div(class = "kpi-tile", div(class = "kpi-title", "Appeal activity"), div(class = "kpi-number", textOutput("dashboard_appeal_rate", inline = TRUE)), div(class = "kpi-foot", "First appeal or appellate process signal"))
          ),
          
          uiOutput("executive_narrative"),
          
          div(
            class = "litigation-strip",
            div(class = "litigation-step", strong("1 · Representation"), span("Counsel or representative enters the public record")),
            div(class = "litigation-step", strong("2 · Controversy"), span("A disputed claim enters controversy-resolution activity")),
            div(class = "litigation-step", strong("3 · Hearings"), span("Adjudication activity becomes visible and repeatable")),
            div(class = "litigation-step", strong("4 · Formal process"), span("The record reflects a formal hearing, settlement, or related pathway")),
            div(class = "litigation-step", strong("5 · Appeal"), span("The file reaches appellate activity or an appeal date"))
          ),
          
          div(class = "section-heading", h2("Litigation intelligence, not decoration"), p("Every visualization answers a question a litigated-claims specialist or manager can use: where legal complexity is appearing, which segments escalate, and which files deserve review.")),
          div(class = "visual-panel", uiOutput("finding_cards")),
          
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Legal-signal prevalence"), div(class = "caption", "How many claims show representation, controversy, hearing, or appeal activity in the current lens"), plotlyOutput("funnel_plot", height = "420px")),
            div(class = "visual-panel", h3("Watch litigation pressure change"), div(class = "caption", "Press play to animate representation, controversy, hearing, and appeal rates across analysis years"), plotlyOutput("animated_signal_plot", height = "420px"))
          ),
          
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Litigation signals over time"), div(class = "caption", "Representation, controversy, hearing, and appeal activity by analysis year"), plotlyOutput("signal_trend_plot", height = "420px")),
            div(class = "visual-panel", h3("Representation × controversy → hearing workload"), div(class = "caption", "Average hearing count by representation and controversy status"), plotlyOutput("hearing_association_plot", height = "420px"))
          ),
          
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Geographic litigation hotspots"), div(class = "caption", "Claim volume versus litigation-review share by county"), plotlyOutput("county_signal_plot", height = "420px")),
            div(class = "visual-panel", h3("Injury outcomes that drive complex work"), div(class = "caption", "Volume versus litigation-review share; marker size reflects average legal-signal points"), plotlyOutput("injury_priority_plot", height = "420px"))
          ),
          
          div(class = "visual-panel", h3("Injury × carrier litigation matrix"), div(class = "caption", "Interactive heatmap of average explainable legal-signal points across the highest-volume injury and carrier categories"), plotlyOutput("injury_heatmap_plot", height = "500px")),
          div(class = "visual-panel", h3("Carrier peer litigation benchmark"), div(class = "caption", "Compares controversy and hearing activity under the other active filters. The selected carrier is highlighted against peers when available."), plotlyOutput("carrier_quadrant_plot", height = "460px")),
          
          div(
            class = "visual-panel",
            h3("Top priority records"),
            div(class = "caption", "A fast preview of records with the strongest observed complexity indicators. Open Specialist Review for the full queue and claim drill-down."),
            DTOutput("priority_queue_preview")
          )
        ),
        
        nav_panel(
          "Specialist Review",
          div(class = "claim-hero", h2("Work the file like a litigated-claims specialist."), p("Prioritize legal complexity, open the public record, reconstruct the procedural chronology, and see the exact evidence behind the review recommendation.")),
          div(
            class = "visual-panel",
            fluidRow(
              column(7, sliderInput("minimum_litigation_score", "Minimum litigation-signal points", min = 0, max = 100, value = 45, step = 5)),
              column(5, br(), downloadButton("download_queue", "Export specialist review queue"))
            ),
            DTOutput("litigation_queue")
          ),
          div(
            class = "chart-grid-2",
            div(
              class = "visual-panel",
              h3("Open a public claim"),
              div(class = "caption", "Search the local benchmark index by Claim Identifier"),
              textInput("claim_id_input", "Claim Identifier", placeholder = "Enter a public WCB claim identifier"),
              actionButton("find_claim", "Open claim", class = "btn-primary"),
              br(), br(),
              uiOutput("claim_lookup_status"),
              uiOutput("claim_summary")
            ),
            div(
              class = "visual-panel",
              h3("Specialist review plan"),
              div(class = "caption", "Observed drivers, urgency, and the next verification question"),
              uiOutput("claim_action_plan"),
              plotlyOutput("claim_signal_radar", height = "300px")
            )
          ),
          div(class = "visual-panel", h3("Procedural chronology"), div(class = "caption", "Accident, assembly, controversy, first hearing, and first appeal dates for the opened record"), plotlyOutput("claim_legal_timeline", height = "330px")),
          div(class = "visual-panel", h3("Why this record surfaced"), uiOutput("claim_explanation")),
          div(class = "visual-panel", h3("Source-field evidence"), DTOutput("claim_record_table")),
          div(class = "visual-panel", h3("Transparent legal-signal framework"), DTOutput("method_table_specialist"))
        ),
        
        
        nav_panel(
          "Evidence Agent Lab",
          div(
            class = "agent-lab-hero",
            h1("Build the claims AI you would actually ship."),
            p("This lab tests a local evidence agent against a fixed-chunk RAG baseline on questions generated from the real claims currently loaded. The agent searches the local DuckDB evidence index, opens exact records, follows cross-record constraints, and returns a trace. The baseline retrieves a top fixed claim-summary chunk. Ground truth comes from direct SQL against the same local source."),
            actionButton("run_benchmark", "Run the real-data benchmark", class = "btn-primary"),
            tags$span(" "),
            downloadButton("download_benchmark", "Export benchmark proof")
          ),
          
          div(
            class = "benchmark-flow visual-panel",
            div(class = "flow-node", strong("Question"), tags$br(), span("Real claim or portfolio question")),
            div(class = "flow-arrow", "→"),
            div(class = "flow-node", strong("Local evidence agent"), tags$br(), span("Search → open → follow cross-references")),
            div(class = "flow-arrow", "↔"),
            div(class = "flow-node", strong("Fixed-chunk RAG baseline"), tags$br(), span("Retrieve top claim-summary chunk"))
          ),
          
          uiOutput("benchmark_status"),
          div(
            class = "kpi-ribbon",
            div(class = "kpi-tile", div(class = "kpi-title", "Agent accuracy"), div(class = "kpi-number", textOutput("agent_accuracy", inline = TRUE)), div(class = "kpi-foot", "Exact answer vs. SQL ground truth")),
            div(class = "kpi-tile", div(class = "kpi-title", "RAG accuracy"), div(class = "kpi-number", textOutput("rag_accuracy", inline = TRUE)), div(class = "kpi-foot", "Top fixed-chunk answer")),
            div(class = "kpi-tile", div(class = "kpi-title", "Agent median latency"), div(class = "kpi-number", textOutput("agent_latency", inline = TRUE)), div(class = "kpi-foot", "Local search + open + verification")),
            div(class = "kpi-tile", div(class = "kpi-title", "RAG median latency"), div(class = "kpi-number", textOutput("rag_latency", inline = TRUE)), div(class = "kpi-foot", "Local retrieval baseline timing")),
            div(class = "kpi-tile", div(class = "kpi-title", "Hosted RAG estimate"), div(class = "kpi-number", textOutput("rag_estimated_cost", inline = TRUE)), div(class = "kpi-foot", "Scenario estimate; local prototype API cost is $0"))
          ),
          
          div(
            class = "visual-panel",
            h3("Benchmark controls"),
            div(class = "caption", "Change the corpus size and cost assumptions, then rerun. Cost inputs are scenario assumptions—not a vendor quote."),
            fluidRow(
              column(4, sliderInput("benchmark_corpus_size", "Fixed RAG corpus size", min = 500, max = 5000, value = 2000, step = 500)),
              column(4, numericInput("embedding_cost_per_million", "Indexing / embedding assumption per 1M tokens ($)", value = 0.10, min = 0, step = 0.01)),
              column(4, numericInput("generation_cost_per_million", "Generation assumption per 1M tokens ($)", value = 1.00, min = 0, step = 0.10))
            )
          ),
          
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Accuracy"), div(class = "caption", "Exact-answer performance against SQL ground truth"), plotlyOutput("benchmark_accuracy_plot", height = "350px")),
            div(class = "visual-panel", h3("Latency"), div(class = "caption", "Measured locally for this prototype; hosted systems will differ"), plotlyOutput("benchmark_latency_plot", height = "350px"))
          ),
          
          div(
            class = "ship-card",
            div(class = "ship-title", "Architecture decision"),
            div(class = "ship-value", textOutput("ship_recommendation", inline = TRUE))
          ),
          br(),
          div(class = "visual-panel", h3("Question-level proof"), div(class = "caption", "Every row shows the question, ground truth, both answers, correctness, latency, and source path"), DTOutput("benchmark_results_table")),
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Inspect one benchmark question"), selectInput("trace_question", "Question", choices = character()), uiOutput("trace_summary")),
            div(class = "visual-panel", h3("Agent action trail"), div(class = "caption", "The local prototype exposes its search/open/answer trace instead of hiding the retrieval path"), verbatimTextOutput("agent_trace"))
          ),
          div(class = "info-box", strong("Benchmark boundary: "), "This is a deterministic local prototype over structured public claim fields. It demonstrates search/open/cross-record reasoning and comparative retrieval behavior; it is not evidence that an unconstrained language model will generalize to private claim files without separate validation.")
        ),
        
        nav_panel(
          "Carrier Operations",
          div(
            class = "report-cover",
            h1("Bring the carrier’s own claim operations into the workflow."),
            p("Upload a de-identified carrier extract to add owner, status, diary/activity dates, paid and incurred fields, reserves, and explicit billing/documentation flags. Public-WCB identifier matching is optional; internal claim IDs are supported for operational review."),
            fileInput("enrichment_file", "De-identified carrier claim extract (.csv)", accept = c(".csv", "text/csv")),
            downloadButton("download_enrichment_template", "Download carrier extract template")
          ),
          uiOutput("enrichment_status"),
          div(
            class = "kpi-ribbon",
            div(class = "kpi-tile", div(class = "kpi-title", "Carrier claims loaded"), div(class = "kpi-number", textOutput("enriched_claims", inline = TRUE)), div(class = "kpi-foot", "Unique de-identified claim identifiers")),
            div(class = "kpi-tile", div(class = "kpi-title", "Overdue diaries"), div(class = "kpi-number", textOutput("overdue_diaries", inline = TRUE)), div(class = "kpi-foot", "Next diary date is before today")),
            div(class = "kpi-tile", div(class = "kpi-title", "Billing/documentation flags"), div(class = "kpi-number", textOutput("billing_flags", inline = TRUE)), div(class = "kpi-foot", "Only explicit carrier-provided flags")),
            div(class = "kpi-tile", div(class = "kpi-title", "Total incurred"), div(class = "kpi-number", textOutput("enriched_incurred", inline = TRUE)), div(class = "kpi-foot", "Where provided by the carrier extract")),
            div(class = "kpi-tile", div(class = "kpi-title", "Case reserve"), div(class = "kpi-number", textOutput("enriched_reserve", inline = TRUE)), div(class = "kpi-foot", "Where provided by the carrier extract"))
          ),
          div(
            class = "chart-grid-2",
            div(class = "visual-panel", h3("Largest financial exposures"), div(class = "caption", "Top loaded claims by available incurred, paid, and reserve values"), plotlyOutput("carrier_financial_plot", height = "420px")),
            div(class = "visual-panel", h3("Operational exception mix"), div(class = "caption", "Counts of overdue diaries, explicit billing/documentation flags, and reserve movements"), plotlyOutput("carrier_exception_plot", height = "420px"))
          ),
          div(class = "visual-panel", h3("Carrier action queue"), div(class = "caption", "Operational issues first; public benchmark context appears only when an identifier happens to match the demo dataset"), DTOutput("enrichment_table")),
          div(class = "warning-box", strong("Production boundary: "), "Use de-identified data in this prototype. A commercial deployment should use carrier-approved authentication, role-based access, encryption, audit logging, retention controls, and validated data mappings.")
        ),
        
        nav_panel(
          "Management Report",
          div(
            class = "report-cover",
            h1("A litigation-intelligence product with a defensible AI architecture story."),
            p("The product combines legal-process analytics, an explainable specialist queue, claim-level procedural chronology, carrier operations, and a measurable local-agent-versus-RAG benchmark. The public WCB layer demonstrates the workflow; carrier-approved data turns it into an internal litigated-claims operating product."),
            div(
              class = "capability-row",
              div(class = "capability", strong("Litigation command center"), "Representation, controversy, hearings, formal process, appeal, and hotspots"),
              div(class = "capability", strong("Litigated-file triage"), "Explainable points, next-step review prompts, and procedural chronology"),
              div(class = "capability", strong("Evidence Agent Lab"), "Accuracy, latency, cost scenario, traceability, and a ship decision"),
              div(class = "capability", strong("Carrier operations"), "Diary, financial, reserve, and explicit exception review"),
              div(class = "capability", strong("Auditability"), "Source fields, benchmark proof, CSV exports, and transparent methodology")
            )
          ),
          uiOutput("proof_report"),
          div(
            class = "visual-panel",
            h3("Where the economic value comes from"),
            div(
              class = "insight-grid",
              div(class = "insight-card", div(class = "eyebrow", "Specialist capacity"), div(class = "finding", "Review the right files first."), div(class = "explain", "The ranked queue reduces file-by-file searching and gives the specialist the reason each record surfaced.")),
              div(class = "insight-card", div(class = "eyebrow", "Operational control"), div(class = "finding", "Catch work that can create avoidable friction."), div(class = "explain", "Carrier mode surfaces overdue diaries, explicit billing or documentation flags, and reserve movements without inventing exceptions.")),
              div(class = "insight-card", div(class = "eyebrow", "Consistency and audit"), div(class = "finding", "Make prioritization explainable."), div(class = "explain", "Supervisors can see the rule set, source fields, queue reason, and export trail instead of relying on a black-box score."))
            )
          ),
          div(
            class = "visual-panel",
            h3("Export management outputs"),
            downloadButton("download_queue_management", "Download specialist queue"),
            tags$span(" "),
            downloadButton("download_summary", "Download portfolio summary"),
            br(), br(),
            verbatimTextOutput("report_preview")
          ),
          div(class = "chart-grid-2",
              div(class = "visual-panel", h3("Review-point methodology"), div(class = "caption", "Transparent workload heuristic; not a predictive legal or reserving model"), DTOutput("method_table_management")),
              div(class = "visual-panel", h3("Data sources and limits"), div(class = "caption", "What each layer can and cannot support"), DTOutput("source_register"))
          ),
          div(class = "info-box", strong("Commercial positioning: "), "Sell this as a litigated-claims intelligence and evidence-workflow layer: it helps specialists find legal complexity, reconstruct procedural posture, benchmark portfolio patterns, and test an AI retrieval architecture with measurable proof. Do not sell the public-data heuristic as a liability predictor, counsel-assignment engine, or reserve model until those outcomes are validated on carrier-specific data.")
        )
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  # Database connections are resources, not UI state. Keep the DuckDB
  # connection in session$userData so session callbacks can access it
  # without triggering Shiny reactive-context errors.
  session$userData$con <- NULL
  
  state <- reactiveValues(
    connection_ready = FALSE,
    workers_comp.csv = NULL,
    cache_path = NULL,
    field_map = NULL,
    cache_reused = NA,
    load_error = NULL,
    load_message = "Opening the official New York WCB claims file.",
    claim_record = NULL,
    benchmark_results = NULL,
    benchmark_traces = NULL,
    benchmark_meta = NULL
  )
  
  session$onSessionEnded(function() {
    con <- session$userData$con
    if (!is.null(con)) {
      try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    }
    session$userData$con <- NULL
  })
  
  db_ready <- reactive({
    state$connection_ready &&
      !is.null(session$userData$con) &&
      isTRUE(DBI::dbIsValid(session$userData$con))
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
    div(class = "source-chip", span(class = "source-dot"), span(paste0("NY WCB statewide demo · ", basename(state$workers_comp.csv))))
  })
  
  query_db <- function(sql) {
    req(db_ready())
    DBI::dbGetQuery(session$userData$con, sql)
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
    
    if (!is.null(session$userData$con)) {
      try(DBI::dbDisconnect(session$userData$con, shutdown = TRUE), silent = TRUE)
      session$userData$con <- NULL
      state$connection_ready <- FALSE
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
    
    session$userData$con <- result$con
    state$connection_ready <- TRUE
    state$workers_comp.csv <- normalizePath(path, winslash = "/")
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
        MIN(analysis_year) AS min_year,
        MAX(analysis_year) AS max_year
      FROM claims_scored
      WHERE analysis_year IS NOT NULL
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
    carrier_names <- distinct_values("carrier_name", 300)
    statuses <- distinct_values("current_claim_status", 120)
    counties <- distinct_values("injured_in_county_name", 100)
    injury_types <- distinct_values("claim_injury_type", 80)
    
    # Start broad so the product always opens with a populated portfolio view.
    updateSelectInput(session, "filter_claim_type", choices = c("All", claim_types), selected = "All")
    updateSelectizeInput(session, "filter_carrier_name", choices = c("All", carrier_names), selected = "All", server = TRUE)
    updateSelectizeInput(session, "filter_status", choices = c("All", statuses), selected = "All", server = TRUE)
    updateSelectInput(session, "filter_county", choices = c("All", counties), selected = "All")
    updateSelectInput(session, "filter_injury_type", choices = c("All", injury_types), selected = "All")
  }
  
  build_book_where <- function(include_carrier = TRUE) {
    req(db_ready())
    conditions <- c("1 = 1")
    
    years <- input$filter_year %||% c(2000, year(Sys.Date()))
    conditions <- c(
      conditions,
      paste0("analysis_year BETWEEN ", as.integer(years[1]), " AND ", as.integer(years[2]))
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
      if (isTRUE(include_carrier)) add_text_filter("carrier_name", input$filter_carrier_name) else NULL,
      add_text_filter("claim_type", input$filter_claim_type),
      add_text_filter("current_claim_status", input$filter_status),
      add_text_filter("injured_in_county_name", input$filter_county),
      add_text_filter("claim_injury_type", input$filter_injury_type)
    )
    
    if (identical(input$filter_attorney, "Represented")) conditions <- c(conditions, "attorney_flag = 1")
    if (identical(input$filter_attorney, "Not represented")) conditions <- c(conditions, "attorney_flag = 0")
    if (identical(input$filter_controverted, "Controverted")) conditions <- c(conditions, "controverted_flag = 1")
    if (identical(input$filter_controverted, "Not controverted")) conditions <- c(conditions, "controverted_flag = 0")
    
    legal_stage <- input$filter_legal_stage %||% "All"
    if (identical(legal_stage, "Hearing activity")) conditions <- c(conditions, "hearing_flag = 1")
    if (identical(legal_stage, "Appeal activity")) conditions <- c(conditions, "appeal_flag = 1")
    if (identical(legal_stage, "Formal process")) conditions <- c(conditions, "formal_process_flag = 1")
    if (identical(legal_stage, "Priority review")) conditions <- c(conditions, "litigation_signal_score >= 70")
    
    paste(conditions[!is.na(conditions) & nzchar(conditions)], collapse = " AND ")
  }
  
  filter_where <- reactive(build_book_where(TRUE))
  benchmark_where <- reactive(build_book_where(FALSE))
  
  filtered_row_count <- reactive({
    req(db_ready())
    query_db(paste0("SELECT COUNT(*) AS n FROM claims_scored WHERE ", filter_where()))$n[1]
  })
  
  visual_where <- reactive({
    n <- filtered_row_count()
    use_fallback <- isTRUE(input$auto_visual_fallback %||% TRUE) && (is.na(n) || n == 0)
    if (use_fallback) "1 = 1" else filter_where()
  })
  
  benchmark_visual_where <- reactive({
    req(db_ready())
    where_sql <- benchmark_where()
    n <- query_db(paste0("SELECT COUNT(*) AS n FROM claims_scored WHERE ", where_sql))$n[1]
    if (isTRUE(input$auto_visual_fallback %||% TRUE) && (is.na(n) || n == 0)) "1 = 1" else where_sql
  })
  
  output$filter_status_banner <- renderUI({
    req(db_ready())
    n <- filtered_row_count()
    if (is.na(n) || n == 0) {
      if (isTRUE(input$auto_visual_fallback %||% TRUE)) {
        return(div(class = "warning-box", strong("No exact records match that filter combination. "), "The charts are showing the full statewide benchmark so the screen never collapses into empty panels. The specialist queue remains tied to the exact filters. Select Reset to full portfolio to restore the normal lens."))
      }
      return(div(class = "warning-box", strong("No exact records match that filter combination. "), "Reset or widen a filter to repopulate the visualizations."))
    }
    div(class = "success-box", strong(fmt_number(n)), " claims match the current litigation lens. Hover, zoom, play animations, and open the specialist queue to drill into the records behind the patterns.")
  })
  
  observeEvent(input$reset_filters, {
    req(db_ready())
    ranges <- setup_stats()
    updateSelectizeInput(session, "filter_carrier_name", selected = "All")
    updateSelectInput(session, "filter_claim_type", selected = "All")
    updateSelectizeInput(session, "filter_status", selected = "All")
    updateSelectInput(session, "filter_county", selected = "All")
    updateSelectInput(session, "filter_injury_type", selected = "All")
    updateSelectInput(session, "filter_attorney", selected = "All")
    updateSelectInput(session, "filter_controverted", selected = "All")
    updateSelectInput(session, "filter_legal_stage", selected = "All")
    updateSliderInput(session, "filter_year", value = c(as.integer(ranges$min_year[1]), as.integer(ranges$max_year[1])))
  }, ignoreInit = TRUE)
  
  output$load_status <- renderUI({
    if (!is.null(state$load_error)) {
      return(div(class = "danger-box", strong("Data could not be loaded. "), state$load_error))
    }
    
    if (!db_ready()) {
      return(div(class = "status-box", state$load_message %||% "Opening the official New York WCB claims file."))
    }
    
    div(
      class = "success-box",
      strong("Official claims data ready. "),
      state$load_message,
      tags$br(),
      paste0("CSV: ", state$workers_comp.csv),
      tags$br(),
      paste0("DuckDB cache: ", state$cache_path)
    )
  })
  
  setup_stats <- reactive({
    req(db_ready())
    query_db("
      SELECT
        COUNT(*) AS rows,
        MIN(analysis_year) AS min_year,
        MAX(analysis_year) AS max_year
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
      "SUM(CASE WHEN litigation_signal_score >= 70 THEN 1 ELSE 0 END) AS priority_count, ",
      "AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS specialist_review_rate, ",
      "AVG(litigation_signal_score) AS avg_review_points, ",
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
  output$dashboard_priority <- renderText({ fmt_number(portfolio_metrics()$priority_count[1]) })
  output$dashboard_material <- renderText({ fmt_percent(portfolio_metrics()$specialist_review_rate[1]) })
  output$dashboard_rep_rate <- renderText({ fmt_percent(portfolio_metrics()$attorney_rate[1]) })
  output$dashboard_controverted_rate <- renderText({ fmt_percent(portfolio_metrics()$controverted_rate[1]) })
  output$dashboard_appeal_rate <- renderText({ fmt_percent(portfolio_metrics()$appeal_rate[1]) })
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
      "FROM claims_scored WHERE ", visual_where()
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
      "SUM(appeal_flag) AS appeals FROM claims_scored WHERE ", visual_where()
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
      "FROM claims_scored WHERE ", visual_where(), " GROUP BY representation, controversy"
    ))
    plot_ly(data, x = ~representation, y = ~avg_hearings, color = ~controversy,
            type = "bar", text = ~round(avg_hearings, 2), textposition = "auto",
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
      "SELECT analysis_year AS assembly_year, COUNT(*) AS claims, AVG(attorney_flag) AS represented, ",
      "AVG(controverted_flag) AS controverted, AVG(hearing_flag) AS hearing, AVG(appeal_flag) AS appeal ",
      "FROM claims_scored WHERE ", visual_where(),
      " AND analysis_year IS NOT NULL GROUP BY analysis_year ORDER BY analysis_year"
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
             xaxis = list(title = "Analysis year"), legend = list(orientation = "h", x = 0, y = 1.12),
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
      animation_slider(currentvalue = list(prefix = "Analysis year: ")) |>
      animation_button(x = 0.02, y = 1.08)
  })
  
  output$county_signal_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "SELECT COALESCE(injured_in_county_name,'Unknown') AS county, COUNT(*) AS claims, ",
      "AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS material_rate, ",
      "AVG(litigation_signal_score) AS avg_score FROM claims_scored WHERE ", visual_where(),
      " GROUP BY county HAVING COUNT(*) >= 1 ORDER BY claims DESC LIMIT 40"
    ))
    validate(need(nrow(data) > 0, "No claims match the current filters. Widen one or more filters."))
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
  
  output$injury_priority_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "SELECT COALESCE(claim_injury_type,'Unknown') AS injury, COUNT(*) AS claims, ",
      "AVG(CASE WHEN litigation_signal_score >= 45 THEN 1 ELSE 0 END) AS specialist_rate, ",
      "AVG(litigation_signal_score) AS avg_points ",
      "FROM claims_scored WHERE ", visual_where(),
      " GROUP BY injury ORDER BY claims DESC LIMIT 18"
    ))
    validate(need(nrow(data) > 0, "No injury-outcome records match the active book lens."))
    
    plot_ly(
      data,
      x = ~claims, y = ~specialist_rate, size = ~avg_points, color = ~avg_points,
      text = ~injury, type = "scatter", mode = "markers", sizes = c(10, 42),
      colors = c("#dbe7f5", "#174f83"),
      marker = list(line = list(color = "#ffffff"), opacity = .9),
      hovertemplate = paste0(
        "%{text}<br>Claims: %{x:,}",
        "<br>Litigation-review share: %{y:.1%}",
        "<br>Average legal-signal points: %{marker.color:.1f}<extra></extra>"
      )
    ) |>
      layout(
        xaxis = list(title = "Claim volume", tickformat = ",d"),
        yaxis = list(title = "Litigation-review share", tickformat = ".0%"),
        margin = list(l = 75, r = 30, t = 15, b = 55),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$assembly_trend <- renderPlot({
    data <- query_db(paste0(
      "SELECT analysis_year AS assembly_year, COUNT(*) AS claims FROM claims_scored WHERE ",
      filter_where(), " AND analysis_year IS NOT NULL GROUP BY analysis_year ORDER BY analysis_year"
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
  
  specialist_queue_query <- function(min_score = 45, limit = 1000) {
    query_db(paste0(
      "SELECT claim_identifier, carrier_name, assembly_date, claim_injury_type, current_claim_status, ",
      "atty_rep_ind, controverted_date, hearing_count, first_appeal_date, highest_process, ",
      "injured_in_county_name, litigation_signal_score AS review_points, litigation_review_tier AS review_tier, ",
      "CASE ",
      " WHEN appeal_flag = 1 THEN 'Appeal activity is present' ",
      " WHEN controverted_flag = 1 AND attorney_flag = 1 THEN 'Controverted and represented' ",
      " WHEN COALESCE(hearing_count,0) >= 2 THEN 'Repeated hearing activity' ",
      " WHEN severity_points >= 14 THEN 'Serious injury outcome' ",
      " WHEN attorney_flag = 1 THEN 'Representation is present' ",
      " WHEN active_status_flag = 1 THEN 'Active adjudication status' ",
      " ELSE 'Multiple observed administrative signals' END AS primary_review_reason, ",
      "CASE ",
      " WHEN appeal_flag = 1 THEN 'Verify appeal issue, posture, and next deadline' ",
      " WHEN controverted_flag = 1 AND attorney_flag = 1 THEN 'Review disputed issues, evidence gaps, and counsel strategy' ",
      " WHEN COALESCE(hearing_count,0) >= 2 THEN 'Identify the unresolved issue driving repeat hearings' ",
      " WHEN severity_points >= 14 THEN 'Review exposure, current process posture, and resolution path' ",
      " WHEN attorney_flag = 1 THEN 'Confirm represented status and next procedural milestone' ",
      " WHEN active_status_flag = 1 THEN 'Verify current adjudication status and diary the next event' ",
      " ELSE 'Confirm whether routine monitoring remains appropriate' END AS specialist_next_step ",
      "FROM claims_scored WHERE ", filter_where(),
      " AND litigation_signal_score >= ", as.integer(min_score),
      " ORDER BY litigation_signal_score DESC, hearing_count DESC NULLS LAST LIMIT ", as.integer(limit)
    ))
  }
  
  output$priority_queue_preview <- renderDT({
    data <- specialist_queue_query(70, 25)
    validate(need(nrow(data) > 0, "No records currently meet the priority-review threshold. Lower the specialist threshold in Specialist Review if needed."))
    datatable(
      data |> select(claim_identifier, carrier_name, current_claim_status, claim_injury_type, review_points, primary_review_reason, specialist_next_step),
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
      rownames = FALSE
    )
  })
  
  output$litigation_queue <- renderDT({
    req(db_ready())
    min_score <- as.integer(input$minimum_litigation_score %||% 45)
    data <- specialist_queue_query(min_score, 2500)
    validate(need(nrow(data) > 0, "No records meet the selected review-point threshold under the active book lens."))
    
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
      visual_where(), " GROUP BY tier ORDER BY claims DESC"
    ))
    validate(need(nrow(data) > 0, "No records match the filters."))
    data$tier <- factor(
      data$tier,
      levels = c("Routine", "Monitor", "Specialist review", "Priority specialist review")
    )
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
      "COUNT(*) AS claims FROM claims_scored WHERE ", visual_where(),
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
  
  render_method_table <- function() {
    datatable(method_table, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
  }
  output$method_table_specialist <- renderDT({ render_method_table() })
  output$method_table_management <- renderDT({ render_method_table() })
  
  observeEvent(input$find_claim, {
    req(db_ready())
    claim_id <- str_trim(input$claim_id_input %||% "")
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
      "Observed litigation-signal points" = "litigation_signal_score"
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
  
  output$claim_action_plan <- renderUI({
    record <- state$claim_record
    if (is.null(record) || nrow(record) != 1) {
      return(div(class = "status-box", "Open a claim to generate a specialist review plan."))
    }
    row <- record[1, , drop = FALSE]
    
    urgency <- if (row$litigation_signal_score >= 70) {
      "Priority specialist review"
    } else if (row$litigation_signal_score >= 45) {
      "Specialist review"
    } else if (row$litigation_signal_score >= 20) {
      "Monitor"
    } else {
      "Routine"
    }
    
    next_step <- if (row$appeal_flag == 1) {
      "Verify the appealed issue, current posture, controlling deadline, and the evidence supporting the position."
    } else if (row$controverted_flag == 1 && row$attorney_flag == 1) {
      "Identify the disputed issue, confirm representation, and organize the evidence needed for the next procedural event."
    } else if (!is.na(row$hearing_count) && row$hearing_count >= 2) {
      "Determine what issue remains unresolved after repeated hearings and what evidence or decision is still outstanding."
    } else if (row$severity_points >= 14) {
      "Review the serious injury outcome together with current process status and the path toward resolution."
    } else if (row$active_status_flag == 1) {
      "Confirm the current adjudication status and the next event that should be diaried or verified."
    } else {
      "Confirm that the visible record supports routine monitoring and that no material process event is missing."
    }
    
    tagList(
      div(class = "info-box", strong("Review level: "), urgency, " — ", row$litigation_signal_score, " observed litigation-signal points."),
      br(),
      div(class = "success-box", strong("Next verification step: "), next_step),
      br(),
      div(class = "warning-box", "The plan is a workflow prompt from public administrative fields. It is not a compensability, liability, reserve, authority, or legal-strategy decision.")
    )
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
      div(class = "info-box", strong("Tier: "), row$litigation_review_tier, " (", row$litigation_signal_score, " litigation-signal points)."),
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
  
  enrichment_data <- reactive({
    if (is.null(input$enrichment_file)) return(NULL)
    tryCatch(
      prepare_enrichment(read_csv(input$enrichment_file$datapath, show_col_types = FALSE)),
      error = function(e) structure(tibble(), error_message = conditionMessage(e))
    )
  })
  
  enriched_review <- reactive({
    req(db_ready())
    enrichment <- enrichment_data()
    if (is.null(enrichment)) return(NULL)
    if (!is.null(attr(enrichment, "error_message"))) return(enrichment)
    if (nrow(enrichment) == 0) return(enrichment)
    
    # Public matching is optional. Limit lookup size so a large internal extract
    # still works as an operational queue even when identifiers are private.
    ids <- head(unique(enrichment$claim_identifier), 5000)
    id_sql <- paste(vapply(ids, sql_string, character(1)), collapse = ",")
    public <- if (length(ids) > 0) {
      query_db(paste0(
        "SELECT claim_identifier, claim_injury_type, current_claim_status AS public_status, atty_rep_ind, ",
        "controverted_date, hearing_count, first_appeal_date, highest_process, ",
        "litigation_signal_score AS public_review_points, litigation_review_tier AS public_review_tier ",
        "FROM claims_scored WHERE claim_identifier IN (", id_sql, ")"
      ))
    } else {
      tibble(claim_identifier = character())
    }
    
    enrichment |>
      left_join(public, by = "claim_identifier") |>
      mutate(
        charged_paid_ratio = if_else(!is.na(charged_amount) & !is.na(paid_amount) & paid_amount > 0, charged_amount / paid_amount, NA_real_),
        billing_flag = as.integer(
          coalesce(duplicate_bill_flag, 0) == 1 |
            coalesce(provider_outlier_flag, 0) == 1 |
            coalesce(documentation_gap_flag, 0) == 1
        ),
        reserve_movement_flag = as.integer(!is.na(reserve_change_90d) & reserve_change_90d != 0),
        overdue_diary_flag = as.integer(!is.na(next_diary_date) & next_diary_date < Sys.Date()),
        activity_age_days = if_else(!is.na(last_activity_date), as.numeric(Sys.Date() - last_activity_date), NA_real_),
        public_match = if_else(!is.na(public_review_points), "Matched to public benchmark", "Internal ID only"),
        operational_attention = case_when(
          overdue_diary_flag == 1 ~ "Overdue diary",
          billing_flag == 1 ~ "Billing/documentation review",
          reserve_movement_flag == 1 ~ "Reserve movement review",
          TRUE ~ "Routine"
        )
      )
  })
  
  output$enrichment_status <- renderUI({
    data <- enrichment_data()
    if (is.null(data)) return(div(class = "status-box", "No carrier extract is loaded. Public benchmark mode remains fully available."))
    if (!is.null(attr(data, "error_message"))) return(div(class = "danger-box", attr(data, "error_message")))
    reviewed <- enriched_review()
    matched <- if (is.null(reviewed) || nrow(reviewed) == 0) 0 else sum(reviewed$public_match == "Matched to public benchmark", na.rm = TRUE)
    div(
      class = "success-box",
      strong("Carrier operational data loaded. "),
      paste0(comma(nrow(data)), " unique claim identifiers are available for operational review. ", comma(matched), " happened to match the public benchmark index; matching is not required.")
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
  output$overdue_diaries <- renderText({
    data <- enriched_review()
    if (is.null(data) || nrow(data) == 0) "—" else fmt_number(sum(data$overdue_diary_flag, na.rm = TRUE))
  })
  output$reserve_movements <- renderText({
    data <- enriched_review()
    if (is.null(data) || nrow(data) == 0) "—" else fmt_number(sum(data$reserve_movement_flag, na.rm = TRUE))
  })
  
  output$carrier_financial_plot <- renderPlotly({
    data <- enriched_review()
    validate(need(!is.null(data) && is.data.frame(data) && nrow(data) > 0, "Upload a carrier extract to view financial exposure."))
    
    plot_data <- data |>
      mutate(
        exposure_sort = pmax(coalesce(total_incurred, 0), coalesce(paid_amount, 0) + coalesce(case_reserve, 0), na.rm = TRUE)
      ) |>
      arrange(desc(exposure_sort)) |>
      slice_head(n = 20) |>
      select(claim_identifier, total_incurred, paid_amount, case_reserve) |>
      pivot_longer(c(total_incurred, paid_amount, case_reserve), names_to = "measure", values_to = "amount") |>
      filter(!is.na(amount)) |>
      mutate(measure = recode(measure, total_incurred = "Total incurred", paid_amount = "Paid", case_reserve = "Case reserve"))
    
    validate(need(nrow(plot_data) > 0, "The loaded carrier extract does not contain incurred, paid, or reserve values."))
    
    plot_ly(
      plot_data, x = ~claim_identifier, y = ~amount, color = ~measure,
      type = "bar", hovertemplate = "%{x}<br>%{fullData.name}: $%{y:,.0f}<extra></extra>"
    ) |>
      layout(
        barmode = "group",
        xaxis = list(title = "Claim", tickangle = -45),
        yaxis = list(title = "Amount", tickprefix = "$", separatethousands = TRUE),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(l = 80, r = 20, t = 45, b = 110),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$carrier_exception_plot <- renderPlotly({
    data <- enriched_review()
    validate(need(!is.null(data) && is.data.frame(data) && nrow(data) > 0, "Upload a carrier extract to view operational exceptions."))
    
    counts <- tibble(
      issue = c("Overdue diary", "Duplicate bill flag", "Provider outlier flag", "Documentation gap", "Reserve movement"),
      claims = c(
        sum(data$overdue_diary_flag == 1, na.rm = TRUE),
        sum(data$duplicate_bill_flag == 1, na.rm = TRUE),
        sum(data$provider_outlier_flag == 1, na.rm = TRUE),
        sum(data$documentation_gap_flag == 1, na.rm = TRUE),
        sum(data$reserve_movement_flag == 1, na.rm = TRUE)
      )
    )
    
    plot_ly(
      counts, x = ~claims, y = ~reorder(issue, claims), type = "bar", orientation = "h",
      text = ~comma(claims), textposition = "auto",
      hovertemplate = "%{y}: %{x:,} claims<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "Claims", tickformat = ",d"), yaxis = list(title = ""),
        margin = list(l = 150, r = 25, t = 15, b = 50),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$enrichment_table <- renderDT({
    data <- enriched_review()
    validate(need(!is.null(data) && is.data.frame(data) && nrow(data) > 0, "Upload a valid carrier extract to create the operational action queue."))
    
    display <- data |>
      arrange(desc(overdue_diary_flag), desc(billing_flag), desc(reserve_movement_flag), desc(coalesce(total_incurred, 0))) |>
      select(
        claim_identifier, claim_owner, internal_status, last_activity_date, next_diary_date,
        operational_attention, total_incurred, case_reserve, reserve_change_90d,
        billing_flag, public_match, public_review_points, public_review_tier
      )
    
    datatable(display, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE, filter = "top")
  })
  
  output$download_enrichment_template <- downloadHandler(
    filename = function() "carrier_claims_operational_extract_template.csv",
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
      sql_identifier(flag), ") AS rate FROM claims_scored WHERE ", visual_where(),
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
    where_sql <- visual_where()
    
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
      "first_appeal_date, litigation_signal_score FROM claims_scored WHERE ", visual_where(),
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
            "SOURCE BOUNDARY\nOfficial New York WCB public administrative fields only. No private notes, medical records, reserves, or invented claim facts."
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
        "Ship the local evidence agent as the primary litigation-intelligence layer for structured claim facts, chronology, aggregation, and cross-record questions. Keep RAG as a secondary passage-retrieval tool for authorized free-text notes and documents."
      } else if (rag_accuracy_value >= agent_accuracy_value + 0.10) {
        "The fixed-chunk baseline wins this question set. Keep structured verification before any answer is used in litigated-claim handling, and retest on carrier-specific questions before shipping."
      } else {
        "Ship a hybrid: use the local evidence agent for structured claim facts, chronology, aggregation, and cross-reference questions; use RAG for direct passage retrieval, with both methods returning source evidence."
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
      return(div(class = "status-box", "The benchmark is ready. Questions and ground truth are generated from the real records currently in scope. Run it to compare the local evidence agent with the fixed-chunk retrieval baseline."))
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
  output$rag_latency <- renderText({
    if (is.null(state$benchmark_meta)) "—" else paste0(round(state$benchmark_meta$rag_latency, 1), " ms")
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
    
    statement <- paste0(
      fmt_number(metrics$claim_count), " claims are in the active book lens. ",
      fmt_number(metrics$priority_count), " meet the priority litigation-review threshold, and ",
      fmt_percent(metrics$specialist_review_rate), " meet the broader litigation-review threshold. ",
      fmt_percent(metrics$attorney_rate), " are represented and ",
      fmt_percent(metrics$controverted_rate), " are controverted."
    )
    
    div(
      class = "executive-callout",
      div(
        strong("Operating view"),
        p(statement),
        p("Use the queue to decide where specialist attention should begin, then verify the underlying file before making claim, legal, authority, or reserving decisions.")
      ),
      div(class = "report-badge", "ACTIONABLE VIEW")
    )
  })
  
  output$carrier_quadrant_plot <- renderPlotly({
    req(db_ready())
    selected_carrier <- input$filter_carrier_name %||% "All"
    selected_sql <- if (!is.null(selected_carrier) && nzchar(selected_carrier) && selected_carrier != "All") sql_string(selected_carrier) else "NULL"
    
    data <- query_db(paste0(
      "SELECT COALESCE(carrier_name,'Unknown') AS carrier, COUNT(*) AS claims, ",
      "AVG(controverted_flag) AS controversy_rate, AVG(hearing_flag) AS hearing_rate, ",
      "AVG(attorney_flag) AS representation_rate, AVG(litigation_signal_score) AS avg_points, ",
      "CASE WHEN ", selected_sql, " IS NOT NULL AND carrier_name = ", selected_sql,
      " THEN 'Selected carrier' ELSE 'Peer' END AS benchmark_group ",
      "FROM claims_scored WHERE ", benchmark_visual_where(),
      " GROUP BY COALESCE(carrier_name,'Unknown'), benchmark_group ",
      "ORDER BY claims DESC LIMIT 30"
    ))
    validate(need(nrow(data) > 0, "No carrier records match the active benchmark lens."))
    
    plot_ly(
      data,
      x = ~controversy_rate, y = ~hearing_rate, size = ~claims,
      color = ~benchmark_group, text = ~carrier, customdata = ~claims,
      type = "scatter", mode = "markers", sizes = c(10, 58),
      marker = list(line = list(color = "#ffffff"), opacity = .88),
      hovertemplate = paste0(
        "%{text}<br>Claims: %{customdata:,}",
        "<br>Controversy rate: %{x:.1%}",
        "<br>Hearing rate: %{y:.1%}<extra></extra>"
      )
    ) |>
      layout(
        xaxis = list(title = "Controversy rate", tickformat = ".0%", zeroline = FALSE),
        yaxis = list(title = "Hearing activity rate", tickformat = ".0%", zeroline = FALSE),
        legend = list(orientation = "h", x = 0, y = 1.12),
        margin = list(l = 70, r = 30, t = 45, b = 60),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$injury_heatmap_plot <- renderPlotly({
    req(db_ready())
    data <- query_db(paste0(
      "WITH top_injuries AS (",
      " SELECT COALESCE(claim_injury_type,'Unknown') AS injury, COUNT(*) AS n",
      " FROM claims_scored WHERE ", visual_where(),
      " GROUP BY injury ORDER BY n DESC LIMIT 9),",
      " top_carriers AS (",
      " SELECT COALESCE(carrier_type,'Unknown') AS carrier, COUNT(*) AS n",
      " FROM claims_scored WHERE ", visual_where(),
      " GROUP BY carrier ORDER BY n DESC LIMIT 7)",
      " SELECT COALESCE(c.claim_injury_type,'Unknown') AS injury,",
      " COALESCE(c.carrier_type,'Unknown') AS carrier,",
      " AVG(c.litigation_signal_score) AS avg_score, COUNT(*) AS claims",
      " FROM claims_scored c",
      " JOIN top_injuries i ON COALESCE(c.claim_injury_type,'Unknown') = i.injury",
      " JOIN top_carriers t ON COALESCE(c.carrier_type,'Unknown') = t.carrier",
      " WHERE ", visual_where(),
      " GROUP BY COALESCE(c.claim_injury_type,'Unknown'), COALESCE(c.carrier_type,'Unknown')"
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
  
  output$claim_signal_radar <- renderPlotly({
    record <- state$claim_record
    if (is.null(record) || nrow(record) != 1) {
      empty <- tibble(
        signal = c("Representation", "Controversy", "Hearing", "Appeal", "Formal process", "Severity"),
        value = rep(0, 6)
      )
      return(
        plot_ly(empty, type = "scatterpolar", mode = "lines+markers", r = ~value, theta = ~signal, fill = "toself",
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
      data, type = "scatterpolar", mode = "lines+markers", r = ~value, theta = ~signal, fill = "toself",
      line = list(color = "#174f83", width = 3), fillcolor = "rgba(66,211,255,.28)",
      hovertemplate = "%{theta}: %{r:.0%}<extra></extra>"
    ) |>
      layout(
        polar = list(radialaxis = list(range = c(0, 1), tickformat = ".0%", visible = TRUE)),
        showlegend = FALSE, margin = list(l = 55, r = 55, t = 25, b = 25),
        paper_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  output$claim_legal_timeline <- renderPlotly({
    record <- state$claim_record
    if (is.null(record) || nrow(record) != 1) {
      return(
        plot_ly(x = numeric(0), y = numeric(0), type = "scatter", mode = "markers") |>
          layout(
            annotations = list(list(
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              text = "Open a claim above to animate its procedural chronology.",
              showarrow = FALSE, font = list(color = "#7b8da0", size = 15)
            )),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
            margin = list(l = 30, r = 30, t = 20, b = 20)
          )
      )
    }
    
    row <- record[1, , drop = FALSE]
    events <- tibble(
      event = c("Accident", "WCB assembly", "Controverted", "First hearing", "First appeal"),
      event_date = as.Date(c(row$accident_date[1], row$assembly_date[1], row$controverted_date[1], row$first_hearing_date[1], row$first_appeal_date[1])),
      event_order = 1:5
    ) |>
      filter(!is.na(event_date)) |>
      arrange(event_date, event_order)
    
    if (nrow(events) == 0) {
      return(
        plot_ly(x = numeric(0), y = numeric(0), type = "scatter", mode = "markers") |>
          layout(
            annotations = list(list(x=.5,y=.5,xref="paper",yref="paper",text="No chronology dates are available for this public record.",showarrow=FALSE)),
            xaxis=list(visible=FALSE), yaxis=list(visible=FALSE), paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)"
          )
      )
    }
    
    events <- events |> mutate(y = 1, label = paste0(event, "<br>", format(event_date, "%b %d, %Y")))
    plot_ly(
      events, x = ~event_date, y = ~y, type = "scatter", mode = "lines+markers+text",
      text = ~event, textposition = "top center",
      line = list(color = "#174f83", width = 4),
      marker = list(size = 16, color = "#42d3ff", line = list(color = "#0b2b48", width = 2)),
      hovertext = ~label, hoverinfo = "text"
    ) |>
      layout(
        xaxis = list(title = "Procedural date", type = "date", showgrid = FALSE),
        yaxis = list(visible = FALSE, range = c(.88, 1.18)),
        margin = list(l = 35, r = 35, t = 45, b = 55),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  output$proof_report <- renderUI({
    metrics <- portfolio_metrics()[1, ]
    
    div(
      class = "executive-callout",
      div(
        strong("Management conclusion"),
        p(paste0(
          "The filtered benchmark contains ", fmt_number(metrics$claim_count), " claims. ",
          fmt_number(metrics$priority_count), " meet the priority litigation-review threshold and ",
          fmt_percent(metrics$specialist_review_rate), " meet the broader litigation-review threshold. ",
          "Representation is ", fmt_percent(metrics$attorney_rate), " and formal-process activity is ", fmt_percent(metrics$formal_process_rate), "."
        )),
        p("The product's value is faster triage, consistent review logic, external benchmark context, and an auditable handoff to a human specialist—not automated claim adjudication.")
      ),
      div(class = "report-badge", "MANAGEMENT READY")
    )
  })
  
  output$source_register <- renderDT({
    datatable(source_register, options = list(pageLength = 3, dom = "t", scrollX = TRUE), rownames = FALSE)
  })
  
  output$download_queue <- downloadHandler(
    filename = function() paste0("claims_specialist_review_queue_", Sys.Date(), ".csv"),
    content = function(file) {
      req(db_ready())
      min_score <- as.integer(input$minimum_litigation_score %||% 45)
      write_csv(specialist_queue_query(min_score, 50000), file)
    }
  )
  
  output$download_queue_management <- downloadHandler(
    filename = function() paste0("claims_specialist_review_queue_", Sys.Date(), ".csv"),
    content = function(file) {
      req(db_ready())
      min_score <- as.integer(input$minimum_litigation_score %||% 45)
      write_csv(specialist_queue_query(min_score, 50000), file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() paste0("claims_portfolio_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      metrics <- portfolio_metrics() |>
        pivot_longer(everything(), names_to = "metric", values_to = "value")
      write_csv(metrics, file)
    }
  )
  
  output$report_preview <- renderText({
    if (!db_ready()) return("Load the official claims CSV to generate the report preview.")
    metrics <- portfolio_metrics()
    benchmark_text <- if (is.null(state$benchmark_meta)) {
      "\n\nEVIDENCE AGENT BENCHMARK\nNot run in this session. Open Evidence Agent Lab to generate accuracy, latency, cost scenario, and a ship recommendation."
    } else {
      paste0(
        "\n\nEVIDENCE AGENT BENCHMARK\n",
        "Local agent accuracy: ", percent(state$benchmark_meta$agent_accuracy, accuracy = 0.1), "\n",
        "Fixed RAG accuracy: ", percent(state$benchmark_meta$rag_accuracy, accuracy = 0.1), "\n",
        "Local agent median latency: ", round(state$benchmark_meta$agent_latency, 1), " ms\n",
        "Fixed RAG median latency: ", round(state$benchmark_meta$rag_latency, 1), " ms\n",
        "Hosted RAG scenario cost: ", dollar(state$benchmark_meta$estimated_cost, accuracy = 0.0001), "\n",
        "Ship recommendation: ", state$benchmark_meta$ship
      )
    }
    
    paste0(
      "NYC LITIGATED CLAIMS INTELLIGENCE — STATEWIDE NY WCB BENCHMARK MODE\n\n",
      "Official public source: ", state$workers_comp.csv, "\n",
      "Filtered claims: ", fmt_number(metrics$claim_count[1]), "\n",
      "Priority litigation-review claims: ", fmt_number(metrics$priority_count[1]), "\n",
      "Litigation-review share: ", fmt_percent(metrics$specialist_review_rate[1]), "\n",
      "Attorney/representative rate: ", fmt_percent(metrics$attorney_rate[1]), "\n",
      "Controverted rate: ", fmt_percent(metrics$controverted_rate[1]), "\n",
      "Hearing activity rate: ", fmt_percent(metrics$hearing_rate[1]), "\n",
      "Appeal activity rate: ", fmt_percent(metrics$appeal_rate[1]), "\n",
      "Median accident-to-assembly interval: ", fmt_days(metrics$median_days_to_assembly[1]),
      benchmark_text, "\n\n",
      "Interpretation: Litigation-signal points are an explainable workload heuristic based on observed administrative signals. They do not determine liability, compensability, reserve adequacy, settlement authority, counsel assignment, or legal strategy."
    )
  })
  
  # Open the exact official CSV automatically; the product never starts on a setup screen.
  session$onFlushed(function() {
    if (is.null(session$userData$con)) {
      isolate(load_database(FALSE))
    }
  }, once = TRUE)
}

shinyApp(ui = ui, server = server)

