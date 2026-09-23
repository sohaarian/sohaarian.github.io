# ============================================================
# Workers' Compensation Litigation Claims Analytics
# Litigation-referral research and claims triage prototype
#
# What this app analyzes:
# - medical inflation and billing behavior
# - claim severity and duration
# - reserve pressure and volatility
# - dispute, compensability, and causation indicators
# - possible litigation-referral indicators
# - historical outcome validation when an outcome field is available
#
# Important:
# This is an explainable research prototype. It does not make legal
# decisions, replace adjuster judgment, or automatically refer claims.
# ============================================================

# Install once if needed:
# install.packages(c("shiny", "bslib", "tidyverse", "DT", "scales", "lubridate"))
install.packages("rsconnect")
library(rsconnect)

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(scales)
library(lubridate)

options(shiny.maxRequestSize = 50 * 1024^2)

# ============================================================
# Brand theme
# ============================================================

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#24303f",
  primary = "#cf4b7c",
  secondary = "#f4c8d8",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

app_css <- '
:root {
  --rose: #cf4b7c;
  --rose-dark: #b33869;
  --rose-soft: #f4c8d8;
  --rose-wash: #fbf2f6;
  --blue: #6da2d4;
  --blue-soft: #dcecf8;
  --ink: #24303f;
  --muted: #687586;
  --line: #eadbe2;
  --paper: #ffffff;
}

body {
  background:
    radial-gradient(circle at top right, rgba(207, 75, 124, 0.10), transparent 30%),
    linear-gradient(180deg, #ffffff 0%, #fbf6f8 100%);
  color: var(--ink);
}

.app-shell {
  max-width: 1380px;
  margin: 0 auto;
  padding: 22px 20px 42px;
}

.topbar {
  background: var(--paper);
  border: 1px solid var(--line);
  border-radius: 22px;
  box-shadow: 0 10px 28px rgba(36, 48, 63, 0.05);
  padding: 16px 22px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 20px;
  margin-bottom: 18px;
}

.topbar-links {
  display: flex;
  align-items: center;
  gap: 26px;
  color: #35506f;
  font-size: 0.98rem;
  font-weight: 600;
}

.wordmark-wrap {
  display: flex;
  align-items: center;
  gap: 12px;
}

.ca-mark-small {
  width: 48px;
  height: 48px;
  border-radius: 50%;
  background: linear-gradient(135deg, var(--rose-dark), var(--rose));
  color: #ffffff;
  display: grid;
  place-items: center;
  font-weight: 900;
  font-size: 1.08rem;
  letter-spacing: -0.08rem;
  box-shadow: 0 10px 22px rgba(207, 75, 124, 0.20);
  transform: skewX(-8deg);
}

.wordmark-text {
  font-size: 2rem;
  line-height: 0.95;
  font-weight: 900;
  letter-spacing: -0.06rem;
  color: var(--rose-dark);
  text-transform: uppercase;
  transform: skewX(-10deg);
}

.brand-submark {
  font-size: 0.78rem;
  font-weight: 700;
  color: var(--muted);
  letter-spacing: 0.16rem;
  text-transform: uppercase;
}

.hero {
  background: linear-gradient(90deg, #e7a9c0 0%, #f1c0d2 48%, #f6f8fb 48%, #f6f8fb 100%);
  border-radius: 32px;
  overflow: hidden;
  box-shadow: 0 20px 52px rgba(36, 48, 63, 0.08);
  margin-bottom: 20px;
  min-height: 470px;
  display: grid;
  grid-template-columns: 1.15fr 0.85fr;
}

.hero-left {
  padding: 74px 62px;
  color: #ffffff;
  position: relative;
}

.hero-left::after {
  content: "";
  position: absolute;
  right: -62px;
  top: 0;
  bottom: 0;
  width: 124px;
  background: #f6f8fb;
  border-top-left-radius: 500px;
  border-bottom-left-radius: 500px;
}

.hero-kicker {
  font-size: 0.90rem;
  letter-spacing: 0.18rem;
  text-transform: uppercase;
  font-weight: 800;
  opacity: 0.95;
  margin-bottom: 18px;
}

.hero-title {
  font-size: clamp(2.8rem, 5vw, 5.1rem);
  line-height: 0.93;
  font-weight: 900;
  letter-spacing: -0.09rem;
  color: #ffffff;
  margin-bottom: 22px;
  text-transform: uppercase;
}

.hero-subtitle {
  max-width: 560px;
  font-size: 1.18rem;
  line-height: 1.65;
  color: rgba(255,255,255,0.96);
  margin-bottom: 28px;
}

.hero-cta {
  display: inline-flex;
  align-items: center;
  gap: 12px;
  background: #ffffff;
  color: #35506f;
  border-radius: 999px;
  padding: 16px 26px;
  font-weight: 700;
  font-size: 1rem;
  box-shadow: 0 10px 26px rgba(36, 48, 63, 0.10);
}

.hero-right {
  position: relative;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 32px;
}

.analytics-orb {
  width: min(88%, 520px);
  aspect-ratio: 1 / 1;
  border-radius: 50%;
  background:
    radial-gradient(circle at 35% 22%, rgba(255,255,255,0.95), transparent 14%),
    linear-gradient(180deg, #edf5fb 0%, #dfeaf4 100%);
  border: 7px solid rgba(255,255,255,0.94);
  box-shadow:
    inset 0 0 0 2px rgba(109, 162, 212, 0.10),
    0 18px 48px rgba(36, 48, 63, 0.08);
  position: relative;
  overflow: hidden;
}

.analytics-orb::before {
  content: "";
  position: absolute;
  width: 120%;
  height: 34%;
  background: linear-gradient(180deg, #b8d5ed 0%, #8fb8dd 100%);
  bottom: -2%;
  left: -10%;
  border-top-left-radius: 50% 100%;
  border-top-right-radius: 50% 100%;
}

.analytics-orb::after {
  content: "";
  position: absolute;
  width: 116%;
  height: 10%;
  background: #ffffff;
  bottom: 22%;
  left: -8%;
  border-radius: 999px;
  box-shadow: 0 0 0 4px rgba(72, 112, 148, 0.13);
}

.ca-monogram {
  position: absolute;
  inset: 0;
  display: grid;
  place-items: center;
  z-index: 3;
  font-size: clamp(4rem, 8vw, 7.8rem);
  font-weight: 950;
  letter-spacing: -0.75rem;
  color: var(--rose);
  transform: skewX(-10deg);
  text-shadow:
    0 8px 18px rgba(207, 75, 124, 0.10),
    0 0 26px rgba(207, 75, 124, 0.10);
}

.analytics-bars {
  position: absolute;
  top: 24%;
  left: 50%;
  transform: translateX(-50%);
  z-index: 4;
  display: flex;
  align-items: end;
  gap: 7px;
}

.analytics-bars div {
  width: 14px;
  background: var(--rose);
  border-radius: 4px 4px 0 0;
  opacity: 0.9;
}

.analytics-bars div:nth-child(1) { height: 24px; opacity: 0.55; }
.analytics-bars div:nth-child(2) { height: 38px; opacity: 0.70; }
.analytics-bars div:nth-child(3) { height: 56px; opacity: 0.85; }
.analytics-bars div:nth-child(4) { height: 74px; opacity: 1; }

.icon-row {
  display: grid;
  grid-template-columns: repeat(6, minmax(0, 1fr));
  gap: 18px;
  margin-bottom: 22px;
}

.icon-pill {
  background: var(--paper);
  border: 1px solid var(--line);
  border-radius: 999px;
  height: 76px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  gap: 6px;
  box-shadow: 0 10px 26px rgba(36, 48, 63, 0.04);
}

.icon-dot {
  width: 18px;
  height: 18px;
  border-radius: 50%;
  background: linear-gradient(180deg, #88b8e1 0%, #6da2d4 100%);
}

.icon-label {
  font-size: 0.74rem;
  font-weight: 700;
  color: #54708a;
  letter-spacing: 0.04rem;
  text-transform: uppercase;
}

.nav-pills .nav-link {
  border-radius: 16px;
  padding: 14px 16px;
  margin-bottom: 8px;
  font-weight: 700;
  color: #607284;
  border: 1px solid transparent;
}

.nav-pills .nav-link:hover {
  background: #fff4f8;
  color: var(--rose-dark);
}

.nav-pills .nav-link.active,
.nav-pills .show > .nav-link {
  background: var(--paper);
  color: var(--rose-dark);
  border: 1px solid var(--line);
  box-shadow: 0 12px 28px rgba(36, 48, 63, 0.05);
}

.panel-card {
  background: var(--paper);
  border: 1px solid var(--line);
  border-radius: 26px;
  padding: 28px;
  box-shadow: 0 18px 44px rgba(36, 48, 63, 0.05);
  margin-bottom: 20px;
}

.panel-title {
  color: var(--rose-dark);
  font-weight: 850;
  letter-spacing: -0.03rem;
  margin-bottom: 12px;
}

.microcopy {
  color: var(--muted);
  line-height: 1.65;
}

.status-box {
  background: var(--rose-wash);
  border: 1px solid var(--line);
  border-left: 5px solid var(--rose);
  border-radius: 18px;
  padding: 16px 18px;
  color: #4b5563;
}

.metric-card {
  background: var(--paper);
  border: 1px solid var(--line);
  border-radius: 24px;
  padding: 24px;
  box-shadow: 0 16px 36px rgba(36, 48, 63, 0.04);
  margin-bottom: 18px;
}

.metric-value {
  color: var(--rose-dark);
  font-size: 2.1rem;
  font-weight: 900;
}

.metric-label {
  color: var(--muted);
  font-size: 0.86rem;
  font-weight: 750;
  letter-spacing: 0.06rem;
  text-transform: uppercase;
}

.btn, .btn-default {
  border-radius: 14px !important;
  font-weight: 800 !important;
  padding: 10px 16px !important;
}

.btn-primary, .btn-default {
  background: linear-gradient(90deg, var(--rose-dark) 0%, var(--rose) 100%) !important;
  border: none !important;
  color: white !important;
}

.form-control, .form-select, .selectize-input {
  border-radius: 14px !important;
  border: 1px solid var(--line) !important;
  min-height: 44px;
}

table.dataTable thead th {
  background-color: #fff5f9 !important;
  color: var(--rose-dark) !important;
  border-bottom: 1px solid var(--line) !important;
}

pre {
  background: #fff9fb !important;
  border: 1px solid var(--line) !important;
  border-radius: 18px !important;
  padding: 18px !important;
  color: #2c3440 !important;
  white-space: pre-wrap;
}

.footer-note {
  text-align: center;
  color: #7c8695;
  font-size: 0.86rem;
  margin-top: 22px;
}

@media (max-width: 1100px) {
  .hero { grid-template-columns: 1fr; }
  .hero-left::after { display: none; }
  .icon-row { grid-template-columns: repeat(3, minmax(0, 1fr)); }
}

@media (max-width: 700px) {
  .topbar { flex-direction: column; align-items: flex-start; }
  .topbar-links { flex-wrap: wrap; gap: 14px; }
  .hero-left { padding: 40px 28px; }
  .hero-title { font-size: 2.35rem; }
  .icon-row { grid-template-columns: repeat(2, minmax(0, 1fr)); }
}
'

# ============================================================
# Helper functions and data definitions
# ============================================================

clean_names_simple <- function(x) {
  x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("(^_+|_+$)", "")
}

safe_sum <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

safe_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_min_date <- function(x) {
  x <- suppressWarnings(as.Date(x))
  if (all(is.na(x))) return(as.Date(NA))
  min(x, na.rm = TRUE)
}

safe_max_date <- function(x) {
  x <- suppressWarnings(as.Date(x))
  if (all(is.na(x))) return(as.Date(NA))
  max(x, na.rm = TRUE)
}

first_non_missing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

normalize_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  case_when(
    x %in% c("yes", "y", "true", "1", "represented", "open") ~ 1,
    x %in% c("no", "n", "false", "0", "unrepresented", "closed") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x))
  )
}

safe_quantile <- function(x, p) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}

safe_percent <- function(x, digits = 0) {
  ifelse(is.na(x), "--", paste0(round(100 * x, digits), "%"))
}

# Flexible aliases let the app recognize common names from claims,
# billing, reserve, and historical litigation datasets.
field_aliases <- list(
  claim_id = c("claim_id", "claim_number", "claim_no", "file_number", "claimant_id", "record_id"),
  bill_id = c("bill_id", "invoice_id", "line_id", "medical_bill_id"),
  provider_id = c("provider_id", "npi", "provider_npi", "billing_provider_id"),
  service_code = c("service_code", "billing_code", "hcpcs_code", "cpt_code"),
  service_category = c("service_category", "service_type", "place_of_service", "procedure_category"),
  jurisdiction = c("jurisdiction", "state", "claim_state", "venue"),
  claim_status = c("claim_status", "status", "open_closed_status"),
  injury_type = c("injury_type", "nature_of_injury", "injury_description", "diagnosis_description"),
  body_part = c("body_part", "part_of_body", "injured_body_part"),
  date_of_injury = c("date_of_injury", "injury_date", "loss_date", "accident_date"),
  date_of_service = c("date_of_service", "service_date", "bill_date"),
  evaluation_date = c("evaluation_date", "as_of_date", "snapshot_date", "report_date"),
  closed_date = c("closed_date", "claim_closed_date", "closure_date"),
  charged_amount = c("charged_amount", "submitted_charge", "submitted_charge_amt", "average_submitted_chrg_amt", "avg_submitted_charge", "billed_amount"),
  paid_amount = c("paid_amount", "paid_amt", "average_medicare_payment_amt", "average_medicare_allowed_amt", "avg_paid_amount", "allowed_amount"),
  units = c("units", "unit_count", "line_srvc_cnt", "service_count"),
  total_incurred = c("total_incurred", "incurred", "total_incurred_amount", "gross_incurred"),
  paid_medical = c("paid_medical", "medical_paid", "medical_paid_amount", "total_medical_paid"),
  paid_indemnity = c("paid_indemnity", "indemnity_paid", "indemnity_paid_amount", "total_indemnity_paid"),
  case_reserve = c("case_reserve", "reserve", "outstanding_reserve", "total_reserve", "case_reserves"),
  reserve_change_90d = c("reserve_change_90d", "reserve_change", "reserve_delta", "reserve_development"),
  demand_amount = c("demand_amount", "settlement_demand", "plaintiff_demand"),
  settlement_offer = c("settlement_offer", "offer_amount", "carrier_offer"),
  treatment_delay_days = c("treatment_delay_days", "days_to_first_treatment", "treatment_lag_days"),
  return_to_work_days = c("return_to_work_days", "rtw_days", "days_to_return_to_work", "lost_work_days"),
  prior_claims_count = c("prior_claims_count", "prior_claim_count", "prior_losses"),
  comorbidity_count = c("comorbidity_count", "comorbidities", "comorbidity_total"),
  lost_time_flag = c("lost_time_flag", "lost_time", "indemnity_flag"),
  denied_flag = c("denied_flag", "denial_flag", "claim_denied"),
  disputed_flag = c("disputed_flag", "dispute_flag", "claim_disputed"),
  causation_issue_flag = c("causation_issue_flag", "causation_flag", "causation_dispute"),
  compensability_issue_flag = c("compensability_issue_flag", "compensability_flag", "compensability_dispute"),
  reopened_claim_flag = c("reopened_claim_flag", "reopened_flag", "claim_reopened"),
  surgery_flag = c("surgery_flag", "surgical_flag", "surgery_indicator"),
  ime_flag = c("ime_flag", "independent_medical_exam_flag", "ime_indicator"),
  permanent_impairment_flag = c("permanent_impairment_flag", "impairment_flag", "mmi_impairment_flag"),
  duplicate_bill_flag = c("duplicate_bill_flag", "duplicate_flag", "duplicate_billing_flag"),
  provider_outlier_flag = c("provider_outlier_flag", "billing_outlier_flag", "provider_risk_flag"),
  documentation_gap_flag = c("documentation_gap_flag", "missing_documentation_flag", "documentation_issue_flag"),
  subrogation_flag = c("subrogation_flag", "third_party_flag", "recovery_flag"),
  represented_flag = c("represented_flag", "attorney_representation_flag", "claimant_represented_flag"),
  attorney_involvement_flag = c("attorney_involvement_flag", "attorney_flag", "counsel_involved_flag"),
  suit_filed_flag = c("suit_filed_flag", "litigation_flag", "lawsuit_flag", "petition_filed_flag"),
  litigation_outcome_flag = c("litigation_outcome_flag", "historical_litigation_flag", "eventual_litigation_flag", "target_litigation")
)

canonical_fields <- unique(c(
  names(field_aliases),
  "record_id", "number_of_bills", "number_of_providers", "number_of_service_codes"
))

flag_fields <- c(
  "lost_time_flag", "denied_flag", "disputed_flag", "causation_issue_flag",
  "compensability_issue_flag", "reopened_claim_flag", "surgery_flag", "ime_flag",
  "permanent_impairment_flag", "duplicate_bill_flag", "provider_outlier_flag",
  "documentation_gap_flag", "subrogation_flag", "represented_flag",
  "attorney_involvement_flag", "suit_filed_flag", "litigation_outcome_flag"
)

numeric_fields <- c(
  "charged_amount", "paid_amount", "units", "total_incurred", "paid_medical",
  "paid_indemnity", "case_reserve", "reserve_change_90d", "demand_amount",
  "settlement_offer", "treatment_delay_days", "return_to_work_days",
  "prior_claims_count", "comorbidity_count", "number_of_bills",
  "number_of_providers", "number_of_service_codes"
)

date_fields <- c("date_of_injury", "date_of_service", "evaluation_date", "closed_date")

minimum_signal_fields <- c(
  "total_incurred", "case_reserve", "lost_time_flag", "denied_flag",
  "disputed_flag", "date_of_injury", "return_to_work_days", "surgery_flag",
  "charged_amount", "paid_amount", "duplicate_bill_flag"
)

standardize_claim_columns <- function(df) {
  names(df) <- clean_names_simple(names(df))

  for (target in names(field_aliases)) {
    matches <- field_aliases[[target]][field_aliases[[target]] %in% names(df)]
    if (length(matches) > 0 && !(target %in% names(df))) {
      names(df)[names(df) == matches[1]] <- target
    }
  }

  if (!("claim_id" %in% names(df))) {
    df$claim_id <- paste0("record_", seq_len(nrow(df)))
  }

  if (!("record_id" %in% names(df))) {
    df$record_id <- as.character(df$claim_id)
  }

  df
}

prepare_claim_data <- function(df) {
  df <- standardize_claim_columns(df)

  for (field in setdiff(canonical_fields, names(df))) {
    df[[field]] <- NA
  }

  df <- df %>%
    mutate(across(all_of(flag_fields), normalize_flag)) %>%
    mutate(across(all_of(numeric_fields), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(all_of(date_fields), ~ suppressWarnings(as.Date(.x)))) %>%
    mutate(
      claim_id = as.character(claim_id),
      record_id = as.character(record_id),
      evaluation_date = coalesce(evaluation_date, Sys.Date())
    )

  df
}

aggregate_to_claims <- function(df) {
  df <- prepare_claim_data(df)
  groups <- split(df, df$claim_id, drop = TRUE)

  get_num_max <- function(g, field) safe_max(g[[field]])
  get_flag <- function(g, field) safe_max(g[[field]])
  get_text <- function(g, field) first_non_missing(g[[field]])

  rows <- lapply(groups, function(g) {
    tibble(
      claim_id = as.character(g$claim_id[1]),
      record_id = as.character(g$claim_id[1]),
      jurisdiction = get_text(g, "jurisdiction"),
      claim_status = get_text(g, "claim_status"),
      injury_type = get_text(g, "injury_type"),
      body_part = get_text(g, "body_part"),
      date_of_injury = safe_min_date(g$date_of_injury),
      date_of_service = safe_max_date(g$date_of_service),
      evaluation_date = safe_max_date(g$evaluation_date),
      closed_date = safe_max_date(g$closed_date),
      charged_amount = safe_sum(g$charged_amount),
      paid_amount = safe_sum(g$paid_amount),
      units = safe_sum(g$units),
      total_incurred = get_num_max(g, "total_incurred"),
      paid_medical = get_num_max(g, "paid_medical"),
      paid_indemnity = get_num_max(g, "paid_indemnity"),
      case_reserve = get_num_max(g, "case_reserve"),
      reserve_change_90d = get_num_max(g, "reserve_change_90d"),
      demand_amount = get_num_max(g, "demand_amount"),
      settlement_offer = get_num_max(g, "settlement_offer"),
      treatment_delay_days = get_num_max(g, "treatment_delay_days"),
      return_to_work_days = get_num_max(g, "return_to_work_days"),
      prior_claims_count = get_num_max(g, "prior_claims_count"),
      comorbidity_count = get_num_max(g, "comorbidity_count"),
      number_of_bills = nrow(g),
      number_of_providers = if (all(is.na(g$provider_id))) NA_real_ else n_distinct(g$provider_id[!is.na(g$provider_id)]),
      number_of_service_codes = if (all(is.na(g$service_code))) NA_real_ else n_distinct(g$service_code[!is.na(g$service_code)]),
      lost_time_flag = get_flag(g, "lost_time_flag"),
      denied_flag = get_flag(g, "denied_flag"),
      disputed_flag = get_flag(g, "disputed_flag"),
      causation_issue_flag = get_flag(g, "causation_issue_flag"),
      compensability_issue_flag = get_flag(g, "compensability_issue_flag"),
      reopened_claim_flag = get_flag(g, "reopened_claim_flag"),
      surgery_flag = get_flag(g, "surgery_flag"),
      ime_flag = get_flag(g, "ime_flag"),
      permanent_impairment_flag = get_flag(g, "permanent_impairment_flag"),
      duplicate_bill_flag = get_flag(g, "duplicate_bill_flag"),
      provider_outlier_flag = get_flag(g, "provider_outlier_flag"),
      documentation_gap_flag = get_flag(g, "documentation_gap_flag"),
      subrogation_flag = get_flag(g, "subrogation_flag"),
      represented_flag = get_flag(g, "represented_flag"),
      attorney_involvement_flag = get_flag(g, "attorney_involvement_flag"),
      suit_filed_flag = get_flag(g, "suit_filed_flag"),
      litigation_outcome_flag = get_flag(g, "litigation_outcome_flag")
    )
  })

  bind_rows(rows)
}

make_demo_claims <- function() {
  tibble(
    claim_id = sprintf("WC-%04d", 1:24),
    jurisdiction = rep(c("TX", "IL", "PA", "CA"), 6),
    claim_status = rep(c("Open", "Open", "Closed"), 8),
    injury_type = c(
      "Lumbar strain", "Rotator cuff tear", "Hand fracture", "Knee sprain",
      "Back surgery", "Shoulder strain", "Crush injury", "Ankle sprain",
      "Cervical strain", "Torn meniscus", "Wrist fracture", "Soft tissue",
      "Disc herniation", "Contusion", "Severe burn", "Hip strain",
      "Shoulder surgery", "Back strain", "Fractured ankle", "Knee strain",
      "Repetitive trauma", "Head injury", "Arm strain", "Torn tendon"
    ),
    body_part = c("Back", "Shoulder", "Hand", "Knee", "Back", "Shoulder", "Hand", "Ankle", "Neck", "Knee", "Wrist", "Multiple", "Back", "Leg", "Multiple", "Hip", "Shoulder", "Back", "Ankle", "Knee", "Upper extremity", "Head", "Arm", "Hand"),
    date_of_injury = Sys.Date() - c(40, 280, 460, 65, 720, 120, 390, 33, 190, 510, 255, 80, 620, 25, 900, 140, 680, 75, 365, 110, 430, 205, 55, 300),
    total_incurred = c(8500, 64000, 118000, 12000, 215000, 25000, 180000, 9000, 34000, 98000, 71000, 15000, 162000, 6800, 290000, 23000, 198000, 17500, 105000, 26000, 87000, 76000, 11000, 92000),
    paid_medical = c(4200, 31000, 65000, 5900, 104000, 12000, 94000, 4100, 16000, 48000, 35000, 7200, 76000, 3300, 144000, 11000, 92000, 8000, 51000, 12000, 42000, 36000, 5200, 44000),
    paid_indemnity = c(0, 14000, 25000, 0, 52000, 5000, 43000, 0, 9000, 21000, 16000, 0, 38000, 0, 78000, 4200, 49000, 0, 27000, 7000, 22000, 19000, 0, 23000),
    case_reserve = c(4300, 19000, 28000, 6100, 59000, 8000, 43000, 4900, 9000, 29000, 20000, 7800, 48000, 3500, 68000, 7800, 57000, 9500, 27000, 7000, 23000, 21000, 5800, 25000),
    reserve_change_90d = c(0, 9000, 17000, 1200, 34000, 3000, 28000, 0, 4500, 16000, 8000, 900, 22000, 0, 41000, 2500, 36000, 1100, 14500, 4000, 12000, 9500, 0, 13000),
    charged_amount = c(9500, 76000, 139000, 14500, 260000, 31000, 225000, 10000, 39000, 120000, 88000, 17000, 198000, 7200, 350000, 27000, 242000, 21000, 129000, 33000, 106000, 91000, 13000, 111000),
    paid_amount = c(4200, 31000, 65000, 5900, 104000, 12000, 94000, 4100, 16000, 48000, 35000, 7200, 76000, 3300, 144000, 11000, 92000, 8000, 51000, 12000, 42000, 36000, 5200, 44000),
    units = c(8, 34, 49, 12, 71, 21, 64, 9, 24, 46, 38, 13, 58, 7, 82, 18, 69, 14, 44, 20, 37, 31, 10, 41),
    number_of_bills = c(5, 18, 27, 7, 35, 12, 31, 4, 15, 24, 21, 8, 29, 4, 42, 11, 34, 9, 23, 13, 19, 17, 6, 22),
    number_of_providers = c(2, 6, 9, 2, 11, 4, 10, 2, 5, 8, 7, 3, 9, 2, 13, 4, 10, 3, 8, 4, 7, 6, 2, 7),
    lost_time_flag = c(0,1,1,0,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,0,1),
    denied_flag = c(0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1),
    disputed_flag = c(0,1,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1,0,1,1,0,1),
    causation_issue_flag = c(0,1,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,1,0,1,1,0,1),
    compensability_issue_flag = c(0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1),
    reopened_claim_flag = c(0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0,1),
    surgery_flag = c(0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,0,0,0,0,1),
    ime_flag = c(0,1,1,0,1,0,1,0,1,1,1,0,1,0,1,0,1,0,1,0,1,1,0,1),
    permanent_impairment_flag = c(0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0,1),
    duplicate_bill_flag = c(0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1),
    provider_outlier_flag = c(0,1,1,0,1,0,1,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,0,1),
    documentation_gap_flag = c(0,1,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,1,0,1,1,0,1),
    subrogation_flag = c(0,0,1,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0),
    treatment_delay_days = c(3,18,42,4,35,10,51,2,24,39,17,6,44,2,58,8,31,5,37,9,27,33,4,29),
    return_to_work_days = c(0,86,180,12,240,45,210,8,72,160,110,18,200,5,300,40,225,25,145,60,130,95,14,120),
    prior_claims_count = c(0,1,2,0,1,0,3,0,1,1,0,0,2,0,3,0,1,0,2,0,2,1,0,1),
    comorbidity_count = c(0,1,1,0,2,0,1,0,1,1,0,0,2,0,2,1,1,0,1,0,2,1,0,1),
    represented_flag = c(0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0,1),
    attorney_involvement_flag = c(0,0,1,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0,1),
    suit_filed_flag = c(0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,1,0,1,0,1,0,0,1),
    litigation_outcome_flag = c(0,1,1,0,1,0,1,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,0,1)
  ) %>%
    mutate(
      evaluation_date = Sys.Date(),
      bill_id = NA_character_,
      provider_id = NA_character_,
      service_code = NA_character_,
      service_category = NA_character_,
      closed_date = as.Date(NA),
      demand_amount = NA_real_,
      settlement_offer = NA_real_
    )
}

prepare_public_cpi <- function(df) {
  names(df) <- clean_names_simple(names(df))

  date_col <- names(df)[names(df) %in% c("observation_date", "date")][1]
  value_col <- names(df)[names(df) %in% c("cpimedsl", "value", "medical_index")][1]

  if (is.na(value_col)) {
    numeric_candidates <- names(df)[sapply(df, is.numeric)]
    value_col <- numeric_candidates[1]
  }

  if (is.na(date_col) || is.na(value_col)) return(NULL)

  df %>%
    transmute(
      date = as.Date(.data[[date_col]]),
      medical_index = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    filter(!is.na(date), !is.na(medical_index)) %>%
    arrange(date) %>%
    mutate(
      yoy_change = 100 * (medical_index / lag(medical_index, 12) - 1),
      month_change = 100 * (medical_index / lag(medical_index, 1) - 1)
    )
}

load_default_public_data <- function() {
  candidates <- c("data/fred_medical_cpi_reference.csv", "fred_medical_cpi_reference.csv")
  csv_file <- candidates[file.exists(candidates)][1]
  if (length(csv_file) == 0 || is.na(csv_file)) return(NULL)
  prepare_public_cpi(read_csv(csv_file, show_col_types = FALSE))
}

score_litigation_referral <- function(df) {
  df <- aggregate_to_claims(df)

  incurred_p75 <- safe_quantile(df$total_incurred, 0.75)
  incurred_p90 <- safe_quantile(df$total_incurred, 0.90)
  reserve_p75 <- safe_quantile(df$case_reserve, 0.75)
  reserve_p90 <- safe_quantile(df$case_reserve, 0.90)
  charge_p75 <- safe_quantile(df$charged_amount, 0.75)
  charge_p90 <- safe_quantile(df$charged_amount, 0.90)
  bills_p90 <- safe_quantile(df$number_of_bills, 0.90)
  providers_p90 <- safe_quantile(df$number_of_providers, 0.90)

  df %>%
    mutate(
      evaluation_date = coalesce(evaluation_date, Sys.Date()),
      claim_age_days = if_else(!is.na(date_of_injury), as.numeric(evaluation_date - date_of_injury), NA_real_),
      total_paid = coalesce(paid_medical, 0) + coalesce(paid_indemnity, 0),
      total_paid = if_else(total_paid == 0 & !is.na(paid_amount), paid_amount, total_paid),
      incurred_paid_gap = if_else(!is.na(total_incurred), pmax(total_incurred - total_paid, 0), NA_real_),
      incurred_paid_gap_ratio = if_else(!is.na(total_incurred) & total_incurred > 0, incurred_paid_gap / total_incurred, NA_real_),
      reserve_change_ratio = if_else(!is.na(reserve_change_90d) & !is.na(case_reserve) & case_reserve > 0, abs(reserve_change_90d) / case_reserve, NA_real_),
      charged_paid_ratio = if_else(!is.na(charged_amount) & !is.na(paid_amount) & paid_amount > 0, charged_amount / paid_amount, NA_real_),
      severe_injury_text_flag = if_else(
        str_detect(
          str_to_lower(paste(coalesce(injury_type, ""), coalesce(body_part, ""))),
          "fracture|torn|tear|crush|surgery|severe|burn|head injury|disc|herniation|amputation"
        ), 1, 0
      ),

      dispute_available = rowSums(!is.na(cbind(denied_flag, disputed_flag, causation_issue_flag, compensability_issue_flag))) > 0,
      severity_available = rowSums(!is.na(cbind(lost_time_flag, surgery_flag, permanent_impairment_flag))) > 0 | !is.na(injury_type),
      duration_available = !is.na(claim_age_days) | !is.na(return_to_work_days) | !is.na(treatment_delay_days),
      reserve_available = rowSums(!is.na(cbind(total_incurred, case_reserve, reserve_change_90d))) > 0,
      billing_available = rowSums(!is.na(cbind(charged_amount, paid_amount, duplicate_bill_flag, provider_outlier_flag, documentation_gap_flag, number_of_bills, number_of_providers))) > 0,
      procedural_available = rowSums(!is.na(cbind(reopened_claim_flag, ime_flag, prior_claims_count, comorbidity_count, subrogation_flag))) > 0,

      dispute_score = pmin(
        25,
        coalesce(denied_flag, 0) * 10 +
          coalesce(disputed_flag, 0) * 8 +
          coalesce(compensability_issue_flag, 0) * 5 +
          coalesce(causation_issue_flag, 0) * 5
      ),

      severity_score = pmin(
        20,
        coalesce(surgery_flag, 0) * 8 +
          coalesce(lost_time_flag, 0) * 5 +
          coalesce(permanent_impairment_flag, 0) * 5 +
          severe_injury_text_flag * 5
      ),

      duration_score = pmin(
        15,
        case_when(
          is.na(claim_age_days) ~ 0,
          claim_age_days >= 365 ~ 8,
          claim_age_days >= 180 ~ 5,
          claim_age_days >= 90 ~ 3,
          TRUE ~ 0
        ) +
          case_when(
            is.na(return_to_work_days) ~ 0,
            return_to_work_days >= 120 ~ 5,
            return_to_work_days >= 60 ~ 4,
            return_to_work_days >= 30 ~ 2,
            TRUE ~ 0
          ) +
          case_when(
            is.na(treatment_delay_days) ~ 0,
            treatment_delay_days >= 30 ~ 2,
            treatment_delay_days >= 14 ~ 1,
            TRUE ~ 0
          )
      ),

      reserve_score = pmin(
        20,
        case_when(
          is.na(total_incurred) | is.na(incurred_p75) ~ 0,
          total_incurred >= incurred_p90 ~ 8,
          total_incurred >= incurred_p75 ~ 5,
          TRUE ~ 0
        ) +
          case_when(
            is.na(case_reserve) | is.na(reserve_p75) ~ 0,
            case_reserve >= reserve_p90 ~ 5,
            case_reserve >= reserve_p75 ~ 3,
            TRUE ~ 0
          ) +
          case_when(
            is.na(reserve_change_ratio) ~ 0,
            reserve_change_ratio >= 0.50 ~ 5,
            reserve_change_ratio >= 0.25 ~ 3,
            TRUE ~ 0
          ) +
          case_when(
            is.na(incurred_paid_gap_ratio) ~ 0,
            incurred_paid_gap_ratio >= 0.50 ~ 4,
            incurred_paid_gap_ratio >= 0.30 ~ 2,
            TRUE ~ 0
          )
      ),

      billing_score = pmin(
        10,
        coalesce(duplicate_bill_flag, 0) * 4 +
          coalesce(provider_outlier_flag, 0) * 3 +
          coalesce(documentation_gap_flag, 0) * 2 +
          case_when(
            is.na(charged_amount) | is.na(charge_p75) ~ 0,
            charged_amount >= charge_p90 ~ 3,
            charged_amount >= charge_p75 ~ 2,
            TRUE ~ 0
          ) +
          case_when(
            !is.na(number_of_bills) & !is.na(bills_p90) & number_of_bills >= bills_p90 ~ 1,
            TRUE ~ 0
          ) +
          case_when(
            !is.na(number_of_providers) & !is.na(providers_p90) & number_of_providers >= providers_p90 ~ 1,
            TRUE ~ 0
          )
      ),

      procedural_score = pmin(
        10,
        coalesce(reopened_claim_flag, 0) * 4 +
          coalesce(ime_flag, 0) * 3 +
          case_when(
            is.na(prior_claims_count) ~ 0,
            prior_claims_count >= 2 ~ 2,
            prior_claims_count >= 1 ~ 1,
            TRUE ~ 0
          ) +
          case_when(
            is.na(comorbidity_count) ~ 0,
            comorbidity_count >= 2 ~ 2,
            comorbidity_count >= 1 ~ 1,
            TRUE ~ 0
          ) +
          coalesce(subrogation_flag, 0)
      ),

      available_max =
        if_else(dispute_available, 25, 0) +
        if_else(severity_available, 20, 0) +
        if_else(duration_available, 15, 0) +
        if_else(reserve_available, 20, 0) +
        if_else(billing_available, 10, 0) +
        if_else(procedural_available, 10, 0),

      available_domains =
        as.integer(dispute_available) + as.integer(severity_available) +
        as.integer(duration_available) + as.integer(reserve_available) +
        as.integer(billing_available) + as.integer(procedural_available),

      raw_score =
        if_else(dispute_available, dispute_score, 0) +
        if_else(severity_available, severity_score, 0) +
        if_else(duration_available, duration_score, 0) +
        if_else(reserve_available, reserve_score, 0) +
        if_else(billing_available, billing_score, 0) +
        if_else(procedural_available, procedural_score, 0),

      referral_score = if_else(available_max > 0, round(100 * raw_score / available_max, 0), NA_real_),
      data_coverage_pct = round(100 * available_max / 100, 0),
      existing_legal_flag = if_else(
        coalesce(represented_flag, 0) == 1 |
          coalesce(attorney_involvement_flag, 0) == 1 |
          coalesce(suit_filed_flag, 0) == 1,
        1, 0
      ),

      referral_tier = case_when(
        existing_legal_flag == 1 ~ "Litigation management",
        available_domains < 2 | data_coverage_pct < 30 ~ "Insufficient data",
        referral_score >= 70 ~ "Immediate counsel review",
        referral_score >= 50 ~ "Early litigation referral",
        referral_score >= 30 ~ "Watchlist",
        TRUE ~ "Routine handling"
      ),

      review_priority = case_when(
        referral_tier == "Litigation management" ~ "Active legal coordination",
        referral_tier == "Immediate counsel review" ~ "Immediate",
        referral_tier == "Early litigation referral" ~ "Priority",
        referral_tier == "Watchlist" ~ "Enhanced monitoring",
        referral_tier == "Routine handling" ~ "Routine",
        TRUE ~ "Data completion needed"
      )
    ) %>%
    rowwise() %>%
    mutate(
      top_referral_drivers = {
        scores <- c(
          "Dispute and compensability" = dispute_score,
          "Medical severity" = severity_score,
          "Claim duration" = duration_score,
          "Reserve pressure" = reserve_score,
          "Billing and documentation" = billing_score,
          "Procedural complexity" = procedural_score
        )
        availability <- c(dispute_available, severity_available, duration_available, reserve_available, billing_available, procedural_available)
        scores <- scores[availability]
        scores <- sort(scores, decreasing = TRUE)
        scores <- scores[scores > 0]
        if (length(scores) == 0) "No material referral indicators identified" else paste(names(scores)[seq_len(min(3, length(scores)))], collapse = "; ")
      },
      recommended_action = case_when(
        existing_legal_flag == 1 ~ "Coordinate litigation strategy, diary deadlines, confirm counsel assignment, and review reserves and authority.",
        referral_tier == "Immediate counsel review" ~ "Escalate for supervisor and claims-counsel review. Confirm compensability issues, reserve adequacy, litigation venue, and evidence preservation.",
        referral_tier == "Early litigation referral" ~ "Complete an early referral review. Strengthen documentation, evaluate dispute posture, and reassess reserves before legal activity increases.",
        referral_tier == "Watchlist" ~ "Place on a litigation watchlist. Track return-to-work progress, treatment development, disputes, and reserve movement.",
        referral_tier == "Routine handling" ~ "Continue routine claim handling and monitor for new disputes, prolonged disability, reserve changes, or representation.",
        TRUE ~ "Add more claim, reserve, dispute, duration, or billing fields before using the triage result."
      )
    ) %>%
    ungroup() %>%
    arrange(desc(existing_legal_flag), desc(referral_score))
}

make_role_output <- function(row, role_name) {
  claim_name <- row[["claim_id"]]
  tier <- row[["referral_tier"]]
  score <- row[["referral_score"]]
  drivers <- row[["top_referral_drivers"]]
  action <- row[["recommended_action"]]

  if (role_name == "Adjuster") {
    paste0(claim_name, ": ", tier, " (score ", score, "). Drivers: ", drivers, ". Next step: ", action)
  } else if (role_name == "Litigation Specialist") {
    paste0(claim_name, ": Review dispute posture, evidence, venue, representation status, reserve adequacy, and settlement strategy. Primary indicators: ", drivers, ".")
  } else if (role_name == "Supervisor") {
    paste0(claim_name, ": ", row[["review_priority"]], ". Validate the score against file facts and decide whether counsel review, watchlist placement, or routine handling is appropriate.")
  } else {
    paste0(claim_name, ": Research triage only. Evaluate legal issues independently. Indicators: ", drivers, ". ", action)
  }
}

referral_matrix <- tribble(
  ~risk_domain, ~example_trigger, ~claims_response, ~primary_owner,
  "Dispute and compensability", "Denial, compensability dispute, or causation issue", "Clarify legal position, preserve evidence, and consider early counsel review", "Adjuster / Claims Counsel",
  "Medical severity", "Surgery, permanent impairment, severe injury, or extended lost time", "Review medical strategy, disability exposure, and settlement posture", "Adjuster / Nurse / Litigation",
  "Claim duration", "Long claim age, delayed treatment, or delayed return to work", "Reassess barriers, diary milestones, and escalation strategy", "Adjuster / Supervisor",
  "Reserve pressure", "High incurred, reserve volatility, or large unpaid exposure", "Complete reserve review and confirm authority needs", "Supervisor / Litigation",
  "Billing and documentation", "Duplicate billing, provider outlier, or missing documentation", "Audit bills and strengthen the claim file before referral", "Bill Review / Adjuster",
  "Procedural complexity", "Reopened claim, IME activity, prior claims, or third-party issues", "Coordinate specialty resources and document strategy", "Supervisor / Litigation"
)

field_dictionary <- tribble(
  ~field_group, ~canonical_field, ~purpose,
  "Identifier", "claim_id", "Groups bill rows into one workers' compensation claim",
  "Financial", "total_incurred", "Measures total estimated claim exposure",
  "Financial", "case_reserve", "Measures outstanding reserve pressure",
  "Financial", "reserve_change_90d", "Captures recent reserve development or volatility",
  "Financial", "paid_medical / paid_indemnity", "Measures paid severity and unpaid exposure",
  "Dispute", "denied_flag / disputed_flag", "Captures denial or active dispute posture",
  "Dispute", "causation_issue_flag", "Captures causation disagreement",
  "Dispute", "compensability_issue_flag", "Captures compensability disagreement",
  "Severity", "lost_time_flag / surgery_flag", "Captures disability and medical severity",
  "Duration", "date_of_injury / return_to_work_days", "Measures claim duration and disability duration",
  "Billing", "charged_amount / paid_amount / units", "Measures billing intensity and payment relationship",
  "Billing", "duplicate_bill_flag / provider_outlier_flag", "Captures billing-review concerns",
  "Procedure", "reopened_claim_flag / ime_flag", "Captures procedural complexity",
  "Legal status", "represented_flag / suit_filed_flag", "Separates active litigation management from pre-litigation referral",
  "Validation", "litigation_outcome_flag", "Optional historical outcome used only to validate the research score"
)

build_validation_metrics <- function(df, threshold = 50) {
  valid <- df %>%
    filter(!is.na(litigation_outcome_flag), !is.na(referral_score), existing_legal_flag == 0) %>%
    mutate(
      actual = as.integer(litigation_outcome_flag == 1),
      predicted = as.integer(referral_score >= threshold)
    )

  if (nrow(valid) == 0) return(NULL)

  tp <- sum(valid$actual == 1 & valid$predicted == 1)
  tn <- sum(valid$actual == 0 & valid$predicted == 0)
  fp <- sum(valid$actual == 0 & valid$predicted == 1)
  fn <- sum(valid$actual == 1 & valid$predicted == 0)

  tibble(
    metric = c("Records", "Accuracy", "Precision", "Recall", "Specificity", "Actual litigation rate"),
    value = c(
      as.character(nrow(valid)),
      safe_percent((tp + tn) / nrow(valid), 1),
      safe_percent(ifelse(tp + fp == 0, NA, tp / (tp + fp)), 1),
      safe_percent(ifelse(tp + fn == 0, NA, tp / (tp + fn)), 1),
      safe_percent(ifelse(tn + fp == 0, NA, tn / (tn + fp)), 1),
      safe_percent(mean(valid$actual), 1)
    )
  )
}

# ============================================================
# User interface
# ============================================================

ui <- page_fluid(
  theme = app_theme,
  tags$head(tags$style(HTML(app_css))),

  div(
    class = "app-shell",

    div(
      class = "topbar",
      div(
        class = "topbar-links",
        span("Research"),
        span("Claim Upload"),
        span("Litigation Triage"),
        span("Validation"),
        span("Export")
      ),
      div(
        class = "wordmark-wrap",
        div(class = "ca-mark-small", "LC"),
        div(
          div(class = "wordmark-text", "Litigation Analytics"),
          div(class = "brand-submark", "Workers' Compensation Claims")
        )
      )
    ),

    div(
      class = "hero",
      div(
        class = "hero-left",
        div(class = "hero-kicker", "Explainable litigation-referral research"),
        div(class = "hero-title", "Litigation Claims Analytics"),
        div(class = "hero-subtitle", "A workers' compensation research application for analyzing medical cost pressure, billing behavior, claim outcomes, reserve development, and possible litigation-referral indicators."),
        div(class = "hero-cta", HTML("&#8250; Review referral indicators and claim-level explanations"))
      ),
      div(
        class = "hero-right",
        div(
          class = "analytics-orb",
          div(class = "analytics-bars", div(), div(), div(), div()),
          div(class = "ca-monogram", "LC")
        )
      )
    ),

    div(
      class = "icon-row",
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Disputes")),
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Severity")),
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Duration")),
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Reserves")),
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Billing")),
      div(class = "icon-pill", div(class = "icon-dot"), div(class = "icon-label", "Referral"))
    ),

    navset_pill_list(
      widths = c(3, 9),

      nav_panel(
        "Research Overview",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Workers' Compensation Litigation Claims Analytics"),
          p(class = "microcopy", "This application is an explainable research prototype. It organizes potential litigation-referral indicators into six domains: dispute posture, medical severity, claim duration, reserve pressure, billing and documentation, and procedural complexity."),
          div(class = "status-box", strong("Important limitation: "), "The score is a triage aid, not a legal conclusion, automated referral decision, or replacement for claim-file review."),
          br(),
          h4(class = "panel-title", "Research question"),
          p(class = "microcopy", "Can claim, medical, billing, reserve, and procedural indicators help identify workers' compensation files that may benefit from earlier litigation-specialist or claims-counsel review?"),
          h4(class = "panel-title", "How the app treats legal status"),
          p(class = "microcopy", "Claims already represented, assigned to counsel, or in suit are separated into Litigation management. The referral score is designed primarily for pre-litigation triage.")
        )
      ),

      nav_panel(
        "Medical Inflation",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Public medical inflation context"),
          p(class = "microcopy", "Upload a FRED CPIMEDSL CSV or place fred_medical_cpi_reference.csv in the project folder. Inflation is contextual evidence; it is not treated as proof that a claim should be litigated."),
          fileInput("public_cpi_file", "Optional FRED medical CPI CSV", accept = ".csv"),
          uiOutput("public_status"),
          br(),
          plotOutput("public_cpi_plot", height = "390px"),
          DTOutput("public_preview")
        )
      ),

      nav_panel(
        "Claim Upload",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Upload claim-level or bill-level data"),
          fileInput("claim_file", "Choose CSV", accept = ".csv"),
          fluidRow(
            column(6, downloadButton("download_template", "Download claim template")),
            column(6, actionButton("use_demo", "Use demonstration data"))
          ),
          br(),
          div(class = "status-box", "Bill-level rows are automatically grouped by claim_id before litigation-referral scoring."),
          br(),
          DTOutput("claim_preview")
        )
      ),

      nav_panel(
        "Data Quality",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Data readiness"),
          uiOutput("claim_quality_status"),
          br(),
          DTOutput("detected_fields")
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Canonical field dictionary"),
          DTOutput("field_dictionary_table")
        )
      ),

      nav_panel(
        "Litigation Dashboard",
        fluidRow(
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("claim_count")), div(class = "metric-label", "Claims"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("immediate_count")), div(class = "metric-label", "Immediate review"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("early_count")), div(class = "metric-label", "Early referral"))),
          column(3, div(class = "metric-card", div(class = "metric-value", textOutput("litigation_count")), div(class = "metric-label", "Litigation management")))
        ),
        div(
          class = "panel-card",
          h3(class = "panel-title", "Claim-level referral triage"),
          DTOutput("scored_table")
        )
      ),

      nav_panel(
        "Portfolio Trends",
        div(class = "panel-card", h3(class = "panel-title", "Referral-tier distribution"), plotOutput("tier_plot", height = "360px")),
        div(class = "panel-card", h3(class = "panel-title", "Average domain score"), plotOutput("driver_plot", height = "360px")),
        div(class = "panel-card", h3(class = "panel-title", "Referral score and incurred exposure"), plotOutput("incurred_plot", height = "390px"))
      ),

      nav_panel(
        "Claim Review",
        fluidRow(
          column(
            4,
            div(
              class = "panel-card",
              h3(class = "panel-title", "Select claim"),
              selectInput("selected_claim", "Claim", choices = NULL),
              selectInput("selected_role", "Perspective", choices = c("Adjuster", "Litigation Specialist", "Supervisor", "Claims Counsel"))
            )
          ),
          column(
            8,
            div(class = "panel-card", h3(class = "panel-title", "Referral explanation"), verbatimTextOutput("claim_explanation")),
            div(class = "panel-card", h3(class = "panel-title", "Role-based output"), verbatimTextOutput("role_output"))
          )
        )
      ),

      nav_panel(
        "Outcome Validation",
        fluidRow(
          column(4, div(class = "metric-card", div(class = "metric-value", textOutput("validation_records")), div(class = "metric-label", "Validation records"))),
          column(4, div(class = "metric-card", div(class = "metric-value", textOutput("validation_accuracy")), div(class = "metric-label", "Accuracy at 50"))),
          column(4, div(class = "metric-card", div(class = "metric-value", textOutput("validation_recall")), div(class = "metric-label", "Recall at 50")))
        ),
        div(class = "panel-card", uiOutput("validation_status"), br(), DTOutput("validation_metrics")),
        div(class = "panel-card", h3(class = "panel-title", "Observed litigation rate by score band"), plotOutput("validation_plot", height = "390px"))
      ),

      nav_panel(
        "Referral Matrix",
        div(class = "panel-card", h3(class = "panel-title", "Explainable referral framework"), DTOutput("referral_matrix_table"))
      ),

      nav_panel(
        "Export",
        div(
          class = "panel-card",
          h3(class = "panel-title", "Export research outputs"),
          downloadButton("download_scored", "Download scored claims"),
          tags$span(" "),
          downloadButton("download_report", "Download referral report"),
          br(), br(),
          verbatimTextOutput("report_preview")
        )
      )
    ),

    div(class = "footer-note", "Workers' Compensation Litigation Claims Analytics | Explainable Research Triage")
  )
)

# ============================================================
# Server logic
# ============================================================

server <- function(input, output, session) {

  demo_version <- reactiveVal(1)

  observeEvent(input$use_demo, {
    demo_version(demo_version() + 1)
    showNotification("Demonstration claims loaded.", type = "message")
  })

  raw_claim_data <- reactive({
    demo_version()

    if (!is.null(input$claim_file)) {
      raw <- read_csv(input$claim_file$datapath, show_col_types = FALSE)
      return(raw)
    }

    make_demo_claims()
  })

  prepared_rows <- reactive({
    prepare_claim_data(raw_claim_data())
  })

  claim_level_data <- reactive({
    aggregate_to_claims(raw_claim_data())
  })

  scored_claims <- reactive({
    score_litigation_referral(raw_claim_data())
  })

  public_data <- reactive({
    if (!is.null(input$public_cpi_file)) {
      return(prepare_public_cpi(read_csv(input$public_cpi_file$datapath, show_col_types = FALSE)))
    }
    load_default_public_data()
  })

  observe({
    scored <- scored_claims()
    updateSelectInput(session, "selected_claim", choices = scored$claim_id, selected = scored$claim_id[1])
  })

  output$claim_preview <- renderDT({
    prepared_rows() %>%
      select(any_of(c(
        "claim_id", "bill_id", "provider_id", "service_code", "jurisdiction",
        "date_of_injury", "total_incurred", "case_reserve", "charged_amount",
        "paid_amount", "lost_time_flag", "denied_flag", "disputed_flag",
        "represented_flag", "suit_filed_flag"
      ))) %>%
      head(30) %>%
      datatable(options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$claim_quality_status <- renderUI({
    source_label <- if (is.null(input$claim_file)) "Demonstration data" else input$claim_file$name
    original_names <- clean_names_simple(names(raw_claim_data()))
    detected_targets <- names(field_aliases)[vapply(field_aliases, function(x) any(x %in% original_names), logical(1))]
    signal_count <- length(intersect(minimum_signal_fields, detected_targets))

    div(
      class = "status-box",
      strong(source_label),
      tags$br(),
      paste("Source rows:", nrow(raw_claim_data())),
      tags$br(),
      paste("Claim-level records after grouping:", nrow(claim_level_data())),
      tags$br(),
      paste("Recognized canonical fields:", length(detected_targets)),
      tags$br(),
      paste("Recognized core research signals:", signal_count, "of", length(minimum_signal_fields))
    )
  })

  output$detected_fields <- renderDT({
    original_names <- clean_names_simple(names(raw_claim_data()))

    tibble(
      canonical_field = names(field_aliases),
      detected = vapply(field_aliases, function(x) any(x %in% original_names), logical(1)),
      matching_source_name = vapply(field_aliases, function(x) {
        hit <- x[x %in% original_names]
        if (length(hit) == 0) "" else hit[1]
      }, character(1))
    ) %>%
      arrange(desc(detected), canonical_field) %>%
      datatable(options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$field_dictionary_table <- renderDT({
    datatable(field_dictionary, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$public_status <- renderUI({
    data <- public_data()
    if (is.null(data) || nrow(data) == 0) {
      div(class = "status-box", "No medical CPI file is loaded. The litigation dashboard still works because CPI is contextual rather than a required referral signal.")
    } else {
      div(class = "status-box", paste("Medical CPI records loaded:", nrow(data)))
    }
  })

  output$public_cpi_plot <- renderPlot({
    data <- public_data()
    validate(need(!is.null(data) && nrow(data) > 0, "Upload a FRED medical CPI CSV to display the inflation trend."))

    ggplot(data, aes(date, medical_index)) +
      geom_line(color = "#cf4b7c", linewidth = 1.2) +
      geom_point(color = "#b33869", size = 2.2) +
      labs(x = NULL, y = "Medical Care CPI") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$public_preview <- renderDT({
    data <- public_data()
    validate(need(!is.null(data) && nrow(data) > 0, "No medical CPI file loaded."))

    data %>%
      arrange(desc(date)) %>%
      mutate(across(c(month_change, yoy_change), ~ round(.x, 2))) %>%
      datatable(options = list(pageLength = 8), rownames = FALSE)
  })

  output$claim_count <- renderText(nrow(scored_claims()))
  output$immediate_count <- renderText(sum(scored_claims()$referral_tier == "Immediate counsel review", na.rm = TRUE))
  output$early_count <- renderText(sum(scored_claims()$referral_tier == "Early litigation referral", na.rm = TRUE))
  output$litigation_count <- renderText(sum(scored_claims()$referral_tier == "Litigation management", na.rm = TRUE))

  output$scored_table <- renderDT({
    scored_claims() %>%
      select(any_of(c(
        "claim_id", "jurisdiction", "claim_status", "injury_type", "claim_age_days",
        "total_incurred", "case_reserve", "reserve_change_90d", "referral_score",
        "data_coverage_pct", "referral_tier", "review_priority",
        "top_referral_drivers", "recommended_action"
      ))) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
      formatCurrency(c("total_incurred", "case_reserve", "reserve_change_90d"), currency = "$", digits = 0)
  })

  output$tier_plot <- renderPlot({
    scored_claims() %>%
      count(referral_tier) %>%
      ggplot(aes(x = reorder(referral_tier, n), y = n)) +
      geom_col(fill = "#cf4b7c") +
      coord_flip() +
      labs(x = NULL, y = "Claims") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$driver_plot <- renderPlot({
    scored_claims() %>%
      summarise(
        `Dispute and compensability` = mean(dispute_score, na.rm = TRUE),
        `Medical severity` = mean(severity_score, na.rm = TRUE),
        `Claim duration` = mean(duration_score, na.rm = TRUE),
        `Reserve pressure` = mean(reserve_score, na.rm = TRUE),
        `Billing and documentation` = mean(billing_score, na.rm = TRUE),
        `Procedural complexity` = mean(procedural_score, na.rm = TRUE)
      ) %>%
      pivot_longer(everything(), names_to = "domain", values_to = "average_score") %>%
      ggplot(aes(x = reorder(domain, average_score), y = average_score)) +
      geom_col(fill = "#6da2d4") +
      coord_flip() +
      labs(x = NULL, y = "Average domain points") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$incurred_plot <- renderPlot({
    data <- scored_claims() %>% filter(!is.na(total_incurred), !is.na(referral_score))
    validate(need(nrow(data) > 0, "Total incurred and referral score are needed for this chart."))

    ggplot(data, aes(x = referral_score, y = total_incurred, shape = referral_tier)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_y_continuous(labels = dollar) +
      labs(x = "Referral score", y = "Total incurred", shape = "Referral tier") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  selected_row <- reactive({
    req(input$selected_claim)
    scored_claims() %>% filter(claim_id == input$selected_claim) %>% slice(1)
  })

  output$claim_explanation <- renderText({
    row <- selected_row()

    paste0(
      "Referral score: ", row$referral_score, "\n",
      "Referral tier: ", row$referral_tier, "\n",
      "Review priority: ", row$review_priority, "\n",
      "Data coverage: ", row$data_coverage_pct, "% across ", row$available_domains, " domains\n\n",
      "Domain scores\n",
      "- Dispute and compensability: ", row$dispute_score, " / 25\n",
      "- Medical severity: ", row$severity_score, " / 20\n",
      "- Claim duration: ", row$duration_score, " / 15\n",
      "- Reserve pressure: ", row$reserve_score, " / 20\n",
      "- Billing and documentation: ", row$billing_score, " / 10\n",
      "- Procedural complexity: ", row$procedural_score, " / 10\n\n",
      "Top indicators: ", row$top_referral_drivers, "\n\n",
      "Recommended action: ", row$recommended_action
    )
  })

  output$role_output <- renderText({
    make_role_output(selected_row(), input$selected_role)
  })

  validation_data <- reactive({
    scored_claims() %>%
      filter(!is.na(litigation_outcome_flag), !is.na(referral_score), existing_legal_flag == 0) %>%
      mutate(
        score_band = cut(
          referral_score,
          breaks = c(-Inf, 29, 49, 69, Inf),
          labels = c("0-29", "30-49", "50-69", "70-100")
        )
      )
  })

  validation_metrics_data <- reactive({
    build_validation_metrics(scored_claims(), threshold = 50)
  })

  output$validation_status <- renderUI({
    metrics <- validation_metrics_data()
    if (is.null(metrics)) {
      div(class = "status-box", "Add litigation_outcome_flag to a historical dataset to validate the score. Existing representation and suit-status fields are excluded from validation to reduce leakage.")
    } else {
      div(class = "status-box", "Historical validation is available. These metrics describe this uploaded sample only and are not evidence of external model validity.")
    }
  })

  output$validation_metrics <- renderDT({
    metrics <- validation_metrics_data()
    validate(need(!is.null(metrics), "No historical outcome field is available."))
    datatable(metrics, options = list(dom = "t"), rownames = FALSE)
  })

  output$validation_records <- renderText({
    metrics <- validation_metrics_data()
    if (is.null(metrics)) "0" else metrics$value[metrics$metric == "Records"]
  })

  output$validation_accuracy <- renderText({
    metrics <- validation_metrics_data()
    if (is.null(metrics)) "--" else metrics$value[metrics$metric == "Accuracy"]
  })

  output$validation_recall <- renderText({
    metrics <- validation_metrics_data()
    if (is.null(metrics)) "--" else metrics$value[metrics$metric == "Recall"]
  })

  output$validation_plot <- renderPlot({
    data <- validation_data()
    validate(need(nrow(data) > 0, "No historical litigation outcome field is available."))

    data %>%
      group_by(score_band) %>%
      summarise(
        claims = n(),
        observed_litigation_rate = mean(litigation_outcome_flag == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = score_band, y = observed_litigation_rate)) +
      geom_col(fill = "#cf4b7c") +
      geom_text(aes(label = paste0(round(100 * observed_litigation_rate), "%")), vjust = -0.4) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(x = "Referral score band", y = "Observed litigation rate") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  output$referral_matrix_table <- renderDT({
    datatable(referral_matrix, options = list(pageLength = 6, dom = "t", scrollX = TRUE), rownames = FALSE)
  })

  output$report_preview <- renderText({
    top_rows <- scored_claims() %>% head(5)

    paste(
      apply(top_rows, 1, function(x) {
        paste0(
          x[["claim_id"]], " | ", x[["referral_tier"]], " | Score ", x[["referral_score"]], "\n",
          "Indicators: ", x[["top_referral_drivers"]], "\n",
          "Action: ", x[["recommended_action"]]
        )
      }),
      collapse = "\n\n"
    )
  })

  output$download_template <- downloadHandler(
    filename = function() "workers_comp_litigation_claim_template.csv",
    content = function(file) {
      make_demo_claims() %>%
        slice(0) %>%
        write_csv(file)
    }
  )

  output$download_scored <- downloadHandler(
    filename = function() "workers_comp_litigation_scored_claims.csv",
    content = function(file) write_csv(scored_claims(), file)
  )

  output$download_report <- downloadHandler(
    filename = function() "workers_comp_litigation_referral_report.csv",
    content = function(file) {
      scored_claims() %>%
        rowwise() %>%
        mutate(
          adjuster_output = make_role_output(pick(everything()), "Adjuster"),
          litigation_specialist_output = make_role_output(pick(everything()), "Litigation Specialist"),
          supervisor_output = make_role_output(pick(everything()), "Supervisor"),
          claims_counsel_output = make_role_output(pick(everything()), "Claims Counsel")
        ) %>%
        ungroup() %>%
        select(
          claim_id, referral_score, data_coverage_pct, referral_tier, review_priority,
          top_referral_drivers, recommended_action,
          adjuster_output, litigation_specialist_output, supervisor_output, claims_counsel_output
        ) %>%
        write_csv(file)
    }
  )
}

# ============================================================
# Run app
# ============================================================

shinyApp(ui = ui, server = server)
