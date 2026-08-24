# Phase 2: reproducible cleaning of the "Oil & NGL Pipeline Ownership" sheet
# from globalEnergyOwnership.xlsx.
#
# Rules enforced throughout:
#   - The workbook in data/raw/ is read only and is never overwritten.
#   - Rows are never silently removed; the ownership-path grain is preserved.
#   - A single physical project may have many rows (one per ownership path).
#   - ProjectID is NOT a unique row identifier.
#   - Missing Share is preserved as NA; it is never converted to zero.
#   - Missing/placeholder CapacityBOEd is preserved as NA; it is never imputed.
#   - No concentration, HHI, ranking, or pipeline-location inference is performed.
#   - No merge with the oil production dataset is performed.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    "Package 'readxl' is not installed. ",
    "Please run:  install.packages(\"readxl\")  then re-run this script."
  )
}

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
workbook_path  <- file.path("data", "raw",        "globalEnergyOwnership.xlsx")
processed_path <- file.path("data", "processed",  "energy_ownership_clean.csv")
summary_path   <- file.path("outputs", "results", "energy_ownership_cleaning_summary.csv")

stopifnot(file.exists(workbook_path))
stopifnot(dir.exists(dirname(processed_path)), dir.exists(dirname(summary_path)))

cat("Reading workbook (read-only):", workbook_path, "\n")

# ---------------------------------------------------------------------------
# Phase 1 audit constants
# ---------------------------------------------------------------------------
PHASE1_ROWS             <- 11760L
PHASE1_COLS             <- 13L
PHASE1_SHARE_MISSING    <- 3643L
PHASE1_EXACT_DUP_GROUPS <- 31L
PHASE1_EXACT_DUP_EXTRA  <- 31L
PHASE1_DISTINCT_PIDS    <- 1685L

required_original_cols <- c(
  "Parent GEM Entity ID", "Parent", "Parent Registration Country",
  "Parent Headquarters Country", "Project", "Share", "Ownership Path",
  "Immediate Project Owner", "Immediate Project Owner GEM Entity ID",
  "Tracker", "Status", "CapacityBOEd", "ProjectID"
)

capacity_placeholders <- c("--", "-", "N/A", "n/a", "NA", "na", "None", "none", "")

# ---------------------------------------------------------------------------
# 1. Import
# ---------------------------------------------------------------------------
sheet_target       <- "Oil & NGL Pipeline Ownership"
all_entities_sheet <- "All Entities"

pipeline_raw <- readxl::read_excel(
  workbook_path,
  sheet        = sheet_target,
  .name_repair = "unique_quiet",
  col_types    = "text"
)

all_entities_raw <- readxl::read_excel(
  workbook_path,
  sheet        = all_entities_sheet,
  .name_repair = "unique_quiet",
  col_types    = "text"
)

cat("Imported sheet '", sheet_target, "': ", nrow(pipeline_raw), " rows x ",
    ncol(pipeline_raw), " columns\n", sep = "")

# ---------------------------------------------------------------------------
# 2. Validate raw import against Phase 1 audit
# ---------------------------------------------------------------------------
missing_cols <- setdiff(required_original_cols, names(pipeline_raw))
if (length(missing_cols) > 0) {
  stop("Pipeline sheet is missing expected columns: ", paste(missing_cols, collapse = ", "))
}
if (nrow(pipeline_raw) != PHASE1_ROWS) {
  stop("Row count mismatch vs Phase 1 audit.  Expected: ", PHASE1_ROWS,
       "  Found: ", nrow(pipeline_raw))
}
if (ncol(pipeline_raw) != PHASE1_COLS) {
  stop("Column count mismatch vs Phase 1 audit.  Expected: ", PHASE1_COLS,
       "  Found: ", ncol(pipeline_raw))
}

if (!"Entity ID" %in% names(all_entities_raw)) {
  stop("'All Entities' sheet is missing required column 'Entity ID'.")
}
all_entity_ids <- unique(as.character(all_entities_raw$`Entity ID`))
cat("All Entities sheet loaded:", length(all_entity_ids), "distinct entity IDs\n")

# ---------------------------------------------------------------------------
# 3. Snapshot raw column names and types before any transformation
# ---------------------------------------------------------------------------
raw_col_types  <- purrr::map_chr(pipeline_raw, ~ class(.x)[1])
original_names <- names(pipeline_raw)

# ---------------------------------------------------------------------------
# 4. Build cleaned dataset
# ---------------------------------------------------------------------------

# 4a. Snapshot raw CapacityBOEd string before conversion
pipeline_clean <- pipeline_raw |>
  mutate(capacity_boed_raw = as.character(CapacityBOEd))

# 4b. Helper: detect blank / placeholder strings
is_blank_or_placeholder <- function(x, placeholders = capacity_placeholders) {
  trimmed <- stringr::str_trim(as.character(x))
  is.na(x) | trimmed == "" | trimmed %in% placeholders
}

# 4c. Standardize column names to snake_case
pipeline_clean <- pipeline_clean |>
  janitor::clean_names()

# 4d. Preserve identifiers as character
pipeline_clean <- pipeline_clean |>
  mutate(
    parent_gem_entity_id                  = as.character(parent_gem_entity_id),
    immediate_project_owner_gem_entity_id = as.character(immediate_project_owner_gem_entity_id),
    project_id                            = as.character(project_id),
    parent                                = as.character(parent),
    parent_registration_country           = as.character(parent_registration_country),
    parent_headquarters_country           = as.character(parent_headquarters_country),
    project                               = as.character(project),
    ownership_path                        = as.character(ownership_path),
    immediate_project_owner               = as.character(immediate_project_owner),
    tracker                               = as.character(tracker),
    status                                = as.character(status)
  )

# 4e. Convert Share to numeric; flag missing
pipeline_clean <- pipeline_clean |>
  mutate(
    share_numeric = suppressWarnings(as.numeric(share)),
    share_missing = is.na(share_numeric),
    share_numeric = if_else(share_missing, NA_real_, share_numeric)
  )

# 4f. Convert CapacityBOEd to numeric; detect placeholders
pipeline_clean <- pipeline_clean |>
  mutate(
    capacity_missing_or_placeholder = is_blank_or_placeholder(capacity_bo_ed),
    capacity_boed_numeric = if_else(
      capacity_missing_or_placeholder,
      NA_real_,
      suppressWarnings(as.numeric(capacity_bo_ed))
    ),
    capacity_missing_or_placeholder = capacity_missing_or_placeholder |
                                       is.na(capacity_boed_numeric)
  )

# 4g. Exact duplicate flag (over the original 13 columns)
row_sig <- function(data) {
  encoded <- lapply(data, function(col) {
    paste(nchar(as.character(col)), as.character(col), sep = ":")
  })
  do.call(paste, c(encoded, sep = "\r"))
}
original_cols_clean <- janitor::make_clean_names(required_original_cols)
pipeline_sig <- row_sig(pipeline_clean[, original_cols_clean])
sig_table    <- table(pipeline_sig)

pipeline_clean <- pipeline_clean |>
  mutate(
    exact_duplicate_flag       = pipeline_sig %in% names(sig_table[sig_table > 1L]),
    exact_duplicate_group_size = as.integer(sig_table[pipeline_sig])
  )

# 4h. Entity ID validation against All Entities
pipeline_clean <- pipeline_clean |>
  mutate(
    parent_entity_id_missing    = is.na(parent_gem_entity_id) |
                                   stringr::str_trim(parent_gem_entity_id) == "",
    parent_entity_id_matched    = !parent_entity_id_missing &
                                   parent_gem_entity_id %in% all_entity_ids,
    imm_owner_entity_id_missing = is.na(immediate_project_owner_gem_entity_id) |
                                   stringr::str_trim(immediate_project_owner_gem_entity_id) == "",
    imm_owner_entity_id_matched = !imm_owner_entity_id_missing &
                                   immediate_project_owner_gem_entity_id %in% all_entity_ids
  )

# ---------------------------------------------------------------------------
# 5. Structural validation before writing output
# ---------------------------------------------------------------------------
if (nrow(pipeline_clean) != PHASE1_ROWS) {
  stop("STRUCTURAL VALIDATION FAILED: output row count ", nrow(pipeline_clean),
       " != Phase 1 audit value ", PHASE1_ROWS)
}

n_distinct_pids <- n_distinct(pipeline_clean$project_id)
if (n_distinct_pids != PHASE1_DISTINCT_PIDS) {
  stop("STRUCTURAL VALIDATION FAILED: distinct ProjectID count ", n_distinct_pids,
       " != Phase 1 audit value ", PHASE1_DISTINCT_PIDS)
}

expected_clean_cols <- janitor::make_clean_names(required_original_cols)
missing_clean_cols  <- setdiff(expected_clean_cols, names(pipeline_clean))
if (length(missing_clean_cols) > 0) {
  stop("STRUCTURAL VALIDATION FAILED: cleaned dataset missing columns: ",
       paste(missing_clean_cols, collapse = ", "))
}

n_share_missing <- sum(pipeline_clean$share_missing)
if (n_share_missing != PHASE1_SHARE_MISSING) {
  stop("STRUCTURAL VALIDATION FAILED: share_missing count ", n_share_missing,
       " != Phase 1 audit value ", PHASE1_SHARE_MISSING)
}

if (any(!is.na(pipeline_clean$share_numeric) & pipeline_clean$share_missing)) {
  stop("STRUCTURAL VALIDATION FAILED: share_missing flag inconsistent with share_numeric.")
}

stopifnot(file.exists(workbook_path))
cat("All structural validation checks passed.\n")

# ---------------------------------------------------------------------------
# 6. Derive summary statistics
# ---------------------------------------------------------------------------
n_capacity_missing      <- sum(pipeline_clean$capacity_missing_or_placeholder)
n_exact_dup_groups      <- sum(sig_table > 1L)
n_exact_dup_extra       <- sum(sig_table[sig_table > 1L] - 1L)
n_exact_dup_rows_flagged <- sum(pipeline_clean$exact_duplicate_flag)

n_parent_missing  <- sum(pipeline_clean$parent_entity_id_missing)
n_parent_matched  <- sum(pipeline_clean$parent_entity_id_matched)
n_parent_unmatched <- sum(!pipeline_clean$parent_entity_id_matched &
                            !pipeline_clean$parent_entity_id_missing)

n_imm_missing    <- sum(pipeline_clean$imm_owner_entity_id_missing)
n_imm_matched    <- sum(pipeline_clean$imm_owner_entity_id_matched)
n_imm_unmatched  <- sum(!pipeline_clean$imm_owner_entity_id_matched &
                           !pipeline_clean$imm_owner_entity_id_missing)

n_projects_multiple_paths <- pipeline_clean |>
  count(project_id, name = "n_paths") |>
  filter(n_paths > 1L) |>
  nrow()

# ---------------------------------------------------------------------------
# 7. Build cleaning summary
# ---------------------------------------------------------------------------
clean_col_types <- purrr::map_chr(pipeline_clean, ~ class(.x)[1])

col_map <- tibble(
  original_column_name = required_original_cols,
  cleaned_column_name  = janitor::make_clean_names(required_original_cols),
  original_type        = raw_col_types[required_original_cols],
  cleaned_type         = clean_col_types[janitor::make_clean_names(required_original_cols)],
  missing_count        = purrr::map_int(
    janitor::make_clean_names(required_original_cols),
    ~ sum(is.na(pipeline_clean[[.x]]) |
            (is.character(pipeline_clean[[.x]]) &
               stringr::str_trim(pipeline_clean[[.x]]) == ""))
  ),
  transformation_performed = case_when(
    original_column_name == "Share" ~
      "Converted text to numeric (share_numeric); added share_missing flag; blanks -> NA, never zero",
    original_column_name == "CapacityBOEd" ~
      "Converted text to numeric (capacity_boed_numeric); raw string preserved in capacity_boed_raw; placeholders -> NA; added capacity_missing_or_placeholder flag",
    original_column_name %in% c("Parent GEM Entity ID",
                                 "Immediate Project Owner GEM Entity ID",
                                 "ProjectID") ~
      "Preserved as character; entity ID match/missing flags added",
    TRUE ~ "Preserved as character; janitor snake_case name applied"
  ),
  column_retained = TRUE,
  data_quality_notes = case_when(
    original_column_name == "Share" ~
      paste0("3,643 of 11,760 rows (30.98%) missing per Phase 1 audit. Confirmed: ",
             n_share_missing, " missing."),
    original_column_name == "CapacityBOEd" ~
      paste0("Placeholder '--' and blank strings treated as NA. Total flagged: ",
             n_capacity_missing, "."),
    original_column_name == "Status" ~
      "1 missing value per Phase 1 missingness audit.",
    original_column_name == "Parent Registration Country" ~
      "2,301 missing (19.57%) per Phase 1 audit. Not an asset location field.",
    original_column_name == "Parent Headquarters Country" ~
      "2,020 missing (17.18%) per Phase 1 audit. Not an asset location field.",
    original_column_name == "Parent GEM Entity ID" ~
      paste0("0 missing per Phase 1 audit. Matched: ", n_parent_matched,
             "; unmatched: ", n_parent_unmatched, "; blank: ", n_parent_missing, "."),
    original_column_name == "Immediate Project Owner GEM Entity ID" ~
      paste0("0 missing per Phase 1 audit. Matched: ", n_imm_matched,
             "; unmatched: ", n_imm_unmatched, "; blank: ", n_imm_missing, "."),
    original_column_name == "ProjectID" ~
      paste0("0 missing per Phase 1 audit. ", n_distinct_pids,
             " distinct project IDs; ", n_projects_multiple_paths,
             " projects with >1 ownership-path row. NOT a unique row key."),
    TRUE ~ "No anomalies identified in Phase 1 audit."
  )
)

derived_rows <- tibble(
  original_column_name = c(
    "share_numeric", "share_missing",
    "capacity_boed_raw", "capacity_boed_numeric", "capacity_missing_or_placeholder",
    "exact_duplicate_flag", "exact_duplicate_group_size",
    "parent_entity_id_missing", "parent_entity_id_matched",
    "imm_owner_entity_id_missing", "imm_owner_entity_id_matched"
  ),
  cleaned_column_name      = original_column_name,
  original_type            = "(derived)",
  cleaned_type             = c("numeric", "logical", "character", "numeric", "logical",
                                "logical", "integer", "logical", "logical", "logical", "logical"),
  missing_count            = 0L,
  transformation_performed = c(
    "Numeric conversion of Share column",
    "Flag: Share is NA after conversion",
    "Raw text snapshot of CapacityBOEd before any conversion",
    "Numeric conversion of CapacityBOEd; NA where placeholder detected",
    "Flag: CapacityBOEd was blank, '--', or other non-numeric placeholder",
    "Flag: row belongs to an exact-duplicate group (across original 13 cols)",
    "Size of the exact-duplicate group this row belongs to (1 = unique)",
    "Flag: Parent GEM Entity ID is blank/NA",
    "Flag: Parent GEM Entity ID matched to All Entities sheet",
    "Flag: Immediate Project Owner GEM Entity ID is blank/NA",
    "Flag: Immediate Project Owner GEM Entity ID matched to All Entities sheet"
  ),
  column_retained = TRUE,
  data_quality_notes = c(
    paste0(n_share_missing, " NA values; no zero imputation."),
    paste0(n_share_missing, " TRUE values."),
    "Retained for provenance; all 11,760 rows populated.",
    paste0(n_capacity_missing, " NA values; no imputation performed."),
    paste0(n_capacity_missing, " flagged rows."),
    paste0(n_exact_dup_rows_flagged, " rows flagged across ", n_exact_dup_groups,
           " duplicate groups; rows RETAINED and flagged only."),
    "Group size 1 = unique row.",
    paste0(n_parent_missing, " blank parent entity IDs."),
    paste0(n_parent_matched, " matched; ", n_parent_unmatched, " unmatched."),
    paste0(n_imm_missing, " blank immediate-owner entity IDs."),
    paste0(n_imm_matched, " matched; ", n_imm_unmatched, " unmatched.")
  )
)

energy_ownership_cleaning_summary <- bind_rows(col_map, derived_rows)

# ---------------------------------------------------------------------------
# 8. Write outputs
# ---------------------------------------------------------------------------
readr::write_csv(pipeline_clean,                    processed_path, na = "")
readr::write_csv(energy_ownership_cleaning_summary,  summary_path,  na = "")

cat("\n=== Energy Ownership Cleaning Completed ===\n")
cat("Workbook read only (not modified):", workbook_path, "\n")
cat("Output written to:                ", processed_path, "\n")
cat("Cleaning summary written to:      ", summary_path, "\n\n")

cat("--- Row counts ---\n")
cat("  Rows before cleaning (Phase 1 audit):      ", PHASE1_ROWS, "\n")
cat("  Rows after cleaning (output):              ", nrow(pipeline_clean), "\n")
cat("  Rows removed:                              ", PHASE1_ROWS - nrow(pipeline_clean), "\n\n")

cat("--- ProjectIDs ---\n")
cat("  Distinct ProjectIDs (Phase 1 audit):       ", PHASE1_DISTINCT_PIDS, "\n")
cat("  Distinct ProjectIDs (this run):            ", n_distinct_pids, "\n")
cat("  Projects with > 1 ownership-path row:      ", n_projects_multiple_paths, "\n\n")

cat("--- Share ---\n")
cat("  Missing Share (Phase 1 audit):             ", PHASE1_SHARE_MISSING, "\n")
cat("  Missing Share (this run):                  ", n_share_missing, "\n")
cat("  Share converted to zero:                    0 (never allowed)\n\n")

cat("--- CapacityBOEd ---\n")
cat("  Missing/placeholder capacity (this run):   ", n_capacity_missing, "\n")
cat("  Capacity imputed:                           0 (never allowed)\n\n")

cat("--- Exact duplicate rows ---\n")
cat("  Exact duplicate groups (Phase 1 audit):    ", PHASE1_EXACT_DUP_GROUPS, "\n")
cat("  Exact duplicate groups (this run):         ", n_exact_dup_groups, "\n")
cat("  Duplicate rows beyond first (Phase 1):     ", PHASE1_EXACT_DUP_EXTRA, "\n")
cat("  Duplicate rows beyond first (this run):    ", n_exact_dup_extra, "\n")
cat("  Rows flagged as duplicates:                ", n_exact_dup_rows_flagged, "\n")
cat("  Duplicate rows removed:                     0 (retained and flagged)\n\n")

cat("--- Parent entity ID ---\n")
cat("  Missing/blank:                             ", n_parent_missing, "\n")
cat("  Matched to All Entities:                   ", n_parent_matched, "\n")
cat("  Unmatched (non-blank, not in All Entities):", n_parent_unmatched, "\n\n")

cat("--- Immediate Project Owner entity ID ---\n")
cat("  Missing/blank:                             ", n_imm_missing, "\n")
cat("  Matched to All Entities:                   ", n_imm_matched, "\n")
cat("  Unmatched (non-blank, not in All Entities):", n_imm_unmatched, "\n\n")

cat("Output columns:", ncol(pipeline_clean), "\n")
cat("Column names:\n")
cat(paste0("  ", names(pipeline_clean)), sep = "\n")
cat("\nDone.\n")
