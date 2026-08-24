# Phase 2: reproducible cleaning of ContainerTransport.csv.
#
# The raw file in data/raw is read only and is never overwritten. This script
# creates a separate analysis copy in data/processed and documents every column
# decision in outputs/results/container_cleaning_summary.csv.

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))

raw_path <- file.path("data", "raw", "ContainerTransport.csv")
processed_path <- file.path("data", "processed", "container_transport_clean.csv")
summary_path <- file.path("outputs", "results", "container_cleaning_summary.csv")

stopifnot(file.exists(raw_path))
stopifnot(dir.exists(dirname(processed_path)), dir.exists(dirname(summary_path)))

# Read an analysis copy only. The source CSV is never modified in place.
container_raw <- readr::read_csv(raw_path, show_col_types = FALSE)
original_names <- names(container_raw)
container_clean <- janitor::clean_names(container_raw)

# This mapping preserves the original variable meaning after standardized names.
column_name_map <- tibble(
  original_column = original_names,
  cleaned_column = names(container_clean)
)

# The following columns are removed only because they are demonstrably constant
# file metadata or completely empty descriptive fields in this raw extract.
# No geographic, time, unit, status, or observation variables are removed.
constant_metadata_columns <- c(
  "structure", "structure_id", "structure_name", "action", "freq",
  "frequency_of_observation", "measure", "measure_2", "vehicle_type",
  "vehicle_type_2", "infrastructure_type", "infrastructure_type_2",
  "decimals", "decimals_2"
)
empty_descriptive_columns <- c("time_period_2", "observation_value")
columns_to_remove <- c(constant_metadata_columns, empty_descriptive_columns)

if (!all(columns_to_remove %in% names(container_clean))) {
  stop("Expected metadata/empty columns were not found; raw schema may have changed.")
}

metadata_is_constant <- vapply(
  container_clean[constant_metadata_columns],
  function(x) dplyr::n_distinct(x, na.rm = FALSE) == 1L,
  logical(1)
)
empty_descriptive_is_empty <- vapply(
  container_clean[empty_descriptive_columns],
  function(x) all(is.na(x) | x == ""),
  logical(1)
)

if (!all(metadata_is_constant) || !all(empty_descriptive_is_empty)) {
  stop("A column proposed for removal is no longer constant or completely empty.")
}

container_clean <- container_clean |>
  select(-all_of(columns_to_remove)) |>
  mutate(
    # IDs and codes stay character; time and the observed value receive types
    # appropriate to their supplied representation.
    ref_area = as.character(ref_area),
    reference_area = as.character(reference_area),
    unit_measure = as.character(unit_measure),
    unit_of_measure = as.character(unit_of_measure),
    transport_mode = as.character(transport_mode),
    transport_mode_2 = as.character(transport_mode_2),
    time_period = as.integer(time_period),
    obs_value = as.numeric(obs_value),
    obs_status = as.character(obs_status),
    observation_status = as.character(observation_status),
    unit_mult = as.integer(unit_mult),
    unit_multiplier = as.character(unit_multiplier),
    # Missing observations remain NA. No missing value is converted to zero.
    observation_is_missing = is.na(obs_value),
    observation_is_provisional = obs_status == "P",
    observation_is_estimated = obs_status == "E",
    observation_has_time_series_break = obs_status == "B"
  )

# UNIT_MULT is retained together with its source label. No standardized value is
# created because this file alone does not explicitly document the arithmetic
# convention needed to apply a multiplier; OBS_VALUE is therefore unchanged.

key_columns <- c("ref_area", "transport_mode", "unit_measure", "time_period")
key_counts <- container_clean |>
  count(across(all_of(key_columns)), name = "number_of_records")
duplicate_key_count <- sum(key_counts$number_of_records > 1L)
unique_key_count <- nrow(key_counts)
exact_duplicate_rows <- sum(duplicated(container_clean))

# Phase 1 structural checks. These stop execution rather than silently allowing
# unexplained observation loss or a changed analytical key.
expected_input_rows <- 624L
expected_input_columns <- 28L
expected_missing_observations <- 118L
expected_reference_areas <- 49L
expected_years <- c(2019L, 2022L)
expected_transport_modes <- 2L
expected_units <- 2L
expected_statuses <- 5L

if (nrow(container_raw) != expected_input_rows || ncol(container_raw) != expected_input_columns) {
  stop("Raw ContainerTransport structure differs from the Phase 1 audit.")
}
if (nrow(container_clean) != nrow(container_raw)) {
  stop("Cleaning changed the number of observations; no row removal is allowed.")
}
if (sum(container_clean$observation_is_missing) != expected_missing_observations) {
  stop("Missing-observation count differs from the Phase 1 audit.")
}
if (n_distinct(container_clean$ref_area) != expected_reference_areas ||
    !identical(range(container_clean$time_period), expected_years) ||
    n_distinct(container_clean$transport_mode) != expected_transport_modes ||
    n_distinct(container_clean$unit_measure) != expected_units ||
    n_distinct(container_clean$obs_status) != expected_statuses) {
  stop("Cleaned ContainerTransport structure differs from the Phase 1 audit.")
}
if (duplicate_key_count != 0L || unique_key_count != nrow(container_clean)) {
  stop("Analytical key is not unique after cleaning; no output was written.")
}

removed_decisions <- column_name_map |>
  mutate(
    removed = cleaned_column %in% columns_to_remove,
    removal_reason = case_when(
      cleaned_column %in% constant_metadata_columns ~ "Constant file metadata; not an analytical variable.",
      cleaned_column %in% empty_descriptive_columns ~ "Completely empty descriptive field in this raw extract.",
      TRUE ~ "Retained as geographic, transport, unit, time, outcome, status, or quality-flag input."
    )
  )

mapping_summary <- removed_decisions |>
  transmute(
    issue = "column_name_mapping",
    before = original_column,
    after = if_else(removed, "removed", cleaned_column),
    action = if_else(removed, "remove_from_analysis_copy", "clean_name_and_retain"),
    reason = removal_reason
  )

container_cleaning_summary <- bind_rows(
  tibble(
    issue = c(
      "row_count", "missing_observation_values", "exact_duplicate_rows",
      "analytical_key_duplicates", "unit_handling", "removed_metadata_columns",
      "removed_empty_descriptive_columns", "type_conversions", "quality_flags"
    ),
    before = c(
      nrow(container_raw), sum(is.na(container_raw$OBS_VALUE)), sum(duplicated(container_raw)),
      0L, "Tonnes and TEU present; OBS_VALUE unchanged", length(constant_metadata_columns),
      length(empty_descriptive_columns), "TIME_PERIOD/OBS_VALUE inferred by import", "No explicit flags"
    ),
    after = c(
      nrow(container_clean), sum(is.na(container_clean$obs_value)), exact_duplicate_rows,
      duplicate_key_count, "Tonnes and TEU retained separately; UNIT_MULT retained without application",
      length(constant_metadata_columns), length(empty_descriptive_columns),
      "time_period integer; obs_value numeric; codes/IDs character",
      "missing, provisional, estimated, and time-series-break flags added"
    ),
    action = c(
      "no_rows_removed", "preserve_as_NA", "none_removed", "validated_not_removed",
      "no_unit_conversion_or_multiplier_application", "removed_from_analysis_copy",
      "removed_from_analysis_copy", "explicit_type_conversion", "added_audit_flags"
    ),
    reason = c(
      "Phase 1 audit identified 624 raw observations.",
      "Source status M identifies missing values; they must not become zero.",
      "Phase 1 found no exact duplicates.",
      "REF_AREA + TRANSPORT_MODE + UNIT_MEASURE + TIME_PERIOD remains unique.",
      "The dataset distinguishes incompatible units and does not document multiplier arithmetic sufficiently.",
      "All 14 fields are constant metadata in this extract.",
      "Both fields are entirely empty in this extract.",
      "Types are made explicit without changing values.",
      "Quality/status information is preserved and made easier to filter."
    )
  ),
  mapping_summary
)

# Only the processed analysis copy and the requested cleaning summary are written.
readr::write_csv(container_clean, processed_path, na = "")
readr::write_csv(container_cleaning_summary, summary_path, na = "")

cat("ContainerTransport cleaning completed; raw CSV was not modified.\n")
cat("Input rows:", nrow(container_raw), " Output rows:", nrow(container_clean), "\n")
cat("Input columns:", ncol(container_raw), " Output columns:", ncol(container_clean), "\n")
cat("Missing observations:", sum(container_clean$observation_is_missing), "\n")
cat("Unique analytical keys:", unique_key_count, "\n")
cat("Duplicate analytical keys:", duplicate_key_count, "\n")
cat("Units:", paste(sort(unique(container_clean$unit_of_measure)), collapse = ", "), "\n")
cat("Years:", paste(range(container_clean$time_period), collapse = "-"), "\n")
