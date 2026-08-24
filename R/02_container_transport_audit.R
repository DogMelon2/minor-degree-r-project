# Phase 1: read-only audit of ContainerTransport.csv.
# The raw CSV is never modified. This script only writes audit tables.

suppressPackageStartupMessages(library(tidyverse))

raw_path <- file.path("data", "raw", "ContainerTransport.csv")
results_dir <- file.path("outputs", "results")
stopifnot(file.exists(raw_path), dir.exists(results_dir))

container_raw <- readr::read_csv(raw_path, show_col_types = FALSE)

key_columns <- c("REF_AREA", "TRANSPORT_MODE", "UNIT_MEASURE", "TIME_PERIOD")
stopifnot(all(key_columns %in% names(container_raw)))

missingness <- tibble(
  column = names(container_raw),
  missing_count = purrr::map_int(container_raw, ~ sum(is.na(.x) | .x == "")),
  missing_percent = round(100 * missing_count / nrow(container_raw), 2)
)

key_audit_all <- container_raw |>
  count(across(all_of(key_columns)), name = "number_of_records") |>
  arrange(desc(number_of_records), across(all_of(key_columns)))

# A sensible observational key is audited but no records are removed or merged.
container_key_audit <- key_audit_all |>
  filter(number_of_records > 1)

exact_duplicate_rows <- sum(duplicated(container_raw))
exact_duplicate_groups <- container_raw |>
  mutate(.raw_row = row_number()) |>
  group_by(across(-.raw_row)) |>
  summarise(number_of_records = n(), .groups = "drop") |>
  filter(number_of_records > 1) |>
  nrow()

container_status_summary <- container_raw |>
  count(OBS_STATUS, `Observation status`, name = "record_count") |>
  left_join(
    container_raw |>
      group_by(OBS_STATUS, `Observation status`) |>
      summarise(
        missing_observation_values = sum(is.na(OBS_VALUE)),
        minimum_observation_value = suppressWarnings(min(OBS_VALUE, na.rm = TRUE)),
        maximum_observation_value = suppressWarnings(max(OBS_VALUE, na.rm = TRUE)),
        .groups = "drop"
      ),
    by = c("OBS_STATUS", "Observation status")
  ) |>
  mutate(
    minimum_observation_value = if_else(is.infinite(minimum_observation_value), NA_real_, minimum_observation_value),
    maximum_observation_value = if_else(is.infinite(maximum_observation_value), NA_real_, maximum_observation_value)
  ) |>
  arrange(desc(record_count))

observation_range <- container_raw |>
  summarise(
    minimum = min(OBS_VALUE, na.rm = TRUE),
    maximum = max(OBS_VALUE, na.rm = TRUE),
    missing = sum(is.na(OBS_VALUE))
  )

container_data_audit_summary <- tibble(
  issue = c(
    "rows", "columns", "exact_duplicate_rows", "exact_duplicate_groups",
    "unique_reference_areas", "year_minimum", "year_maximum", "transport_modes",
    "units", "unit_multipliers", "observation_status_categories",
    "missing_observation_values", "observation_value_minimum", "observation_value_maximum",
    "repeated_analytical_keys"
  ),
  count = c(
    nrow(container_raw), ncol(container_raw), exact_duplicate_rows, exact_duplicate_groups,
    n_distinct(container_raw$REF_AREA), min(container_raw$TIME_PERIOD), max(container_raw$TIME_PERIOD),
    n_distinct(container_raw$TRANSPORT_MODE), n_distinct(container_raw$UNIT_MEASURE),
    n_distinct(container_raw$UNIT_MULT), n_distinct(container_raw$OBS_STATUS),
    observation_range$missing, observation_range$minimum, observation_range$maximum,
    nrow(container_key_audit)
  ),
  description = c(
    "Physical rows read from the raw CSV.",
    "Visible columns in the raw CSV.",
    "Rows beyond the first in exact duplicate groups across all columns.",
    "Groups identical across all visible columns.",
    "Distinct REF_AREA codes; these are reference areas, not necessarily countries.",
    "Earliest TIME_PERIOD value.",
    "Latest TIME_PERIOD value.",
    "Distinct TRANSPORT_MODE codes.",
    "Distinct UNIT_MEASURE codes; tonnes and TEU remain separate measures.",
    "Distinct UNIT_MULT values; their labels are retained, not interpreted further.",
    "Distinct observation-status codes.",
    "Blank/NA OBS_VALUE observations.",
    "Minimum observed OBS_VALUE without applying UNIT_MULT.",
    "Maximum observed OBS_VALUE without applying UNIT_MULT.",
    "Keys repeated for REF_AREA + transport mode + unit + time period."
  )
)

# The following inventory documents roles in the raw schema. No columns are
# removed, renamed, or transformed in container_raw.
column_roles <- tribble(
  ~role, ~columns,
  "metadata", "STRUCTURE; STRUCTURE_ID; STRUCTURE_NAME; ACTION; FREQ; Frequency of observation; MEASURE; Measure; VEHICLE_TYPE; Vehicle type; INFRASTRUCTURE_TYPE; Infrastructure type; DECIMALS; Decimals",
  "geographic", "REF_AREA; Reference area",
  "transport", "TRANSPORT_MODE; Transport mode",
  "unit_measurement", "UNIT_MEASURE; Unit of measure; UNIT_MULT; Unit multiplier",
  "time", "TIME_PERIOD; Time period",
  "analytical_outcome", "OBS_VALUE; Observation value",
  "data_quality_status", "OBS_STATUS; Observation status"
)

cat("Container transport audit completed (raw CSV was read only).\n")
cat("Rows:", nrow(container_raw), " Columns:", ncol(container_raw), "\n")
cat("Column names:", paste(names(container_raw), collapse = ", "), "\n")
cat("Data types:\n"); print(purrr::map_chr(container_raw, ~ class(.x)[1]))
cat("Missing values by column:\n"); print(missingness)
cat("Transport modes:", paste(sort(unique(container_raw$`Transport mode`)), collapse = ", "), "\n")
cat("Units:", paste(sort(unique(container_raw$`Unit of measure`)), collapse = ", "), "\n")
cat("Unit multipliers:", paste(sort(unique(container_raw$`Unit multiplier`)), collapse = ", "), "\n")
cat("Observation statuses:\n"); print(container_status_summary)
cat("Column roles:\n"); print(column_roles)

# Only the requested audit outputs are written.
readr::write_csv(container_data_audit_summary, file.path(results_dir, "container_data_audit_summary.csv"))
readr::write_csv(missingness, file.path(results_dir, "container_missingness.csv"))
readr::write_csv(container_key_audit, file.path(results_dir, "container_key_audit.csv"))
readr::write_csv(container_status_summary, file.path(results_dir, "container_status_summary.csv"))
