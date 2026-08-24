# Phase 1: read-only audit of Refinery.csv.
# Raw dates are intentionally retained as character strings. They appear to be
# Persian/Iranian-calendar dates and are not converted to Gregorian dates here.

suppressPackageStartupMessages(library(tidyverse))

raw_path <- file.path("data", "raw", "Refinery.csv")
results_dir <- file.path("outputs", "results")
stopifnot(file.exists(raw_path), dir.exists(results_dir))

# Explicit character types preserve raw date text and avoid an unjustified
# automatic date conversion. Numeric weight fields are parsed only for audit.
refinery_raw <- readr::read_csv(
  raw_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

weight_columns <- c("Origin Net Weight", "Destination Net Weight")
refinery_audit <- refinery_raw |>
  mutate(
    raw_row_number = row_number() + 1L,
    origin_weight_numeric = readr::parse_number(`Origin Net Weight`),
    destination_weight_numeric = readr::parse_number(`Destination Net Weight`),
    destination_minus_origin = destination_weight_numeric - origin_weight_numeric,
    absolute_weight_difference = abs(destination_minus_origin)
  )

missingness <- tibble(
  column = names(refinery_raw),
  missing_count = purrr::map_int(refinery_raw, ~ sum(is.na(.x) | .x == "")),
  missing_percent = round(100 * missing_count / nrow(refinery_raw), 2)
)

row_signature <- do.call(paste, c(lapply(refinery_raw, as.character), sep = "\r"))
signature_counts <- table(row_signature)
duplicate_signatures <- names(signature_counts[signature_counts > 1])
refinery_duplicate_audit <- refinery_audit |>
  mutate(
    .row_signature = row_signature,
    exact_duplicate_group_size = as.integer(signature_counts[.row_signature])
  ) |>
  filter(.row_signature %in% duplicate_signatures) |>
  select(-.row_signature) |>
  select(raw_row_number, everything()) |>
  arrange(`Refinery`, Type, `Origin Departure Date`, raw_row_number)

# This regular-expression check only validates the visible YYYY/MM/DD form and
# broad component ranges. It does not claim Gregorian or Persian calendar validity.
parse_ymd_text <- function(x) {
  parts <- str_split_fixed(x, "/", 3)
  tibble(
    year_text = parts[, 1],
    month_text = parts[, 2],
    day_text = parts[, 3],
    format_matches_yyyy_mm_dd = str_detect(x, "^[0-9]{4}/[0-9]{2}/[0-9]{2}$"),
    components_within_broad_ranges = suppressWarnings(
      as.integer(parts[, 2]) >= 1 & as.integer(parts[, 2]) <= 12 &
        as.integer(parts[, 3]) >= 1 & as.integer(parts[, 3]) <= 31
    )
  )
}

origin_date_parts <- parse_ymd_text(refinery_raw$`Origin Departure Date`)
destination_date_parts <- parse_ymd_text(refinery_raw$`Destination Arrival Date`)

# Lexicographic ordering is valid for fixed-width YYYY/MM/DD labels within the
# same calendar, but it is not a conversion to a real calendar date.
refinery_date_audit <- refinery_audit |>
  transmute(
    raw_row_number,
    `Origin Departure Date`,
    `Destination Arrival Date`,
    origin_format_matches_yyyy_mm_dd = origin_date_parts$format_matches_yyyy_mm_dd,
    destination_format_matches_yyyy_mm_dd = destination_date_parts$format_matches_yyyy_mm_dd,
    origin_components_within_broad_ranges = origin_date_parts$components_within_broad_ranges,
    destination_components_within_broad_ranges = destination_date_parts$components_within_broad_ranges,
    destination_label_after_or_equal_origin = `Destination Arrival Date` >= `Origin Departure Date`
  )

refinery_weight_audit <- refinery_audit |>
  mutate(
    is_largest_absolute_difference = absolute_weight_difference == max(absolute_weight_difference, na.rm = TRUE),
    is_smallest_absolute_difference = absolute_weight_difference == min(absolute_weight_difference, na.rm = TRUE)
  ) |>
  select(
    raw_row_number, `Refinery`, Type, `Work shift`, `Origin Departure Date`,
    `Destination Arrival Date`, `Origin Net Weight`, `Destination Net Weight`,
    origin_weight_numeric, destination_weight_numeric, destination_minus_origin,
    absolute_weight_difference, is_largest_absolute_difference,
    is_smallest_absolute_difference
  )

refinery_data_audit_summary <- tibble(
  issue = c(
    "rows", "columns", "exact_duplicate_groups", "duplicate_rows_beyond_first",
    "unique_refineries", "unique_product_types", "unique_work_shifts",
    "origin_date_minimum_label", "origin_date_maximum_label",
    "destination_date_minimum_label", "destination_date_maximum_label",
    "unique_origin_dates", "unique_destination_dates",
    "origin_weight_minimum", "origin_weight_maximum",
    "destination_weight_minimum", "destination_weight_maximum",
    "minimum_destination_minus_origin", "maximum_destination_minus_origin",
    "destination_label_before_origin"
  ),
  count = c(
    nrow(refinery_raw), ncol(refinery_raw), length(duplicate_signatures),
    sum(signature_counts[signature_counts > 1] - 1L),
    n_distinct(refinery_raw$Refinery), n_distinct(refinery_raw$Type), n_distinct(refinery_raw$`Work shift`),
    min(refinery_raw$`Origin Departure Date`), max(refinery_raw$`Origin Departure Date`),
    min(refinery_raw$`Destination Arrival Date`), max(refinery_raw$`Destination Arrival Date`),
    n_distinct(refinery_raw$`Origin Departure Date`), n_distinct(refinery_raw$`Destination Arrival Date`),
    min(refinery_audit$origin_weight_numeric), max(refinery_audit$origin_weight_numeric),
    min(refinery_audit$destination_weight_numeric), max(refinery_audit$destination_weight_numeric),
    min(refinery_audit$destination_minus_origin), max(refinery_audit$destination_minus_origin),
    sum(!refinery_date_audit$destination_label_after_or_equal_origin)
  ),
  description = c(
    "Physical rows read from the raw CSV.", "Visible columns in the raw CSV.",
    "Groups identical across every visible raw column.",
    "Rows beyond the first in exact-duplicate groups.",
    "Distinct labels in the Refinery column; no geography is inferred.",
    "Distinct labels in Type.", "Distinct labels in Work shift.",
    "Lexicographic minimum raw origin-date label; not converted to Gregorian.",
    "Lexicographic maximum raw origin-date label; not converted to Gregorian.",
    "Lexicographic minimum raw destination-date label; not converted to Gregorian.",
    "Lexicographic maximum raw destination-date label; not converted to Gregorian.",
    "Distinct raw origin-date labels.", "Distinct raw destination-date labels.",
    "Minimum numeric origin weight as recorded; its unit is not inferred.",
    "Maximum numeric origin weight as recorded; its unit is not inferred.",
    "Minimum numeric destination weight as recorded; its unit is not inferred.",
    "Maximum numeric destination weight as recorded; its unit is not inferred.",
    "Minimum row-level destination weight minus origin weight; not a loss estimate.",
    "Maximum row-level destination weight minus origin weight; not a gain estimate.",
    "Raw labels where destination sorts before origin within YYYY/MM/DD notation."
  )
)

cat("Refinery audit completed (raw CSV was read only).\n")
cat("Rows:", nrow(refinery_raw), " Columns:", ncol(refinery_raw), "\n")
cat("Column names:", paste(names(refinery_raw), collapse = ", "), "\n")
cat("Raw data types:\n"); print(purrr::map_chr(refinery_raw, ~ class(.x)[1]))
cat("Missing values by column:\n"); print(missingness)
cat("Unique refineries:", paste(sort(unique(refinery_raw$Refinery)), collapse = ", "), "\n")
cat("Exact duplicate groups:", length(duplicate_signatures), "\n")
cat("Destination label before origin:", sum(!refinery_date_audit$destination_label_after_or_equal_origin), "\n")

readr::write_csv(refinery_data_audit_summary, file.path(results_dir, "refinery_data_audit_summary.csv"))
readr::write_csv(missingness, file.path(results_dir, "refinery_missingness.csv"))
readr::write_csv(refinery_duplicate_audit, file.path(results_dir, "refinery_duplicate_audit.csv"))
readr::write_csv(refinery_weight_audit, file.path(results_dir, "refinery_weight_audit.csv"))
readr::write_csv(refinery_date_audit, file.path(results_dir, "refinery_date_audit.csv"))
