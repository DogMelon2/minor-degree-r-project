# Phase 2: reproducible cleaning of Refinery.csv.
#
# The raw CSV in data/raw is read only and never overwritten. Persian/Iranian
# calendar conversion is intentionally not attempted: the installed date tools
# parse Gregorian dates, while the available project materials do not establish
# a reliable conversion method for these source labels.

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))

raw_path <- file.path("data", "raw", "Refinery.csv")
processed_path <- file.path("data", "processed", "refinery_clean.csv")
summary_path <- file.path("outputs", "results", "refinery_cleaning_summary.csv")

stopifnot(file.exists(raw_path))
stopifnot(dir.exists(dirname(processed_path)), dir.exists(dirname(summary_path)))

# Read all source columns as text first. This protects the raw date labels from
# automatic Gregorian parsing. Numeric analysis-copy fields are created below.
refinery_raw <- readr::read_csv(
  raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

original_names <- names(refinery_raw)
refinery_clean <- janitor::clean_names(refinery_raw)
column_name_map <- tibble(
  original_column = original_names,
  cleaned_column = names(refinery_clean)
)

make_signature <- function(data) {
  encoded_columns <- lapply(data, function(column) {
    paste(nchar(as.character(column)), as.character(column), sep = ":")
  })
  do.call(paste, c(encoded_columns, sep = "\r"))
}

extract_calendar_components <- function(date_text) {
  matched <- stringr::str_match(date_text, "^([0-9]{4})/([0-9]{2})/([0-9]{2})$")
  year <- suppressWarnings(as.integer(matched[, 2]))
  month <- suppressWarnings(as.integer(matched[, 3]))
  day <- suppressWarnings(as.integer(matched[, 4]))
  valid_shape_and_broad_range <- !is.na(year) & month >= 1L & month <= 12L & day >= 1L & day <= 31L
  tibble(year = year, month = month, day = day, valid_shape_and_broad_range = valid_shape_and_broad_range)
}

origin_components <- extract_calendar_components(refinery_clean$origin_departure_date)
destination_components <- extract_calendar_components(refinery_clean$destination_arrival_date)

# Every source field is retained. No columns are removed because all seven are
# operational variables rather than demonstrated constant metadata.
source_signature <- make_signature(refinery_raw)
source_signature_counts <- table(source_signature)

refinery_clean <- refinery_clean |>
  mutate(
    raw_row_number = row_number() + 1L,
    # Weight units and physical meaning are unknown. These are numeric copies
    # of the source fields only; they are not converted or named as oil volume.
    origin_net_weight = readr::parse_number(origin_net_weight),
    destination_net_weight = readr::parse_number(destination_net_weight),
    missing_weight_flag = is.na(origin_net_weight) | is.na(destination_net_weight),
    # This is a raw numerical subtraction with intentionally no gain/loss claim.
    weight_difference = destination_net_weight - origin_net_weight,
    weight_difference_sign = case_when(
      missing_weight_flag ~ "missing_weight",
      weight_difference > 0 ~ "positive_raw_difference",
      weight_difference == 0 ~ "zero_raw_difference",
      weight_difference < 0 ~ "negative_raw_difference"
    ),
    duplicate_group_size = as.integer(source_signature_counts[source_signature]),
    duplicate_flag = duplicate_group_size > 1L,
    origin_calendar_year = origin_components$year,
    origin_calendar_month = origin_components$month,
    origin_calendar_day = origin_components$day,
    destination_calendar_year = destination_components$year,
    destination_calendar_month = destination_components$month,
    destination_calendar_day = destination_components$day,
    # This flag verifies only label shape/component ranges; it does not validate
    # a particular calendar or create a Gregorian date.
    date_format_flag = case_when(
      origin_components$valid_shape_and_broad_range & destination_components$valid_shape_and_broad_range ~
        "valid_yyyy_mm_dd_components",
      TRUE ~ "invalid_or_missing_date_label"
    ),
    # Fixed-width labels can be compared within their stated calendar notation,
    # but this is not a date conversion or elapsed-time calculation.
    date_label_order_flag = case_when(
      date_format_flag != "valid_yyyy_mm_dd_components" ~ "unavailable",
      destination_arrival_date >= origin_departure_date ~ "destination_label_after_or_equal_origin",
      TRUE ~ "destination_label_before_origin"
    )
  )

missing_by_column <- refinery_clean |>
  summarise(across(everything(), ~ sum(is.na(.x) | (is.character(.x) & .x == "")))) |>
  pivot_longer(everything(), names_to = "column", values_to = "missing_count")

exact_duplicate_groups <- sum(source_signature_counts > 1L)
duplicate_rows_beyond_first <- sum(source_signature_counts[source_signature_counts > 1L] - 1L)
duplicate_flagged_rows <- sum(refinery_clean$duplicate_flag)

# Phase 1 structural validation: stop rather than create a silently altered
# processed dataset if the audited source structure is not reproduced.
expected_input_rows <- 264L
expected_input_columns <- 7L
expected_refineries <- 4L
expected_types <- 4L
expected_shifts <- 3L
expected_duplicate_groups <- 1L
expected_duplicate_rows_beyond_first <- 1L
expected_weight_difference_range <- c(-510, 440)

if (nrow(refinery_raw) != expected_input_rows || ncol(refinery_raw) != expected_input_columns) {
  stop("Raw Refinery structure differs from the Phase 1 audit.")
}
if (nrow(refinery_clean) != nrow(refinery_raw)) {
  stop("Cleaning changed the number of observations; no row removal is allowed.")
}
if (n_distinct(refinery_clean$refinery) != expected_refineries ||
    n_distinct(refinery_clean$type) != expected_types ||
    n_distinct(refinery_clean$work_shift) != expected_shifts) {
  stop("Refinery/type/work-shift counts differ from the Phase 1 audit.")
}
if (exact_duplicate_groups != expected_duplicate_groups ||
    duplicate_rows_beyond_first != expected_duplicate_rows_beyond_first ||
    duplicate_flagged_rows != 2L) {
  stop("Exact-duplicate structure differs from the Phase 1 audit.")
}
if (!identical(range(refinery_clean$weight_difference, na.rm = TRUE), expected_weight_difference_range)) {
  stop("Weight-difference range differs from the Phase 1 audit.")
}
if (any(refinery_clean$date_label_order_flag == "destination_label_before_origin")) {
  stop("Raw date-label ordering differs from the Phase 1 audit.")
}

mapping_summary <- column_name_map |>
  transmute(
    issue = "column_name_mapping",
    before = original_column,
    after = cleaned_column,
    action = "clean_name_and_retain",
    reason = "All source columns are retained in the analysis copy."
  )

refinery_cleaning_summary <- bind_rows(
  tibble(
    issue = c(
      "row_count", "column_count", "missing_values", "exact_duplicate_groups",
      "duplicate_rows_beyond_first", "duplicate_handling", "date_handling",
      "weight_handling", "quality_flags"
    ),
    before = c(
      nrow(refinery_raw), ncol(refinery_raw), sum(is.na(refinery_raw) | refinery_raw == ""),
      exact_duplicate_groups, duplicate_rows_beyond_first,
      "One exact duplicate group identified in Phase 1", "Raw YYYY/MM/DD labels",
      "Origin and destination net-weight text values", "No explicit quality flags"
    ),
    after = c(
      nrow(refinery_clean), ncol(refinery_clean), sum(missing_by_column$missing_count),
      exact_duplicate_groups, duplicate_rows_beyond_first,
      paste(duplicate_flagged_rows, "rows retained and flagged"),
      "Original labels retained; non-converted calendar components added",
      "Numeric copies retained; raw numerical difference and sign added",
      "Duplicate, missing-weight, date-format, date-label-order, and difference-sign flags added"
    ),
    action = c(
      "no_rows_removed", "retain_all_source_columns_and_add_audit_fields", "preserve_missingness",
      "none_removed", "none_removed", "retain_and_flag",
      "no_Gregorian_conversion", "no_unit_conversion", "add_nonsemantic_audit_flags"
    ),
    reason = c(
      "Phase 1 audited 264 physical observations.",
      "All seven raw fields are retained; derived audit fields are appended.",
      "No missing source values are replaced or imputed.",
      "Exact duplicate evidence is preserved for provenance.",
      "Rows are not silently deleted.",
      "Available evidence cannot establish that the exact duplicate is accidental.",
      "Installed tools do not safely convert Persian-calendar labels without an established conversion method.",
      "Weight unit and physical interpretation are unknown.",
      "Flags support later filtering without making unsupported physical claims."
    )
  ),
  mapping_summary
)

# Only the requested processed copy and summary are written. The raw CSV remains untouched.
readr::write_csv(refinery_clean, processed_path, na = "")
readr::write_csv(refinery_cleaning_summary, summary_path, na = "")

cat("Refinery cleaning completed; raw CSV was not modified.\n")
cat("Input rows:", nrow(refinery_raw), " Output rows:", nrow(refinery_clean), "\n")
cat("Input columns:", ncol(refinery_raw), " Output columns:", ncol(refinery_clean), "\n")
cat("Missing values in output:", sum(missing_by_column$missing_count), "\n")
cat("Exact duplicate groups:", exact_duplicate_groups, " Flagged rows:", duplicate_flagged_rows, "\n")
cat("Unique refineries/types/shifts:", n_distinct(refinery_clean$refinery), "/",
    n_distinct(refinery_clean$type), "/", n_distinct(refinery_clean$work_shift), "\n")
cat("Raw date-label range:", min(refinery_clean$origin_departure_date), "to",
    max(refinery_clean$destination_arrival_date), " (not converted)\n")
cat("Weight range (origin):", min(refinery_clean$origin_net_weight), "to",
    max(refinery_clean$origin_net_weight), "\n")
cat("Weight range (destination):", min(refinery_clean$destination_net_weight), "to",
    max(refinery_clean$destination_net_weight), "\n")
cat("Weight-difference range:", paste(range(refinery_clean$weight_difference), collapse = " to "),
    " (raw numerical difference; unit/meaning unknown)\n")
