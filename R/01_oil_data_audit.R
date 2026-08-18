# Phase 1: read-only forensic audit of oil_production_statistics.csv
#
# This script deliberately does not alter the raw dataset. It only reads the
# CSV, profiles its structure, and writes audit tables to outputs/results/.
# In particular, it does not remove duplicates, change values, infer units,
# aggregate production, or create a cleaned dataset.

options(stringsAsFactors = FALSE)

raw_path <- file.path("data", "raw", "oil_production_statistics.csv")
results_dir <- file.path("outputs", "results")

if (!file.exists(raw_path)) {
  stop("Raw data file not found at project-relative path: ", raw_path)
}

if (!dir.exists(results_dir)) {
  stop("Expected output directory not found: ", results_dir)
}

# The source file is Latin-1 encoded. Reading it does not modify it.
oil_raw <- read.csv(
  raw_path,
  fileEncoding = "latin1",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

key_columns <- c("country_name", "product", "flow", "year")
required_columns <- c(key_columns, "value")
missing_required <- setdiff(required_columns, names(oil_raw))

if (length(missing_required) > 0) {
  stop("Required columns are missing: ", paste(missing_required, collapse = ", "))
}

make_key <- function(data, columns) {
  do.call(paste, c(data[columns], sep = "\r"))
}

make_row_signature <- function(data) {
  encoded_columns <- lapply(data, function(column) {
    paste(nchar(as.character(column)), as.character(column), sep = ":")
  })
  do.call(paste, c(encoded_columns, sep = "\r"))
}

key_id <- make_key(oil_raw, key_columns)
key_frequency <- table(key_id)
key_count <- as.integer(key_frequency[key_id])

# One row per visible country-product-flow-year key. These counts are audit
# information only; they are not an analytical aggregation of oil values.
key_lookup <- oil_raw[!duplicated(key_id), key_columns, drop = FALSE]
key_lookup$number_of_records <- as.integer(key_frequency[key_id[!duplicated(key_id)]])

duplicate_key_audit <- key_lookup[key_lookup$number_of_records > 1, , drop = FALSE]
duplicate_key_audit <- duplicate_key_audit[
  order(
    duplicate_key_audit$country_name,
    duplicate_key_audit$product,
    duplicate_key_audit$flow,
    duplicate_key_audit$year
  ),
  , drop = FALSE
]

# Exact duplicates are evaluated across every visible raw column, not only the
# proposed analytical key. They are reported but intentionally retained in
# oil_raw because source provenance has not yet been resolved.
row_signature <- make_row_signature(oil_raw)
row_frequency <- table(row_signature)
exact_duplicate_signatures <- names(row_frequency[row_frequency > 1])
exact_duplicates <- oil_raw[row_signature %in% exact_duplicate_signatures, , drop = FALSE]
exact_duplicates$raw_row_number <- which(row_signature %in% exact_duplicate_signatures) + 1L
exact_duplicates$exact_duplicate_group_size <- as.integer(
  row_frequency[row_signature[row_signature %in% exact_duplicate_signatures]]
)
exact_duplicates <- exact_duplicates[
  order(
    exact_duplicates$country_name,
    exact_duplicates$product,
    exact_duplicates$flow,
    exact_duplicates$year,
    exact_duplicates$value,
    exact_duplicates$raw_row_number
  ),
  , drop = FALSE
]

# A conflicting key occurs when records share the proposed key but have more
# than one distinct visible value. The output preserves every distinct value
# and reports its frequency; no value is selected or changed.
values_by_key <- split(as.character(oil_raw$value), key_id)
conflicting_key_ids <- names(values_by_key)[
  vapply(values_by_key, function(values) length(unique(values)) > 1L, logical(1))
]
conflicting_records <- oil_raw[key_id %in% conflicting_key_ids, , drop = FALSE]
conflicting_value_id <- make_key(conflicting_records, c(key_columns, "value"))
conflicting_value_frequency <- table(conflicting_value_id)
conflicting_keys <- conflicting_records[!duplicated(conflicting_value_id),
                                        c(key_columns, "value"), drop = FALSE]
conflicting_keys$number_of_records <- as.integer(
  conflicting_value_frequency[conflicting_value_id[!duplicated(conflicting_value_id)]]
)
conflicting_keys <- conflicting_keys[
  order(
    conflicting_keys$country_name,
    conflicting_keys$product,
    conflicting_keys$flow,
    conflicting_keys$year,
    conflicting_keys$value
  ),
  , drop = FALSE
]

# The 2021 anomaly output contains every physical record whose visible key is
# repeated in 2021. For each record, it shows the labelled 2022 value(s) for
# the same country-product-flow and whether that exact value matches. This is
# a provenance check, not a correction or a year-over-year calculation.
repeated_2021_ids <- make_key(
  duplicate_key_audit[duplicate_key_audit$year == 2021, key_columns, drop = FALSE],
  key_columns
)
raw_2021_key_id <- key_id
anomaly_records <- oil_raw[raw_2021_key_id %in% repeated_2021_ids & oil_raw$year == 2021,
                           , drop = FALSE]
anomaly_records$raw_row_number <- which(
  raw_2021_key_id %in% repeated_2021_ids & oil_raw$year == 2021
) + 1L
anomaly_records$number_of_records <- key_count[
  raw_2021_key_id %in% repeated_2021_ids & oil_raw$year == 2021
]

values_2022 <- oil_raw[oil_raw$year == 2022, c("country_name", "product", "flow", "value")]
values_2022_id <- make_key(values_2022, c("country_name", "product", "flow"))
values_2022_by_series <- split(as.character(values_2022$value), values_2022_id)
anomaly_series_id <- make_key(anomaly_records, c("country_name", "product", "flow"))

anomaly_records$`2021_value` <- anomaly_records$value
anomaly_records$`2022_value` <- vapply(anomaly_series_id, function(series_id) {
  values <- values_2022_by_series[[series_id]]
  if (is.null(values)) NA_character_ else paste(unique(values), collapse = " | ")
}, character(1))
anomaly_records$match <- mapply(
  function(value_2021, series_id) {
    values <- values_2022_by_series[[series_id]]
    !is.null(values) && as.character(value_2021) %in% values
  },
  anomaly_records$`2021_value`,
  anomaly_series_id
)
anomaly_records$is_consumption_pattern <- anomaly_records$flow == "Consumption Pattern"

oil_2021_anomalies <- anomaly_records[c(
  "country_name", "product", "flow", "year", "2021_value", "2022_value",
  "match", "number_of_records", "is_consumption_pattern", "raw_row_number"
)]
oil_2021_anomalies <- oil_2021_anomalies[
  order(
    oil_2021_anomalies$country_name,
    oil_2021_anomalies$product,
    oil_2021_anomalies$flow,
    oil_2021_anomalies$raw_row_number
  ),
  , drop = FALSE
]

missing_by_column <- colSums(is.na(oil_raw) | oil_raw == "")
negative_values <- sum(oil_raw$value < 0, na.rm = TRUE)
zero_values <- sum(oil_raw$value == 0, na.rm = TRUE)
exact_duplicate_groups <- length(exact_duplicate_signatures)
duplicate_rows_beyond_first <- sum(row_frequency[row_frequency > 1] - 1L)
conflicting_duplicate_keys <- length(conflicting_key_ids)
repeated_2021_keys <- sum(duplicate_key_audit$year == 2021)

data_quality_summary <- data.frame(
  issue = c(
    "missing_values",
    "exact_duplicate_groups",
    "duplicate_rows_beyond_first",
    "conflicting_duplicate_keys",
    "repeated_2021_keys",
    "negative_values",
    "zero_values"
  ),
  count = c(
    sum(missing_by_column),
    exact_duplicate_groups,
    duplicate_rows_beyond_first,
    conflicting_duplicate_keys,
    repeated_2021_keys,
    negative_values,
    zero_values
  ),
  description = c(
    "Total blank or NA cells across all visible columns.",
    "Groups of rows identical across every visible raw column.",
    "Rows beyond the first row in each exact-duplicate group.",
    "Proposed keys with more than one distinct visible value.",
    "Repeated country-product-flow-year keys labelled as 2021.",
    "Rows whose visible value is less than zero; meaning is not inferred.",
    "Rows whose visible value equals zero; meaning is not inferred."
  ),
  stringsAsFactors = FALSE
)

# Only audit outputs are written. The raw input file is never written to.
write.csv(data_quality_summary,
          file.path(results_dir, "oil_data_audit_summary.csv"),
          row.names = FALSE)
write.csv(duplicate_key_audit,
          file.path(results_dir, "oil_duplicate_key_audit.csv"),
          row.names = FALSE)
write.csv(exact_duplicates,
          file.path(results_dir, "oil_exact_duplicates.csv"),
          row.names = FALSE)
write.csv(conflicting_keys,
          file.path(results_dir, "oil_conflicting_keys.csv"),
          row.names = FALSE)
write.csv(oil_2021_anomalies,
          file.path(results_dir, "oil_2021_anomalies.csv"),
          row.names = FALSE)

cat("Oil data audit completed (raw CSV was read only).\n\n")
cat("Rows:", nrow(oil_raw), "\n")
cat("Columns:", ncol(oil_raw), "\n")
cat("Column names:", paste(names(oil_raw), collapse = ", "), "\n")
cat("Data types:\n")
print(vapply(oil_raw, class, character(1)))
cat("Countries:", length(unique(oil_raw$country_name)), "\n")
cat("Products:", length(unique(oil_raw$product)), "\n")
cat("Flows:", length(unique(oil_raw$flow)), "\n")
cat("Years:", length(unique(oil_raw$year)), "\n")
cat("Missing values by column:\n")
print(missing_by_column)
cat("Negative values:", negative_values, "\n")
cat("Zero values:", zero_values, "\n\n")

cat("Unique proposed keys:", length(key_frequency), "\n")
cat("Keys occurring once:", sum(key_frequency == 1L), "\n")
cat("Keys occurring twice:", sum(key_frequency == 2L), "\n")
cat("Keys occurring more than twice:", sum(key_frequency > 2L), "\n")
cat("Repeated keys written to: outputs/results/oil_duplicate_key_audit.csv\n")
cat("Exact duplicate groups:", exact_duplicate_groups, "\n")
cat("Duplicate rows beyond first:", duplicate_rows_beyond_first, "\n")
cat("Conflicting duplicate keys:", conflicting_duplicate_keys, "\n")
cat("Repeated 2021 keys:", repeated_2021_keys, "\n")
cat("2021 anomaly records written to: outputs/results/oil_2021_anomalies.csv\n")
