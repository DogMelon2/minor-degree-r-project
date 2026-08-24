# Phase 4C: data-quality-aware descriptive EDA for the audited oil dataset.
# The raw CSV is read only. No values are reconciled, removed, aggregated, or
# assigned a unit. All numeric summaries treat `value` as a raw reported field.

suppressPackageStartupMessages(library(tidyverse))

raw_path <- file.path("data", "raw", "oil_production_statistics.csv")
figure_dir <- file.path("outputs", "figures", "oil")
table_dir <- file.path("outputs", "tables", "oil")
stopifnot(file.exists(raw_path))
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

oil <- readr::read_csv(
  raw_path, locale = readr::locale(encoding = "Latin1"),
  col_types = readr::cols(
    country_name = readr::col_character(), type = readr::col_character(),
    product = readr::col_character(), flow = readr::col_character(),
    year = readr::col_integer(), value = readr::col_double()
  ), show_col_types = FALSE
) |>
  mutate(raw_row_number = row_number())

key_columns <- c("country_name", "product", "flow", "year")
expected_years <- c(2021L, 2022L, 2023L)
if (nrow(oil) != 2376L || n_distinct(oil$country_name) != 36L ||
    n_distinct(oil$product) != 11L || n_distinct(oil$flow) != 4L ||
    !identical(sort(unique(oil$year)), expected_years) || any(is.na(oil))) {
  stop("The audited oil dataset structure differs from the established audit.")
}

write_table <- function(data, filename) readr::write_csv(data, file.path(table_dir, filename), na = "")
save_plot <- function(plot, filename, width = 8, height = 5) {
  ggplot2::ggsave(file.path(figure_dir, filename), plot = plot, width = width,
                  height = height, dpi = 300, bg = "white")
}
theme_oil <- theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey30"),
  panel.grid.minor = element_blank(), axis.title = element_text(face = "bold")
)

# Key and exact-row checks reproduce the existing audit and retain every row.
key_counts <- oil |> count(across(all_of(key_columns)), name = "number_of_records")
repeated_keys <- key_counts |> filter(number_of_records > 1L)
repeated_2021_keys <- repeated_keys |> filter(year == 2021L)
row_signature <- do.call(paste, c(lapply(oil[c("country_name", "type", "product", "flow", "year", "value")], function(x) {
  paste(nchar(as.character(x)), as.character(x), sep = ":")
}), sep = "\r"))
exact_frequency <- table(row_signature)
exact_duplicate_groups <- sum(exact_frequency > 1L)
conflicting_keys <- oil |>
  group_by(across(all_of(key_columns))) |>
  summarise(number_of_records = n(), distinct_raw_values = n_distinct(value), .groups = "drop") |>
  filter(distinct_raw_values > 1L)

values_2022 <- oil |>
  filter(year == 2022L) |>
  select(country_name, product, flow, value_2022 = value)
anomaly_records <- oil |>
  semi_join(repeated_2021_keys, by = key_columns) |>
  filter(year == 2021L) |>
  left_join(values_2022, by = c("country_name", "product", "flow")) |>
  mutate(matches_labelled_2022_value = value == value_2022,
         is_consumption_pattern = flow == "Consumption Pattern")
consumption_match_series <- anomaly_records |>
  filter(is_consumption_pattern, matches_labelled_2022_value) |>
  count(product, flow, name = "matching_2021_records") |>
  arrange(product)

if (nrow(repeated_2021_keys) != 540L || nrow(conflicting_keys) != 496L ||
    exact_duplicate_groups != 44L || sum(oil$value < 0) != 82L ||
    sum(oil$value == 0) != 223L || nrow(consumption_match_series) != 6L) {
  stop("Oil data-quality results differ from the established audit.")
}

# These tables contain only record counts and key/data-quality frequencies.
oil_record_structure <- oil |> count(year, name = "observation_count") |>
  mutate(percentage_of_all_records = 100 * observation_count / nrow(oil))
oil_country_year_coverage <- oil |> count(country_name, year, name = "observation_count") |>
  arrange(country_name, year)
oil_product_structure <- oil |> count(product, name = "observation_count", sort = TRUE) |>
  mutate(percentage_of_all_records = 100 * observation_count / nrow(oil))
oil_flow_structure <- oil |> count(flow, name = "observation_count", sort = TRUE) |>
  mutate(percentage_of_all_records = 100 * observation_count / nrow(oil))
oil_product_flow_structure <- oil |> count(product, flow, year, name = "observation_count") |>
  arrange(product, flow, year)
key_frequency_distribution <- key_counts |> count(number_of_records, name = "number_of_keys") |>
  arrange(number_of_records)

oil_duplicate_summary <- bind_rows(
  tibble(metric = "physical_records", count = nrow(oil)),
  tibble(metric = "unique_country_product_flow_year_keys", count = nrow(key_counts)),
  tibble(metric = "keys_occurring_once", count = sum(key_counts$number_of_records == 1L)),
  tibble(metric = "repeated_keys", count = nrow(repeated_keys)),
  tibble(metric = "physical_records_in_repeated_keys", count = sum(repeated_keys$number_of_records)),
  tibble(metric = "repeated_2021_keys", count = nrow(repeated_2021_keys)),
  tibble(metric = "exact_duplicate_groups", count = exact_duplicate_groups),
  tibble(metric = "physical_rows_in_exact_duplicate_groups", count = sum(exact_frequency[exact_frequency > 1L])),
  tibble(metric = "duplicate_rows_beyond_first", count = sum(exact_frequency[exact_frequency > 1L] - 1L)),
  key_frequency_distribution |> transmute(metric = paste0("keys_with_", number_of_records, "_record", if_else(number_of_records == 1L, "", "s")), count = number_of_keys)
)
oil_conflicting_value_summary <- conflicting_keys |> count(year, flow, name = "conflicting_key_count") |>
  arrange(year, flow)
oil_2021_anomaly_summary <- bind_rows(
  tibble(summary_type = "overall", product = NA_character_, flow = NA_character_,
         metric = c("repeated_2021_keys", "physical_2021_records_in_repeated_keys", "conflicting_keys", "exact_duplicate_groups"),
         count = c(nrow(repeated_2021_keys), nrow(anomaly_records), nrow(conflicting_keys), exact_duplicate_groups)),
  consumption_match_series |> transmute(summary_type = "consumption_pattern_2021_to_2022_raw_value_match", product, flow,
                                         metric = "matching_2021_records", count = matching_2021_records)
)
summarise_value <- function(data, label) tibble(
  year = label, observations = nrow(data), minimum = min(data$value),
  q1 = quantile(data$value, .25, names = FALSE), median = median(data$value),
  mean = mean(data$value), q3 = quantile(data$value, .75, names = FALSE),
  maximum = max(data$value), negative_value_count = sum(data$value < 0),
  zero_value_count = sum(data$value == 0)
)
oil_value_distribution_summary <- bind_rows(
  summarise_value(oil, "overall"),
  map_dfr(sort(unique(oil$year)), ~ summarise_value(filter(oil, year == .x), as.character(.x)))
)

write_table(oil_record_structure, "oil_record_structure.csv")
write_table(oil_country_year_coverage, "oil_country_year_coverage.csv")
write_table(oil_product_structure, "oil_product_structure.csv")
write_table(oil_flow_structure, "oil_flow_structure.csv")
write_table(oil_product_flow_structure, "oil_product_flow_structure.csv")
write_table(oil_duplicate_summary, "oil_duplicate_summary.csv")
write_table(oil_conflicting_value_summary, "oil_conflicting_value_summary.csv")
write_table(oil_2021_anomaly_summary, "oil_2021_anomaly_summary.csv")
write_table(oil_value_distribution_summary, "oil_value_distribution_summary.csv")

plot_year <- ggplot(oil_record_structure, aes(factor(year), observation_count)) +
  geom_col(fill = "#1B6CA8") + labs(title = "Oil dataset records by year", subtitle = "Record counts only; not production quantities", x = "Year", y = "Recorded observations") + theme_oil
country_order <- oil_country_year_coverage |> group_by(country_name) |>
  summarise(total_records = sum(observation_count), .groups = "drop") |>
  arrange(total_records, country_name) |> pull(country_name)
plot_country <- ggplot(oil_country_year_coverage, aes(factor(year), factor(country_name, levels = country_order), fill = observation_count)) +
  geom_tile(colour = "white", linewidth = .2) + scale_fill_viridis_c(option = "C") +
  labs(title = "Country-by-year record coverage", subtitle = "Fill is number of recorded observations, not production", x = "Year", y = "Country", fill = "Records") + theme_oil
plot_product <- ggplot(oil_product_structure, aes(reorder(product, observation_count), observation_count)) +
  geom_col(fill = "#2A9D8F") + coord_flip() + labs(title = "Records by product label", subtitle = "Record counts only; categories must not be summed", x = "Product label", y = "Recorded observations") + theme_oil
plot_flow <- ggplot(oil_flow_structure, aes(reorder(flow, observation_count), observation_count)) +
  geom_col(fill = "#E76F51") + coord_flip() + labs(title = "Records by flow label", subtitle = "Record counts only; flow meanings are incompletely documented", x = "Flow label", y = "Recorded observations") + theme_oil
product_flow_all <- oil |> count(product, flow, name = "observation_count")
plot_product_flow <- ggplot(product_flow_all, aes(flow, product, fill = observation_count)) +
  geom_tile(colour = "white") + geom_text(aes(label = observation_count), size = 3) + scale_fill_viridis_c(option = "D") +
  labs(title = "Product-by-flow record structure", subtitle = "Cells are record counts, not numeric values or production", x = "Flow label", y = "Product label", fill = "Records") + theme_oil
plot_value <- ggplot(oil, aes(value)) + geom_histogram(bins = 40, fill = "#6A4C93", colour = "white") + facet_wrap(~year, scales = "free_y") +
  labs(title = "Distribution of the raw reported numeric field", subtitle = "Raw value only: unit and semantic meaning are unknown", x = "Raw reported value", y = "Recorded observations") + theme_oil
plot_duplicate <- ggplot(key_frequency_distribution |> mutate(label = paste(number_of_records, if_else(number_of_records == 1L, "record", "records"))), aes(label, number_of_keys)) +
  geom_col(fill = "#D62828") + geom_text(aes(label = number_of_keys), vjust = -.35) +
  labs(title = "Proposed key frequency structure", subtitle = "Key = country + product + flow + year; repeated keys are retained", x = "Records per proposed key", y = "Number of proposed keys") + theme_oil
plot_anomaly <- ggplot(consumption_match_series, aes(reorder(product, matching_2021_records), matching_2021_records)) +
  geom_col(fill = "#7B2CBF") + coord_flip() + labs(title = "2021 anomaly: Consumption Pattern matches to labelled 2022 values", subtitle = "Six product series; equality is documented only, not used to correct any record", x = "Product label", y = "Matching 2021 records") + theme_oil

save_plot(plot_year, "01_oil_records_by_year.png")
save_plot(plot_country, "02_oil_country_record_coverage.png", 8, 9)
save_plot(plot_product, "03_oil_product_record_counts.png")
save_plot(plot_flow, "04_oil_flow_record_counts.png")
save_plot(plot_product_flow, "05_oil_product_flow_heatmap.png", 9, 6)
save_plot(plot_value, "06_oil_value_distribution.png", 9, 5)
save_plot(plot_duplicate, "07_oil_duplicate_key_structure.png")
save_plot(plot_anomaly, "08_oil_2021_anomaly_summary.png")

expected_figures <- c("01_oil_records_by_year.png", "02_oil_country_record_coverage.png", "03_oil_product_record_counts.png", "04_oil_flow_record_counts.png", "05_oil_product_flow_heatmap.png", "06_oil_value_distribution.png", "07_oil_duplicate_key_structure.png", "08_oil_2021_anomaly_summary.png")
expected_tables <- c("oil_record_structure.csv", "oil_country_year_coverage.csv", "oil_product_structure.csv", "oil_flow_structure.csv", "oil_product_flow_structure.csv", "oil_duplicate_summary.csv", "oil_conflicting_value_summary.csv", "oil_2021_anomaly_summary.csv", "oil_value_distribution_summary.csv")
if (!all(file.exists(file.path(figure_dir, expected_figures))) || !all(file.exists(file.path(table_dir, expected_tables)))) stop("Expected oil EDA outputs were not generated.")
if (sum(oil_record_structure$observation_count) != nrow(oil) || sum(oil_country_year_coverage$observation_count) != nrow(oil) || sum(oil_product_structure$observation_count) != nrow(oil) || sum(oil_flow_structure$observation_count) != nrow(oil) || sum(oil_product_flow_structure$observation_count) != nrow(oil)) stop("Record-count output totals do not equal the raw row count.")
cat("Phase 4C oil EDA completed without modifying the raw CSV.\nRows:", nrow(oil), "\nFigures:", length(expected_figures), "\nTables:", length(expected_tables), "\n")
