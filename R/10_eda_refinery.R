# Phase 4B: descriptive EDA of the refinery analysis copy.
#
# This script reads the existing cleaned dataset only. It does not modify raw
# data, the cleaned dataset, or any earlier project scripts. Weight values are
# reported without a unit or physical interpretation; their difference is only
# destination net weight minus origin net weight. Source-calendar labels and
# components are retained without Gregorian conversion.

suppressPackageStartupMessages(library(tidyverse))

input_path <- file.path("data", "processed", "refinery_clean.csv")
figure_dir <- file.path("outputs", "figures", "refinery")
table_dir <- file.path("outputs", "tables", "refinery")

stopifnot(file.exists(input_path))
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

refinery <- readr::read_csv(
  input_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  mutate(
    raw_row_number = readr::parse_integer(raw_row_number),
    origin_net_weight = readr::parse_double(origin_net_weight),
    destination_net_weight = readr::parse_double(destination_net_weight),
    missing_weight_flag = as.logical(missing_weight_flag),
    weight_difference = readr::parse_double(weight_difference),
    duplicate_group_size = readr::parse_integer(duplicate_group_size),
    duplicate_flag = as.logical(duplicate_flag),
    origin_calendar_year = readr::parse_integer(origin_calendar_year),
    origin_calendar_month = readr::parse_integer(origin_calendar_month),
    origin_calendar_day = readr::parse_integer(origin_calendar_day),
    destination_calendar_year = readr::parse_integer(destination_calendar_year),
    destination_calendar_month = readr::parse_integer(destination_calendar_month),
    destination_calendar_day = readr::parse_integer(destination_calendar_day)
  )

# Validation is intentionally based on the retained cleaned observations.
expected_rows <- 264L
if (nrow(refinery) != expected_rows) {
  stop("The cleaned refinery row count differs from the Phase 2 audited total.")
}
total_rows <- nrow(refinery)
if (sum(refinery$duplicate_flag, na.rm = TRUE) != 2L ||
    n_distinct(refinery$duplicate_group_size[refinery$duplicate_flag]) != 1L ||
    unique(refinery$duplicate_group_size[refinery$duplicate_flag]) != 2L) {
  stop("The expected two-row exact duplicate group is not retained and flagged.")
}

write_table <- function(data, filename) {
  readr::write_csv(data, file.path(table_dir, filename), na = "")
}

save_plot <- function(plot, filename, width = 8, height = 5) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, filename), plot = plot,
    width = width, height = height, dpi = 300, bg = "white"
  )
}

theme_refinery <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey30"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Directly observed category frequencies; all retained rows, including the
# duplicate group, remain in the denominators.
refinery_counts <- refinery |>
  count(refinery, name = "observation_count", sort = TRUE) |>
  mutate(percentage_of_rows = 100 * observation_count / total_rows)

product_type_distribution <- refinery |>
  count(type, name = "observation_count", sort = TRUE) |>
  mutate(percentage_of_rows = 100 * observation_count / total_rows)

work_shift_distribution <- refinery |>
  count(work_shift, name = "observation_count", sort = TRUE) |>
  mutate(percentage_of_rows = 100 * observation_count / total_rows)

# Descriptive summaries of numeric copies. No weight unit is inferred.
net_weight_summary <- refinery |>
  select(origin_net_weight, destination_net_weight) |>
  pivot_longer(everything(), names_to = "weight_variable", values_to = "net_weight") |>
  group_by(weight_variable) |>
  summarise(
    observations = n(),
    non_missing = sum(!is.na(net_weight)),
    missing = sum(is.na(net_weight)),
    minimum = min(net_weight, na.rm = TRUE),
    q1 = quantile(net_weight, 0.25, na.rm = TRUE, names = FALSE),
    median = median(net_weight, na.rm = TRUE),
    mean = mean(net_weight, na.rm = TRUE),
    q3 = quantile(net_weight, 0.75, na.rm = TRUE, names = FALSE),
    maximum = max(net_weight, na.rm = TRUE),
    standard_deviation = sd(net_weight, na.rm = TRUE),
    .groups = "drop"
  )

# This is a raw numerical subtraction only, with no physical interpretation.
weight_difference_summary <- refinery |>
  summarise(
    observations = n(),
    non_missing = sum(!is.na(weight_difference)),
    missing = sum(is.na(weight_difference)),
    minimum = min(weight_difference, na.rm = TRUE),
    q1 = quantile(weight_difference, 0.25, na.rm = TRUE, names = FALSE),
    median = median(weight_difference, na.rm = TRUE),
    mean = mean(weight_difference, na.rm = TRUE),
    q3 = quantile(weight_difference, 0.75, na.rm = TRUE, names = FALSE),
    maximum = max(weight_difference, na.rm = TRUE),
    standard_deviation = sd(weight_difference, na.rm = TRUE),
    negative_raw_difference_count = sum(weight_difference < 0, na.rm = TRUE),
    zero_raw_difference_count = sum(weight_difference == 0, na.rm = TRUE),
    positive_raw_difference_count = sum(weight_difference > 0, na.rm = TRUE)
  )

# Source-calendar components become a display label only; they are not dates.
source_calendar_activity <- refinery |>
  count(origin_calendar_year, origin_calendar_month, name = "observation_count") |>
  arrange(origin_calendar_year, origin_calendar_month) |>
  mutate(
    source_calendar_year_month = sprintf("%04d/%02d", origin_calendar_year, origin_calendar_month),
    percentage_of_rows = 100 * observation_count / total_rows
  ) |>
  select(source_calendar_year_month, origin_calendar_year, origin_calendar_month,
         observation_count, percentage_of_rows)

# Every flagged duplicate row is shown; no record is removed or collapsed.
exact_duplicate_analysis <- refinery |>
  filter(duplicate_flag) |>
  arrange(duplicate_group_size, raw_row_number) |>
  transmute(
    raw_row_number, duplicate_flag, duplicate_group_size, refinery, type,
    work_shift, origin_departure_date, origin_net_weight,
    destination_arrival_date, destination_net_weight, weight_difference
  )

write_table(refinery_counts, "refinery_observation_counts.csv")
write_table(product_type_distribution, "product_type_distribution.csv")
write_table(work_shift_distribution, "work_shift_distribution.csv")
write_table(net_weight_summary, "net_weight_summary.csv")
write_table(weight_difference_summary, "weight_difference_summary.csv")
write_table(source_calendar_activity, "source_calendar_year_month_activity.csv")
write_table(exact_duplicate_analysis, "exact_duplicate_analysis.csv")

plot_refinery_counts <- ggplot(refinery_counts, aes(x = reorder(refinery, observation_count), y = observation_count)) +
  geom_col(fill = "#1B6CA8") + coord_flip() +
  labs(title = "Retained observations by refinery label", x = "Refinery label", y = "Observation count") +
  theme_refinery

plot_product_types <- ggplot(product_type_distribution, aes(x = reorder(type, observation_count), y = observation_count)) +
  geom_col(fill = "#2A9D8F") + coord_flip() +
  labs(title = "Retained observations by product/type label", x = "Product/type label", y = "Observation count") +
  theme_refinery

plot_work_shifts <- ggplot(work_shift_distribution, aes(x = reorder(work_shift, observation_count), y = observation_count)) +
  geom_col(fill = "#E76F51") + coord_flip() +
  labs(title = "Retained observations by work-shift label", x = "Work-shift label", y = "Observation count") +
  theme_refinery

plot_origin_weight <- ggplot(refinery, aes(x = origin_net_weight)) +
  geom_histogram(bins = 25, fill = "#457B9D", colour = "white") +
  labs(title = "Origin net-weight distribution", subtitle = "Recorded numeric values; unit unknown", x = "Origin net weight", y = "Observation count") +
  theme_refinery

plot_destination_weight <- ggplot(refinery, aes(x = destination_net_weight)) +
  geom_histogram(bins = 25, fill = "#6A4C93", colour = "white") +
  labs(title = "Destination net-weight distribution", subtitle = "Recorded numeric values; unit unknown", x = "Destination net weight", y = "Observation count") +
  theme_refinery

weight_limits <- range(c(refinery$origin_net_weight, refinery$destination_net_weight), na.rm = TRUE)
plot_weight_scatter <- ggplot(refinery, aes(x = origin_net_weight, y = destination_net_weight)) +
  geom_abline(slope = 1, intercept = 0, colour = "#D62828", linetype = "dashed", linewidth = 0.8) +
  geom_point(alpha = 0.65, colour = "#1D3557", size = 2) +
  coord_equal(xlim = weight_limits, ylim = weight_limits) +
  labs(title = "Origin versus destination net weight", subtitle = "Dashed line: equal recorded values; unit unknown", x = "Origin net weight", y = "Destination net weight") +
  theme_refinery

plot_weight_difference <- ggplot(refinery, aes(x = weight_difference)) +
  geom_histogram(bins = 25, fill = "#F4A261", colour = "white") +
  geom_vline(xintercept = 0, colour = "#D62828", linetype = "dashed", linewidth = 0.8) +
  labs(title = "Raw weight-difference distribution", subtitle = "Destination net weight minus origin net weight; no physical interpretation", x = "Raw weight difference", y = "Observation count") +
  theme_refinery

plot_source_calendar <- ggplot(source_calendar_activity, aes(x = source_calendar_year_month, y = observation_count, group = 1)) +
  geom_line(colour = "#264653", linewidth = 0.9) +
  geom_point(colour = "#264653", size = 2.5) +
  labs(title = "Activity by source-calendar year/month", subtitle = "Uses retained source-calendar components; no Gregorian conversion", x = "Source-calendar year/month", y = "Observation count") +
  theme_refinery + theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(plot_refinery_counts, "01_refinery_observation_counts.png")
save_plot(plot_product_types, "02_product_type_distribution.png")
save_plot(plot_work_shifts, "03_work_shift_distribution.png")
save_plot(plot_origin_weight, "04_origin_net_weight_distribution.png")
save_plot(plot_destination_weight, "05_destination_net_weight_distribution.png")
save_plot(plot_weight_scatter, "06_origin_vs_destination_net_weight.png", width = 7, height = 6)
save_plot(plot_weight_difference, "07_raw_weight_difference_distribution.png")
save_plot(plot_source_calendar, "08_source_calendar_year_month_activity.png")

expected_figures <- c(
  "01_refinery_observation_counts.png", "02_product_type_distribution.png",
  "03_work_shift_distribution.png", "04_origin_net_weight_distribution.png",
  "05_destination_net_weight_distribution.png", "06_origin_vs_destination_net_weight.png",
  "07_raw_weight_difference_distribution.png", "08_source_calendar_year_month_activity.png"
)
expected_tables <- c(
  "refinery_observation_counts.csv", "product_type_distribution.csv",
  "work_shift_distribution.csv", "net_weight_summary.csv",
  "weight_difference_summary.csv", "source_calendar_year_month_activity.csv",
  "exact_duplicate_analysis.csv"
)
if (!all(file.exists(file.path(figure_dir, expected_figures))) ||
    !all(file.exists(file.path(table_dir, expected_tables)))) {
  stop("Expected Phase 4B EDA outputs were not generated.")
}
if (sum(refinery_counts$observation_count) != nrow(refinery) ||
    sum(product_type_distribution$observation_count) != nrow(refinery) ||
    sum(work_shift_distribution$observation_count) != nrow(refinery) ||
    sum(source_calendar_activity$observation_count) != nrow(refinery) ||
    nrow(exact_duplicate_analysis) != 2L) {
  stop("EDA output validation failed: retained-row or duplicate totals are inconsistent.")
}

cat("Phase 4B refinery EDA completed.\n")
cat("Retained rows:", nrow(refinery), "\n")
cat("Figures generated:", length(expected_figures), "\n")
cat("Tables generated:", length(expected_tables), "\n")
cat("Exact duplicate rows retained and flagged:", nrow(exact_duplicate_analysis), "\n")
