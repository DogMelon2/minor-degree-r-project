# Phase 4: Exploratory Data Analysis (EDA) for Container Transport
#
# Dataset: data/processed/container_transport_clean.csv
#
# Rules enforced:
#   - Tonnes and TEU are completely segregated. Never pooled or directly compared.
#   - UNIT_MULT is retained but not applied arithmetically.
#   - Missing observations (118 rows) remain NA; never treated as zero.
#   - Observation status categories (Normal, Provisional, Estimated, Break, Missing)
#     are explicitly distinguished.
#   - Exploratory and descriptive only; no causal claims or econometric forecasting.

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(scales))

# ---------------------------------------------------------------------------
# 1. Configuration, Directories & Pre-flight Validation
# ---------------------------------------------------------------------------
input_path  <- file.path("data", "processed", "container_transport_clean.csv")
fig_dir     <- file.path("outputs", "figures", "container")
tab_dir     <- file.path("outputs", "tables", "container")

if (!file.exists(input_path)) {
  stop("Cleaned container transport dataset not found at: ", input_path)
}

if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

cont <- readr::read_csv(input_path, show_col_types = FALSE)

# Structural validation
expected_rows <- 624L
expected_cols <- c(
  "ref_area", "reference_area", "unit_measure", "unit_of_measure",
  "transport_mode", "transport_mode_2", "time_period", "obs_value",
  "obs_status", "observation_status", "unit_mult", "unit_multiplier",
  "observation_is_missing", "observation_is_provisional",
  "observation_is_estimated", "observation_has_time_series_break"
)

if (nrow(cont) != expected_rows) {
  stop("Input row count ", nrow(cont), " != expected ", expected_rows)
}
if (!all(expected_cols %in% names(cont))) {
  stop("Missing expected columns in cleaned container dataset.")
}
if (sum(is.na(cont$obs_value)) != 118L) {
  stop("Missing observation count ", sum(is.na(cont$obs_value)), " != expected 118.")
}

cat("Pre-flight validation passed. Observations: ", nrow(cont), " (506 valid, 118 missing NA).\n")

# ---------------------------------------------------------------------------
# 2. Coverage Analysis & Coverage Summary Table + Heatmap
# ---------------------------------------------------------------------------
coverage_summary <- cont |>
  group_by(ref_area, reference_area) |>
  summarise(
    total_records = n(),
    valid_records = sum(!observation_is_missing),
    missing_records = sum(observation_is_missing),
    coverage_rate_pct = round(100 * valid_records / total_records, 1),
    mar_tonnes_valid = sum(transport_mode == "MAR" & unit_measure == "T" & !observation_is_missing),
    mar_tonnes_missing = sum(transport_mode == "MAR" & unit_measure == "T" & observation_is_missing),
    rail_tonnes_valid = sum(transport_mode == "RAIL" & unit_measure == "T" & !observation_is_missing),
    rail_tonnes_missing = sum(transport_mode == "RAIL" & unit_measure == "T" & observation_is_missing),
    mar_teu_valid = sum(transport_mode == "MAR" & unit_measure == "TEU" & !observation_is_missing),
    mar_teu_missing = sum(transport_mode == "MAR" & unit_measure == "TEU" & observation_is_missing),
    rail_teu_valid = sum(transport_mode == "RAIL" & unit_measure == "TEU" & !observation_is_missing),
    rail_teu_missing = sum(transport_mode == "RAIL" & unit_measure == "TEU" & observation_is_missing),
    coverage_category = case_when(
      coverage_rate_pct == 100 ~ "Full Coverage (100%)",
      coverage_rate_pct > 0    ~ "Partial Coverage",
      TRUE                     ~ "Zero Valid Records (100% Missing)"
    ),
    .groups = "drop"
  ) |>
  arrange(desc(coverage_rate_pct), reference_area)

readr::write_csv(coverage_summary, file.path(tab_dir, "container_coverage_summary.csv"))
cat("Saved: container_coverage_summary.csv (", nrow(coverage_summary), " reference areas)\n")

# Figure 01: Coverage Heatmap
status_palette <- c(
  "Normal value"                      = "#2b5c8f",
  "Provisional value"                 = "#4da2db",
  "Estimated value"                   = "#e69f00",
  "Time series break"                 = "#cc79a7",
  "Missing value; data cannot exist"  = "#d55e00"
)

area_order <- coverage_summary |>
  arrange(missing_records, reference_area) |>
  pull(reference_area)

heatmap_data <- cont |>
  mutate(
    reference_area = factor(reference_area, levels = rev(area_order)),
    facet_label = paste(transport_mode_2, "-", unit_of_measure)
  )

p1 <- ggplot(heatmap_data, aes(x = factor(time_period), y = reference_area, fill = observation_status)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ facet_label, nrow = 1) +
  scale_fill_manual(values = status_palette, name = "Observation Status") +
  labs(
    title = "Figure 1: Container Transport Reporting Coverage Matrix (2019–2022)",
    subtitle = "Reporting coverage across 49 reference areas by transport mode (Maritime vs. Rail) and unit (Tonnes vs. TEU)",
    x = "Observation Year",
    y = "Reference Area",
    caption = "Source: ContainerTransport dataset. Note: Missing values (M) predominantly reflect structural absence of maritime ports in landlocked areas."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "#444444"),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave(file.path(fig_dir, "01_container_coverage_heatmap.png"), p1, width = 12, height = 11, dpi = 300)
cat("Saved: 01_container_coverage_heatmap.png\n")
# ---------------------------------------------------------------------------
# 3. Annual Trends (Segregated by Unit) & Summary Table
# ---------------------------------------------------------------------------
annual_summary <- cont |>
  group_by(unit_measure, unit_of_measure, transport_mode, transport_mode_2, time_period) |>
  summarise(
    total_records = n(),
    valid_records = sum(!observation_is_missing),
    missing_records = sum(observation_is_missing),
    mean_obs_value = mean(obs_value, na.rm = TRUE),
    median_obs_value = median(obs_value, na.rm = TRUE),
    sd_obs_value = sd(obs_value, na.rm = TRUE),
    iqr_obs_value = IQR(obs_value, na.rm = TRUE),
    min_obs_value = suppressWarnings(min(obs_value, na.rm = TRUE)),
    max_obs_value = suppressWarnings(max(obs_value, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(across(c(min_obs_value, max_obs_value), ~ if_else(is.infinite(.x), NA_real_, .x))) |>
  arrange(unit_measure, transport_mode, time_period)

readr::write_csv(annual_summary, file.path(tab_dir, "container_annual_summary.csv"))
cat("Saved: container_annual_summary.csv\n")

# Figure 02: Annual Trend for Tonnes (T)
tonnes_data <- cont |> filter(unit_measure == "T", !observation_is_missing)

p2 <- ggplot(tonnes_data, aes(x = factor(time_period), y = obs_value, fill = transport_mode_2)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.6, alpha = 0.7) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10), labels = label_comma(), name = "Observed Volume (Tonnes, pseudo-log10 scale)") +
  scale_fill_manual(values = c("Maritime" = "#1f78b4", "Rail" = "#33a02c"), name = "Transport Mode") +
  labs(
    title = "Figure 2: Annual Distribution of Container Transport in Tonnes (2019–2022)",
    subtitle = "Annual volume distributions by mode for reference areas reporting in Tonnes",
    x = "Observation Year",
    caption = "Source: ContainerTransport dataset (Unit: Tonnes). Excludes 59 missing observation cells across Tonnes series."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "#444444"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "02_container_annual_trend_tonnes.png"), p2, width = 9, height = 6, dpi = 300)
cat("Saved: 02_container_annual_trend_tonnes.png\n")

# Figure 03: Annual Trend for TEU
teu_data <- cont |> filter(unit_measure == "TEU", !observation_is_missing)

p3 <- ggplot(teu_data, aes(x = factor(time_period), y = obs_value, fill = transport_mode_2)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.6, alpha = 0.7) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10), labels = label_comma(), name = "Observed Volume (TEU, pseudo-log10 scale)") +
  scale_fill_manual(values = c("Maritime" = "#ff7f00", "Rail" = "#6a3d9a"), name = "Transport Mode") +
  labs(
    title = "Figure 3: Annual Distribution of Container Transport in TEU (2019–2022)",
    subtitle = "Annual volume distributions by mode for reference areas reporting in TEU",
    x = "Observation Year",
    caption = "Source: ContainerTransport dataset (Unit: TEU - Twenty-foot Equivalent Units). Excludes 59 missing observation cells."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "#444444"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "03_container_annual_trend_teu.png"), p3, width = 9, height = 6, dpi = 300)
cat("Saved: 03_container_annual_trend_teu.png\n")

# ---------------------------------------------------------------------------
# 4. Transport-Mode Comparison (Within Units) & Mode Summary Table
# ---------------------------------------------------------------------------
mode_summary <- cont |>
  group_by(unit_measure, unit_of_measure, transport_mode, transport_mode_2) |>
  summarise(
    total_records = n(),
    valid_records = sum(!observation_is_missing),
    missing_records = sum(observation_is_missing),
    missing_pct = round(100 * missing_records / total_records, 1),
    mean_obs_value = mean(obs_value, na.rm = TRUE),
    median_obs_value = median(obs_value, na.rm = TRUE),
    sd_obs_value = sd(obs_value, na.rm = TRUE),
    iqr_obs_value = IQR(obs_value, na.rm = TRUE),
    min_obs_value = min(obs_value, na.rm = TRUE),
    max_obs_value = max(obs_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(unit_measure, transport_mode)

readr::write_csv(mode_summary, file.path(tab_dir, "container_mode_summary.csv"))
cat("Saved: container_mode_summary.csv\n")

# Figure 04: Transport Mode Comparison (Tonnes)
p4 <- ggplot(tonnes_data, aes(x = transport_mode_2, y = obs_value, fill = transport_mode_2)) +
  geom_violin(alpha = 0.4, color = "gray40", trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.size = 2, alpha = 0.8, color = "black") +
  geom_jitter(width = 0.1, alpha = 0.35, size = 1.8, color = "#1a1a1a") +
  scale_y_continuous(trans = pseudo_log_trans(base = 10), labels = label_comma(), name = "Observed Volume (Tonnes, pseudo-log10 scale)") +
  scale_fill_manual(values = c("Maritime" = "#1f78b4", "Rail" = "#33a02c"), name = "Transport Mode") +
  labs(
    title = "Figure 4: Comparison of Maritime vs. Rail Container Transport (Tonnes)",
    subtitle = "Volume distribution and density across all valid observations (2019–2022 pooled)",
    x = "Transport Mode",
    caption = "Source: ContainerTransport dataset (Unit: Tonnes). Maritime N = 121 valid; Rail N = 139 valid."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "#444444"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "04_container_mode_comparison_tonnes.png"), p4, width = 8, height = 6, dpi = 300)
cat("Saved: 04_container_mode_comparison_tonnes.png\n")

# Figure 05: Transport Mode Comparison (TEU)
p5 <- ggplot(teu_data, aes(x = transport_mode_2, y = obs_value, fill = transport_mode_2)) +
  geom_violin(alpha = 0.4, color = "gray40", trim = FALSE) +
  geom_boxplot(width = 0.2, outlier.size = 2, alpha = 0.8, color = "black") +
  geom_jitter(width = 0.1, alpha = 0.35, size = 1.8, color = "#1a1a1a") +
  scale_y_continuous(trans = pseudo_log_trans(base = 10), labels = label_comma(), name = "Observed Volume (TEU, pseudo-log10 scale)") +
  scale_fill_manual(values = c("Maritime" = "#ff7f00", "Rail" = "#6a3d9a"), name = "Transport Mode") +
  labs(
    title = "Figure 5: Comparison of Maritime vs. Rail Container Transport (TEU)",
    subtitle = "Volume distribution and density across all valid observations (2019–2022 pooled)",
    x = "Transport Mode",
    caption = "Source: ContainerTransport dataset (Unit: TEU). Maritime N = 131 valid; Rail N = 115 valid."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "#444444"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "05_container_mode_comparison_teu.png"), p5, width = 8, height = 6, dpi = 300)
cat("Saved: 05_container_mode_comparison_teu.png\n")
# ---------------------------------------------------------------------------
# 5. Country / Reference-Area Analysis & Rankings
# ---------------------------------------------------------------------------
# Ranking for Tonnes
ranking_tonnes <- tonnes_data |>
  group_by(ref_area, reference_area, transport_mode, transport_mode_2) |>
  summarise(
    valid_years = n(),
    mean_volume = mean(obs_value),
    median_volume = median(obs_value),
    min_volume = min(obs_value),
    max_volume = max(obs_value),
    unit = "Tonnes",
    .groups = "drop"
  ) |>
  group_by(transport_mode) |>
  arrange(desc(mean_volume)) |>
  mutate(mode_rank = row_number()) |>
  ungroup() |>
  arrange(transport_mode, mode_rank)

readr::write_csv(ranking_tonnes, file.path(tab_dir, "container_country_ranking_tonnes.csv"))
cat("Saved: container_country_ranking_tonnes.csv\n")

# Figure 06: Country Ranking (Tonnes) - Top 15 per mode
top_tonnes <- ranking_tonnes |>
  group_by(transport_mode_2) |>
  slice_max(mean_volume, n = 15) |>
  ungroup() |>
  arrange(transport_mode_2, mean_volume) |>
  mutate(area_factor = factor(paste0(reference_area, "_", transport_mode), levels = paste0(reference_area, "_", transport_mode)))

p6 <- ggplot(top_tonnes, aes(x = mean_volume, y = area_factor, fill = transport_mode_2)) +
  geom_col(alpha = 0.85, width = 0.7) +
  facet_wrap(~ transport_mode_2, scales = "free", nrow = 1) +
  scale_x_continuous(labels = label_comma(), name = "Mean Annual Volume (Tonnes)") +
  scale_y_discrete(labels = function(x) sub("_[A-Z]+$", "", x), name = "Reference Area") +
  scale_fill_manual(values = c("Maritime" = "#1f78b4", "Rail" = "#33a02c"), name = "Transport Mode") +
  labs(
    title = "Figure 6: Top 15 Reference Areas by Mean Container Volume (Tonnes)",
    subtitle = "Mean annual container transport volume (2019–2022) segregated by Maritime and Rail modes",
    caption = "Source: ContainerTransport dataset (Unit: Tonnes). Rankings computed separately within each transport mode."
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "#444444"),
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "06_container_country_ranking_tonnes.png"), p6, width = 11, height = 7, dpi = 300)
cat("Saved: 06_container_country_ranking_tonnes.png\n")

# Ranking for TEU
ranking_teu <- teu_data |>
  group_by(ref_area, reference_area, transport_mode, transport_mode_2) |>
  summarise(
    valid_years = n(),
    mean_volume = mean(obs_value),
    median_volume = median(obs_value),
    min_volume = min(obs_value),
    max_volume = max(obs_value),
    unit = "TEU",
    .groups = "drop"
  ) |>
  group_by(transport_mode) |>
  arrange(desc(mean_volume)) |>
  mutate(mode_rank = row_number()) |>
  ungroup() |>
  arrange(transport_mode, mode_rank)

readr::write_csv(ranking_teu, file.path(tab_dir, "container_country_ranking_teu.csv"))
cat("Saved: container_country_ranking_teu.csv\n")

# Figure 07: Country Ranking (TEU) - Top 15 per mode
top_teu <- ranking_teu |>
  group_by(transport_mode_2) |>
  slice_max(mean_volume, n = 15) |>
  ungroup() |>
  arrange(transport_mode_2, mean_volume) |>
  mutate(area_factor = factor(paste0(reference_area, "_", transport_mode), levels = paste0(reference_area, "_", transport_mode)))

p7 <- ggplot(top_teu, aes(x = mean_volume, y = area_factor, fill = transport_mode_2)) +
  geom_col(alpha = 0.85, width = 0.7) +
  facet_wrap(~ transport_mode_2, scales = "free", nrow = 1) +
  scale_x_continuous(labels = label_comma(), name = "Mean Annual Volume (TEU)") +
  scale_y_discrete(labels = function(x) sub("_[A-Z]+$", "", x), name = "Reference Area") +
  scale_fill_manual(values = c("Maritime" = "#ff7f00", "Rail" = "#6a3d9a"), name = "Transport Mode") +
  labs(
    title = "Figure 7: Top 15 Reference Areas by Mean Container Volume (TEU)",
    subtitle = "Mean annual container transport volume (2019–2022) segregated by Maritime and Rail modes",
    caption = "Source: ContainerTransport dataset (Unit: TEU). Rankings computed separately within each transport mode."
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "#444444"),
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "07_container_country_ranking_teu.png"), p7, width = 11, height = 7, dpi = 300)
cat("Saved: 07_container_country_ranking_teu.png\n")

# ---------------------------------------------------------------------------
# 6. Data-Quality & Observation-Status Analysis + Figure 08 + Status Table
# ---------------------------------------------------------------------------
status_summary <- cont |>
  group_by(obs_status, observation_status) |>
  summarise(
    record_count = n(),
    record_pct = round(100 * record_count / nrow(cont), 2),
    missing_count = sum(observation_is_missing),
    valid_count = sum(!observation_is_missing),
    min_value = if_else(valid_count > 0, suppressWarnings(min(obs_value, na.rm = TRUE)), NA_real_),
    max_value = if_else(valid_count > 0, suppressWarnings(max(obs_value, na.rm = TRUE)), NA_real_),
    mean_value = if_else(valid_count > 0, mean(obs_value, na.rm = TRUE), NA_real_),
    median_value = if_else(valid_count > 0, median(obs_value, na.rm = TRUE), NA_real_),
    affected_reference_areas_count = n_distinct(reference_area),
    affected_reference_areas_list = paste(sort(unique(reference_area)), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(record_count))

readr::write_csv(status_summary, file.path(tab_dir, "container_status_summary.csv"))
cat("Saved: container_status_summary.csv\n")

# Figure 08: Status Distribution across Units and Modes
status_dist_data <- cont |>
  count(unit_of_measure, transport_mode_2, observation_status) |>
  mutate(
    facet_label = paste(transport_mode_2, "-", unit_of_measure),
    observation_status = factor(observation_status, levels = names(status_palette))
  )

p8 <- ggplot(status_dist_data, aes(x = observation_status, y = n, fill = observation_status)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ facet_label, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = status_palette, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Figure 8: Distribution of Observation Status Codes across Transport Modes and Units",
    subtitle = "Frequencies of Normal (A), Missing (M), Provisional (P), Estimated (E), and Break (B) records",
    x = "Observation Status",
    y = "Record Count",
    caption = "Source: ContainerTransport dataset (624 total observations across 49 reference areas, 2019–2022)."
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, color = "#444444"),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "08_container_status_distribution.png"), p8, width = 10, height = 7, dpi = 300)
cat("Saved: 08_container_status_distribution.png\n")

# ---------------------------------------------------------------------------
# 7. Final Verification of Generated Artifacts
# ---------------------------------------------------------------------------
expected_figures <- paste0(sprintf("%02d", 1:8), c(
  "_container_coverage_heatmap.png",
  "_container_annual_trend_tonnes.png",
  "_container_annual_trend_teu.png",
  "_container_mode_comparison_tonnes.png",
  "_container_mode_comparison_teu.png",
  "_container_country_ranking_tonnes.png",
  "_container_country_ranking_teu.png",
  "_container_status_distribution.png"
))

expected_tables <- c(
  "container_annual_summary.csv",
  "container_mode_summary.csv",
  "container_country_ranking_tonnes.csv",
  "container_country_ranking_teu.csv",
  "container_status_summary.csv",
  "container_coverage_summary.csv"
)

fig_missing <- setdiff(expected_figures, list.files(fig_dir))
tab_missing <- setdiff(expected_tables, list.files(tab_dir))

if (length(fig_missing) > 0) {
  stop("Missing generated figures: ", paste(fig_missing, collapse = ", "))
}
if (length(tab_missing) > 0) {
  stop("Missing generated tables: ", paste(tab_missing, collapse = ", "))
}

cat("\n========================================================\n")
cat("Container Transport Exploratory Data Analysis Completed\n")
cat("========================================================\n")
cat("Figures created in: ", fig_dir, " (", length(expected_figures), " PNG files)\n")
cat("Tables created in:  ", tab_dir, " (", length(expected_tables), " CSV files)\n\n")

cat("--- Key Numerical Highlights ---\n")
cat("1. Total Records: 624 observations across 49 reference areas (2019-2022)\n")
cat("2. Valid Observations: 506 (81.09%); Missing (M): 118 (18.91%)\n")
cat("3. Units: Tonnes (319 rows; 260 valid) vs TEU (305 rows; 246 valid)\n")
cat("4. Transport Modes: Maritime (344 rows; 252 valid) vs Rail (280 rows; 254 valid)\n")
cat("5. Quality Flags: 9 Provisional (P), 4 Estimated (E), 1 Time-series Break (B), 492 Normal (A)\n")
cat("6. Top Maritime Tonnes: Belgium (117,072 avg), France (55,172 avg), Chile (40,637 avg)\n")
cat("7. Top Maritime TEU: Belgium (12.46M avg), Canada (7.00M avg), France (5.95M avg)\n")
cat("8. Missingness Structure: 11 landlocked nations account for the bulk of missing maritime series.\n\n")
cat("Done.\n")
