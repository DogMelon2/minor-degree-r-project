# Phase 4D: ownership-path-aware descriptive EDA.
# Rows are ownership-path records, not physical pipelines. ProjectID is not a
# unique row key. Capacity is never summed, and owner geography is never used
# as project/pipeline geography.

suppressPackageStartupMessages(library(tidyverse))

input_path <- file.path("data", "processed", "energy_ownership_clean.csv")
figure_dir <- file.path("outputs", "figures", "energy_ownership")
table_dir <- file.path("outputs", "tables", "energy_ownership")
stopifnot(file.exists(input_path))
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

energy <- readr::read_csv(input_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE) |>
  mutate(
    share_numeric = readr::parse_double(share_numeric),
    share_missing = as.logical(share_missing),
    capacity_missing_or_placeholder = as.logical(capacity_missing_or_placeholder),
    capacity_boed_numeric = readr::parse_double(capacity_boed_numeric),
    exact_duplicate_flag = as.logical(exact_duplicate_flag),
    exact_duplicate_group_size = readr::parse_integer(exact_duplicate_group_size),
    parent_entity_id_matched = as.logical(parent_entity_id_matched),
    imm_owner_entity_id_matched = as.logical(imm_owner_entity_id_matched)
  )

if (nrow(energy) != 11760L || n_distinct(energy$project_id) != 1685L ||
    sum(energy$share_missing) != 3643L || sum(energy$capacity_missing_or_placeholder) != 2359L ||
    sum(energy$exact_duplicate_flag) != 62L || sum(energy$exact_duplicate_group_size > 1L) != 62L ||
    sum(energy$parent_entity_id_matched) != 11760L || sum(energy$imm_owner_entity_id_matched) != 11760L) {
  stop("Processed energy ownership data differs from the established audit.")
}

write_table <- function(data, filename) readr::write_csv(data, file.path(table_dir, filename), na = "")
save_plot <- function(plot, filename, width = 8, height = 5) {
  ggplot2::ggsave(file.path(figure_dir, filename), plot = plot, width = width, height = height, dpi = 300, bg = "white")
}
theme_energy <- theme_minimal(base_size = 12) + theme(
  plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey30"),
  panel.grid.minor = element_blank(), axis.title = element_text(face = "bold")
)

# A project-level table reports path-level coverage only; capacity is not summed.
energy_project_summary <- energy |>
  group_by(project_id) |>
  summarise(
    project_name = first(project),
    ownership_path_records = n(),
    distinct_parent_entities = n_distinct(parent_gem_entity_id),
    distinct_immediate_owners = n_distinct(immediate_project_owner_gem_entity_id),
    statuses_as_reported = str_c(sort(unique(replace_na(status, "(Missing status)"))), collapse = " | "),
    valid_reported_capacity_path_records = sum(!is.na(capacity_boed_numeric)),
    missing_or_placeholder_capacity_path_records = sum(capacity_missing_or_placeholder),
    distinct_reported_capacity_values = n_distinct(capacity_boed_numeric, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(ownership_path_records), project_id)

energy_status_summary <- energy |>
  mutate(status = replace_na(status, "(Missing status)")) |>
  group_by(status) |>
  summarise(
    ownership_path_records = n(),
    distinct_project_ids_represented = n_distinct(project_id),
    valid_reported_capacity_path_records = sum(!is.na(capacity_boed_numeric)),
    missing_share_path_records = sum(share_missing),
    .groups = "drop"
  ) |>
  arrange(desc(ownership_path_records))

energy_ownership_path_summary <- tibble(
  metric = c("ownership_path_records", "distinct_project_ids", "projects_with_multiple_ownership_paths",
             "distinct_parent_entities", "distinct_immediate_owners", "distinct_parent_to_immediate_owner_relationships",
             "parent_entity_id_match_rate_percent", "immediate_owner_entity_id_match_rate_percent", "missing_share_path_records"),
  value = c(nrow(energy), n_distinct(energy$project_id), sum(energy_project_summary$ownership_path_records > 1L),
            n_distinct(energy$parent_gem_entity_id), n_distinct(energy$immediate_project_owner_gem_entity_id),
            n_distinct(paste(energy$parent_gem_entity_id, energy$immediate_project_owner_gem_entity_id, sep = "\r")),
            100 * mean(energy$parent_entity_id_matched), 100 * mean(energy$imm_owner_entity_id_matched), sum(energy$share_missing))
)

capacity_summary_one <- function(data, scope) {
  valid <- data |> filter(!is.na(capacity_boed_numeric))
  tibble(
    scope = scope, ownership_path_records = nrow(data),
    valid_reported_capacity_path_records = nrow(valid),
    missing_or_placeholder_capacity_path_records = sum(data$capacity_missing_or_placeholder),
    minimum_reported_capacity = if (nrow(valid) > 0) min(valid$capacity_boed_numeric) else NA_real_,
    median_reported_capacity = if (nrow(valid) > 0) median(valid$capacity_boed_numeric) else NA_real_,
    mean_reported_capacity = if (nrow(valid) > 0) mean(valid$capacity_boed_numeric) else NA_real_,
    maximum_reported_capacity = if (nrow(valid) > 0) max(valid$capacity_boed_numeric) else NA_real_
  )
}
energy_capacity_summary <- bind_rows(
  capacity_summary_one(energy, "overall"),
  energy |> mutate(status = replace_na(status, "(Missing status)")) |> group_split(status) |>
    map_dfr(~ capacity_summary_one(.x, unique(.x$status)))
)

owner_geography_summary <- function(country_column, output_column) {
  energy |>
    mutate(owner_country = replace_na(.data[[country_column]], "(Missing owner geography)")) |>
    group_by(owner_country) |>
    summarise(
      ownership_path_records = n(),
      distinct_parent_entities = n_distinct(parent_gem_entity_id),
      .groups = "drop"
    ) |>
    rename(!!output_column := owner_country) |>
    arrange(desc(ownership_path_records), .data[[output_column]])
}
energy_owner_headquarters_summary <- owner_geography_summary("parent_headquarters_country", "parent_headquarters_country")
energy_owner_registration_summary <- owner_geography_summary("parent_registration_country", "parent_registration_country")

energy_entity_relationship_summary <- energy |>
  count(parent_gem_entity_id, parent, immediate_project_owner_gem_entity_id, immediate_project_owner,
        name = "ownership_path_records") |>
  arrange(desc(ownership_path_records), parent_gem_entity_id, immediate_project_owner_gem_entity_id)

duplicate_group_count <- sum(energy$exact_duplicate_flag) / 2L
energy_duplicate_summary <- tibble(
  metric = c("exact_duplicate_groups", "physical_rows_in_exact_duplicate_groups", "duplicate_rows_beyond_first", "all_ownership_path_records"),
  count = c(duplicate_group_count, sum(energy$exact_duplicate_flag), duplicate_group_count, nrow(energy))
)
# Every duplicate group has size two in the audited data; verify before reporting extra rows.
if (sum(unique(energy$exact_duplicate_group_size) > 1L) != 1L ||
    nrow(energy |> filter(exact_duplicate_flag) |> distinct(parent_gem_entity_id, project_id, .keep_all = TRUE)) < 1L ||
    duplicate_group_count != 31L) {
  stop("Exact-duplicate structure differs from the established audit.")
}

write_table(energy_project_summary, "energy_project_summary.csv")
write_table(energy_status_summary, "energy_status_summary.csv")
write_table(energy_ownership_path_summary, "energy_ownership_path_summary.csv")
write_table(energy_capacity_summary, "energy_capacity_summary.csv")
write_table(energy_owner_headquarters_summary, "energy_owner_headquarters_summary.csv")
write_table(energy_owner_registration_summary, "energy_owner_registration_summary.csv")
write_table(energy_entity_relationship_summary, "energy_entity_relationship_summary.csv")
write_table(energy_duplicate_summary, "energy_duplicate_summary.csv")

plot_status <- ggplot(energy_status_summary, aes(reorder(status, distinct_project_ids_represented), distinct_project_ids_represented)) +
  geom_col(fill = "#1B6CA8") + coord_flip() +
  labs(title = "ProjectIDs represented by status", subtitle = "Distinct ProjectIDs within ownership-path records; not a physical-pipeline total", x = "Status", y = "Distinct ProjectIDs represented") + theme_energy
plot_paths <- ggplot(energy_project_summary, aes(ownership_path_records)) +
  geom_histogram(bins = 35, fill = "#2A9D8F", colour = "white") +
  labs(title = "Ownership-path records per ProjectID", subtitle = "A ProjectID can have multiple ownership paths", x = "Ownership-path records per ProjectID", y = "Number of ProjectIDs") + theme_energy
plot_capacity <- ggplot(filter(energy, !is.na(capacity_boed_numeric)), aes(capacity_boed_numeric)) +
  geom_histogram(bins = 40, fill = "#6A4C93", colour = "white") +
  labs(title = "Distribution of reported capacity values", subtitle = "Valid reported values on ownership-path records; not summed", x = "Reported CapacityBOEd", y = "Ownership-path records") + theme_energy
capacity_missing_plot <- tibble(category = c("Valid reported capacity", "Missing/placeholder capacity"), count = c(sum(!is.na(energy$capacity_boed_numeric)), sum(energy$capacity_missing_or_placeholder)))
plot_capacity_missing <- ggplot(capacity_missing_plot, aes(category, count, fill = category)) +
  geom_col(show.legend = FALSE) + geom_text(aes(label = count), vjust = -.35) +
  labs(title = "Capacity reporting coverage", subtitle = "Ownership-path records; missing capacity remains NA", x = NULL, y = "Ownership-path records") + theme_energy
top_headquarters <- energy_owner_headquarters_summary |> slice_max(ownership_path_records, n = 15, with_ties = FALSE)
plot_headquarters <- ggplot(top_headquarters, aes(reorder(parent_headquarters_country, ownership_path_records), ownership_path_records)) +
  geom_col(fill = "#457B9D") + coord_flip() +
  labs(title = "Owner headquarters geography: most-recorded labels", subtitle = "Owner geography only; not pipeline/project locations", x = "Parent headquarters country", y = "Ownership-path records") + theme_energy
top_registration <- energy_owner_registration_summary |> slice_max(ownership_path_records, n = 15, with_ties = FALSE)
plot_registration <- ggplot(top_registration, aes(reorder(parent_registration_country, ownership_path_records), ownership_path_records)) +
  geom_col(fill = "#E76F51") + coord_flip() +
  labs(title = "Owner registration geography: most-recorded labels", subtitle = "Owner geography only; not pipeline/project locations", x = "Parent registration country", y = "Ownership-path records") + theme_energy
entity_counts <- tibble(entity_role = c("Parent entities", "Immediate-owner entities"), distinct_entities = c(n_distinct(energy$parent_gem_entity_id), n_distinct(energy$immediate_project_owner_gem_entity_id)))
plot_entities <- ggplot(entity_counts, aes(entity_role, distinct_entities, fill = entity_role)) +
  geom_col(show.legend = FALSE) + geom_text(aes(label = distinct_entities), vjust = -.35) +
  labs(title = "Distinct entities represented in ownership paths", subtitle = "Entity counts, not ownership concentration", x = NULL, y = "Distinct entity IDs") + theme_energy
duplicate_plot <- tibble(category = c("Exact duplicate groups", "Flagged duplicate rows", "Rows beyond first"), count = c(31L, sum(energy$exact_duplicate_flag), 31L))
plot_duplicates <- ggplot(duplicate_plot, aes(category, count, fill = category)) +
  geom_col(show.legend = FALSE) + geom_text(aes(label = count), vjust = -.35) +
  labs(title = "Retained exact-duplicate summary", subtitle = "Duplicate ownership-path rows remain in the analytical dataset", x = NULL, y = "Count") + theme_energy

save_plot(plot_status, "01_pipeline_projects_by_status.png")
save_plot(plot_paths, "02_pipeline_ownership_paths_per_project.png")
save_plot(plot_capacity, "03_pipeline_capacity_distribution.png")
save_plot(plot_capacity_missing, "04_pipeline_capacity_missingness.png")
save_plot(plot_headquarters, "05_pipeline_owner_headquarters_distribution.png", 8, 7)
save_plot(plot_registration, "06_pipeline_owner_registration_distribution.png", 8, 7)
save_plot(plot_entities, "07_pipeline_parent_vs_immediate_owner_counts.png")
save_plot(plot_duplicates, "08_pipeline_duplicate_summary.png")

expected_figures <- c("01_pipeline_projects_by_status.png", "02_pipeline_ownership_paths_per_project.png", "03_pipeline_capacity_distribution.png", "04_pipeline_capacity_missingness.png", "05_pipeline_owner_headquarters_distribution.png", "06_pipeline_owner_registration_distribution.png", "07_pipeline_parent_vs_immediate_owner_counts.png", "08_pipeline_duplicate_summary.png")
expected_tables <- c("energy_project_summary.csv", "energy_status_summary.csv", "energy_ownership_path_summary.csv", "energy_capacity_summary.csv", "energy_owner_headquarters_summary.csv", "energy_owner_registration_summary.csv", "energy_entity_relationship_summary.csv", "energy_duplicate_summary.csv")
if (!all(file.exists(file.path(figure_dir, expected_figures))) || !all(file.exists(file.path(table_dir, expected_tables)))) stop("Expected energy ownership EDA outputs were not generated.")
if (nrow(energy_project_summary) != 1685L || sum(energy_status_summary$ownership_path_records) != nrow(energy) || sum(energy_owner_headquarters_summary$ownership_path_records) != nrow(energy) || sum(energy_owner_registration_summary$ownership_path_records) != nrow(energy)) stop("Ownership-path record counts are inconsistent.")
cat("Phase 4D energy ownership EDA completed. Rows:", nrow(energy), " Projects:", nrow(energy_project_summary), " Figures:", length(expected_figures), " Tables:", length(expected_tables), "\n")
