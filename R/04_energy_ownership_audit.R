# Phase 1: read-only audit of globalEnergyOwnership.xlsx.
# This script does not alter the workbook, sum capacity, calculate ownership
# concentration, or interpret owner countries as physical asset locations.

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))

workbook_path <- file.path("data", "raw", "globalEnergyOwnership.xlsx")
results_dir <- file.path("outputs", "results")
stopifnot(file.exists(workbook_path), dir.exists(results_dir))

sheet_names <- readxl::excel_sheets(workbook_path)

read_workbook_sheet <- function(sheet_name) {
  readxl::read_excel(
    workbook_path,
    sheet = sheet_name,
    .name_repair = "unique_quiet"
  )
}

is_missing_value <- function(x) {
  is.na(x) | stringr::str_trim(as.character(x)) == ""
}

row_signature <- function(data) {
  encoded <- lapply(data, function(column) {
    paste(nchar(as.character(column)), as.character(column), sep = ":")
  })
  do.call(paste, c(encoded, sep = "\r"))
}

sheet_data <- purrr::set_names(
  purrr::map(sheet_names, read_workbook_sheet),
  sheet_names
)

relevance_for_project <- function(sheet_name) {
  dplyr::case_when(
    sheet_name %in% c("All Entities", "Entity Ownership", "Asset Ownership", "Oil & NGL Pipeline Ownership") ~ "directly relevant",
    sheet_name %in% c("Gas Pipeline Ownership", "Gas Plant Ownership", "Coal Plant Ownership", "Coal Mine Ownership") ~ "contextually relevant to wider energy infrastructure",
    sheet_name %in% c("Bioenergy Power Ownership", "Iron Mine Ownership", "Steel Plant Ownership", "Cement and Concrete Ownership") ~ "potential wider energy-transition context; not central to current oil scope",
    TRUE ~ "metadata or out of current analytical scope"
  )
}

sheet_audit_list <- purrr::imap(sheet_data, function(data, sheet_name) {
  signatures <- row_signature(data)
  counts <- table(signatures)
  tibble(
    sheet = sheet_name,
    rows = nrow(data),
    columns = ncol(data),
    column_names = paste(names(data), collapse = " | "),
    data_types = paste(purrr::map_chr(data, ~ class(.x)[1]), collapse = " | "),
    total_missing_cells = sum(purrr::map_int(data, ~ sum(is_missing_value(.x)))),
    exact_duplicate_groups = sum(counts > 1),
    exact_duplicate_rows_beyond_first = sum(counts[counts > 1] - 1L),
    relevance_to_project = relevance_for_project(sheet_name)
  )
})
energy_workbook_sheet_audit <- bind_rows(sheet_audit_list)

energy_workbook_missingness <- purrr::imap_dfr(sheet_data, function(data, sheet_name) {
  tibble(
    sheet = sheet_name,
    column = names(data),
    missing_count = purrr::map_int(data, ~ sum(is_missing_value(.x))),
    missing_percent = round(100 * missing_count / nrow(data), 2)
  )
})

required_pipeline_columns <- c(
  "Parent GEM Entity ID", "Parent", "Parent Registration Country",
  "Parent Headquarters Country", "Project", "Share", "Ownership Path",
  "Immediate Project Owner", "Immediate Project Owner GEM Entity ID", "Tracker",
  "Status", "CapacityBOEd", "ProjectID"
)
pipeline_raw <- sheet_data[["Oil & NGL Pipeline Ownership"]]
missing_pipeline_columns <- setdiff(required_pipeline_columns, names(pipeline_raw))
if (length(missing_pipeline_columns) > 0) {
  stop("Pipeline sheet is missing expected columns: ", paste(missing_pipeline_columns, collapse = ", "))
}

pipeline <- pipeline_raw |>
  mutate(
    project_id = as.character(ProjectID),
    project_name = as.character(Project),
    parent_entity_id = as.character(`Parent GEM Entity ID`),
    immediate_owner_entity_id = as.character(`Immediate Project Owner GEM Entity ID`),
    ownership_share_is_missing = is_missing_value(Share),
    capacity_is_missing_or_placeholder = is_missing_value(CapacityBOEd) | as.character(CapacityBOEd) == "--"
  )

# The eventual physical analytical grain is project/unit, not an ownership-path
# row. This audit therefore counts paths per ProjectID but never sums capacity.
energy_pipeline_project_audit <- pipeline |>
  group_by(project_id, project_name) |>
  summarise(
    ownership_path_records = n(),
    distinct_parent_entity_ids = n_distinct(parent_entity_id[!is_missing_value(parent_entity_id)]),
    distinct_immediate_owner_entity_ids = n_distinct(immediate_owner_entity_id[!is_missing_value(immediate_owner_entity_id)]),
    missing_ownership_share_records = sum(ownership_share_is_missing),
    missing_or_placeholder_capacity_records = sum(capacity_is_missing_or_placeholder),
    distinct_reported_capacity_values = n_distinct(as.character(CapacityBOEd)[!capacity_is_missing_or_placeholder]),
    statuses_as_reported = paste(sort(unique(as.character(Status)[!is_missing_value(Status)])), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(desc(ownership_path_records), project_id)

pipeline_signature <- row_signature(pipeline_raw)
pipeline_signature_counts <- table(pipeline_signature)

energy_pipeline_duplicate_audit <- energy_pipeline_project_audit |>
  filter(ownership_path_records > 1) |>
  mutate(
    project_id_is_unique = FALSE,
    workbook_exact_duplicate_groups = sum(pipeline_signature_counts > 1),
    workbook_exact_duplicate_rows_beyond_first = sum(pipeline_signature_counts[pipeline_signature_counts > 1] - 1L)
  )

all_entities <- sheet_data[["All Entities"]]
if (!"Entity ID" %in% names(all_entities)) {
  stop("All Entities sheet is missing Entity ID.")
}
all_entity_ids <- unique(as.character(all_entities$`Entity ID`))

make_entity_audit <- function(ids, role_name) {
  tibble(entity_id = as.character(ids)) |>
    mutate(entity_id_missing = is_missing_value(entity_id)) |>
    count(entity_id, entity_id_missing, name = "pipeline_reference_records") |>
    mutate(
      reference_role = role_name,
      matched_to_all_entities = !entity_id_missing & entity_id %in% all_entity_ids
    ) |>
    select(reference_role, entity_id, entity_id_missing, pipeline_reference_records, matched_to_all_entities)
}

energy_entity_key_audit <- bind_rows(
  make_entity_audit(pipeline$parent_entity_id, "parent_entity"),
  make_entity_audit(pipeline$immediate_owner_entity_id, "immediate_project_owner")
) |>
  arrange(reference_role, desc(pipeline_reference_records), entity_id)

# This comparison is a metadata register, not a combined analytical dataset.
container_rows <- readr::read_csv(file.path("data", "raw", "ContainerTransport.csv"), show_col_types = FALSE)
refinery_rows <- readr::read_csv(
  file.path("data", "raw", "Refinery.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)
phase1_dataset_comparison <- tribble(
  ~dataset, ~rows, ~columns, ~years, ~geographies, ~main_subject, ~main_outcomes, ~major_quality_issue, ~safe_analysis, ~limitations,
  "ContainerTransport.csv", nrow(container_rows), ncol(container_rows), "2019-2022", "49 reference areas", "Annual container transport by mode and unit", "OBS_VALUE with status, unit, multiplier", "Tonnes and TEU are distinct measures; 118 observation values are missing", "Coverage, status, and within-unit/mode descriptive profiling", "Not oil-tanker, chokepoint, route, or petroleum-flow data",
  "Refinery.csv", nrow(refinery_rows), ncol(refinery_rows), "Raw Persian-calendar labels: 1398/12/27-1399/03/20", "No explicit geography field", "Facility-labelled origin/destination weight records", "Origin and destination net weights; raw date labels", "Weight unit undefined; no formal row ID; date calendar requires confirmation", "Record counts and raw-label/weight-consistency audit", "Not refinery-capacity, country, route, or oil-production data",
  "globalEnergyOwnership.xlsx", sum(energy_workbook_sheet_audit$rows), NA_integer_, "No observation-year field across core ownership sheets", "Owner registration/headquarters countries; not asset locations", "Global energy entities, assets, and ownership paths", "Entity/project IDs, shares, status, sector capacities", "Ownership-path rows can duplicate projects; shares/capacity are missing for some records", "Workbook/schema audit and project-level ownership-path counts", "Do not sum capacity or infer pipeline location/concentration before grain validation"
)

cat("Energy ownership workbook audit completed (workbook was read only).\n")
cat("Sheets:", paste(sheet_names, collapse = ", "), "\n")
print(energy_workbook_sheet_audit)
cat("Pipeline ownership-path records:", nrow(pipeline), "\n")
cat("Distinct pipeline ProjectID values:", n_distinct(pipeline$project_id), "\n")
cat("Pipeline project IDs with more than one ownership-path record:", nrow(energy_pipeline_duplicate_audit), "\n")
cat("Parent entity references unmatched to All Entities:", sum(!energy_entity_key_audit$matched_to_all_entities & !energy_entity_key_audit$entity_id_missing & energy_entity_key_audit$reference_role == "parent_entity"), "\n")
cat("Immediate-owner references unmatched to All Entities:", sum(!energy_entity_key_audit$matched_to_all_entities & !energy_entity_key_audit$entity_id_missing & energy_entity_key_audit$reference_role == "immediate_project_owner"), "\n")

readr::write_csv(energy_workbook_sheet_audit, file.path(results_dir, "energy_workbook_sheet_audit.csv"))
readr::write_csv(energy_workbook_missingness, file.path(results_dir, "energy_workbook_missingness.csv"))
readr::write_csv(energy_pipeline_project_audit, file.path(results_dir, "energy_pipeline_project_audit.csv"))
readr::write_csv(energy_pipeline_duplicate_audit, file.path(results_dir, "energy_pipeline_duplicate_audit.csv"))
readr::write_csv(energy_entity_key_audit, file.path(results_dir, "energy_entity_key_audit.csv"))
readr::write_csv(phase1_dataset_comparison, file.path(results_dir, "phase1_dataset_comparison.csv"))
