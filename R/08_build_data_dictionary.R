# Phase 3: reproducible generation of the project-wide Data Dictionary
# and Provenance Register.
#
# This script reads metadata and structure from existing raw/processed datasets
# and Phase 1/Phase 2 audit outputs. It never modifies any raw datasets, existing
# scripts, or processed data files.
#
# Outputs produced:
#   1. outputs/results/project_data_dictionary.csv
#   2. outputs/results/project_provenance_register.csv

suppressPackageStartupMessages(library(tidyverse))

# ---------------------------------------------------------------------------
# Path configuration & pre-flight checks
# ---------------------------------------------------------------------------
results_dir    <- file.path("outputs", "results")
data_raw_dir   <- file.path("data", "raw")
data_proc_dir  <- file.path("data", "processed")

dict_path      <- file.path(results_dir, "project_data_dictionary.csv")
prov_path      <- file.path(results_dir, "project_provenance_register.csv")

stopifnot(dir.exists(results_dir), dir.exists(data_raw_dir), dir.exists(data_proc_dir))

oil_raw_path        <- file.path(data_raw_dir, "oil_production_statistics.csv")
container_raw_path  <- file.path(data_raw_dir, "ContainerTransport.csv")
refinery_raw_path   <- file.path(data_raw_dir, "Refinery.csv")
energy_raw_path     <- file.path(data_raw_dir, "globalEnergyOwnership.xlsx")

container_proc_path <- file.path(data_proc_dir, "container_transport_clean.csv")
refinery_proc_path  <- file.path(data_proc_dir, "refinery_clean.csv")
energy_proc_path    <- file.path(data_proc_dir, "energy_ownership_clean.csv")

stopifnot(
  file.exists(oil_raw_path),
  file.exists(container_raw_path),
  file.exists(refinery_raw_path),
  file.exists(energy_raw_path),
  file.exists(container_proc_path),
  file.exists(refinery_proc_path),
  file.exists(energy_proc_path)
)

cat("All raw and processed files verified.\n")

# ---------------------------------------------------------------------------
# 1. Construct Provenance Register
# ---------------------------------------------------------------------------
provenance_register <- tibble(
  dataset = c(
    "Oil Production Statistics",
    "Container Transport",
    "Refinery Shipments",
    "Global Energy Ownership (Oil & NGL Pipelines)"
  ),
  source_file = c(
    "oil_production_statistics.csv",
    "ContainerTransport.csv",
    "Refinery.csv",
    "globalEnergyOwnership.xlsx"
  ),
  source_format = c(
    "CSV (Latin-1 encoded, 2,376 rows x 6 columns)",
    "CSV (UTF-8 encoded, 624 rows x 28 columns)",
    "CSV (UTF-8 encoded, 264 rows x 7 columns)",
    "Excel Workbook XLSX (13 sheets, target sheet: 11,760 rows x 13 columns)"
  ),
  raw_file_path = c(
    "data/raw/oil_production_statistics.csv",
    "data/raw/ContainerTransport.csv",
    "data/raw/Refinery.csv",
    "data/raw/globalEnergyOwnership.xlsx"
  ),
  processed_file_path = c(
    "None (audited only; blocked from cleaning/aggregation due to unresolved conflicting keys)",
    "data/processed/container_transport_clean.csv",
    "data/processed/refinery_clean.csv",
    "data/processed/energy_ownership_clean.csv"
  ),
  audit_script = c(
    "R/01_oil_data_audit.R",
    "R/02_container_transport_audit.R",
    "R/03_refinery_audit.R",
    "R/04_energy_ownership_audit.R"
  ),
  cleaning_script = c(
    "None (Phase 2 cleaning intentionally blocked by data integrity gates)",
    "R/05_clean_container_transport.R",
    "R/06_clean_refinery.R",
    "R/07_clean_energy_ownership.R"
  ),
  phase1_audit_status = c(
    "Completed: Identified 44 exact duplicate groups (44 extra rows), 540 repeated 2021 keys, 496 conflicting keys with divergent values, 82 negative values, 223 zero values, unknown unit, and 2021 Consumption Pattern values matching 2022 values.",
    "Completed: Verified 624 rows x 28 columns across 49 reference areas (2019-2022). Identified 118 missing OBS_VALUE cells (OBS_STATUS = 'M'), distinct Tonnes vs TEU units, 14 constant metadata fields, 2 empty fields, and unique 4-part analytical key.",
    "Completed: Verified 264 rows x 7 columns across 4 refineries and 4 product types. Identified 1 exact duplicate group (2 rows), Persian/Solar Hijri calendar date labels (1398/12/27 to 1399/03/20), unknown weight units, and numerical weight differences (-510 to +440).",
    "Completed: Audited 13 sheets (343,197 total rows). For 'Oil & NGL Pipeline Ownership' (11,760 rows x 13 cols), identified 1,685 distinct ProjectIDs, 1,351 multi-path projects, 31 exact duplicate groups (31 extra rows), 3,643 missing Share values, and 2,359 missing/placeholder CapacityBOEd values."
  ),
  phase2_cleaning_status = c(
    "Blocked/Audited only: No cleaned dataset created. Unresolvable duplicate/conflicting keys and undocumented measurement units prevent valid aggregation, ranking, or cleaning.",
    "Completed: Retained all 624 observations; dropped 14 constant metadata and 2 empty descriptive fields; preserved Tonnes and TEU separately; retained UNIT_MULT without arithmetic application; preserved 118 missing values as NA; added missing, provisional, estimated, and break flags.",
    "Completed: Retained all 264 observations; preserved raw Persian-calendar date strings and extracted calendar components without unsupported Gregorian conversion; retained numeric weight copies and raw differences without unit conversion; flagged exact duplicates.",
    "Completed: Retained all 11,760 ownership-path rows without row removal or aggregation; standardized column names to snake_case; converted Share to numeric (NA preserved, never zero); converted CapacityBOEd to numeric (placeholders -> NA, never imputed); preserved raw CapacityBOEd; flagged exact duplicates; verified 100% entity-ID matching against All Entities sheet."
  ),
  known_data_quality_issues = c(
    "1. 540 repeated keys in 2021; 2. 496 conflicting keys where same country-product-flow has divergent values; 3. 44 exact duplicate groups; 4. Unknown measurement unit for value; 5. 82 negative and 223 zero values; 6. 2021 Consumption Pattern values match 2022 values exactly.",
    "1. Tonnes (T) and TEU are fundamentally distinct measures; 2. 118 missing observations (OBS_STATUS = 'M'); 3. 14 constant metadata columns in raw extract; 4. Multiplier arithmetic convention is not documented.",
    "1. Dates use Persian/Solar Hijri calendar notation (1398-1399) without confirmed Gregorian conversion formula; 2. Net weight measurement unit is completely undefined (unknown if kg, tonnes, or barrels); 3. 1 exact duplicate group (2 identical rows); 4. No explicit geographic location field.",
    "1. ProjectID is not a unique row key (1,351 projects have multiple ownership paths); 2. 3,643 rows (30.98%) missing Share; 3. 2,359 rows have missing or placeholder ('--') CapacityBOEd; 4. 31 exact duplicate groups (62 rows flagged); 5. Parent registration/headquarters countries represent corporate jurisdiction, NOT asset physical location."
  ),
  unresolved_provenance_questions = c(
    "1. Source dataset documentation and measurement unit are missing; 2. Cause of 2021 key duplication and divergent values is unknown; 3. Origin/definition of 'Balance' type is undocumented; 4. Whether negative values represent stock drawdowns, trade deficits, or data entry errors is unknown.",
    "1. Arithmetic multiplier rule for UNIT_MULT is not explicitly defined in file metadata; 2. Specific reason for 118 missing observation values (M) across certain reference areas.",
    "1. Official Gregorian calendar mapping for Iranian operational date labels is unconfirmed; 2. Specific physical unit of origin/destination net weights is undocumented; 3. Physical mechanism underlying origin vs destination weight discrepancies (-510 to +440) is unknown.",
    "1. Basis for missing Share percentages in GEM tracker is unspecified; 2. Exact physical throughput meaning of '--' placeholder capacity; 3. Physical routing/geographic coordinates of pipelines are separate from owner corporate registration."
  ),
  is_analysis_ready = c(
    "NO (Strictly blocked from aggregation, ranking, or modeling due to conflicting duplicate keys and unknown units)",
    "YES (Conditional on strict segregation by unit_measure and transport_mode; no cross-unit aggregation)",
    "YES (Conditional on treating dates as ordinal/calendar-specific labels and weights as uncalibrated numerical values; no Gregorian date math or physical mass claims)",
    "YES (Conditional on preserving ownership-path grain; strictly no capacity summation across paths and no ownership concentration calculation without missing share handling)"
  )
)
# ---------------------------------------------------------------------------
# 2. Construct Data Dictionary
# ---------------------------------------------------------------------------
data_dictionary <- tribble(
  ~dataset, ~original_variable_name, ~cleaned_variable_name, ~data_type, ~observed_values_range, ~confirmed_meaning, ~confirmed_unit, ~unit_known_or_unknown, ~missing_value_treatment, ~is_safe_for_analysis, ~analytical_restrictions, ~source_evidence, ~unresolved_questions,

  # --- OIL PRODUCTION STATISTICS ---
  "Oil Production Statistics", "country_name", "NA (raw only)", "character", "36 unique countries (e.g. Australia, Canada, Saudi Arabia, United States)", "Name of the reporting country or territory", "Unitless", "Unitless", "0 missing (0.00%)", "Conditional", "Safe for filtering/stratification. Unsafe for cross-country aggregation due to key conflicts in other dimensions.", "data/raw/oil_production_statistics.csv; outputs/results/oil_data_audit_summary.csv", "Latin-1 encoding required for reading; political/territorial boundaries not explicitly defined.",
  "Oil Production Statistics", "type", "NA (raw only)", "character", "Constant value: 'Balance' (2,376 rows = 100%)", "Reporting balance category / classification type", "Unitless", "Unitless", "0 missing (0.00%)", "NO", "Non-informative constant metadata field in this extract. Do not use as an analytical variable.", "outputs/results/oil_data_audit_summary.csv", "Origin and full definition of 'Balance' type are undocumented in the raw file.",
  "Oil Production Statistics", "product", "NA (raw only)", "character", "11 unique categories (Crude oil, Gasoline and diesel, Liquified Petroleum Gas, Middle distillates, Naphtha, Other oil products, Residual fuel oil, Total gas oil production, Total kerosene production, Total oil production, Total oil products production)", "Petroleum product or aggregate category", "Unitless", "Unitless", "0 missing (0.00%)", "Conditional", "Do NOT sum across products: aggregate categories (e.g. 'Total oil production') overlap with sub-products (e.g. 'Crude oil').", "data/raw/oil_production_statistics.csv; R/01_oil_data_audit.R", "Hierarchy and aggregation rules between sub-products and 'Total' products are undocumented.",
  "Oil Production Statistics", "flow", "NA (raw only)", "character", "4 categories: 'Consumption Pattern' (864), 'Industrial Production' (144), 'Net Deliveries' (1,152), 'Storage Channelization' (216)", "Supply chain flow or statistical balance flow category", "Unitless", "Unitless", "0 missing (0.00%)", "Conditional", "Do NOT sum across flows: distinct accounting flows with incompatible physical interpretations.", "data/raw/oil_production_statistics.csv; R/01_oil_data_audit.R", "Definitions of 'Storage Channelization' and 'Consumption Pattern' are not provided in source metadata.",
  "Oil Production Statistics", "year", "NA (raw only)", "integer", "2021 (1,080 rows), 2022 (540 rows), 2023 (756 rows)", "Calendar year of the reported statistical observation", "Year", "Known", "0 missing (0.00%)", "Conditional", "Year 2021 contains 540 repeated keys and 496 conflicting values; do not compute 2021-2022 changes or totals.", "outputs/results/oil_2021_anomalies.csv; outputs/results/oil_duplicate_key_audit.csv", "Cause of excessive row counts and duplicate keys in 2021 is unknown.",
  "Oil Production Statistics", "value", "NA (raw only)", "numeric", "Range: -2,442.00 to 790,672.41 (Median: 462.11, Mean: 6,298.35, 82 negative, 223 zero)", "Reported quantitative flow value", "Unknown", "Unknown", "0 missing (0.00%); 82 negative values and 223 zeros preserved as reported", "NO", "UNSAFE for totals, sums, rankings, or econometric modeling. 496 conflicting keys give divergent values for identical keys.", "outputs/results/oil_conflicting_keys.csv; outputs/results/oil_data_audit_summary.csv", "Measurement unit (thousand barrels, kilotonnes, TJ, USD?) is completely unspecified; meaning of negative values is unknown.",

  # --- CONTAINER TRANSPORT ---
  "Container Transport", "REF_AREA", "ref_area", "character", "49 unique codes (e.g. AUS, CAN, DEU, USA, WLD)", "ISO 3-letter or statistical country/area reference code", "Unitless (Code)", "Unitless", "0 missing (0.00%)", "YES", "Primary geographic identifier. Safe for filtering and grouping.", "data/processed/container_transport_clean.csv; R/05_clean_container_transport.R", "None.",
  "Container Transport", "Reference area", "reference_area", "character", "49 unique names (e.g. Australia, Canada, Germany, United States, World)", "Full human-readable country/reference area name", "Unitless (Label)", "Unitless", "0 missing (0.00%)", "YES", "Display label corresponding 1:1 with ref_area.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "UNIT_MEASURE", "unit_measure", "character", "2 categories: 'T' (319 rows), 'TEU' (305 rows)", "Measurement unit code ('T' = Tonnes, 'TEU' = Twenty-foot Equivalent Units)", "Unitless (Code)", "Unitless", "0 missing (0.00%)", "YES", "CRITICAL: Must ALWAYS segregate analysis by unit_measure. NEVER sum or compare T and TEU directly.", "outputs/results/container_cleaning_summary.csv; R/05_clean_container_transport.R", "None.",
  "Container Transport", "Unit of measure", "unit_of_measure", "character", "2 categories: 'Tonnes' (319 rows), 'Twenty-foot equivalent units' (305 rows)", "Full name of measurement unit", "Unitless (Label)", "Unitless", "0 missing (0.00%)", "YES", "Display label corresponding 1:1 with unit_measure.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "TRANSPORT_MODE", "transport_mode", "character", "2 categories: 'MAR' (344 rows), 'RAIL' (280 rows)", "Transport mode code ('MAR' = Maritime, 'RAIL' = Rail)", "Unitless (Code)", "Unitless", "0 missing (0.00%)", "YES", "Must segregate or control for transport mode. Do not sum across modes without verifying non-overlap.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "Transport mode", "transport_mode_2", "character", "2 categories: 'Maritime' (344 rows), 'Rail' (280 rows)", "Full name of transport mode", "Unitless (Label)", "Unitless", "0 missing (0.00%)", "YES", "Display label corresponding 1:1 with transport_mode.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "TIME_PERIOD", "time_period", "integer", "Range: 2019 to 2022 (4 annual periods)", "Calendar year of container transport observation", "Year", "Known", "0 missing (0.00%)", "YES", "Annual time series index. Valid for panel structure across 2019-2022.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "OBS_VALUE", "obs_value", "numeric", "Range: 0 to 1,029,910,000 (Median: 2,542,000; 118 NA)", "Observed quantitative volume of container transport", "Tonnes or TEU (governed by unit_measure)", "Known", "118 missing values (OBS_STATUS = 'M') preserved as NA; NEVER converted to zero", "Conditional", "Safe ONLY within segregated unit_measure and transport_mode subsets. Must handle 118 NAs explicitly.", "data/processed/container_transport_clean.csv; outputs/results/container_cleaning_summary.csv", "Whether reported values incorporate unapplied multipliers.",
  "Container Transport", "OBS_STATUS", "obs_status", "character", "5 codes: 'A' (Normal, 492), 'B' (Break, 1), 'E' (Estimated, 4), 'M' (Missing, 118), 'P' (Provisional, 9)", "Observation quality and status code", "Unitless (Code)", "Unitless", "0 missing (0.00%)", "YES", "Use to filter or stratify observations by statistical reliability.", "outputs/results/container_status_summary.csv", "Specific methodology for 'Estimated' (E) values is not detailed.",
  "Container Transport", "Observation status", "observation_status", "character", "5 labels: 'Normal value', 'Time series break', 'Estimated value', 'Missing value; data cannot exist', 'Provisional value'", "Full descriptive label of observation status", "Unitless (Label)", "Unitless", "0 missing (0.00%)", "YES", "Display label corresponding 1:1 with obs_status.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "UNIT_MULT", "unit_mult", "integer", "Constant value: 0 (624 rows = 100%)", "Unit multiplier exponent code", "Unitless (Code)", "Unitless", "0 missing (0.00%)", "YES", "Retained for provenance. Multiplier was NOT arithmetically applied to obs_value.", "outputs/results/container_cleaning_summary.csv", "Arithmetic convention of multiplier is not documented in file.",
  "Container Transport", "Unit multiplier", "unit_multiplier", "character", "Constant value: 'Units' (624 rows = 100%)", "Descriptive label for unit multiplier", "Unitless (Label)", "Unitless", "0 missing (0.00%)", "YES", "Display label corresponding 1:1 with unit_mult.", "data/processed/container_transport_clean.csv", "None.",
  "Container Transport", "(derived)", "observation_is_missing", "logical", "FALSE: 506 rows (81.09%), TRUE: 118 rows (18.91%)", "Boolean flag indicating whether obs_value is missing (NA)", "Unitless (Flag)", "Unitless", "0 missing; derived directly from is.na(obs_value)", "YES", "Use for filtering out missing observations before numeric operations.", "R/05_clean_container_transport.R", "None.",
  "Container Transport", "(derived)", "observation_is_provisional", "logical", "FALSE: 615 rows (98.56%), TRUE: 9 rows (1.44%)", "Boolean flag indicating provisional observation status (obs_status == 'P')", "Unitless (Flag)", "Unitless", "0 missing; derived from obs_status == 'P'", "YES", "Use for sensitivity testing or filtering provisional data.", "R/05_clean_container_transport.R", "None.",
  "Container Transport", "(derived)", "observation_is_estimated", "logical", "FALSE: 620 rows (99.36%), TRUE: 4 rows (0.64%)", "Boolean flag indicating estimated observation status (obs_status == 'E')", "Unitless (Flag)", "Unitless", "0 missing; derived from obs_status == 'E'", "YES", "Use for sensitivity testing or filtering estimated data.", "R/05_clean_container_transport.R", "None.",
  "Container Transport", "(derived)", "observation_has_time_series_break", "logical", "FALSE: 623 rows (99.84%), TRUE: 1 row (0.16%)", "Boolean flag indicating a structural break in time series (obs_status == 'B')", "Unitless (Flag)", "Unitless", "0 missing; derived from obs_status == 'B'", "YES", "Note break when conducting longitudinal time-series modeling for affected series.", "R/05_clean_container_transport.R", "None.",
  "Container Transport", "STRUCTURE...DECIMALS (16 columns)", "removed", "character / numeric", "Constant metadata (14 cols) or completely empty (2 cols)", "Constant SDMX file headers, structure definitions, and empty labels", "Unitless", "Unitless", "Removed from processed analysis copy", "NO", "Do not reintroduce into analytical copy; verified constant/empty across all 624 rows.", "outputs/results/container_cleaning_summary.csv", "None.",
  # --- REFINERY ---
  "Refinery Shipments", "Work shift", "work_shift", "character", "3 categories: 'Day shift' (129), 'Evening shift' (39), 'Night shift' (96)", "Operational work shift during which shipment departed/arrived", "Unitless", "Unitless", "0 missing (0.00%)", "YES", "Operational grouping variable.", "data/processed/refinery_clean.csv; R/06_clean_refinery.R", "Exact start and end hours of shifts are not documented.",
  "Refinery Shipments", "Type", "type", "character", "4 categories: 'VB' (205), 'C5+' (36), 'Condensate' (18), 'Iso-Recycle' (5)", "Product / chemical stream type of shipment", "Unitless", "Unitless", "0 missing (0.00%)", "YES", "Categorical stream filter. Do not pool streams without checking compatibility.", "data/processed/refinery_clean.csv", "Full chemical definitions and specifications of 'VB' and 'Iso-Recycle' are not provided.",
  "Refinery Shipments", "Refinery", "refinery", "character", "4 facilities: 'Shiraz' (123), 'Tehran' (87), 'Pars Petrochemical' (36), 'South Pars' (18)", "Name of the processing refinery or petrochemical complex", "Unitless", "Unitless", "0 missing (0.00%)", "YES", "Facility identifier. Safe for grouping and comparing facility activity.", "data/processed/refinery_clean.csv", "Exact physical plant boundaries and ownership are not documented in dataset.",
  "Refinery Shipments", "Origin Departure Date", "origin_departure_date", "character", "Range: '1398/12/27' to '1399/03/20' (Persian Solar Hijri calendar format)", "Recorded shipment departure date from origin facility", "Persian Calendar Date", "Known format, unmapped Gregorian", "0 missing (0.00%)", "Conditional", "Safe for ordinal comparison within Persian calendar. Do NOT perform Gregorian date arithmetic without verified conversion.", "outputs/results/refinery_date_audit.csv; R/06_clean_refinery.R", "Official calendar conversion formula to Gregorian dates is unconfirmed.",
  "Refinery Shipments", "Origin Net Weight", "origin_net_weight", "numeric", "Range: 15,050 to 28,770 (Median: 24,670, Mean: 23,803.94)", "Recorded net weight of shipment at origin facility", "Unknown", "Unknown", "0 missing (0.00%)", "Conditional", "Safe for relative comparisons and within-row difference math. Unsafe for physical mass claims (unit unknown).", "outputs/results/refinery_weight_audit.csv; R/06_clean_refinery.R", "Physical measurement unit (kg, tonnes, lbs?) is completely undocumented.",
  "Refinery Shipments", "Destination Arrival Date", "destination_arrival_date", "character", "Range: '1398/12/27' to '1399/03/20' (Persian Solar Hijri calendar format)", "Recorded shipment arrival date at destination facility", "Persian Calendar Date", "Known format, unmapped Gregorian", "0 missing (0.00%)", "Conditional", "Safe for ordinal comparison. All arrival dates are >= departure dates.", "data/processed/refinery_clean.csv", "Official calendar conversion formula to Gregorian dates is unconfirmed.",
  "Refinery Shipments", "Destination Net Weight", "destination_net_weight", "numeric", "Range: 15,110 to 28,720 (Median: 24,660, Mean: 23,801.78)", "Recorded net weight of shipment at destination facility", "Unknown", "Unknown", "0 missing (0.00%)", "Conditional", "Safe for relative comparisons and within-row difference math. Unsafe for physical mass claims (unit unknown).", "outputs/results/refinery_weight_audit.csv; R/06_clean_refinery.R", "Physical measurement unit is completely undocumented.",
  "Refinery Shipments", "(derived)", "raw_row_number", "integer", "Range: 2 to 265", "1-indexed row number in the raw CSV file (accounting for header)", "Unitless (Index)", "Unitless", "0 missing", "YES", "Audit and provenance traceability index.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "missing_weight_flag", "logical", "Constant value: FALSE (264 rows = 100%)", "Boolean flag indicating missing origin or destination weight", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms complete weight reporting across all rows.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "weight_difference", "numeric", "Range: -510 to +440 (Median: 0, Mean: -2.16)", "Raw numerical subtraction: destination_net_weight - origin_net_weight", "Unknown (same as net weight)", "Unknown", "0 missing", "Conditional", "Safe as raw numerical difference. Do NOT interpret as physical transit loss/gain without verified unit.", "outputs/results/refinery_weight_audit.csv; R/06_clean_refinery.R", "Mechanism for positive/negative weight discrepancies is undocumented.",
  "Refinery Shipments", "(derived)", "weight_difference_sign", "character", "4 categories: 'zero_raw_difference' (101), 'negative_raw_difference' (84), 'positive_raw_difference' (79)", "Directional category of raw weight difference", "Unitless (Category)", "Unitless", "0 missing", "YES", "Categorical indicator of weight discrepancy direction.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "duplicate_group_size", "integer", "1 (262 rows), 2 (2 rows in 1 duplicate group)", "Number of records sharing identical values across all raw fields", "Unitless (Count)", "Unitless", "0 missing", "YES", "Use to identify and account for the 1 exact duplicate pair.", "outputs/results/refinery_duplicate_audit.csv", "Whether duplicate shipment record was intentional double-haul or recording artifact.",
  "Refinery Shipments", "(derived)", "duplicate_flag", "logical", "FALSE: 262 rows (99.24%), TRUE: 2 rows (0.76%)", "Boolean flag indicating membership in an exact duplicate group", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Filter flag for sensitivity analysis with/without duplicate records.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "origin_calendar_year", "integer", "1398 (4 rows), 1399 (260 rows)", "Extracted calendar year component from origin departure date string", "Persian Calendar Year", "Known format", "0 missing", "Conditional", "Year in Solar Hijri calendar; not Gregorian year.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "origin_calendar_month", "integer", "Range: 1 to 12 (Month 1: 82, Month 2: 95, Month 3: 83, Month 12: 4)", "Extracted calendar month component from origin departure date string", "Persian Calendar Month", "Known format", "0 missing", "Conditional", "Month in Solar Hijri calendar; not Gregorian month.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "origin_calendar_day", "integer", "Range: 1 to 31", "Extracted calendar day component from origin departure date string", "Persian Calendar Day", "Known format", "0 missing", "Conditional", "Day in Solar Hijri calendar; not Gregorian day.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "destination_calendar_year", "integer", "1398 (4 rows), 1399 (260 rows)", "Extracted calendar year component from destination arrival date string", "Persian Calendar Year", "Known format", "0 missing", "Conditional", "Year in Solar Hijri calendar; not Gregorian year.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "destination_calendar_month", "integer", "Range: 1 to 12 (Month 1: 82, Month 2: 95, Month 3: 83, Month 12: 4)", "Extracted calendar month component from destination arrival date string", "Persian Calendar Month", "Known format", "0 missing", "Conditional", "Month in Solar Hijri calendar; not Gregorian month.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "destination_calendar_day", "integer", "Range: 1 to 31", "Extracted calendar day component from destination arrival date string", "Persian Calendar Day", "Known format", "0 missing", "Conditional", "Day in Solar Hijri calendar; not Gregorian day.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "date_format_flag", "character", "Constant value: 'valid_yyyy_mm_dd_components' (264 rows = 100%)", "Validation flag verifying standard YYYY/MM/DD string structure and component ranges", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms syntactic validity of date strings.", "R/06_clean_refinery.R", "None.",
  "Refinery Shipments", "(derived)", "date_label_order_flag", "character", "Constant value: 'destination_label_after_or_equal_origin' (264 rows = 100%)", "Validation flag verifying arrival date string is >= departure date string", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms logical sequence of shipment dates.", "R/06_clean_refinery.R", "None.",

  # --- GLOBAL ENERGY OWNERSHIP ---
  "Global Energy Ownership", "Parent GEM Entity ID", "parent_gem_entity_id", "character", "748 unique IDs (e.g. E100001015587, E100001010595)", "Global Energy Monitor identifier for the ultimate parent entity", "Unitless (ID)", "Unitless", "0 missing (0.00%); 100% matched to All Entities sheet", "YES", "Primary corporate ownership identifier. 11,760 / 11,760 matched to All Entities.", "data/processed/energy_ownership_clean.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "Parent", "parent", "character", "748 unique names (e.g. Government of Russia, Enbridge, Enterprise Products Partners)", "Full legal/trading name of the ultimate parent entity", "Unitless (Name)", "Unitless", "0 missing (0.00%)", "YES", "Human-readable parent company label.", "data/processed/energy_ownership_clean.csv", "None.",
  "Global Energy Ownership", "Parent Registration Country", "parent_registration_country", "character", "84 unique countries; 2,301 missing (19.57%)", "Jurisdiction of legal incorporation / registration of the parent entity", "Unitless (Country)", "Unitless", "2,301 missing values preserved as NA", "Conditional", "CRITICAL: Represents owner corporate jurisdiction ONLY. Strictly NOT physical pipeline asset location.", "outputs/results/energy_ownership_cleaning_summary.csv; R/07_clean_energy_ownership.R", "Physical route coordinates are not in this sheet.",
  "Global Energy Ownership", "Parent Headquarters Country", "parent_headquarters_country", "character", "82 unique countries; 2,020 missing (17.18%)", "Country where parent company global headquarters is located", "Unitless (Country)", "Unitless", "2,020 missing values preserved as NA", "Conditional", "CRITICAL: Represents owner headquarters ONLY. Strictly NOT physical pipeline asset location.", "outputs/results/energy_ownership_cleaning_summary.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "Project", "project", "character", "1,685 distinct project names (e.g. Druzhba Oil Pipeline, Keystone Pipeline)", "Physical name of the oil/NGL pipeline project", "Unitless (Name)", "Unitless", "0 missing (0.00%)", "YES", "Physical asset name.", "data/processed/energy_ownership_clean.csv", "None.",
  "Global Energy Ownership", "Share", "share", "character", "Text representations of numeric percentage share or blanks", "Raw percentage ownership share claimed by the parent entity", "Percentage (%)", "Known", "3,643 missing values (30.98%)", "Conditional", "Superseded by share_numeric in processed dataset.", "outputs/results/energy_ownership_cleaning_summary.csv", "None.",
  "Global Energy Ownership", "Ownership Path", "ownership_path", "character", "Text string detailing chain of ownership from parent through intermediaries to immediate owner", "Corporate equity holding chain", "Unitless (Text)", "Unitless", "0 missing (0.00%)", "YES", "Documents intermediate holding companies and ownership structures.", "data/processed/energy_ownership_clean.csv", "None.",
  "Global Energy Ownership", "Immediate Project Owner", "immediate_project_owner", "character", "472 unique names (e.g. Transneft, Enbridge Pipelines Inc.)", "Name of the direct operating entity that immediately owns the project", "Unitless (Name)", "Unitless", "0 missing (0.00%)", "YES", "Direct operating owner entity label.", "data/processed/energy_ownership_clean.csv", "None.",
  "Global Energy Ownership", "Immediate Project Owner GEM Entity ID", "immediate_project_owner_gem_entity_id", "character", "472 unique IDs (e.g. E100000003733, E100001015277)", "GEM entity identifier for the immediate project operating owner", "Unitless (ID)", "Unitless", "0 missing (0.00%); 100% matched to All Entities sheet", "YES", "Direct owner entity key. 11,760 / 11,760 matched to All Entities.", "data/processed/energy_ownership_clean.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "Tracker", "tracker", "character", "Constant value: 'Global Oil Infrastructure Tracker' (11,760 rows = 100%)", "Name of the originating GEM database tracker", "Unitless (Metadata)", "Unitless", "0 missing (0.00%)", "YES", "Provenance metadata.", "data/processed/energy_ownership_clean.csv", "None.",
  "Global Energy Ownership", "Status", "status", "character", "8 categories: 'operating' (9,273), 'cancelled' (861), 'proposed' (678), 'construction' (388), 'idle' (332), 'retired' (129), 'mothballed' (61), 'shelved' (37); 1 missing", "Operational status of the pipeline project", "Unitless (Status)", "Unitless", "1 missing value preserved as NA", "YES", "Safe for filtering by asset lifecycle status.", "data/processed/energy_ownership_clean.csv", "Status of 1 missing row is unknown.",
  "Global Energy Ownership", "CapacityBOEd", "capacity_bo_ed", "character", "Raw text numbers or placeholder '--' strings", "Rated daily throughput capacity of the pipeline project", "Barrels of Oil Equivalent per Day (BOE/d)", "Known", "2,359 missing or placeholder '--' values", "Conditional", "Superseded by capacity_boed_numeric in processed dataset.", "outputs/results/energy_ownership_cleaning_summary.csv", "None.",
  "Global Energy Ownership", "ProjectID", "project_id", "character", "1,685 unique project IDs (e.g. P0653, P2020, P0060)", "GEM unique identifier for the physical pipeline project", "Unitless (ID)", "Unitless", "0 missing (0.00%)", "YES", "CRITICAL: ProjectID is NOT a unique row key. 1,351 projects have multiple ownership paths. Do not collapse rows.", "outputs/results/energy_pipeline_project_audit.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "capacity_boed_raw", "character", "Verbatim raw text strings from CapacityBOEd", "Provenance snapshot of original capacity text prior to conversion", "BOE/d text", "Known", "0 missing (retains original text)", "YES", "Audit and provenance tracking.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "share_numeric", "numeric", "Range: 0.00 to 100.00% (Median: 11.57%, Mean: 33.66%, 3,643 NA)", "Numeric conversion of equity ownership share percentage", "Percentage (%)", "Known", "3,643 missing values (30.98%) preserved as NA; NEVER converted to zero", "Conditional", "Safe for equity weighting where present. Unsafe for complete ownership summation/HHI when missing.", "data/processed/energy_ownership_clean.csv; R/07_clean_energy_ownership.R", "Basis of missing share values in GEM database.",
  "Global Energy Ownership", "(derived)", "share_missing", "logical", "FALSE: 8,117 rows (69.02%), TRUE: 3,643 rows (30.98%)", "Boolean flag indicating whether Share is missing (NA)", "Unitless (Flag)", "Unitless", "0 missing; derived from is.na(share_numeric)", "YES", "Filter flag for complete-case ownership analyses.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "capacity_missing_or_placeholder", "logical", "FALSE: 9,401 rows (79.94%), TRUE: 2,359 rows (20.06%)", "Boolean flag indicating capacity was '--', blank, or non-numeric", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Filter flag to exclude missing/placeholder capacity rows before numeric ops.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "capacity_boed_numeric", "numeric", "Range: 0 to 5,000,000 BOE/d (Median: 200,000, Mean: 346,036; 2,359 NA)", "Numeric conversion of pipeline throughput capacity", "Barrels of Oil Equivalent per Day (BOE/d)", "Known", "2,359 placeholder/missing values converted to NA; NEVER imputed", "Conditional", "CRITICAL: Must NEVER be summed across ownership-path rows for the same project. Represents project total, not path-specific slice.", "data/processed/energy_ownership_clean.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "exact_duplicate_flag", "logical", "FALSE: 11,698 rows (99.47%), TRUE: 62 rows (0.53%)", "Boolean flag indicating row is an exact duplicate across all 13 original columns", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Preserved for provenance; rows retained and flagged.", "outputs/results/energy_ownership_cleaning_summary.csv; R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "exact_duplicate_group_size", "integer", "1 (11,698 rows), 2 (62 rows across 31 duplicate pairs)", "Size of exact duplicate group (1 = unique row, 2 = duplicate pair)", "Unitless (Count)", "Unitless", "0 missing", "YES", "Use for sensitivity analysis with/without duplicate rows.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "parent_entity_id_missing", "logical", "Constant value: FALSE (11,760 rows = 100%)", "Boolean flag indicating missing parent entity ID", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms 0 missing parent IDs.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "parent_entity_id_matched", "logical", "Constant value: TRUE (11,760 rows = 100%)", "Boolean flag confirming parent entity ID exists in 'All Entities' sheet", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms 100% referential integrity with All Entities master register.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "imm_owner_entity_id_missing", "logical", "Constant value: FALSE (11,760 rows = 100%)", "Boolean flag indicating missing immediate owner entity ID", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms 0 missing immediate owner IDs.", "R/07_clean_energy_ownership.R", "None.",
  "Global Energy Ownership", "(derived)", "imm_owner_entity_id_matched", "logical", "Constant value: TRUE (11,760 rows = 100%)", "Boolean flag confirming immediate owner ID exists in 'All Entities' sheet", "Unitless (Flag)", "Unitless", "0 missing", "YES", "Confirms 100% referential integrity with All Entities master register.", "R/07_clean_energy_ownership.R", "None."
)
# ---------------------------------------------------------------------------
# 3. Validation checks
# ---------------------------------------------------------------------------
# Verify that all columns in clean datasets are accounted for in data dictionary
dict_clean_cols_cont <- data_dictionary |>
  filter(dataset == "Container Transport", !cleaned_variable_name %in% c("removed", "NA (raw only)")) |>
  pull(cleaned_variable_name)

clean_cont <- readr::read_csv(container_proc_path, col_types = readr::cols(.default = readr::col_character()))
missing_cont_dict <- setdiff(names(clean_cont), dict_clean_cols_cont)
if (length(missing_cont_dict) > 0) {
  stop("Container clean dataset has undocumented columns: ", paste(missing_cont_dict, collapse = ", "))
}

dict_clean_cols_ref <- data_dictionary |>
  filter(dataset == "Refinery Shipments", !cleaned_variable_name %in% c("removed", "NA (raw only)")) |>
  pull(cleaned_variable_name)

clean_ref <- readr::read_csv(refinery_proc_path, col_types = readr::cols(.default = readr::col_character()))
missing_ref_dict <- setdiff(names(clean_ref), dict_clean_cols_ref)
if (length(missing_ref_dict) > 0) {
  stop("Refinery clean dataset has undocumented columns: ", paste(missing_ref_dict, collapse = ", "))
}

dict_clean_cols_energy <- data_dictionary |>
  filter(dataset == "Global Energy Ownership", !cleaned_variable_name %in% c("removed", "NA (raw only)")) |>
  pull(cleaned_variable_name)

clean_energy <- readr::read_csv(energy_proc_path, col_types = readr::cols(.default = readr::col_character()))
missing_energy_dict <- setdiff(names(clean_energy), dict_clean_cols_energy)
if (length(missing_energy_dict) > 0) {
  stop("Energy clean dataset has undocumented columns: ", paste(missing_energy_dict, collapse = ", "))
}

# Verify required dictionary columns exist
required_dict_cols <- c(
  "dataset", "original_variable_name", "cleaned_variable_name", "data_type",
  "observed_values_range", "confirmed_meaning", "confirmed_unit",
  "unit_known_or_unknown", "missing_value_treatment", "is_safe_for_analysis",
  "analytical_restrictions", "source_evidence", "unresolved_questions"
)
stopifnot(identical(names(data_dictionary), required_dict_cols))

# Verify required provenance columns exist
required_prov_cols <- c(
  "dataset", "source_file", "source_format", "raw_file_path",
  "processed_file_path", "audit_script", "cleaning_script",
  "phase1_audit_status", "phase2_cleaning_status", "known_data_quality_issues",
  "unresolved_provenance_questions", "is_analysis_ready"
)
stopifnot(identical(names(provenance_register), required_prov_cols))

# ---------------------------------------------------------------------------
# 4. Write outputs
# ---------------------------------------------------------------------------
readr::write_csv(data_dictionary, dict_path, na = "")
readr::write_csv(provenance_register, prov_path, na = "")

cat("\n=== Data Dictionary and Provenance Register Generated Successfully ===\n")
cat("Data Dictionary written to:      ", dict_path, "\n")
cat("  Total documented variables:    ", nrow(data_dictionary), "\n")
cat("Provenance Register written to:  ", prov_path, "\n")
cat("  Total documented datasets:     ", nrow(provenance_register), "\n\n")

cat("Summary of Documented Datasets:\n")
print(provenance_register |> select(dataset, is_analysis_ready))
cat("\nDone.\n")
