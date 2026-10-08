# ==============================================================================
# Script:   Wharminda_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Wharminda 2026
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2026, plus the 2024 workbook for the plot key)
#   GRDC_SAND__Wharminda_O2_PS26_Water_Sowing_soil_sampling.xlsx - sheet "Gravimetric Moisture"
#   Batch-50050-...-2026-04-24.xlsx                              - APAL nitrate and ammonium
#   GRDC_Sands_O2_Wharminda_PS26_Nutrition_bulk_submission.xlsx  - sample number -> row, range, layer
#   ../2024/SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx - plot key
# OUTPUTS (same folder)
#   Wharminda_2026_depth_layers_N_and_H2O.csv
#   Wharminda_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup and Copeville)
#   moisture % = (wet - dry) / dry x 100 (the sheet's own formula; wet and dry treated as net
#   weights, tray not weighed - TO CONFIRM); above 40% or below 0 set to NA
#   water mm   = moisture/100 x bulk density x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   One event: baseline (pre-sow PS26), 48 plots, sampled 24 Mar 2026 (APAL SamplingDate and
#   bulk submission). The water sheet has a note "Sample Date: 26th March 2026" - 24 Mar used.
#   APAL has 5 layers (60-100 bulked); water has 6 (60-80, 80-100). The 60-100 N value is used
#   for both water layers (each with its own thickness).
#   Plot = (range - 1) x 40 + row. APAL SampleName = sample number in the bulk submission sheet.
#   Manual override: C7 80-100 cm moisture blanked (22% vs 5.6% in the layer above; dry weight
#   looks mistyped). C41 60-80 cm is negative (-26%) and is blanked by the moisture rule.
#   C112 80-100 cm has no weights (tipped out); APAL sample 269 (C112 60-100) is missing.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/3. SSO2_Wharminda-Masters"
data_dir   <- file.path(site_dir, "2. Soil Data and Nutrition/2026")
out_dir    <- data_dir
water_file <- file.path(data_dir, "GRDC_SAND__Wharminda_O2_PS26_Water_Sowing_soil_sampling.xlsx")
bulk_file  <- file.path(data_dir, "GRDC_Sands_O2_Wharminda_PS26_Nutrition_bulk_submission.xlsx")
key_file   <- file.path(site_dir, "2. Soil Data and Nutrition/2024",
                        "SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx")
n_file     <- list.files(data_dir, pattern = "Batch-50050.*\\.xlsx$", full.names = TRUE)
n_file     <- n_file[!str_detect(basename(n_file), "^~\\$")]
stopifnot(file.exists(water_file), file.exists(bulk_file), file.exists(key_file), length(n_file) == 1)

bulk_density <- 1.3
sample_date  <- as.Date("2026-03-24")   # APAL SamplingDate; water sheet note says 26 Mar

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key ----
key <- read_excel(key_file, sheet = "Wharminda_ShortID_treatments", skip = 1,
                  .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(Plot), treatment_label = label, treatment_desc = TreatmentDescription) %>%
  filter(!is.na(plot))

# ---- Step 3: water (wet / dry weights, 6 layers) ----
water <- read_excel(water_file, sheet = "Gravimetric Moisture", col_types = "text",
                    .name_repair = "unique_quiet") %>%
  filter(!is.na(Row), !is.na(Range)) %>%
  transmute(plot = paste0("C", (as.integer(Range) - 1) * 40 + as.integer(Row)),
            combo = `Treatment Combo`, depth_label = str_trim(Depth),
            wet = suppressWarnings(as.numeric(WET)), dry = suppressWarnings(as.numeric(DRY)))

# ---- Step 4: APAL nitrate and ammonium ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))
}

# Bulk submission, by position: 6 sample #, 7 Row, 8 Range, 9 Treatment Combo, 11 Depth
bulk <- read_excel(bulk_file, col_names = FALSE, col_types = "text", skip = 1,
                   .name_repair = "unique_quiet") %>%
  filter(!is.na(`...6`)) %>%
  transmute(sample_no = as.integer(`...6`),
            plot = paste0("C", (as.integer(`...8`) - 1) * 40 + as.integer(`...7`)),
            combo = `...9`, n_depth = str_trim(`...11`))

n_lab <- read_apal(n_file) %>%
  transmute(sample_no = as.integer(SampleName), apal_depth = str_trim(SampleDepth),
            apal_date = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`)) %>%
  left_join(bulk, by = "sample_no")

# ---- Step 5: layers ----
layers <- water %>%
  mutate(sample_event = "baseline", sample_date = sample_date,
         dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / dry * 100,
         moisture_pct = if_else(moisture_pct > 40 | moisture_pct < 0, NA_real_, moisture_pct),
         # manual override: C7 80-100 cm (22% vs 5.6% above; dry weight looks mistyped)
         moisture_pct = if_else(plot == "C7" & top_cm == 80, NA_real_, moisture_pct),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = if_else(top_cm >= 60, "60-100", depth_label)) %>%
  select(-dm) %>%
  left_join(key, by = "plot") %>%
  left_join(n_lab %>% select(plot, n_depth, no3_mgkg, nh4_mgkg), by = c("plot", "n_depth")) %>%
  mutate(mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10)

# ---- Step 6: depth groups ----
groups <- layers %>%
  mutate(depth_group = case_when(top_cm < 20 ~ "0-20", top_cm < 60 ~ "20-60", TRUE ~ "60-100"),
         group_thick = if_else(depth_group == "0-20", 20, 40)) %>%
  group_by(sample_date, sample_event, plot, treatment_label, treatment_desc, depth_group, group_thick) %>%
  summarise(n_layers = n(), thick_have = sum(thick_cm),
            water_mm = if (thick_have == first(group_thick)) strict_sum(water_mm) else NA_real_,
            mineral_n_kgha = if (thick_have == first(group_thick)) strict_sum(mineral_n_kgha) else NA_real_,
            .groups = "drop") %>%
  select(-group_thick, -thick_have)

# ---- Step 7: checks (paste output back) ----
cat("\n--- 1. Rows and plots (layers) ---\n")
print(layers %>% group_by(sample_event, sample_date) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(), .groups = "drop"))

cat("\n--- 2. Missing weights, and moisture above 40 or below 0 (set to NA) ---\n")
print(layers %>% mutate(raw_moist = (wet - dry) / dry * 100) %>%
        filter(is.na(wet) | is.na(dry) | raw_moist > 40 | raw_moist < 0) %>%
        select(plot, depth_label, wet, dry, raw_moist))

cat("\n--- 3. Key checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(plot))
print(layers %>% filter(combo != treatment_label) %>% distinct(plot, combo, treatment_label))
print(n_lab %>% filter(is.na(plot) | apal_depth != n_depth) %>% select(sample_no, plot, apal_depth, n_depth))

cat("\n--- 4. APAL vs water: plots, dates, missing N samples ---\n")
cat("APAL plots not in water:", paste(setdiff(unique(n_lab$plot), unique(water$plot)), collapse = ", "), "\n")
cat("Water plots not in APAL:", paste(setdiff(unique(water$plot), unique(n_lab$plot)), collapse = ", "), "\n")
cat("APAL sample date(s):", format(unique(n_lab$apal_date)), " | date used:", format(sample_date), "\n")
cat("Bulk samples with no APAL result:\n")
print(bulk %>% anti_join(n_lab %>% select(sample_no), by = "sample_no"))

cat("\n--- 5. Non-missing group totals by depth group ---\n")
print(groups %>% group_by(depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"))

cat("\n--- 6. Profile (0-100) water: min / mean / max ---\n")
print(groups %>% group_by(plot) %>%
        summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_water)), min = min(profile_water, na.rm = TRUE),
                  mean = mean(profile_water, na.rm = TRUE), max = max(profile_water, na.rm = TRUE)))

# ---- Step 8: N checks ----
cat("\n--- 7. N: values set to 0 (below detection) and profile N ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "\n")
print(groups %>% group_by(plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))

# ---- Step 9: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Wharminda_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Wharminda_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
