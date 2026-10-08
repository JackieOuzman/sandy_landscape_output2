# ==============================================================================
# Script:   Wharminda_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Wharminda 2025
# Purpose:  Mineral N (kg/ha) by depth layer and depth group (N only; no soil water found)
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2025, plus the 2024 workbook for the plot key)
#   O2_Wharminda_Yr2_DS1APAL_25.xlsx   - APAL N, layers 10-20 to 60-100 (224 samples)
#   O2_Wharminda_Yr2SB4APAL_25.xlsx    - APAL N, layer 0-10 (56 samples)
#   GRDC_Wharminda_Core_PS25_nutrition_analysis_interog.xlsx
#       sheet Sample_list_Rev190325: barcode -> row, bay, treatment, layer
#   ../2024/SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx
#       sheet Wharminda_ShortID_treatments: plot key (plot, treatment description)
# OUTPUTS (same folder)
#   Wharminda_2025_depth_layers_N_and_H2O.csv
#   Wharminda_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup and Copeville)
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   Pre-sow sampling (PS25), 56 plots x 5 layers. APAL files have no sampling date; 5 May 2025
#   is the lab received date. sample_date is left NA - set sample_date_pre below if known.
#   No soil water file found for 2025 (trial wind-eroded, resown to barley in August).
#   Plot = (bay - 1) x 40 + row. Sample list row is blank for sample 013; its row is filled from
#   the other samples in its block of five (sample numbers run in fives per plot) -> C4, RT5_T4.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/3. SSO2_Wharminda-Masters"
data_dir <- file.path(site_dir, "2. Soil Data and Nutrition/2025")
out_dir  <- data_dir
list_file <- file.path(data_dir, "GRDC_Wharminda_Core_PS25_nutrition_analysis_interog.xlsx")
key_file  <- file.path(site_dir, "2. Soil Data and Nutrition/2024",
                       "SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx")
n_files   <- list.files(data_dir, pattern = "Yr2_DS1APAL|Yr2SB4APAL", full.names = TRUE)
n_files   <- n_files[!str_detect(basename(n_files), "^~\\$")]
stopifnot(file.exists(list_file), file.exists(key_file), length(n_files) == 2)

bulk_density    <- 1.3
sample_date_pre <- as.Date(NA)    # sampling date not found; replace with as.Date("2025-MM-DD") if known

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (plot -> treatment) ----
key <- read_excel(key_file, sheet = "Wharminda_ShortID_treatments", skip = 1,
                  .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(Plot), treatment_label = label, treatment_desc = TreatmentDescription) %>%
  filter(!is.na(plot))

# ---- Step 3: sample list (barcode -> plot, layer) ----
sample_list <- read_excel(list_file, sheet = str_subset(excel_sheets(list_file), "^Sample_list"),
                          col_types = "text", .name_repair = "unique_quiet") %>%
  filter(!is.na(`Sample #`), !is.na(Barcode)) %>%
  transmute(barcode = str_trim(Barcode), bay = as.integer(Bay),
            row_raw = suppressWarnings(as.integer(str_trim(Row))),
            sample_no = as.integer(`Sample #`), depth_label = str_trim(Depth),
            combo = `Treatment Combo`) %>%
  mutate(block = (sample_no - 1) %/% 5) %>%
  group_by(bay, block) %>%
  mutate(row = max(row_raw, na.rm = TRUE)) %>%       # fills the one blank row (sample 013)
  ungroup() %>%
  mutate(plot = paste0("C", (bay - 1) * 40 + row)) %>%
  left_join(key, by = "plot")

# ---- Step 4: APAL nitrate and ammonium (two files) ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))
}
n_lab <- bind_rows(lapply(n_files, read_apal)) %>%
  transmute(barcode = str_trim(Barcode), sample_name = SampleName,
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# ---- Step 5: layers ----
layers <- n_lab %>%
  left_join(sample_list, by = "barcode") %>%
  mutate(sample_event = "baseline", sample_date = sample_date_pre,
         dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         bulk_density = bulk_density,
         moisture_pct = NA_real_, water_mm = NA_real_,
         n_depth = depth_label,
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10) %>%
  select(-dm)

# ---- Step 5b: depth groups ----
groups <- layers %>%
  mutate(depth_group = case_when(top_cm < 20 ~ "0-20", top_cm < 60 ~ "20-60", TRUE ~ "60-100"),
         group_thick = if_else(depth_group == "0-20", 20, 40)) %>%
  group_by(sample_date, sample_event, plot, treatment_label, treatment_desc, depth_group, group_thick) %>%
  summarise(n_layers = n(), thick_have = sum(thick_cm),
            water_mm = NA_real_,
            mineral_n_kgha = if (thick_have == first(group_thick)) strict_sum(mineral_n_kgha) else NA_real_,
            .groups = "drop") %>%
  select(-group_thick, -thick_have)

# ---- Step 6: checks (paste output back) ----
cat("\n--- 1. Rows, plots and layers ---\n")
print(layers %>% summarise(n_rows = n(), n_plots = n_distinct(plot), n_barcodes = n_distinct(barcode)))
print(layers %>% count(depth_label))

cat("\n--- 2. Samples with no plot or treatment match (should be 0 rows) ---\n")
print(layers %>% filter(is.na(plot) | is.na(treatment_label)) %>% select(barcode, sample_name, plot))
cat("Sample list vs key treatment label disagreements (should be 0 rows):\n")
print(layers %>% filter(combo != treatment_label) %>% distinct(plot, combo, treatment_label))


cat("\n--- 3. Plots without 5 layers, and the repaired sample 013 ---\n")
print(layers %>% count(plot) %>% filter(n != 5))
print(layers %>% filter(sample_no == 13) %>% select(sample_name, plot, treatment_label, depth_label))

cat("\n--- 4. Group totals present ---\n")
print(groups %>% group_by(depth_group) %>%
        summarise(n_groups = n(), n_N = sum(!is.na(mineral_n_kgha)), .groups = "drop"))

cat("\n--- 5. N: values set to 0 and baseline profile N ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "\n")
print(groups %>% group_by(plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))

# ---- Step 7: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Wharminda_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Wharminda_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
