# ==============================================================================
# Script:   Wharminda_2024_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Wharminda 2024
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2024)
#   SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx
#       sheet In-Season_soil_moisture  - gravimetric moisture % (GS31 15 Aug, GS69 23 Sep)
#       sheet Wharminda_ShortID_treatments - plot key (row, bay -> plot, treatment)
#   Batch-38044-38103-...xlsx - APAL baseline (20 Mar 2024) nitrate and ammonium
#       sheet "ID information" gives sample number -> plot
# OUTPUTS (same folder)
#   Wharminda_2024_depth_layers_N_and_H2O.csv
#   Wharminda_2024_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup and Copeville)
#   water mm   = moisture/100 x bulk density x layer cm x 10; moisture above 40 or below 0 -> NA
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   sample_event: baseline (20 Mar, N only, 8 plots, no soil water), GS31 (15 Aug, water
#   only, 24 plots, T2 and T6), GS69 (23 Sep, water only, 36 plots, T2, T4, T6).
#   Moisture is already gravimetric % in the workbook (no wet / dry weights available).
#   APAL layers (0-10, 10-20, 20-40, 40-60, 60-100) match the water layers exactly.
#   APAL sample "Verran 8a" is repeated for all 5 layers; depth comes from SampleDepth.
#   Batch 38042 (Lucerne Hay feed test) is not used.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/3. SSO2_Wharminda-Masters"
data_dir   <- file.path(site_dir, "2. Soil Data and Nutrition/2024")
out_dir    <- data_dir
water_file <- file.path(data_dir, "SSO2_Wharminda_Soil data and Nutrition Budgets_2024.xlsx")
n_file     <- list.files(data_dir, pattern = "Batch-38044.*\\.xlsx$", full.names = TRUE)
n_file     <- n_file[!str_detect(basename(n_file), "^~\\$")]
stopifnot(file.exists(water_file), length(n_file) == 1)

bulk_density  <- 1.3
baseline_date <- as.Date("2024-03-20")   # APAL SamplingDate and ID sheet

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (row, bay -> plot, treatment) ----
key <- read_excel(water_file, sheet = "Wharminda_ShortID_treatments", skip = 1,
                  .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(Plot), row = as.integer(row), bay = as.integer(bay),
            treatment_label = label, treatment_desc = TreatmentDescription) %>%
  filter(!is.na(plot))

# ---- Step 3: in-season gravimetric moisture (GS31, GS69) ----
# Columns by position: 1 Row, 2 Bay, 8 Treatment Combo, 11 SampleDepth, 19 GS31 %, 20 GS69 %
ms_raw <- read_excel(water_file, sheet = "In-Season_soil_moisture", col_names = FALSE,
                     col_types = "text", .name_repair = "unique_quiet")
parse_hdr_date <- function(txt) {
  m <- str_match(txt, "\\((\\d+)/(\\d+)/(\\d+)\\)")
  as.Date(sprintf("20%02d-%02d-%02d", as.integer(m[, 4]), as.integer(m[, 3]), as.integer(m[, 2])))
}
gs31_date <- parse_hdr_date(ms_raw[[19]][1])
gs69_date <- parse_hdr_date(ms_raw[[20]][1])

ms <- ms_raw[-(1:2), ] %>%
  transmute(row = as.integer(`...1`), bay = as.integer(`...2`), combo = `...8`,
            depth_label = `...11`, GS31 = as.numeric(`...19`), GS69 = as.numeric(`...20`)) %>%
  filter(depth_label %in% c("0-10", "10-20", "20-40", "40-60", "60-100")) %>%
  pivot_longer(c(GS31, GS69), names_to = "sample_event", values_to = "moisture_raw") %>%
  filter(!is.na(moisture_raw)) %>%
  mutate(sample_date = if_else(sample_event == "GS31", gs31_date, gs69_date)) %>%
  left_join(key, by = c("row", "bay"))

# ---- Step 3b: APAL baseline nitrate and ammonium ----
read_apal <- function(file, sheet) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))
}
sheets <- excel_sheets(n_file)

id_map <- read_apal(n_file, str_subset(sheets, "^ID information")) %>%
  transmute(sample_no = str_extract(SampleName, "\\d+"), plot = str_trim(`Plot no.`)) %>%
  distinct()

n_lab <- read_apal(n_file, "Data") %>%
  filter(str_starts(SampleName, "Verran")) %>%
  transmute(sample_no = str_extract(SampleName, "\\d+"),
            depth_label = case_when(SampleDepth == "0-0.1"   ~ "0-10",
                                    SampleDepth == "0.1-0.2" ~ "10-20",
                                    SampleDepth == "0.2-0.4" ~ "20-40",
                                    SampleDepth == "0.4-0.6" ~ "40-60",
                                    SampleDepth == "0.6-1"   ~ "60-100"),
            apal_date = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`)) %>%
  left_join(id_map, by = "sample_no") %>%
  left_join(key %>% select(plot, treatment_label, treatment_desc), by = "plot")

# ---- Step 4: layers = water events + baseline (N only) ----
water_part <- ms %>%
  transmute(sample_event, sample_date, plot, treatment_label, treatment_desc, depth_label,
            moisture_pct = moisture_raw)
base_part <- n_lab %>%
  transmute(sample_event = "baseline", sample_date = baseline_date, plot, treatment_label,
            treatment_desc, depth_label, moisture_pct = NA_real_)

layers <- bind_rows(base_part, water_part) %>%
  mutate(dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         moisture_pct = if_else(moisture_pct > 40 | moisture_pct < 0, NA_real_, moisture_pct),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = depth_label) %>%
  select(-dm) %>%
  left_join(n_lab %>% transmute(sample_date = baseline_date, plot, n_depth = depth_label,
                                no3_mgkg, nh4_mgkg),
            by = c("sample_date", "plot", "n_depth")) %>%
  mutate(mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10)

# ---- Step 5: depth groups ----
groups <- layers %>%
  mutate(depth_group = case_when(top_cm < 20 ~ "0-20", top_cm < 60 ~ "20-60", TRUE ~ "60-100"),
         group_thick = if_else(depth_group == "0-20", 20, 40)) %>%
  group_by(sample_date, sample_event, plot, treatment_label, treatment_desc, depth_group, group_thick) %>%
  summarise(n_layers = n(), thick_have = sum(thick_cm),
            water_mm = if (thick_have == first(group_thick)) strict_sum(water_mm) else NA_real_,
            mineral_n_kgha = if (thick_have == first(group_thick)) strict_sum(mineral_n_kgha) else NA_real_,
            .groups = "drop") %>%
  select(-group_thick, -thick_have)

# ---- Step 6: checks (paste output back) ----
cat("\n--- 1. Rows and plots per sampling event (layers) ---\n")
print(layers %>% group_by(sample_event, sample_date) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(), .groups = "drop"))

cat("\n--- 2. Moisture above 40 or below 0 (set to NA) ---\n")
print(ms %>% filter(moisture_raw > 40 | moisture_raw < 0) %>%
        select(sample_event, row, bay, depth_label, moisture_raw))

cat("\n--- 3. Plots with no key match, and treatment label disagreements (both should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(sample_event, plot))
print(ms %>% filter(!is.na(combo), combo != treatment_label) %>% distinct(row, bay, combo, treatment_label))

cat("\n--- 4. APAL: sample number -> plot, dates, and layer count ---\n")
print(n_lab %>% group_by(sample_no, plot) %>%
        summarise(n_layers = n(), date = paste(unique(na.omit(apal_date)), collapse = ","),
                  .groups = "drop"))
cat("Baseline date used:", format(baseline_date), "\n")

cat("\n--- 5. Non-missing group totals by event and depth group ---\n")
print(groups %>% group_by(sample_event, depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"))

cat("\n--- 6. Profile (0-100) water by event: min / mean / max ---\n")
print(groups %>% filter(sample_event != "baseline") %>% group_by(sample_event, sample_date, plot) %>%
        summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_, .groups = "drop") %>%
        group_by(sample_event, sample_date) %>%
        summarise(n = sum(!is.na(profile_water)), min = min(profile_water, na.rm = TRUE),
                  mean = mean(profile_water, na.rm = TRUE), max = max(profile_water, na.rm = TRUE),
                  .groups = "drop"))

# ---- Step 7: N checks ----
cat("\n--- 7. N: values set to 0 (below detection) and baseline profile N ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "\n")
print(groups %>% filter(sample_event == "baseline") %>% group_by(plot, treatment_label) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop"))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Wharminda_2024_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Wharminda_2024_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")

