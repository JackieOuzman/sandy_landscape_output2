# ==============================================================================
# Script:   Bute_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Bute-Kreig 2025
# Purpose:  Baseline soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/1.SSO2_Soil data and nutrition)
#   2025/Baseline soil sampling gravimetric water content.xlsx - sheet "SSO2": tin, wet, dry weights
#        72 samples = 12 sample points x 6 layers (A 0-10, B 10-20, C 20-40, D 40-60, E 60-80, F 80-100)
#   Baseline/Baseline soil sample results 2025_Bute.xlsx       - sheet "Data": APAL nitrate and ammonium
#        12 sample points x layers A-E (0-10, 10-20, 20-40, 40-60 and ONE 60-90 layer)
#   Baseline/SSO2_Bute_Baseline_soil_characterisation Krieg paddock.xlsx - sampling date (9 Apr 2025)
#   2026/Copy of CSV of Soil N_Sowing. .xlsx - sheet "Sheet1", columns P:AS: plot key
#        (plot no, bay, row, ripping and nutrition treatments)
# OUTPUTS (written to <site>/1.SSO2_Soil data and nutrition/2025)
#   Bute_2025_depth_layers_N_and_H2O.csv
#   Bute_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   moisture % = (wet - dry) / (dry - tin) x 100;  water mm = moisture/100 x BD x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x BD x layer cm / 10;  BD assumed 1.3
#   "<" results set to 0; moisture above 40 or below 0 set to NA
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up to the
#   full group thickness and none are NA
# NOTES
#   * Only ONE baseline event; 12 sample points (1-12). The APAL sheet gives the trial row and
#     column (bay) of each point; row + bay is looked up in the plot key to give the plot
#     (plot = "C" + plot no, as the other sites) and treatment.
#   * APAL reports one 60-90 cm N layer. It is applied to BOTH water layers (60-80 and 80-100),
#     so the 60-100 group has N. Set apply_60_90_to_60_100 <- FALSE to leave 60-100 N as NA.
#   * Water sheet has "9C" twice; the second one (row order, between 9C and 11C) is taken as 10C.
#   * APAL SamplingDate is blank (received 8 May 2025). The sampling date used is 9 Apr 2025,
#     from the Krieg paddock soil characterisation sheet.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/5. SS02_Bute-Kreig/1.SSO2_Soil data and nutrition"
dir_2025 <- file.path(site_dir, "2025")
dir_base <- file.path(site_dir, "Baseline")
out_dir  <- dir_2025

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
water_file <- find_file(dir_2025, "Baseline.soil.sampling.gravimetric.water.content\\.xlsx$")
n_file     <- find_file(dir_base, "Baseline.soil.sample.results.2025_Bute\\.xlsx$")
char_file  <- find_file(dir_base, "characterisation.Krieg.paddock\\.xlsx$")
key_file   <- find_file(file.path(site_dir, "2026"), "Copy.of.CSV.of.Soil.N.Sowing.*\\.xlsx$")

bulk_density <- 1.3
apply_60_90_to_60_100 <- TRUE   # APAL 60-90 layer used for both water layers 60-80 and 80-100

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: sampling date (from the Krieg paddock characterisation sheet) ----
char_raw <- read_excel(char_file, col_names = FALSE, col_types = "text", .name_repair = "minimal")
char_dates <- tibble(d = suppressWarnings(as.numeric(char_raw[[2]]))) %>%
  filter(!is.na(d)) %>% mutate(d = as.Date(d, origin = "1899-12-30"))
sample_date_baseline <- unique(char_dates$d)
stopifnot(length(sample_date_baseline) == 1)

# ---- Step 2b: plot key (row + bay -> plot, treatment) ----
key <- read_excel(key_file, sheet = "Sheet1", range = "P2:AS300", .name_repair = "unique_quiet") %>%
  filter(!is.na(ID)) %>%
  transmute(plot = paste0("C", `Plot no`), trial_row = as.integer(row), trial_col = as.integer(bay),
            treatment_label = paste(Ripping_main, Nutrition_sub, sep = "-"),
            treatment_desc  = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "))
stopifnot(!anyDuplicated(key[c("trial_row", "trial_col")]))

# ---- Step 3: water (wet / dry / tin weights, 6 layers) ----
water_layers <- tibble(letter = LETTERS[1:6],
                       depth_label = c("0-10", "10-20", "20-40", "40-60", "60-80", "80-100"))

water <- read_excel(water_file, sheet = "SSO2", col_types = "text", .name_repair = "unique_quiet") %>%
  transmute(sample = str_trim(Sample),
            tin  = suppressWarnings(as.numeric(`Tin Weight (g)`)),
            wet  = suppressWarnings(as.numeric(`Gross Wet Weight (g)`)),
            dry  = suppressWarnings(as.numeric(`Gross Dry Weight (g)`))) %>%
  filter(!is.na(sample)) %>%
  mutate(letter = str_extract(sample, "[A-Z]$"),
         point  = as.integer(str_extract(sample, "^\\d+"))) %>%
  group_by(sample) %>%
  mutate(dup_9C = sample == "9C" & row_number() == 2,
         point  = if_else(dup_9C, 10L, point)) %>%      # second "9C" is taken as 10C
  ungroup() %>%
  left_join(water_layers, by = "letter") %>%
  transmute(sample, point, depth_label, wet, dry, tin, dup_9C)

# ---- Step 4: APAL nitrate and ammonium ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName), !is.na(SampleDepth))
}

n_lab <- read_apal(n_file) %>%
  # the lab's SampleName is wrong for some plots (e.g. "10 F"); "Updated sample name" is clean
  filter(!str_detect(`Updated sample name`, "pH")) %>%  # pH1 / pH2 (0-5, 5-10) are pH only
  # the first two columns of the sheet are the trial row and column (bay) of each sample point
  mutate(trial_row = as.integer(.[[1]]), trial_col = as.integer(.[[2]])) %>%
  transmute(sample_name = `Updated sample name`,
            point = as.integer(str_match(`Updated sample name`, "^(\\d+)\\s")[, 2]),
            trial_row, trial_col,
            letter = str_match(`Updated sample name`, "\\s([A-Z])$")[, 2],
            n_depth = str_trim(SampleDepth),
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# sample point -> plot (via trial row and bay); each point must sit at one row + bay
point_map <- n_lab %>% distinct(point, trial_row, trial_col) %>%
  left_join(key, by = c("trial_row", "trial_col"))
stopifnot(!anyDuplicated(point_map$point))
n_lab <- n_lab %>% left_join(point_map %>% select(point, plot), by = "point")

# ---- Step 5: layers (water layer is the base; N matched by layer, 60-90 applied to 60-80 and 80-100) ----
layers <- water %>%
  left_join(point_map %>% select(point, plot, treatment_label, treatment_desc), by = "point") %>%
  mutate(sample_date = sample_date_baseline, sample_event = "baseline",
         dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         raw_moist = (wet - dry) / (dry - tin) * 100,
         moisture_pct = if_else(raw_moist > 40 | raw_moist < 0, NA_real_, raw_moist),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = case_when(top_cm >= 60 & apply_60_90_to_60_100 ~ "60-90",
                             top_cm >= 60 ~ "none",
                             TRUE ~ depth_label)) %>%
  select(-dm) %>%
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
cat("\n--- 1. Rows, plots, sampling date ---\n")
cat("Sampling date used:", format(sample_date_baseline), "| APAL received:",
    paste(unique(n_lab$apal_received), collapse = ", "), "| APAL SamplingDate:",
    paste(unique(n_lab$apal_sampled), collapse = ", "), "\n")
print(layers %>% summarise(n_plots = n_distinct(plot), n_rows = n()))
cat("Sample point -> trial row, bay, plot, treatment:\n")
print(point_map, n = Inf)
print(layers %>% count(depth_label, n_depth))

cat("\n--- 2. Water: missing weights, moisture above 40 or below 0 (set to NA) ---\n")
print(layers %>% filter(is.na(wet) | is.na(dry) | is.na(tin) | raw_moist > 40 | raw_moist < 0) %>%
        select(sample, plot, depth_label, wet, dry, tin, raw_moist))
cat("Duplicate sample label re-numbered (9C -> 10C):\n")
print(layers %>% filter(dup_9C) %>% select(sample, plot, depth_label, raw_moist))

cat("\n--- 3. Key checks (all should be 0 rows) ---\n")
print(layers %>% count(plot, depth_label) %>% filter(n > 1))
print(layers %>% filter(is.na(top_cm)) %>% distinct(depth_label))
print(n_lab %>% filter(is.na(letter) | is.na(plot)))
print(point_map %>% filter(is.na(plot) | is.na(treatment_label)))
print(n_lab %>% count(plot, n_depth) %>% filter(n > 1))

cat("\n--- 4. APAL vs water: sample points, missing N ---\n")
cat("APAL sample points not in water:", paste(setdiff(unique(n_lab$point), unique(water$point)), collapse = ", "), "\n")
cat("Water sample points not in APAL:", paste(setdiff(unique(water$point), unique(n_lab$point)), collapse = ", "), "\n")
cat("APAL layers:", paste(sort(unique(n_lab$n_depth)), collapse = ", "), "| N samples:", nrow(n_lab), "\n")
print(layers %>% filter(is.na(mineral_n_kgha)) %>% select(plot, depth_label, n_depth))

cat("\n--- 5. Non-missing group totals by depth group ---\n")
print(groups %>% group_by(depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"), n = Inf)

cat("\n--- 6. Profile (0-100) water and N: n plots / min / mean / max ---\n")
prof <- groups %>% group_by(plot) %>%
  summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_,
            profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop")
print(prof %>% summarise(n_water = sum(!is.na(profile_water)), water_min = min(profile_water, na.rm = TRUE),
                         water_mean = mean(profile_water, na.rm = TRUE), water_max = max(profile_water, na.rm = TRUE),
                         n_N = sum(!is.na(profile_N)), N_min = min(profile_N, na.rm = TRUE),
                         N_mean = mean(profile_N, na.rm = TRUE), N_max = max(profile_N, na.rm = TRUE)))

cat("\n--- 7. Mean by layer: moisture %, water mm, NO3, NH4, N kg/ha ---\n")
print(layers %>% group_by(depth_label) %>%
        summarise(moist = mean(moisture_pct, na.rm = TRUE), water_mm = mean(water_mm, na.rm = TRUE),
                  no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"), n = Inf)
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")

cat("\n--- 8. Our moisture % against the source sheet's own column ---\n")
src <- read_excel(water_file, sheet = "SSO2", .name_repair = "unique_quiet") %>%
  transmute(sample = str_trim(Sample), src_pct = suppressWarnings(as.numeric(`Soil Moisture (Gravametric %)`))) %>%
  group_by(sample) %>% mutate(occ = row_number()) %>% ungroup()
print(layers %>% group_by(sample) %>% mutate(occ = row_number()) %>% ungroup() %>%
        inner_join(src, by = c("sample", "occ")) %>%
        filter(!is.na(moisture_pct), !is.na(src_pct)) %>%
        summarise(n = n(), max_abs_diff = max(abs(moisture_pct - src_pct))))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Bute_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Bute_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
