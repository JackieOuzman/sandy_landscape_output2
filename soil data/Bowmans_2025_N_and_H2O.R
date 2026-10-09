# ==============================================================================
# Script:   Bowmans_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Bowmans-Roberts 2025
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/2. SSO2_Soildata and nutrition/2025)
#   Bowmans Water Data Raw 2026.xlsx - (the file name says 2026, the data are all 2025)
#       sheets "Baseline WAter " (25 Mar), "GS31Water" (14 Aug), "GS69 Waters " (24 Sep),
#       "Harvest Water" (2 Dec): chip, wet and dry weights, 6 layers (0-10 ... 80-100)
#   O2_Bowmans_25_Baseline_APAL.xlsx - sheet "Data": APAL nitrate and ammonium, baseline only
#   SS02_Bowmans_soil_mois.xlsx      - sheet "des_SS02-Bowmans_split-Rip-Nut-", columns P:AA:
#       plot key (plot, bay, row, ripping and nutrition treatments)
# OUTPUTS (same folder)
#   Bowmans_2025_depth_layers_N_and_H2O.csv
#   Bowmans_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   moisture % = (wet - dry) / (dry - chip) x 100;  water mm = moisture/100 x BD x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x BD x layer cm / 10;  BD assumed 1.3
#   "<" results set to 0; moisture above 40 or below 0 set to NA
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up to the
#   full group thickness and none are NA
# NOTES
#   * Four events: baseline (12 plots, water + N), GS31 (48 plots), GS69 (32 plots),
#     harvest (32 plots); the last three are water only (no in-season N).
#   * APAL baseline has ONE 60-100 N layer; it is applied to both water layers 60-80 and 80-100.
#   * APAL SamplingDate is blank (received 7 Apr 2025); the sampling date is the 25 Mar 2025
#     date on the baseline water sheet.
#   * Not included: the GS20 (15 Jul, 0-40 cm) and destructive-trial moisture tabs in
#     SS02_Bowmans_soil_mois.xlsx.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/6. SS02_Bowmans-Roberts/2. SSO2_Soildata and nutrition"
data_dir <- file.path(site_dir, "2025")
out_dir  <- data_dir

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
water_file <- find_file(data_dir, "Bowmans.Water.Data.Raw.2026\\.xlsx$")
n_file     <- find_file(data_dir, "O2_Bowmans_25_Baseline_APAL\\.xlsx$")
key_file   <- find_file(data_dir, "SS02_Bowmans_soil_mois\\.xlsx$")

bulk_density <- 1.3

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key ----
key <- read_excel(key_file, sheet = "des_SS02-Bowmans_split-Rip-Nut-", range = "P1:AA81",
                  .name_repair = "unique_quiet") %>%
  filter(!is.na(PLOT)) %>%
  transmute(plot = str_trim(PLOT), treatment_label = Treatments_mainsub,
            treatment_desc = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "))

# ---- Step 3: water (chip / wet / dry weights, 6 layers, four events) ----
# Columns are taken by position after checking the header names.
read_water <- function(sheet, event, plot_i, depth_i, chip_i, wet_i, dry_i, date_fun) {
  raw <- read_excel(water_file, sheet = sheet, col_types = "text", .name_repair = "minimal")
  stopifnot(str_detect(names(raw)[chip_i], "^Chip Wt"), str_detect(names(raw)[wet_i], "^Wet Wt"),
            str_detect(names(raw)[dry_i], "^Dry Wt"))
  tibble(sample_event = event,
         plot = str_trim(raw[[plot_i]]), depth_label = str_trim(raw[[depth_i]]),
         date_txt = date_fun(raw),
         chip = suppressWarnings(as.numeric(raw[[chip_i]])),
         wet  = suppressWarnings(as.numeric(raw[[wet_i]])),
         dry  = suppressWarnings(as.numeric(raw[[dry_i]]))) %>%
    filter(!is.na(plot), !is.na(depth_label))
}
# in-season sheets carry the date as Day "D14", Month "M08", Year "YR25"
date_from_dmy <- function(raw) {
  as.character(as.Date(paste0(2000 + as.integer(str_extract(raw[["Year"]], "\\d+")), "-",
                              str_extract(raw[["Month"]], "\\d+"), "-",
                              str_extract(raw[["Day"]], "\\d+"))))
}
date_from_serial <- function(raw) {
  as.character(as.Date(suppressWarnings(as.numeric(raw[["Date"]])), origin = "1899-12-30"))
}

water <- bind_rows(
  read_water("Baseline WAter ", "baseline", 2, 9, 12, 13, 14, date_from_serial),
  read_water("GS31Water",       "GS31",     7, 20, 23, 24, 25, date_from_dmy),
  read_water("GS69 Waters ",    "GS69",     7, 20, 23, 24, 25, date_from_dmy),
  read_water("Harvest Water",   "harvest",  7, 20, 23, 24, 25, date_from_dmy)) %>%
  mutate(sample_date = as.Date(date_txt))

# ---- Step 4: APAL nitrate and ammonium (baseline) ----
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
  transmute(sample_name = SampleName,
            plot = str_match(SampleName, "^(C\\d+)_")[, 2],
            n_depth = str_trim(SampleDepth),
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`),
            sample_event = "baseline")

# ---- Step 5: layers ----
layers <- water %>%
  mutate(dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         raw_moist = (wet - dry) / (dry - chip) * 100,
         moisture_pct = if_else(raw_moist > 40 | raw_moist < 0, NA_real_, raw_moist),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = if_else(top_cm >= 60, "60-100", depth_label)) %>%
  select(-dm) %>%
  left_join(key, by = "plot") %>%
  left_join(n_lab %>% select(plot, sample_event, n_depth, no3_mgkg, nh4_mgkg),
            by = c("plot", "sample_event", "n_depth")) %>%
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
cat("\n--- 1. Rows, plots and sampling date by event ---\n")
print(layers %>% group_by(sample_event, sample_date) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(), .groups = "drop"))
print(layers %>% count(sample_event, depth_label) %>% arrange(sample_event, depth_label), n = Inf)

cat("\n--- 2. Missing weights, and moisture above 40 or below 0 (set to NA) ---\n")
print(layers %>% filter(is.na(wet) | is.na(dry) | is.na(chip) | raw_moist > 40 | raw_moist < 0) %>%
        count(sample_event, depth_label, name = "n_rows") %>% arrange(sample_event, depth_label), n = Inf)
print(layers %>% filter(raw_moist > 40 | raw_moist < 0) %>%
        select(sample_event, plot, depth_label, wet, dry, chip, raw_moist))
cat("Plots with all six layers missing, by event:\n")
print(layers %>% group_by(sample_event, plot) %>% filter(all(is.na(moisture_pct))) %>%
        distinct(sample_event, plot))

cat("\n--- 3. Key checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(plot))
print(layers %>% filter(is.na(top_cm)) %>% distinct(depth_label))
print(layers %>% count(sample_event, plot, depth_label) %>% filter(n > 1))
print(n_lab %>% filter(is.na(plot)))

cat("\n--- 4. APAL vs baseline water: plots, dates, missing N ---\n")
base_plots <- unique(water$plot[water$sample_event == "baseline"])
cat("APAL plots not in baseline water:", paste(setdiff(unique(n_lab$plot), base_plots), collapse = ", "), "\n")
cat("Baseline water plots not in APAL:", paste(setdiff(base_plots, unique(n_lab$plot)), collapse = ", "), "\n")
cat("APAL SamplingDate:", paste(unique(n_lab$apal_sampled), collapse = ", "),
    "| APAL received:", paste(unique(n_lab$apal_received), collapse = ", "),
    "| baseline water date:", paste(unique(water$sample_date[water$sample_event == "baseline"]), collapse = ", "), "\n")
print(layers %>% filter(sample_event == "baseline", is.na(mineral_n_kgha)) %>% select(plot, depth_label, n_depth))

cat("\n--- 5. Non-missing group totals by event and depth group ---\n")
print(groups %>% group_by(sample_event, depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"), n = Inf)

cat("\n--- 6. Profile (0-100) water by event: n plots / min / mean / max ---\n")
print(groups %>% group_by(sample_event, plot) %>%
        summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_, .groups = "drop") %>%
        group_by(sample_event) %>%
        summarise(n = sum(!is.na(profile_water)),
                  min = if (n > 0) min(profile_water, na.rm = TRUE) else NA_real_,
                  mean = if (n > 0) mean(profile_water, na.rm = TRUE) else NA_real_,
                  max = if (n > 0) max(profile_water, na.rm = TRUE) else NA_real_, .groups = "drop"))

cat("\n--- 7. Mean moisture % and water mm by layer and event ---\n")
print(layers %>% group_by(sample_event, depth_label) %>%
        summarise(moist = mean(moisture_pct, na.rm = TRUE), water_mm = mean(water_mm, na.rm = TRUE),
                  .groups = "drop"), n = Inf)

cat("\n--- 8. Baseline N: values set to 0, profile N, mean by layer ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")
print(groups %>% filter(sample_event == "baseline") %>% group_by(plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))
print(layers %>% filter(sample_event == "baseline") %>% group_by(depth_label) %>%
        summarise(no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"))

cat("\n--- 9. Our moisture % against the source sheets' own moisture column ---\n")
src <- map_dfr(c("Baseline WAter ", "GS31Water", "GS69 Waters ", "Harvest Water"), function(sh) {
  raw <- read_excel(water_file, sheet = sh, col_types = "text", .name_repair = "minimal")
  pc <- if (sh == "Baseline WAter ") 2 else 7
  dc <- if (sh == "Baseline WAter ") 9 else 20
  mc <- if (sh == "Baseline WAter ") 15 else 26
  tibble(sample_event = c("Baseline WAter " = "baseline", "GS31Water" = "GS31",
                          "GS69 Waters " = "GS69", "Harvest Water" = "harvest")[[sh]],
         plot = str_trim(raw[[pc]]), depth_label = str_trim(raw[[dc]]),
         src_pct = suppressWarnings(as.numeric(raw[[mc]])))
})
print(layers %>% inner_join(src, by = c("sample_event", "plot", "depth_label")) %>%
        filter(!is.na(moisture_pct), !is.na(src_pct)) %>%
        summarise(n = n(), max_abs_diff = max(abs(moisture_pct - src_pct))))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Bowmans_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Bowmans_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")

