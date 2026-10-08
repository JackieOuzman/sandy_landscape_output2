# ==============================================================================
# Script:   Copeville_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Copeville 2025
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2025)
#   Copeville 2025 Water Data Base .xlsx  (note the space before .xlsx)
#       sheets: Pre Sow, GS31, Flowering, Harvest Moisture, Yield Final and Plan (plot key)
#   O2_Copeville_Yr2_APAL_presow.xlsx - nitrate and ammonium (pre-sow only)
# OUTPUTS (same folder)
#   Copeville_2025_depth_layers_N_and_H2O.csv
#   Copeville_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup and Copeville 2024)
#   moisture % = (wet - dry) / (dry - chip) x 100; above 40% or below 0 set to NA
#   water mm   = moisture/100 x bulk density x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   sample_event: baseline (pre-sow, 23 Apr, has N), GS31 (12 Aug), flowering (1 Oct),
#   harvest (9 Dec). Only the baseline has N.
#   Pre Sow sheet date is the text "Apr 25" (the readme turned this into 25 Apr).
#   APAL sample names (D23_M04) and the Field Diary give 23 Apr 2025, which is used.
#   APAL 60-100 cm N is used for both water layers 60-80 and 80-100 (own thickness).
#   GS31: 22 plots have no 80-100 cm weights, so their 60-100 group is NA.
#   The four events use different sets of 56 plots (see check 1).
#   The GS21 soil moisture (15 Jul, 0-40 cm, on/off row) in "SS02 Copeville 2025 soil
#   mois.xlsx" is not included here.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley"
data_dir   <- file.path(site_dir, "2. Soil Data and Nutrition/2025")
out_dir    <- data_dir
water_file <- file.path(data_dir, "Copeville 2025 Water Data Base .xlsx")
n_file     <- file.path(data_dir, "O2_Copeville_Yr2_APAL_presow.xlsx")

bulk_density <- 1.3
presow_date  <- as.Date("2025-04-23")   # from APAL sample names and the Field Diary

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (plot -> treatment) ----
key <- read_excel(water_file, sheet = "Yield Final and Plan") %>%
  transmute(plot = str_trim(PlotID), treatment_label = `Treatment Combo`,
            treatment_desc = `Treatment Description`)

# ---- Step 3: read the four water events into one common format ----
pre_raw <- read_excel(water_file, sheet = "Pre Sow", .name_repair = "unique_quiet")
pre <- pre_raw %>%
  transmute(sample_event = "baseline", sample_date = presow_date,
            plot = str_trim(as.character(pre_raw[[grep("^PlotID", names(pre_raw))[1]]])),
            depth_label = as.character(Depth), wet = as.numeric(`Wet Wt`),
            dry = as.numeric(`Dry Wt`), tare = as.numeric(`Chip Wt`))

gs31 <- read_excel(water_file, sheet = "GS31") %>%
  transmute(sample_event = "GS31", sample_date = as.Date(Date), plot = str_trim(`Plot no.`),
            depth_label = as.character(`Depth (cm)`), wet = as.numeric(`Wet Wt`),
            dry = as.numeric(`Dry Wt`), tare = as.numeric(`Chip Wt`))

# Flowering and harvest sheets keep the date as Day / Month / Year text (D01, M10, YR25)
read_dmy_sheet <- function(sheet, event) {
  raw  <- read_excel(water_file, sheet = sheet, .name_repair = "unique_quiet")
  dcol <- grep("^Day", names(raw))[1]
  pcol <- grep("^Plot no", names(raw))[1]
  raw %>% transmute(
    sample_event = event,
    sample_date  = as.Date(sprintf("20%02d-%02d-%02d",
                                   as.integer(str_extract(Year, "\\d+")),
                                   as.integer(str_extract(Month, "\\d+")),
                                   as.integer(str_extract(raw[[dcol]], "\\d+")))),
    plot = str_trim(as.character(raw[[pcol]])), depth_label = as.character(`Depth (cm)`),
    wet = as.numeric(`Wet Wt`), dry = as.numeric(`Dry Wt`), tare = as.numeric(`Chip Wt`))
}
flow <- read_dmy_sheet("Flowering", "flowering")
harv <- read_dmy_sheet("Harvest Moisture", "harvest")

water_raw <- bind_rows(pre, gs31, flow, harv) %>% filter(!is.na(plot))

# ---- Step 4: layers (moisture, water mm, N layer label) ----
layers <- water_raw %>%
  mutate(dm = str_match(str_remove_all(depth_label, "[cm\\s]"), "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare) * 100,
         moisture_pct = if_else(moisture_pct > 40 | moisture_pct < 0, NA_real_, moisture_pct),
         # Manual override: C93 harvest 20-40 cm reads 37.8% (neighbouring layers 4-5%), almost certainly a weighing or labelling error
         moisture_pct = if_else(sample_event == "harvest" & plot == "C93" & top_cm == 20, NA_real_, moisture_pct),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = case_when(top_cm == 0  & bottom_cm == 10 ~ "0-10",
                             top_cm == 10 & bottom_cm == 20 ~ "10-20",
                             top_cm == 20 & bottom_cm == 40 ~ "20-40",
                             top_cm == 40 & bottom_cm == 60 ~ "40-60",
                             top_cm >= 60 & bottom_cm <= 100 ~ "60-100",
                             TRUE ~ NA_character_)) %>%
  select(-dm) %>%
  left_join(key, by = "plot")

# ---- Step 4b: APAL nitrate and ammonium (pre-sow) ----
read_apal <- function(file, sheet) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))
}

n_lab <- read_apal(n_file, "Data") %>%
  transmute(sample_name = SampleName,
            plot = str_match(SampleName, "_(C\\d+)_D\\d+_M\\d+_YR\\d+_")[, 2],
            apal_date = as.Date(sprintf("20%02d-%02d-%02d",
                                        as.integer(str_match(SampleName, "_YR(\\d+)_")[, 2]),
                                        as.integer(str_match(SampleName, "_M(\\d+)_")[, 2]),
                                        as.integer(str_match(SampleName, "_D(\\d+)_")[, 2]))),
            n_depth = str_trim(str_match(SampleName, "_YR\\d+_(.+)$")[, 2]),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# N only applies to the baseline event
layers <- layers %>%
  left_join(n_lab %>% select(plot, n_depth, no3_mgkg, nh4_mgkg), by = c("plot", "n_depth")) %>%
  mutate(no3_mgkg = if_else(sample_event == "baseline", no3_mgkg, NA_real_),
         nh4_mgkg = if_else(sample_event == "baseline", nh4_mgkg, NA_real_),
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10)

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

# ---- Step 6: water checks (paste output back) ----
cat("\n--- 1. Rows and plots per sampling event (layers) ---\n")
print(layers %>% group_by(sample_event, sample_date) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(), .groups = "drop"))

cat("\n--- 2. Missing weights, and moisture above 40 or below 0 (set to NA) ---\n")
print(layers %>% mutate(raw_moist = (wet - dry) / (dry - tare) * 100) %>%
        filter(is.na(wet) | is.na(dry) | is.na(tare) | raw_moist > 40 | raw_moist < 0) %>%
        count(sample_event, depth_label, name = "n_rows"))
print(layers %>% mutate(raw_moist = (wet - dry) / (dry - tare) * 100) %>%
        filter(!is.na(raw_moist), raw_moist > 40 | raw_moist < 0) %>%
        select(sample_event, plot, depth_label, wet, dry, tare, raw_moist))

cat("\n--- 3. Plots with no treatment match in the key (should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(sample_event, plot))

cat("\n--- 4. N plots vs baseline water plots, and APAL date vs sample date ---\n")
cat("In APAL but not in baseline water: ",
    paste(setdiff(unique(n_lab$plot), unique(pre$plot)), collapse = ", "), "\n")
cat("In baseline water but not in APAL: ",
    paste(setdiff(unique(pre$plot), unique(n_lab$plot)), collapse = ", "), "\n")
cat("APAL sample date(s):", format(unique(n_lab$apal_date)), " | water baseline date:", format(presow_date), "\n")

cat("\n--- 5. Non-missing group totals by event and depth group ---\n")
print(groups %>% group_by(sample_event, depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"))

cat("\n--- 6. Profile (0-100) water by event: min / mean / max ---\n")
print(groups %>% group_by(sample_event, sample_date, plot) %>%
        summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_, .groups = "drop") %>%
        group_by(sample_event, sample_date) %>%
        summarise(n = sum(!is.na(profile_water)), min = min(profile_water, na.rm = TRUE),
                  mean = mean(profile_water, na.rm = TRUE), max = max(profile_water, na.rm = TRUE),
                  .groups = "drop"))

# ---- Step 7: N checks ----
cat("\n--- 7. N: values set to 0 (below detection) and baseline profile N ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "\n")
print(groups %>% filter(sample_event == "baseline") %>% group_by(plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Copeville_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Copeville_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
