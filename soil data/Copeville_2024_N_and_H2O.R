# ==============================================================================
# Script:   Copeville_2024_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Copeville 2024
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2024)
#   Copeville Soil water.xlsx  - raw wet/dry/chip weights (sheets for 12 Mar + 5 Apr
#                                baseline, 29 May pre-sow, 8 Aug GS31, 28 Nov harvest)
#                              - plot key sheet "serenity used for lookup"
#   APAL_baseline soil data_copeville.xlsx - nitrate and ammonium (baseline only)
# OUTPUTS (same folder)
#   Copeville_2024_depth_layers_N_and_H2O.csv
#   Copeville_2024_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup)
#   moisture % = (wet - dry) / (dry - chip) x 100; above 40% set to NA
#   water mm   = moisture/100 x bulk density x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100 (cut on top of layer); a group total is NA
#   unless the layers present add up to the full group thickness and none are NA
# NOTES
#   N was only measured at the baseline (12 Mar / 5 Apr). Other events have N = NA.
#   APAL 60-100 cm N is used for both water layers 60-80 and 80-100 (own thickness).
#   29 May pre-sow cores only reach 30 cm, so only the 0-20 group is complete.
#   GS69 sheet: no plots sampled (corer broke).
#   #   Water sheet plot "C73" (baseline, 5 Apr) relabelled C72 (matches APAL; RT5_T6).
#   Negative moisture (wet < dry) set to NA, as for moisture above 40%: C69 40-60 cm, 28 Nov.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley"
data_dir   <- file.path(site_dir, "2. Soil Data and Nutrition/2024")
out_dir    <- data_dir
water_file <- file.path(data_dir, "Copeville Soil water.xlsx")
n_file     <- file.path(data_dir, "APAL_baseline soil data_copeville.xlsx")

bulk_density <- 1.3
sample_year  <- 2024

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (plot -> treatment) from the water workbook ----
key <- read_excel(water_file, sheet = "serenity used for lookup") %>%
  transmute(plot = Plot, treatment_label = label, treatment_desc = TreatmentDescription)

# ---- Step 3: read the five water events into one common format ----
sheets <- excel_sheets(water_file)

base <- read_excel(water_file, sheet = "Core Baseline soil", skip = 1) %>%
  filter(!is.na(Plot)) %>%
  transmute(sample_event = "baseline", sample_date = as.Date(Date),
            plot = if_else(Plot == "C73", "C72", Plot),   # water sheet typo: APAL, summary and the RT5_T6 baseline set all say C72
            depth_label = Depth, wet = as.numeric(`Wet Wt`),
            dry = as.numeric(`Dry Wt`), tare = as.numeric(`Chip Wt`))

presow <- read_excel(water_file, sheet = "Core Pre-sow soils") %>%
  filter(!is.na(Plot)) %>%
  transmute(sample_event = "pre-sow", sample_date = as.Date(Date), plot = Plot,
            depth_label = Depth, wet = as.numeric(`wet wt`),
            dry = as.numeric(`dry wt`), tare = as.numeric(`chip wt`))

gs31_sheet <- str_subset(sheets, "^Core GS31-Soil water\\s*$")
stopifnot(length(gs31_sheet) == 1)
gs31 <- read_excel(water_file, sheet = gs31_sheet, skip = 2) %>%
  filter(str_detect(`Plot No.`, "^C\\d+$")) %>%
  transmute(sample_event = "GS31", sample_date = as.Date(Date), plot = `Plot No.`,
            depth_label = `Depth (cm)`, wet = as.numeric(`Wet wt`),
            dry = as.numeric(`Dry Wt`), tare = as.numeric(`Chip wt`))

har_raw <- read_excel(water_file, sheet = "Core Harvest - Soil water", .name_repair = "unique_quiet")
plot_col <- grep("^PlotID", names(har_raw))[1]
har <- har_raw %>%
  transmute(sample_event = "harvest", sample_date = as.Date(Date),
            plot = as.character(har_raw[[plot_col]]), depth_label = as.character(Depth),
            wet = as.numeric(`wet wt (g)`), dry = as.numeric(`Dry wt (g)`),
            tare = as.numeric(`chip wt (g)`)) %>%
  filter(!is.na(plot))

water_raw <- bind_rows(base, presow, gs31, har) %>% mutate(plot = str_trim(plot))

# ---- Step 4: layers (moisture, water mm, N layer label) ----
layers <- water_raw %>%
  mutate(dm = str_match(str_remove_all(depth_label, "[cm\\s]"), "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare) * 100,
         moisture_pct = if_else(moisture_pct > 40 | moisture_pct < 0, NA_real_, moisture_pct),
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

# ---- Step 4b: APAL nitrate and ammonium (baseline) ----
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
            plot = str_match(SampleName, "^(C\\d+)_")[, 2],
            n_depth = str_trim(str_match(SampleName, "^C\\d+_\\d+_(.+)$")[, 2]),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# C72 in APAL is correct; the water sheet's C73 is relabelled to C72 in Step 3

# N only applies to the baseline event (plots are re-sampled later for water only)
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

cat("\n--- 2. Bad weights or moisture (missing weights, negative, or above 40% set to NA) ---\n")
print(water_raw %>% filter(is.na(wet) | is.na(dry) | is.na(tare)))
print(layers %>% mutate(raw_moist = (wet - dry) / (dry - tare) * 100) %>%
        filter(raw_moist > 40 | raw_moist < 0) %>%
        select(sample_event, sample_date, plot, depth_label, wet, dry, tare, raw_moist))

cat("\n--- 3. Plots with no treatment match in the key (should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(sample_event, plot))

cat("\n--- 4. N plots vs baseline water plots (C72 / C73) ---\n")
cat("In APAL but not in baseline water: ",
    paste(setdiff(unique(n_lab$plot), unique(base$plot)), collapse = ", "), "\n")
cat("In baseline water but not in APAL: ",
    paste(setdiff(unique(base$plot), unique(n_lab$plot)), collapse = ", "), "\n")

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
write_csv(layers_out, file.path(out_dir, "Copeville_2024_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Copeville_2024_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
