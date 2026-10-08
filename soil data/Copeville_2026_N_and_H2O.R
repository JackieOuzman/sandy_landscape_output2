# ==============================================================================
# Script:   Copeville_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Copeville 2026
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# INPUTS  (in <site>/2. Soil Data and Nutrition/2026)
#   SSO2_2026_Copeville_Soilmoistures.xlsx
#       sheets: Sheet1 (plot key), Presow, GS37, GS65  (Sheet2 = labels only, not used)
#   APAL batch files, found by batch number:
#       Batch-51093 (pre-sow, 7 May), Batch-52529 (GS37, 5 Aug), Batch-53306 (GS65, 15 Sep)
# OUTPUTS (same folder)
#   Copeville_2026_depth_layers_N_and_H2O.csv
#   Copeville_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup and Copeville 2024, 2025)
#   moisture % = (wet - dry) / (dry - chip) x 100; above 40% or below 0 set to NA
#   water mm   = moisture/100 x bulk density x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0 (the 2026 APAL files already report plain zeros); bulk density 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   sample_event: baseline (pre-sow, 7 May), GS37 (5 Aug), GS65 (15 Sep).
#   Unlike 2024/2025, N is available at all three events (matched on date, plot, layer).
#   Pre-sow APAL has one 60-100 cm layer; it is used for both water layers 60-80 and
#   80-100 (each with its own thickness). GS37 and GS65 APAL have 60-80 and 80-100 separately.
#   The water Presow sheet has a "60-100" label row with no weights; it is dropped.
#   GS37: 80-100 cm weights exist for only 9 plots (60-80 missing for C23, C99), so most
#   plots have NA for the 60-100 group (water and N).
#   Sheet2 (8 GS65 plots, labels only, no weights) is not used.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley"
data_dir   <- file.path(site_dir, "2. Soil Data and Nutrition/2026")
out_dir    <- data_dir
water_file <- file.path(data_dir, "SSO2_2026_Copeville_Soilmoistures.xlsx")

find_batch <- function(batch) {
  f <- list.files(data_dir, pattern = paste0("Batch-", batch, ".*\\.xlsx$"), full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
n_files <- c(find_batch("51093"), find_batch("52529"), find_batch("53306"))

bulk_density <- 1.3

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (plot -> treatment) ----
key <- read_excel(water_file, sheet = "Sheet1", .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(PlotID), treatment_label = `Treatment Combo`,
            treatment_desc = `Treatment Description`)

# ---- Step 3: read the three water events ----
# Date is kept as Day / Month / Year text (D07, M05, YR26)
read_water_sheet <- function(sheet, event) {
  raw <- read_excel(water_file, sheet = sheet, .name_repair = "unique_quiet")
  raw %>%
    filter(!is.na(`Sample ID`), !is.na(PlotID)) %>%
    transmute(
      sample_event = event,
      sample_date  = as.Date(sprintf("20%02d-%02d-%02d",
                                     as.integer(str_extract(Year, "\\d+")),
                                     as.integer(str_extract(Month, "\\d+")),
                                     as.integer(str_extract(Day, "\\d+")))),
      plot = str_trim(as.character(PlotID)), depth_label = as.character(Depth),
      wet = as.numeric(`Wet Wt (g)`), dry = as.numeric(`Dry Wt (g)`),
      tare = as.numeric(`Chip Wt (g)`))
}

water_raw <- bind_rows(read_water_sheet("Presow", "baseline"),
                       read_water_sheet("GS37",   "GS37"),
                       read_water_sheet("GS65",   "GS65")) %>%
  filter(depth_label != "60-100")      # label-only row in Presow (no weights)

# ---- Step 4: layers (moisture, water mm, N layer label) ----
layers <- water_raw %>%
  mutate(dm = str_match(str_remove_all(depth_label, "[cm\\s]"), "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare) * 100,
         moisture_pct = if_else(moisture_pct > 40 | moisture_pct < 0, NA_real_, moisture_pct),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         # pre-sow APAL has a single 60-100 layer; GS37 and GS65 have 60-80 and 80-100
         n_depth = if_else(sample_event == "baseline" & top_cm >= 60, "60-100",
                           paste0(top_cm, "-", bottom_cm))) %>%
  select(-dm) %>%
  left_join(key, by = "plot")

# ---- Step 4b: APAL nitrate and ammonium (all three events) ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName), str_starts(SampleName, "SSO2"))
}

# Sample names look like SSO2_COP_FAR_D15_M09_YR26_C7_0-10
n_lab <- bind_rows(lapply(n_files, read_apal)) %>%
  transmute(sample_name = SampleName,
            sample_date = as.Date(sprintf("20%02d-%02d-%02d",
                                          as.integer(str_match(SampleName, "_YR(\\d+)_")[, 2]),
                                          as.integer(str_match(SampleName, "_M(\\d+)_")[, 2]),
                                          as.integer(str_match(SampleName, "_D(\\d+)_")[, 2]))),
            plot = str_match(SampleName, "_YR\\d+_(C\\d+)_")[, 2],
            n_depth = str_trim(str_match(SampleName, "_YR\\d+_C\\d+_(.+)$")[, 2]),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

layers <- layers %>%
  left_join(n_lab %>% select(sample_date, plot, n_depth, no3_mgkg, nh4_mgkg),
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

cat("\n--- 4. N plots vs water plots (by date), duplicate N rows, N rows per date ---\n")
water_pd <- layers %>% distinct(sample_date, plot)
n_pd     <- n_lab  %>% distinct(sample_date, plot)
cat("In APAL but not in water:\n");  print(anti_join(n_pd, water_pd, by = c("sample_date", "plot")))
cat("In water but not in APAL:\n");  print(anti_join(water_pd, n_pd, by = c("sample_date", "plot")))
cat("Duplicate N rows (should be 0):\n")
print(n_lab %>% count(sample_date, plot, n_depth) %>% filter(n > 1))
print(n_lab %>% count(sample_date, name = "n_apal_rows"))

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
cat("\n--- 7. N: zeros reported and profile N by event ---\n")
cat("Nitrate zero / '<':",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "\n")
cat("Ammonium zero / '<':", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "\n")
print(groups %>% group_by(sample_event, sample_date, plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        group_by(sample_event, sample_date) %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE),
                  .groups = "drop"))
cat("Highest ammonium layers (look for outliers):\n")
print(layers %>% arrange(desc(nh4_mgkg)) %>% slice_head(n = 6) %>%
        select(sample_event, plot, depth_label, no3_mgkg, nh4_mgkg, mineral_n_kgha))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Copeville_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Copeville_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")

