# ==============================================================================
# Script:   Annuello_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Annuello-Aikman 2026
# Purpose:  Soil mineral N (kg/ha) and soil water (mm) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS
#   <site>/2.SSO2_Soil Data and Nutrition/2026/Annuello Output 2 2026_pre-sowing soil sampling.xlsx
#        sheet "Pre-sowing Soil Results" (the site's own compiled table, columns A:T):
#        48 plots x 6 layers (0-10, 10-20, 20-40, 40-60, 60-80, 80-100); wet and dry weights (g),
#        nitrate and ammonium (mg/kg). The lab "<" results were typed in as 0.9, so the raw "<"
#        is not in this file (see below). Plot Code starts with the sampling date (yymmdd).
#   <site>/1. SSO2_Trial design_MetaData/2026/Annuello_Output2_2026_Design.xlsx - sheet "Data"
#        plot key: bay, row, ripping (main) and nutrition (sub) treatments
# OUTPUTS (soil folder, 2026)
#   Annuello_2026_depth_layers_N_and_H2O.csv
#   Annuello_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   water mm = moisture % / 100 x bulk density x layer cm x 10; bulk density assumed 1.3
#   moisture = (wet - dry) / dry x 100; above 40 or below 0 set to NA
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   * One event (pre-sow baseline), 48 plots. Plot = "R<row>B<bay>" from the file's Row! and Bay!
#     (Row! is the trial row, as used in the design file; "BCG Row" is that row + 1).
#   * Sample date = 4 May 2026, read from the Plot Code (2605040102 = 26-05-04, bay 01, BCG row 02).
#     Sowing was 14 May 2026.
#   * "<" results: the site typed 0.9 for results below the 1.0 detection limit. We cannot see the
#     raw "<", so values equal to lt_value_in_file (0.9) are treated as "<" and set to lt_value_used.
#     To use 0.9 instead of 0 (the open 0 versus 0.9 question), set lt_value_used <- 0.9; then the
#     N matches the site's own Layer N column exactly (check 8).
#   * The weights are assumed to be soil only (no tin or chip weight in this file).
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/7. SS02_Annuello-Aikman"
data_dir <- file.path(site_dir, "2.SSO2_Soil Data and Nutrition", "2026")
key_dir  <- file.path(site_dir, "1. SSO2_Trial design_MetaData", "2026")
out_dir  <- data_dir

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
pre_file <- find_file(data_dir, "^Annuello.Output.2.2026.pre-sowing.soil.sampling.*\\.xlsx$")
key_file <- find_file(key_dir,  "^Annuello_Output2_2026_Design\\.xlsx$")

bulk_density     <- 1.3
lt_value_in_file <- 0.9    # what the site typed in place of "<1.0"
lt_value_used    <- 0      # 0 = our convention; set to 0.9 to match the site's own N

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)

# ---- Step 2: plot key (row + bay -> plot, treatment) ----
key <- read_excel(key_file, sheet = "Data", .name_repair = "unique_quiet") %>%
  filter(!is.na(bay), !is.na(row)) %>%
  transmute(trial_row = as.integer(row), trial_col = as.integer(bay),
            plot = paste0("R", row, "B", bay),
            treatment_label = Treatments_mainsub,
            treatment_desc  = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "))
stopifnot(!anyDuplicated(key[c("trial_row", "trial_col")]))

# ---- Step 3: the site's pre-sowing results (water and N together) ----
pre <- read_excel(pre_file, sheet = "Pre-sowing Soil Results", range = "A2:T400",
                  .name_repair = "unique_quiet") %>%
  filter(!is.na(`Plot Code`)) %>%
  transmute(trial_row = as.integer(`Row!`), trial_col = as.integer(`Bay!`),
            plot_code = as.character(`Plot Code`),
            file_treatment = str_replace(`Full Ripping/Nutrition!`, " ", "_"),
            apal_depth = str_trim(`Depth!`),
            wet_g = as.numeric(`Wet Weight (g)`), dry_g = as.numeric(`Dry Weight (g)`),
            no3_file = as.numeric(`Nitrate - N (2M KCl)(mg/kg)`),
            nh4_file = as.numeric(`Ammonium - N (2M KCl) (mg/kg)`),
            site_layer_n = as.numeric(`Layer N (kg/ha)`),
            site_water_mm = as.numeric(`Layer Volmetric water (mm)`)) %>%
  left_join(key, by = c("trial_row", "trial_col"))

sample_date_baseline <- unique(as.Date(str_sub(pre$plot_code, 1, 6), format = "%y%m%d"))
stopifnot(length(sample_date_baseline) == 1)

# ---- Step 4: layers ----
layers <- pre %>%
  mutate(sample_event = "baseline", sample_date = sample_date_baseline,
         dm = str_match(apal_depth, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         bulk_density = bulk_density,
         moisture_raw = (wet_g - dry_g) / dry_g * 100,
         moisture_pct = if_else(moisture_raw > 40 | moisture_raw < 0, NA_real_, moisture_raw),
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = apal_depth,
         no3_mgkg = if_else(no3_file == lt_value_in_file, lt_value_used, no3_file),
         nh4_mgkg = if_else(nh4_file == lt_value_in_file, lt_value_used, nh4_file),
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10) %>%
  select(-dm)

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
cat("\n--- 1. Rows, plots, layers, date ---\n")
cat("Sample date (from Plot Code):", format(sample_date_baseline), "\n")
print(layers %>% summarise(n_plots = n_distinct(plot), n_rows = n()))
print(layers %>% count(apal_depth, n_depth, top_cm, bottom_cm))

cat("\n--- 2. Key and duplicate checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(plot) | is.na(treatment_label)) %>% distinct(plot_code))
print(layers %>% filter(is.na(top_cm)) %>% distinct(apal_depth))
print(layers %>% count(plot, n_depth) %>% filter(n > 1))
print(layers %>% filter(file_treatment != treatment_label) %>% distinct(plot, file_treatment, treatment_label))

cat("\n--- 3. Missing weights, and moisture set to NA (should be 0) ---\n")
cat("Missing wet or dry weight:", sum(is.na(layers$wet_g) | is.na(layers$dry_g)),
    "| moisture outside 0-40 set to NA:", sum(is.na(layers$moisture_pct) & !is.na(layers$moisture_raw)), "\n")
print(crossing(plot = unique(layers$plot), n_depth = unique(layers$n_depth)) %>%
        anti_join(layers, by = c("plot", "n_depth")))

cat("\n--- 4. Treatments sampled (plots per treatment) ---\n")
print(layers %>% distinct(plot, treatment_label, treatment_desc) %>%
        count(treatment_label, treatment_desc), n = Inf)

cat("\n--- 5. Values equal to", lt_value_in_file, "(taken as '<') and set to", lt_value_used, "---\n")
cat("Nitrate:", sum(pre$no3_file == lt_value_in_file, na.rm = TRUE), "of", nrow(pre),
    "| Ammonium:", sum(pre$nh4_file == lt_value_in_file, na.rm = TRUE), "of", nrow(pre), "\n")

cat("\n--- 6. Non-missing group totals by depth group ---\n")
print(groups %>% group_by(depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"), n = Inf)

cat("\n--- 7. Profile (0-100): n plots / min / mean / max for water (mm) and N (kg/ha) ---\n")
prof <- groups %>% group_by(plot) %>%
  summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_,
            profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop")
print(prof %>% summarise(n_water = sum(!is.na(profile_water)), mean_water = mean(profile_water, na.rm = TRUE),
                         min_water = min(profile_water, na.rm = TRUE), max_water = max(profile_water, na.rm = TRUE),
                         n_N = sum(!is.na(profile_N)), mean_N = mean(profile_N, na.rm = TRUE),
                         min_N = min(profile_N, na.rm = TRUE), max_N = max(profile_N, na.rm = TRUE)))
print(layers %>% group_by(n_depth) %>%
        summarise(moisture = mean(moisture_pct), water_mm = mean(water_mm),
                  no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"))

cat("\n--- 8. Against the site's own columns (water should match; N matches only if lt_value_used = 0.9) ---\n")
print(layers %>% summarise(max_abs_diff_water_mm = max(abs(water_mm - site_water_mm), na.rm = TRUE),
                           mean_ours_N = mean(mineral_n_kgha), mean_site_N = mean(site_layer_n),
                           layers_N_differ = sum(abs(mineral_n_kgha - site_layer_n) > 0.01)))

# ---- Step 7: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Annuello_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Annuello_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
