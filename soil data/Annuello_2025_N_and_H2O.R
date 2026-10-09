# ==============================================================================
# Script:   Annuello_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Annuello-Aikman 2025
# Purpose:  Soil mineral N (kg/ha) by depth layer and depth group (N ONLY - no water file found)
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS
#   <site>/2.SSO2_Soil Data and Nutrition/2025/Batch-44073-...-2025-06-05.xlsx - sheet "Data"
#        APAL nitrate and ammonium: 12 sample points x 5 layers (0-10, 10-20, 20-40, 40-60, 60-100)
#        SampleName = SSO2_ANN_PRE_R<row>_B<bay>_YR25_<depth>; DateReceived 26 May 2025.
#        (SSO2_Annuello_Baseline_2025.xlsx and "baseline soil Annuello...xlsx" hold the same
#        APAL data, so only the Batch file is read)
#   <site>/1. SSO2_Trial design_MetaData/2026/Annuello_Output2_2026_Design.xlsx - sheet "Data"
#        plot key: bay, row, ripping (main) and nutrition (sub) treatments
#        (a 2026 design file, assumed to be the same layout as 2025)
# OUTPUTS (soil folder, 2025)
#   Annuello_2025_depth_layers_N_and_H2O.csv
#   Annuello_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
#   Water columns are written as NA so the file stacks with the other site-years.
# NOTES
#   * One event (pre-sow baseline), 12 trial rows x bay points. Plot = "R<row>B<bay>".
#   * APAL SamplingDate is blank (received 26 May 2025), so the sample date is NOT KNOWN:
#     set sample_date_baseline in Step 1 once found.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/7. SS02_Annuello-Aikman"
data_dir <- file.path(site_dir, "2.SSO2_Soil Data and Nutrition", "2025")
key_dir  <- file.path(site_dir, "1. SSO2_Trial design_MetaData", "2026")
out_dir  <- data_dir

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
apal_file <- find_file(data_dir, "^Batch-44073.*\\.xlsx$")
key_file  <- find_file(key_dir,  "^Annuello_Output2_2026_Design\\.xlsx$")

bulk_density <- 1.3

# SAMPLING DATE NOT FOUND - fill in when known (leave NA until then)
sample_date_baseline <- as.Date(NA)   # APAL received 2025-05-26

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (row + bay -> plot, treatment) ----
key <- read_excel(key_file, sheet = "Data", .name_repair = "unique_quiet") %>%
  filter(!is.na(bay), !is.na(row)) %>%
  transmute(trial_row = as.integer(row), trial_col = as.integer(bay),
            plot = paste0("R", row, "B", bay),
            treatment_label = Treatments_mainsub,
            treatment_desc  = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "))
stopifnot(!anyDuplicated(key[c("trial_row", "trial_col")]))

# ---- Step 3: APAL nitrate and ammonium ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName), !is.na(SampleDepth))
}

n_lab <- read_apal(apal_file) %>%
  transmute(sample_name = SampleName,
            trial_row = as.integer(str_match(SampleName, "_R(\\d+)_B(\\d+)_")[, 2]),
            trial_col = as.integer(str_match(SampleName, "_R(\\d+)_B(\\d+)_")[, 3]),
            apal_depth = str_trim(SampleDepth),
            name_depth = str_match(SampleName, "_(\\d+-\\d+)$")[, 2],
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`)) %>%
  left_join(key, by = c("trial_row", "trial_col"))

# ---- Step 4: layers (one row per APAL sample; no water) ----
layers <- n_lab %>%
  mutate(sample_event = "baseline", sample_date = sample_date_baseline,
         dm = str_match(apal_depth, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         bulk_density = bulk_density,
         moisture_pct = NA_real_, water_mm = NA_real_,
         n_depth = apal_depth,
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10) %>%
  select(-dm)

# ---- Step 5: depth groups ----
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
cat("\n--- 1. Rows, plots, layers ---\n")
cat("APAL received:", paste(unique(n_lab$apal_received), collapse = ", "),
    "| APAL SamplingDate:", paste(unique(n_lab$apal_sampled), collapse = ", "),
    "| sample date used:", format(sample_date_baseline), "\n")
print(layers %>% summarise(n_plots = n_distinct(plot), n_rows = n()))
print(layers %>% count(apal_depth, n_depth, top_cm, bottom_cm))

cat("\n--- 2. Key and duplicate checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(plot) | is.na(treatment_label)) %>% distinct(sample_name))
print(layers %>% filter(is.na(top_cm)) %>% distinct(apal_depth))
print(layers %>% count(plot, n_depth) %>% filter(n > 1))
print(layers %>% filter(apal_depth != name_depth) %>% distinct(sample_name, apal_depth))

cat("\n--- 3. Plots missing a layer (should be none) ---\n")
print(crossing(plot = unique(layers$plot), n_depth = unique(layers$n_depth)) %>%
        anti_join(layers, by = c("plot", "n_depth")))

cat("\n--- 4. Treatments sampled ---\n")
print(layers %>% distinct(plot, treatment_label, treatment_desc) %>% arrange(plot), n = Inf)

cat("\n--- 5. Values set to 0 (below detection) ---\n")
cat("Nitrate '<':", sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab),
    "| Ammonium '<':", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")

cat("\n--- 6. Non-missing N group totals by depth group ---\n")
print(groups %>% group_by(depth_group) %>%
        summarise(n_groups = n(), n_N = sum(!is.na(mineral_n_kgha)), .groups = "drop"), n = Inf)

cat("\n--- 7. Profile N (0-100): n plots / min / mean / max, and mean by layer ---\n")
prof <- groups %>% group_by(plot) %>%
  summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop")
print(prof %>% summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                         mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))
print(layers %>% group_by(n_depth) %>%
        summarise(no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"))

cat("\n--- 8. Site's own pivot (BD 1.3): mean N by layer, kg/ha. 0-10 should match. Their 10-20 row uses 20 cm\n",
    "    thickness (should be 10) so it is double ours; 20-100 differ a little because their pivot ignores '<' results ---\n")
print(tibble(n_depth = c("0-10", "10-20", "20-40", "40-60", "60-100"),
             site_kgha = c(23.57, 22.97, 10.24, 7.03, 13.70)) %>%
        left_join(layers %>% group_by(n_depth) %>% summarise(ours_kgha = mean(mineral_n_kgha), .groups = "drop"),
                  by = "n_depth"))

# ---- Step 7: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Annuello_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Annuello_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")

