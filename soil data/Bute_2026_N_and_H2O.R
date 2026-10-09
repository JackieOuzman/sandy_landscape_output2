# ==============================================================================
# Script:   Bute_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Bute-Kreig 2026
# Purpose:  Soil mineral N (kg/ha) by depth layer and depth group (N ONLY - no water file yet)
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/1.SSO2_Soil data and nutrition/2026)
#   Batch-51090-51089-51080-51079-...-2026-05-26.xlsx - sheet "Data": APAL nitrate and ammonium
#        56 plots x 5 layers (0-10, "0-20", 20-40, 40-60, 60-100); DateReceived 12 May 2026
#   Copy of CSV of Soil N_Sowing. .xlsx - sheet "Sheet1": columns P:AS are the plot key
#        (plot no, bay, row, ripping and nutrition treatments) and the site's own N totals
#        (cross-check only; they use bulk density 1.45)
# OUTPUTS (same folder)
#   Bute_2026_depth_layers_N_and_H2O.csv
#   Bute_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
#   Water columns are written as NA so the file stacks with the other site-years.
# NOTES
#   * One event (sowing / baseline), 56 plots. Plot = "C" + plot no, from SampleName "R<row>C<bay>".
#   * APAL SamplingDate is blank (received 12 May 2026), so the sample date is NOT KNOWN: set
#     sample_date_baseline in Step 1 once found (sowing was 13 May 2026).
#   * The lab labels the second layer "0-20" but the site's own sheet treats it as 10 cm thick
#     (0-10 and "0-20" would overlap), so it is taken as 10-20 cm. Set
#     fix_0_20_label <- FALSE to keep the lab label (then the profile sums are NOT valid).
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/5. SS02_Bute-Kreig/1.SSO2_Soil data and nutrition"
data_dir <- file.path(site_dir, "2026")
out_dir  <- data_dir

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
apal_file <- find_file(data_dir, "Batch-51090.*\\.xlsx$")
key_file  <- find_file(data_dir, "Copy.of.CSV.of.Soil.N.Sowing.*\\.xlsx$")

bulk_density <- 1.3
fix_0_20_label <- TRUE     # lab "0-20" layer is taken as 10-20 cm

# SAMPLING DATE NOT FOUND - fill in when known (leave NA until then)
sample_date_baseline <- as.Date(NA)   # APAL received 2026-05-12; sowing 2026-05-13

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (row + bay -> plot, treatment) ----
key <- read_excel(key_file, sheet = "Sheet1", range = "P2:AS300", .name_repair = "unique_quiet") %>%
  filter(!is.na(ID)) %>%
  transmute(plot = paste0("C", `Plot no`), trial_row = as.integer(row), trial_col = as.integer(bay),
            treatment_label = paste(Ripping_main, Nutrition_sub, sep = "-"),
            treatment_desc  = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "),
            sampled = `Soil Sample` == "Yes",
            site_total_n = suppressWarnings(as.numeric(`0-100`)))
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
            trial_row = as.integer(str_match(SampleName, "^R(\\d+)C(\\d+)$")[, 2]),
            trial_col = as.integer(str_match(SampleName, "^R(\\d+)C(\\d+)$")[, 3]),
            apal_depth = str_trim(SampleDepth),
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`)) %>%
  left_join(key, by = c("trial_row", "trial_col"))

# ---- Step 4: layers (one row per APAL sample; no water) ----
layers <- n_lab %>%
  mutate(sample_event = "baseline", sample_date = sample_date_baseline,
         depth_used = if_else(fix_0_20_label & apal_depth == "0-20", "10-20", apal_depth),
         dm = str_match(depth_used, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         bulk_density = bulk_density,
         moisture_pct = NA_real_, water_mm = NA_real_,
         n_depth = depth_used,
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
print(layers %>% filter(!sampled) %>% distinct(plot))

cat("\n--- 3. Plots missing a layer (should be none) ---\n")
print(crossing(plot = unique(layers$plot), n_depth = unique(layers$n_depth)) %>%
        anti_join(layers, by = c("plot", "n_depth")))
cat("Key plots flagged 'Soil Sample = Yes' but not in APAL:",
    paste(setdiff(key$plot[key$sampled %in% TRUE], unique(layers$plot)), collapse = ", "), "\n")

cat("\n--- 4. Treatments sampled ---\n")
print(layers %>% distinct(plot, treatment_label, treatment_desc) %>% count(treatment_label, treatment_desc))

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
cat("Highest ammonium results (mg/kg):\n")
print(layers %>% arrange(desc(nh4_mgkg)) %>% select(plot, n_depth, no3_mgkg, nh4_mgkg) %>% head(6))

cat("\n--- 8. Our profile N against the site's own 0-100 total (their BD is 1.45, so expect ratio near 0.90) ---\n")
cmp <- prof %>% inner_join(key %>% select(plot, site_total_n), by = "plot") %>%
  filter(!is.na(profile_N), !is.na(site_total_n))
print(cmp %>% summarise(n = n(), mean_ours = mean(profile_N), mean_site = mean(site_total_n),
                        mean_ratio = mean(profile_N / site_total_n), r = cor(profile_N, site_total_n)))

# ---- Step 7: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Bute_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Bute_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
