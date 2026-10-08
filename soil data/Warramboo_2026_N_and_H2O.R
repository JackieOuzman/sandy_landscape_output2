# ==============================================================================
# Script:   Warramboo_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Warramboo-Sampson 2026
# Purpose:  Soil mineral N (kg/ha) by depth layer and depth group (N ONLY - no water file yet)
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/3. Soil Data and Nutrition/2026, plus the 2025 Master Database for the plot key)
#   Batch-51032-...-2026-05-25.xlsx            - APAL nitrate and ammonium, 0-10 cm (event 1)
#   Batch-51033-51034-51035-...-2026-05-22.xlsx - APAL, 10-20 to 60-100 cm (event 1)
#   Copy of Batch-52802-52803-52804-52805-...-2026-09-08.xlsx - APAL, 0-10 to 80-100 cm (event 2)
#   Waramboo Estimates Starting Mineral N.xlsx - per-plot starting N estimate (cross-check only)
#   ../2025/Waramboo_Master_Database_WITH_WATER_2025.xlsx - sheet "Trial_Plan" (plot key)
# OUTPUTS (same folder)
#   Warramboo_2026_depth_layers_N_and_H2O.csv
#   Warramboo_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
#   Water columns are written as NA so the file stacks with the other site-years.
# NOTES
#   Two events, 48 plots each (the same plots):
#     baseline  - APAL received 11 May 2026; 5 layers (0-10, 10-20, 20-40, 40-60, 60-100)
#     in-season - APAL received 28 Aug 2026; 6 layers (60-80 and 80-100 reported separately)
#   APAL SamplingDate is blank, so the sample dates are NOT KNOWN: set sample_date_baseline
#   and sample_date_inseason in Step 1 once found (Field Diary). The in-season growth stage
#   is also to confirm.
#   Plot = number in the APAL SampleName ("Warramboo Plot 12" -> C12).
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/4. SSO2_Warramboo-Sampson"
data_dir <- file.path(site_dir, "3. Soil Data and Nutrition/2026")
out_dir  <- data_dir
key_file <- file.path(site_dir, "3. Soil Data and Nutrition/2025",
                      "Waramboo_Master_Database_WITH_WATER_2025.xlsx")
est_file <- file.path(data_dir, "Waramboo Estimates Starting Mineral N.xlsx")

find_batch <- function(pattern) {
  f <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
file_1a <- find_batch("Batch-51032.*\\.xlsx$")
file_1b <- find_batch("Batch-51033.*\\.xlsx$")
file_2  <- find_batch("Batch-52802.*\\.xlsx$")
stopifnot(file.exists(key_file), file.exists(est_file))

bulk_density <- 1.3

# SAMPLING DATES NOT FOUND - fill in when known (leave NA until then)
sample_date_baseline  <- as.Date(NA)   # APAL received 2026-05-11
sample_date_inseason  <- as.Date(NA)   # APAL received 2026-08-28

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key (same key as 2025) ----
key <- read_excel(key_file, sheet = "Trial_Plan", .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(PlotID), treatment_label = Treatment,
            treatment_desc = paste(Ripping_Label, Nutrition_Label, sep = " + ")) %>%
  filter(!is.na(plot))

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

n_lab <- bind_rows(
  read_apal(file_1a) %>% mutate(sample_event = "baseline"),
  read_apal(file_1b) %>% mutate(sample_event = "baseline"),
  read_apal(file_2)  %>% mutate(sample_event = "in-season")) %>%
  transmute(sample_event, batch = BatchID, sample_name = SampleName,
            plot = paste0("C", str_match(SampleName, "(?i)plot\\s*(\\d+)")[, 2]),
            apal_depth = str_trim(SampleDepth),
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# ---- Step 4: layers (one row per APAL sample; no water) ----
layers <- n_lab %>%
  mutate(sample_date = if_else(sample_event == "baseline", sample_date_baseline, sample_date_inseason),
         dm = str_match(apal_depth, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         bulk_density = bulk_density,
         moisture_pct = NA_real_, water_mm = NA_real_,
         n_depth = apal_depth,
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10) %>%
  select(-dm) %>%
  left_join(key, by = "plot")

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
cat("\n--- 1. Rows, plots and layers by event ---\n")
print(layers %>% group_by(sample_event) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(),
                  apal_received = paste(unique(apal_received), collapse = ", "),
                  apal_sampled = paste(unique(apal_sampled), collapse = ", "), .groups = "drop"))
print(layers %>% count(sample_event, n_depth) %>% arrange(sample_event, n_depth), n = Inf)

cat("\n--- 2. Key and duplicate checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(plot) | plot == "CNA" | is.na(treatment_label)) %>% distinct(plot))
print(layers %>% filter(is.na(top_cm)) %>% distinct(apal_depth))
print(layers %>% count(sample_event, plot, n_depth) %>% filter(n > 1))
print(layers %>% group_by(sample_event) %>% summarise(n_plots_not_in_key = sum(!plot %in% key$plot)))

cat("\n--- 3. Plots missing a layer (event, layer, plots) ---\n")
all_plots <- sort(unique(layers$plot))
miss <- layers %>% group_by(sample_event) %>% group_modify(~ {
  expected <- unique(.x$n_depth)
  crossing(plot = unique(.x$plot), n_depth = expected) %>%
    anti_join(.x, by = c("plot", "n_depth"))
})
print(miss %>% group_by(sample_event, n_depth) %>%
        summarise(n_missing = n(), plots = paste(sort(plot), collapse = " "), .groups = "drop"), n = Inf)

cat("\n--- 4. Values set to 0 (below detection) by event ---\n")
print(n_lab %>% group_by(sample_event) %>%
        summarise(n = n(), no3_lt = sum(no3_mgkg == 0, na.rm = TRUE),
                  nh4_lt = sum(nh4_mgkg == 0, na.rm = TRUE), .groups = "drop"))

cat("\n--- 5. Non-missing N group totals by event and depth group ---\n")
print(groups %>% group_by(sample_event, depth_group) %>%
        summarise(n_groups = n(), n_N = sum(!is.na(mineral_n_kgha)), .groups = "drop"), n = Inf)

cat("\n--- 6. Profile N (0-100, all three groups present): n plots / min / mean / max ---\n")
print(groups %>% group_by(sample_event, plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        group_by(sample_event) %>%
        summarise(n = sum(!is.na(profile_N)),
                  min = if (n > 0) min(profile_N, na.rm = TRUE) else NA_real_,
                  mean = if (n > 0) mean(profile_N, na.rm = TRUE) else NA_real_,
                  max = if (n > 0) max(profile_N, na.rm = TRUE) else NA_real_, .groups = "drop"))

cat("\n--- 7. Mean N by layer and event (mg/kg and kg/ha) ---\n")
print(layers %>% group_by(sample_event, n_depth) %>%
        summarise(no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"), n = Inf)
cat("Highest ammonium results (mg/kg):\n")
print(layers %>% arrange(desc(nh4_mgkg)) %>% select(sample_event, plot, n_depth, no3_mgkg, nh4_mgkg) %>% head(8))

cat("\n--- 8. Baseline profile N against the 'Estimates Starting Mineral N' sheet ---\n")
est <- read_excel(est_file, sheet = "Sheet1", .name_repair = "unique_quiet")
est <- tibble(plot = paste0("C", est[[4]]), est_N = suppressWarnings(as.numeric(est[[17]]))) %>%
  filter(!is.na(est_N))
cmp <- groups %>% filter(sample_event == "baseline") %>% group_by(plot) %>%
  summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
  inner_join(est, by = "plot") %>% filter(!is.na(profile_N))
print(cmp %>% summarise(n = n(), mean_ours = mean(profile_N), mean_estimate = mean(est_N),
                        mean_diff = mean(profile_N - est_N), r = cor(profile_N, est_N)))

# ---- Step 7: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Warramboo_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Warramboo_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
