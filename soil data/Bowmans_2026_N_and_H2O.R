# ==============================================================================
# Script:   Bowmans_2026_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Bowmans-Roberts 2026
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/2. SSO2_Soildata and nutrition/2026)
#   SSO2_Bowmans_soilmoistures_2026.xlsx - water (chip / wet / dry weights, 6 layers):
#       sheets "Presow core" (12 May), "GS37 core" (13 Aug), "GS65 core" (16 Sep);
#       sheet "Sheet1" is the plot key (plot, bay, row, ripping and nutrition treatments)
#   Batch-51252-...-2026-05-28.xlsx - APAL nitrate and ammonium, pre-sow (5 layers, one 60-100)
#   Batch-52753-...-Gs37-...-2026-09-08.xlsx - APAL, GS37 (6 layers)
#   Batch-53304-...-Gs65-...-2026-10-02.xlsx - APAL, GS65 (6 layers)
# OUTPUTS (same folder)
#   Bowmans_2026_depth_layers_N_and_H2O.csv
#   Bowmans_2026_depth_groups_N_and_H2O.csv
# CONVENTIONS (as the other site-years)
#   moisture % = (wet - dry) / (dry - chip) x 100;  water mm = moisture/100 x BD x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x BD x layer cm / 10;  BD assumed 1.3
#   "<" results set to 0; moisture above 40 or below 0 set to NA
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up to the
#   full group thickness and none are NA
# NOTES
#   * Three events: pre-sow = "baseline" (48 plots), GS37 (48 plots), GS65 (16 plots).
#     Sample dates come from the Day / Month / Year columns of the water sheets (APAL
#     SamplingDate is blank).
#   * Pre-sow APAL has ONE 60-100 N layer: it is applied to both water layers 60-80 and
#     80-100. The "60-100" rows on the Presow core water sheet are the APAL bulk sample
#     (no weights) and are dropped.
#   * The lab's SampleDepth is wrong for the GS37 10-20 cm samples (labelled 0-10), so the
#     layer is read from the end of SampleName. One pre-sow sample (C2) is named "60-80"
#     but is the 60-100 sample (SampleDepth and the submission form say 60-100).
#   * Bulk density files (Bowmans CORE / Dest bulk dens 2026) are NOT used; BD stays at 1.3.
#   * Not included: the destructive-trial moisture tabs.
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir <- "H:/Output-2/Site-Data/6. SS02_Bowmans-Roberts/2. SSO2_Soildata and nutrition"
data_dir <- file.path(site_dir, "2026")
out_dir  <- data_dir

find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  f <- f[!str_detect(basename(f), "^~\\$")]
  stopifnot(length(f) == 1)
  f
}
water_file <- find_file(data_dir, "SSO2_Bowmans_soilmoistures_2026\\.xlsx$")
apal_1 <- find_file(data_dir, "Batch-51252.*\\.xlsx$")
apal_2 <- find_file(data_dir, "Batch-52753.*\\.xlsx$")
apal_3 <- find_file(data_dir, "Batch-53304.*\\.xlsx$")

bulk_density <- 1.3

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key ----
key <- read_excel(water_file, sheet = "Sheet1", col_types = "text", .name_repair = "unique_quiet") %>%
  filter(!is.na(PLOT)) %>%
  transmute(plot = str_trim(PLOT), treatment_label = Treatments_mainsub,
            treatment_desc = paste(Ripping_main_lab, Nutrition_sub_lab, sep = " + "))

# ---- Step 3: water (chip / wet / dry weights, 6 layers, three events) ----
read_water <- function(sheet, event) {
  raw <- read_excel(water_file, sheet = sheet, col_types = "text", .name_repair = "minimal")
  stopifnot(names(raw)[c(4, 5, 6, 7, 20)] == c("Day", "Month", "Year", "PLOT", "Depth"),
            str_detect(names(raw)[22], "^Chip Wt"), str_detect(names(raw)[23], "^Wet Wt"),
            str_detect(names(raw)[24], "^Dry Wt"))
  tibble(sample_event = event,
         sample_date = as.Date(paste0(2000 + as.integer(str_extract(raw[[6]], "\\d+")), "-",
                                      str_extract(raw[[5]], "\\d+"), "-",
                                      str_extract(raw[[4]], "\\d+"))),
         plot = str_trim(raw[[7]]), depth_label = str_trim(raw[[20]]),
         chip = suppressWarnings(as.numeric(raw[[22]])),
         wet  = suppressWarnings(as.numeric(raw[[23]])),
         dry  = suppressWarnings(as.numeric(raw[[24]]))) %>%
    filter(!is.na(plot), !is.na(depth_label), depth_label != "60-100")   # 60-100 = APAL bulk sample
}
water <- bind_rows(read_water("Presow core", "baseline"),
                   read_water("GS37 core",   "GS37"),
                   read_water("GS65 core",   "GS65"))

# ---- Step 4: APAL nitrate and ammonium (three batches) ----
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
  read_apal(apal_1) %>% mutate(sample_event = "baseline"),
  read_apal(apal_2) %>% mutate(sample_event = "GS37"),
  read_apal(apal_3) %>% mutate(sample_event = "GS65")) %>%
  transmute(sample_event, batch = BatchID, sample_name = SampleName,
            plot = str_match(SampleName, "_(C\\d+)_")[, 2],
            apal_depth = str_trim(SampleDepth),
            name_depth = str_match(SampleName, "_\\s*(\\d+-\\d+)$")[, 2],
            apal_received = as.Date(DateReceived), apal_sampled = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`)) %>%
  # layer from the sample name; the one pre-sow C2 sample named 60-80 is really 60-100
  mutate(n_depth = if_else(name_depth != apal_depth & apal_depth == "60-100", "60-100", name_depth))

# ---- Step 5: layers ----
layers <- water %>%
  mutate(dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         raw_moist = (wet - dry) / (dry - chip) * 100,
         moisture_pct = if_else(raw_moist > 40 | raw_moist < 0, NA_real_, raw_moist),
         bulk_density = bulk_density,
         water_mm = moisture_pct / 100 * bulk_density * thick_cm * 10,
         n_depth = if_else(sample_event == "baseline" & top_cm >= 60, "60-100", depth_label)) %>%
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

cat("\n--- 3. Key checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(plot))
print(layers %>% filter(is.na(top_cm)) %>% distinct(depth_label))
print(layers %>% count(sample_event, plot, depth_label) %>% filter(n > 1))
print(n_lab %>% filter(is.na(plot) | is.na(n_depth)))
print(n_lab %>% count(sample_event, plot, n_depth) %>% filter(n > 1))

cat("\n--- 4. APAL vs water: plots, depth labels, dates, missing N ---\n")
for (ev in c("baseline", "GS37", "GS65")) {
  wp <- unique(water$plot[water$sample_event == ev]); ap <- unique(n_lab$plot[n_lab$sample_event == ev])
  cat(ev, "| APAL not in water:", paste(setdiff(ap, wp), collapse = ", "),
      "| water not in APAL:", paste(setdiff(wp, ap), collapse = ", "), "\n")
}
print(n_lab %>% group_by(sample_event) %>%
        summarise(apal_received = paste(unique(apal_received), collapse = ", "),
                  apal_sampled = paste(unique(apal_sampled), collapse = ", "),
                  n_samples = n(), n_depth_mislabelled = sum(name_depth != apal_depth), .groups = "drop"))
print(n_lab %>% filter(name_depth != apal_depth) %>% count(sample_event, name_depth, apal_depth, n_depth))
print(layers %>% filter(is.na(mineral_n_kgha)) %>% count(sample_event, depth_label, n_depth))

cat("\n--- 5. Non-missing group totals by event and depth group ---\n")
print(groups %>% group_by(sample_event, depth_group) %>%
        summarise(n_groups = n(), n_water = sum(!is.na(water_mm)), n_N = sum(!is.na(mineral_n_kgha)),
                  .groups = "drop"), n = Inf)

cat("\n--- 6. Profile (0-100) water and N by event: n plots / min / mean / max ---\n")
prof <- groups %>% group_by(sample_event, plot) %>%
  summarise(profile_water = if (n() == 3) strict_sum(water_mm) else NA_real_,
            profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop")
print(prof %>% group_by(sample_event) %>%
        summarise(n_w = sum(!is.na(profile_water)), w_min = min(profile_water, na.rm = TRUE),
                  w_mean = mean(profile_water, na.rm = TRUE), w_max = max(profile_water, na.rm = TRUE),
                  n_N = sum(!is.na(profile_N)), N_min = min(profile_N, na.rm = TRUE),
                  N_mean = mean(profile_N, na.rm = TRUE), N_max = max(profile_N, na.rm = TRUE), .groups = "drop"))

cat("\n--- 7. Mean by layer and event: moisture %, water mm, NO3, NH4, N kg/ha ---\n")
print(layers %>% group_by(sample_event, depth_label) %>%
        summarise(moist = mean(moisture_pct, na.rm = TRUE), water_mm = mean(water_mm, na.rm = TRUE),
                  no3 = mean(no3_mgkg), nh4 = mean(nh4_mgkg), n_kgha = mean(mineral_n_kgha), .groups = "drop"), n = Inf)
print(n_lab %>% group_by(sample_event) %>%
        summarise(n = n(), no3_lt = sum(no3_mgkg == 0, na.rm = TRUE), nh4_lt = sum(nh4_mgkg == 0, na.rm = TRUE),
                  .groups = "drop"))
cat("Highest ammonium results (mg/kg):\n")
print(layers %>% arrange(desc(nh4_mgkg)) %>% select(sample_event, plot, depth_label, no3_mgkg, nh4_mgkg) %>% head(6))

cat("\n--- 8. Our moisture % against the source sheets' own moisture column ---\n")
src <- map2_dfr(c("Presow core", "GS37 core", "GS65 core"), c("baseline", "GS37", "GS65"), function(sh, ev) {
  raw <- read_excel(water_file, sheet = sh, col_types = "text", .name_repair = "minimal")
  tibble(sample_event = ev, plot = str_trim(raw[[7]]), depth_label = str_trim(raw[[20]]),
         src_pct = suppressWarnings(as.numeric(raw[[25]])))
})
print(layers %>% inner_join(src, by = c("sample_event", "plot", "depth_label")) %>%
        filter(!is.na(moisture_pct), !is.na(src_pct)) %>%
        summarise(n = n(), max_abs_diff = max(abs(moisture_pct - src_pct))))

# ---- Step 8: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Bowmans_2026_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Bowmans_2026_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
