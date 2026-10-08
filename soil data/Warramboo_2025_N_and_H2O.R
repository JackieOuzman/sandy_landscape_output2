# ==============================================================================
# Script:   Warramboo_2025_N_and_H2O.R
# Project:  Sandy Soils II, Output 2, Warramboo-Sampson 2025
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer and depth group
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-09
#
# INPUTS  (in <site>/3. Soil Data and Nutrition/2025)
#   Waramboo Soil water Output2025_SSKP May.xlsx - sheets "Baseline Sowing 4 April" and
#                                                  "In season Water" (wet / dry / cup weights)
#   APAL results-Warramboo baseline.xlsx         - APAL nitrate and ammonium (baseline only)
#   Waramboo_Master_Database_WITH_WATER_2025.xlsx - sheet "Trial_Plan" (plot key)
# OUTPUTS (same folder)
#   Warramboo_2025_depth_layers_N_and_H2O.csv
#   Warramboo_2025_depth_groups_N_and_H2O.csv
# CONVENTIONS (as Walpeup, Copeville and Wharminda)
#   moisture % = (wet - dry) / (dry - cup) x 100; above 40% or below 0 set to NA
#   water mm   = moisture/100 x bulk density x layer cm x 10
#   mineral N kg/ha = (NO3 + NH4 mg/kg) x bulk density x layer cm / 10
#   "<" results set to 0; bulk density assumed 1.3 (the source files use 1.5 / 1.6)
#   Depth groups 0-20, 20-60, 60-100; a group total is NA unless the layers add up
#   to the full group thickness and none are NA
# NOTES
#   Five events: baseline (12 plots, water + N), GS31, GS37, GS69 (32 plots, water only)
#   and a post-harvest water sample (16 Jan 2026, labelled GS89 in the water file).
#   Baseline date: APAL sample names, the soil characterisation sheet and the Soil Database
#   column header all say 2 April 2025. The Master Database and Soil Database "Read me"
#   say 3 April, and the water sheet name says 4 April - 2 April used (TO CONFIRM).
#   Baseline weights ("Wet + chip", "Dry + chip") have no tray weight recorded, so they are
#   treated as net weights (cup = 0), as in the source GM column - TO CONFIRM.
#   The GS89 date is blank in the water file; 16 Jan 2026 is from the Read me sheets.
#   APAL has 5 layers (60-100 bulked); water has 6 (60-80, 80-100). The 60-100 N value is
#   used for both water layers (each with its own thickness).
#   Plot = "C" + row number (1 bay x 64 rows). APAL SampleName holds the plot (e.g. ..._C11_...).
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(readr)

site_dir   <- "H:/Output-2/Site-Data/4. SSO2_Warramboo-Sampson"
data_dir   <- file.path(site_dir, "3. Soil Data and Nutrition/2025")
out_dir    <- data_dir
water_file <- file.path(data_dir, "Waramboo Soil water Output2025_SSKP May.xlsx")
n_file     <- file.path(data_dir, "APAL results-Warramboo baseline.xlsx")
key_file   <- file.path(data_dir, "Waramboo_Master_Database_WITH_WATER_2025.xlsx")
stopifnot(file.exists(water_file), file.exists(n_file), file.exists(key_file))

bulk_density <- 1.3

# event key in the water file -> event name and date used here
events <- tibble(event_key   = c("Sow", "GS31", "GS37", "GS69", "GS89"),
                 sample_event = c("baseline", "GS31", "GS37", "GS69", "harvest"),
                 sample_date  = as.Date(c("2025-04-02", "2025-08-20", "2025-09-12",
                                          "2025-09-22", "2026-01-16")))

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)
less_than_to_zero <- function(x) {
  x <- str_trim(x)
  if_else(str_starts(x, "<"), 0, suppressWarnings(as.numeric(x)))
}

# ---- Step 2: plot key ----
key <- read_excel(key_file, sheet = "Trial_Plan", .name_repair = "unique_quiet") %>%
  transmute(plot = str_trim(PlotID), treatment_label = Treatment,
            treatment_desc = paste(Ripping_Label, Nutrition_Label, sep = " + ")) %>%
  filter(!is.na(plot))

# ---- Step 3: water (wet / dry / cup weights, 6 layers) ----
# 3a. baseline (sowing) sheet: no cup weight, so cup = 0
water_base <- read_excel(water_file, sheet = "Baseline Sowing 4 April", col_types = "text",
                         .name_repair = "unique_quiet") %>%
  filter(!is.na(row), !is.na(`Depth lAyer`)) %>%
  transmute(event_key = "Sow", plot = paste0("C", row), combo = Treatments_mainsub,
            depth_label = str_trim(`Depth lAyer`),
            wet = suppressWarnings(as.numeric(`Wet + chip`)),
            dry = suppressWarnings(as.numeric(`Dry + chip`)),
            cup = 0)

# 3b. in-season sheet: wet, dry and cup weights for each event sit side by side.
# Columns are found by name (wet), then dry and cup are the next two columns.
season_raw <- read_excel(water_file, sheet = "In season Water", col_types = "text",
                         .name_repair = "unique_quiet") %>%
  filter(!is.na(`Plot ID`), !is.na(`Depth (cm)`))

pull_event <- function(ev) {
  i <- which(str_starts(names(season_raw), paste0("Wet Weight ", ev)))
  stopifnot(length(i) == 1)
  tibble(event_key = ev, plot = paste0("C", season_raw[["Plot ID"]]),
         combo = season_raw[["Treatments_mainsub"]],
         depth_label = str_trim(season_raw[["Depth (cm)"]]),
         wet = suppressWarnings(as.numeric(season_raw[[i]])),
         dry = suppressWarnings(as.numeric(season_raw[[i + 1]])),
         cup = suppressWarnings(as.numeric(season_raw[[i + 2]])))
}
water_season <- map_dfr(c("GS31", "GS37", "GS69", "GS89"), pull_event)

water <- bind_rows(water_base, water_season)

# ---- Step 4: APAL nitrate and ammonium ----
read_apal <- function(file, sheet = "Data") {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  nm <- as.character(unlist(raw[hdr, ])); nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))
}

n_lab <- read_apal(n_file) %>%
  transmute(sample_name = SampleName,
            plot = str_match(SampleName, "_(C\\d+)_")[, 2],
            n_depth = str_trim(SampleDepth),
            apal_date = as.Date(SamplingDate),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`),
            sample_event = "baseline")

# ---- Step 5: layers ----
layers <- water %>%
  left_join(events, by = "event_key") %>%
  mutate(dm = str_match(depth_label, "^(\\d+)-(\\d+)$"),
         top_cm = as.numeric(dm[, 2]), bottom_cm = as.numeric(dm[, 3]),
         thick_cm = bottom_cm - top_cm,
         raw_moist = (wet - dry) / (dry - cup) * 100,
         moisture_pct = if_else(raw_moist > 40 | raw_moist < 0, NA_real_, raw_moist),
         # manual overrides: GS69 C51 80-100 cm (18.6% vs 4.8% above) and
         # harvest C42 20-40 cm (7.0% vs 0.3% above) look mistyped
         moisture_pct = if_else(sample_event == "GS69" & plot == "C51" & top_cm == 80, NA_real_, moisture_pct),
         moisture_pct = if_else(sample_event == "harvest" & plot == "C42" & top_cm == 20, NA_real_, moisture_pct),
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
cat("\n--- 1. Rows and plots (layers) ---\n")
print(layers %>% group_by(sample_event, sample_date) %>%
        summarise(n_plots = n_distinct(plot), n_rows = n(), .groups = "drop"))

cat("\n--- 2. Missing weights, and moisture above 40 or below 0 (set to NA) ---\n")
print(layers %>% filter(is.na(wet) | is.na(dry) | is.na(cup) | raw_moist > 40 | raw_moist < 0) %>%
        count(sample_event, depth_label, name = "n_rows") %>% arrange(sample_event, depth_label), n = Inf)
print(layers %>% filter(raw_moist > 40 | raw_moist < 0) %>%
        select(sample_event, plot, depth_label, wet, dry, cup, raw_moist))

cat("\n--- 3. Key checks (all should be 0 rows) ---\n")
print(layers %>% filter(is.na(treatment_label)) %>% distinct(plot))
print(layers %>% filter(combo != treatment_label) %>% distinct(plot, combo, treatment_label))
print(layers %>% filter(is.na(top_cm)) %>% distinct(depth_label))
print(n_lab %>% filter(is.na(plot)))

cat("\n--- 4. APAL vs water: plots, dates, missing N samples ---\n")
base_plots <- unique(water$plot[water$event_key == "Sow"])
cat("APAL plots not in baseline water:", paste(setdiff(unique(n_lab$plot), base_plots), collapse = ", "), "\n")
cat("Baseline water plots not in APAL:", paste(setdiff(base_plots, unique(n_lab$plot)), collapse = ", "), "\n")
cat("APAL SamplingDate values:", paste(unique(n_lab$apal_date), collapse = ", "), "| baseline date used:",
    format(events$sample_date[1]), "\n")
cat("APAL samples:", nrow(n_lab), "| plot x depth combinations:", nrow(distinct(n_lab, plot, n_depth)), "\n")

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
                  max = if (n > 0) max(profile_water, na.rm = TRUE) else NA_real_,
                  .groups = "drop"))

# ---- Step 8: N checks ----
cat("\n--- 7. N: values set to 0 (below detection) and baseline profile N ---\n")
cat("Nitrate '<' set to 0:",  sum(n_lab$no3_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")
cat("Ammonium '<' set to 0:", sum(n_lab$nh4_mgkg == 0, na.rm = TRUE), "of", nrow(n_lab), "\n")
print(groups %>% filter(sample_event == "baseline") %>% group_by(plot) %>%
        summarise(profile_N = if (n() == 3) strict_sum(mineral_n_kgha) else NA_real_, .groups = "drop") %>%
        summarise(n = sum(!is.na(profile_N)), min = min(profile_N, na.rm = TRUE),
                  mean = mean(profile_N, na.rm = TRUE), max = max(profile_N, na.rm = TRUE)))

# ---- Step 9: moisture check against the source sheet's own moisture column ----
cat("\n--- 8. Our moisture % against the source file's (GS31 / GS37 / GS69 / GS89) ---\n")
src <- map_dfr(c("GS31", "GS37", "GS69", "GS89"), function(ev) {
  i <- which(str_starts(names(season_raw), paste0("Soil Moisture \\(%\\) ", ev)))
  tibble(event_key = ev, plot = paste0("C", season_raw[["Plot ID"]]),
         depth_label = str_trim(season_raw[["Depth (cm)"]]),
         src_pct = suppressWarnings(as.numeric(season_raw[[i]])) * 100)
})
print(layers %>% inner_join(src, by = c("event_key", "plot", "depth_label")) %>%
        filter(!is.na(moisture_pct), !is.na(src_pct)) %>%
        summarise(n = n(), max_abs_diff = max(abs(moisture_pct - src_pct))))

# ---- Step 10: save ----
layers_out <- layers %>%
  select(sample_date, sample_event, plot, treatment_label, treatment_desc, top_cm, bottom_cm,
         thick_cm, bulk_density, moisture_pct, water_mm, n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)
write_csv(layers_out, file.path(out_dir, "Warramboo_2025_depth_layers_N_and_H2O.csv"))
write_csv(groups,     file.path(out_dir, "Warramboo_2025_depth_groups_N_and_H2O.csv"))
cat("\nSaved to:", out_dir, "\n")
