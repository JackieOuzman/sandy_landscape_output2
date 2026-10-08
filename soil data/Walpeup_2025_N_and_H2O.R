# =============================================================================
# Script:   Walpeup_2025_N_and_H2O.R
# Project:  Sandy Soils II - Output 2 (starting soil N and soil water)
# Site:     SSO2_Walpeup-Pole (Walpeup)
# Year:     2025 pre-sow (56 sample points, water and N on the same points)
# Author:   Jackie Ouzman, CSIRO Systems Analysis
# Created:  2026-10-08
#
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer, depth group
#           and profile total. Coordinates and zone are joined at stacking.
#
# Input:    .../2. Soil Data and Nutrition/2025/Soil Coring Worksheet 2025.xlsx
#             (sheet "Water Analysis")
#           .../APAL soils/2025/O2_Walpeup_Yr2_presow_25.xlsx (sheet "Data")
#           .../2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx
#             (plot / treatment key only)
#
# Output:   Walpeup_2025_depth_layers_N_and_H2O.csv
#           Walpeup_2025_depth_groups_N_and_H2O.csv
#
# Method:   As Walpeup 2024: moisture = (wet - dry) / (dry - bag) x 100,
#           >40% set to NA; water mm = moisture/100 x BD x cm x 10;
#           mineral N kg/ha = (NO3 + NH4) x BD x cm / 10, "<" = 0;
#           depth groups 0-20, 20-60, 60-100; strict totals
#
# Assumes:  Bulk density 1.3 g/cm3. Points matched to plots by Row + Bay.
#           N 60-100 cm result applied to water layers 60-80 and 80-100.
#
# Gaps:     Sampling date not recorded in any file (APAL SamplingDate blank,
#           received 2025-05-26). Set sampling_date below once confirmed.
#           Row 2 Bay 1 0-10 cm: different bag brand (comment in worksheet);
#           effect on moisture is negligible, no adjustment made.
# =============================================================================

# ---- Step 1: Set up --------------------------------------------------------
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

site_dir   <- "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole"
water_file <- file.path(site_dir, "2. Soil Data and Nutrition/2025/Soil Coring Worksheet 2025.xlsx")
n_file     <- file.path(site_dir, "APAL soils/2025/O2_Walpeup_Yr2_presow_25.xlsx")
key_file   <- file.path(site_dir, "2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx")
out_dir    <- file.path(site_dir, "2. Soil Data and Nutrition/2025")

bulk_density  <- 1.3                 # g/cm3, assumed (not measured)
sample_year   <- 2025
sampling_date <- as.Date(NA)         # TODO: confirm the real sampling date

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)

# "<1.0" style results are set to 0 (our convention)
less_than_to_zero <- function(x) {
  x <- as.character(x)
  if_else(str_detect(x, "^\\s*<"), 0, suppressWarnings(as.numeric(x)))
}

# Read an APAL sheet whose header row is not row 1 (found by "SampleName").
# Reads everything as text, so it does not matter how many empty rows are above.
read_apal <- function(file, sheet) {
  raw <- read_excel(file, sheet = sheet, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  # "SampleName" can also appear in a helper column on an earlier row, so of the
  # rows containing it, take the one with the most filled cells (the real header)
  cand <- which(apply(raw, 1, function(r) any(r == "SampleName", na.rm = TRUE)))
  stopifnot(length(cand) > 0)
  hdr <- cand[which.max(rowSums(!is.na(raw[cand, ])))]
  stopifnot(!is.na(hdr))
  nm <- as.character(unlist(raw[hdr, ]))
  nm[is.na(nm)] <- "blank"
  names(raw) <- make.unique(nm)
  raw[-seq_len(hdr), ] %>% filter(!is.na(SampleName))   # also drops the units row
}

# ---- Step 2: Plot / treatment key (from the 2024 workbook) -----------------
key_sheets <- excel_sheets(key_file)
sheet_key  <- key_sheets[str_detect(key_sheets, "ShortID")]
stopifnot(length(sheet_key) == 1)

plot_key <- read_excel(key_file, sheet = sheet_key) %>%
  select(plot = Plot, row, bay, treatment_label = label,
         treatment_desc = TreatmentDescription)

# ---- Step 3: Read the soil water weights ------------------------------------
water_raw <- read_excel(water_file, sheet = "Water Analysis") %>%
  filter(!is.na(Depth)) %>%                      # drops the stray bag-only row
  transmute(row = Row, bay = Bay,
            label_ws    = paste0(Ripping, "_", Treatment),
            depth_label = str_remove(str_trim(Depth), "cm"),
            wet  = as.numeric(`Wet (g)`),
            dry  = as.numeric(`Dry (g)`),
            tare = as.numeric(`Bag (g)`))

# ---- Step 4: Soil water by depth layer --------------------------------------
layers <- water_raw %>%
  left_join(plot_key, by = c("row", "bay")) %>%
  mutate(site = "Walpeup", year = sample_year, sample_date = sampling_date) %>%
  separate(depth_label, into = c("top_cm", "bottom_cm"), sep = "-", convert = TRUE) %>%
  mutate(thick_cm     = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare) * 100,
         moisture_pct = if_else(moisture_pct > 40, NA_real_, moisture_pct),  # >40% = error
         bulk_density = bulk_density,
         water_mm     = moisture_pct / 100 * bulk_density * thick_cm * 10) %>%
  arrange(plot, top_cm)

# ---- Step 4b: Soil mineral N (APAL pre-sow 2025) ----------------------------
n_lab <- read_apal(n_file, sheet = "Data") %>%
  transmute(sample_name = SampleName,
            row      = as.numeric(str_match(SampleName, "_R(\\d+)_B(\\d+)_")[, 2]),
            bay      = as.numeric(str_match(SampleName, "_R(\\d+)_B(\\d+)_")[, 3]),
            n_depth  = str_trim(str_match(SampleName, "_YR\\d+_(.+)$")[, 2]),
            no3_mgkg = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg = less_than_to_zero(`Ammonium - N (2M KCl)`))

# The 60-100 N result applies to both the 60-80 and 80-100 water layers,
# each with its own thickness, so the 60-100 total is not double-counted.
layers <- layers %>%
  mutate(n_depth = case_when(top_cm < 10 ~ "0-10",
                             top_cm < 20 ~ "10-20",
                             top_cm < 40 ~ "20-40",
                             top_cm < 60 ~ "40-60",
                             TRUE        ~ "60-100")) %>%
  left_join(n_lab, by = c("row", "bay", "n_depth")) %>%
  mutate(mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10)

# ---- Step 5: Depth groups and profile totals (strict) ----------------------
depth_groups <- layers %>%
  mutate(depth_group = case_when(top_cm < 20 ~ "0-20",
                                 top_cm < 60 ~ "20-60",
                                 TRUE        ~ "60-100")) %>%
  group_by(site, year, sample_date, plot, treatment_label, treatment_desc, depth_group) %>%
  summarise(n_layers = n(),
            water_mm       = strict_sum(water_mm),
            mineral_n_kgha = strict_sum(mineral_n_kgha),
            .groups = "drop")

profile <- layers %>%
  group_by(site, year, sample_date, plot, treatment_label, treatment_desc) %>%
  summarise(n_layers = n(),
            water_mm_0_100       = strict_sum(water_mm),
            mineral_n_kgha_0_100 = strict_sum(mineral_n_kgha),
            .groups = "drop")

# ---- Step 6: Checks (paste the output back) ---------------------------------
cat("\n1. Layers per point (expect 6) and points (expect 56):\n")
print(layers %>% count(row, bay, name = "n_layers") %>% count(n_layers, name = "points"))

cat("\n2. Missing values, counted explicitly (expect all 0 except sample_date = 336):\n")
print(layers %>% summarise(across(c(plot, treatment_label, sample_date, wet, dry, tare,
                                    moisture_pct, water_mm, no3_mgkg, nh4_mgkg,
                                    mineral_n_kgha), ~ sum(is.na(.x)))))

cat("\n3. Duplicate row/bay/depth (expect 0):\n")
print(sum(duplicated(layers[c("row", "bay", "top_cm")])))

cat("\n4. Treatment label: worksheet vs plot key (expect 0 mismatches):\n")
print(layers %>% filter(is.na(label_ws) | is.na(treatment_label) |
                          label_ws != treatment_label) %>%
        distinct(row, bay, label_ws, treatment_label))

cat("\n5. Ten highest moisture readings (look for typos):\n")
print(layers %>% arrange(desc(moisture_pct)) %>%
        select(plot, treatment_label, top_cm, wet, dry, moisture_pct) %>% head(10))

cat("\n6. Lab N: rows 280, points 56, layers 5, duplicates 0:\n")
print(n_lab %>% summarise(rows = n(), points = n_distinct(row, bay)))
print(n_lab %>% count(row, bay, name = "layers") %>% count(layers, name = "points"))
print(sum(duplicated(n_lab[c("row", "bay", "n_depth")])))
print(n_lab %>% summarise(across(c(row, bay, n_depth, no3_mgkg, nh4_mgkg), ~ sum(is.na(.x)))))

cat("\n7. Points in water but not lab N, and the reverse (expect both 0 rows):\n")
print(anti_join(distinct(layers, row, bay), distinct(n_lab, row, bay), by = c("row", "bay")))
print(anti_join(distinct(n_lab, row, bay), distinct(layers, row, bay), by = c("row", "bay")))

cat("\n8. Rows after N join (expect 336):\n")
print(nrow(layers))

cat("\n9. '<' results set to 0 (expect nitrate 1, ammonium 130):\n")
print(n_lab %>% summarise(no3_zero = sum(no3_mgkg == 0, na.rm = TRUE),
                          nh4_zero = sum(nh4_mgkg == 0, na.rm = TRUE)))

cat("\n10. Profile totals with NA (expect 0 rows):\n")
print(profile %>% filter(is.na(water_mm_0_100) | is.na(mineral_n_kgha_0_100)))

cat("\n11. Profile summary (expect water 40.8 / 73.7 / 119.3 mm, N 37.7 / 67.2 / 117.5 kg/ha):\n")
print(profile %>% summarise(points = n(),
                            water_min = min(water_mm_0_100, na.rm = TRUE),
                            water_mean = mean(water_mm_0_100, na.rm = TRUE),
                            water_max = max(water_mm_0_100, na.rm = TRUE),
                            n_min = min(mineral_n_kgha_0_100, na.rm = TRUE),
                            n_mean = mean(mineral_n_kgha_0_100, na.rm = TRUE),
                            n_max = max(mineral_n_kgha_0_100, na.rm = TRUE)))

# ---- Step 7: Save the two CSVs (run only after the checks are clean) -------
layers_out <- layers %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         top_cm, bottom_cm, thick_cm, bulk_density, moisture_pct, water_mm,
         n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)

groups_out <- depth_groups %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         depth_group, n_layers, water_mm, mineral_n_kgha)

readr::write_csv(layers_out, file.path(out_dir, "Walpeup_2025_depth_layers_N_and_H2O.csv"))
readr::write_csv(groups_out, file.path(out_dir, "Walpeup_2025_depth_groups_N_and_H2O.csv"))
c(layers = nrow(layers_out), groups = nrow(groups_out))   # expect 336 and 168
