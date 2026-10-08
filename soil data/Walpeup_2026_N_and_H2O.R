# =============================================================================
# Script:   Walpeup_2026_N_and_H2O.R
# Project:  Sandy Soils II - Output 2 (starting soil N and soil water)
# Site:     SSO2_Walpeup-Pole (Walpeup)
# Year:     2026 pre-sow (48 sample points, water and N on the same points)
# Author:   Jackie Ouzman, CSIRO Systems Analysis
# Created:  2026-10-08
#
# Purpose:  Soil water (mm) and mineral N (kg/ha) by depth layer, depth group
#           and profile total. Coordinates and zone are joined at stacking.
#
# Input:    .../2. Soil Data and Nutrition/2026/2026 Walpeup pre-sowing soil results.xlsx
#             (sheet "Pre-sowing Soil Results"; compiled wet/dry weights + APAL N)
#           .../2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx
#             (plot / treatment key only)
#
# Output:   Walpeup_2026_depth_layers_N_and_H2O.csv
#           Walpeup_2026_depth_groups_N_and_H2O.csv
#
# Method:   As 2024/2025: moisture = (wet - dry) / (dry - tare) x 100, >40% = NA;
#           water mm = moisture/100 x BD x cm x 10;
#           mineral N kg/ha = (NO3 + NH4) x BD x cm / 10;
#           depth groups 0-20, 20-60, 60-100; strict totals
#
# Assumes:  Bulk density 1.3 g/cm3. Points matched to plots by Row + Bay.
#           Tare = 0 g (the file gives no bag/chip weights; it calculated
#           moisture as (wet - dry) / dry). Change tare_g if a weight is known.
#           The file replaces "<1.0" N results with 0.9. Any N below 1.0 is
#           set to 0 here (our convention). With the file's 0.9 rule the mean
#           profile N would be about 33 kg/ha instead of about 16.
#
# Gaps:     Sampling date not recorded in any file. Set sampling_date below.
#           The file's own Layer N (kg/ha) column is wrong for two rows (Row 21
#           Bay 2, 0-10 and 20-40 cm); N is recalculated here from mg/kg.
# =============================================================================

# ---- Step 1: Set up --------------------------------------------------------
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

site_dir <- "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole"
res_file <- file.path(site_dir, "2. Soil Data and Nutrition/2026/2026 Walpeup pre-sowing soil results.xlsx")
key_file <- file.path(site_dir, "2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx")
out_dir  <- file.path(site_dir, "2. Soil Data and Nutrition/2026")

bulk_density  <- 1.3                 # g/cm3, assumed (not measured)
tare_g        <- 0                   # bag/chip weight (g); the file gives none
sample_year   <- 2026
sampling_date <- as.Date(NA)         # TODO: confirm the real sampling date

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)

# ---- Step 2: Plot / treatment key (from the 2024 workbook) -----------------
key_sheets <- excel_sheets(key_file)
sheet_key  <- key_sheets[str_detect(key_sheets, "ShortID")]
stopifnot(length(sheet_key) == 1)

plot_key <- read_excel(key_file, sheet = sheet_key) %>%
  select(plot = Plot, row, bay, treatment_label = label,
         treatment_desc = TreatmentDescription)

# ---- Step 3: Read the 2026 compiled results ---------------------------------
res <- read_excel(res_file, sheet = "Pre-sowing Soil Results") %>%
  rename_with(str_trim)

need <- c("Row!", "Bay!", "Ripping Treatment!", "Nutrition!", "Depth!",
          "Wet Weight (g)", "Dry Weight (g)", "Layer Volmetric water (mm)",
          "Nitrate - N (2M KCl) (mg/kg)", "Ammonium - N (2M KCl) (mg/kg)",
          "Layer N (kg/ha)", "Bulk Density", "Layer Thickness")
stopifnot(all(need %in% names(res)))

raw26 <- res %>%
  filter(!is.na(`Row!`)) %>%
  transmute(row = `Row!`, bay = `Bay!`,
            label_ws    = paste0(`Ripping Treatment!`, "_", `Nutrition!`),
            depth_label = str_trim(as.character(`Depth!`)),
            wet = `Wet Weight (g)`, dry = `Dry Weight (g)`,
            no3_raw = `Nitrate - N (2M KCl) (mg/kg)`,
            nh4_raw = `Ammonium - N (2M KCl) (mg/kg)`,
            mm_file = `Layer Volmetric water (mm)`,
            n_file  = `Layer N (kg/ha)`,
            bd_file = `Bulk Density`, thick_file = `Layer Thickness`)

# ---- Step 4: Soil water and mineral N by depth layer ------------------------
# Any N result below 1.0 is a "<1.0" substitution (APAL reports nothing lower)
layers <- raw26 %>%
  left_join(plot_key, by = c("row", "bay")) %>%
  mutate(site = "Walpeup", year = sample_year, sample_date = sampling_date) %>%
  separate(depth_label, into = c("top_cm", "bottom_cm"), sep = "-", convert = TRUE) %>%
  mutate(n_depth      = paste(top_cm, bottom_cm, sep = "-"),
         thick_cm     = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare_g) * 100,
         moisture_pct = if_else(moisture_pct > 40, NA_real_, moisture_pct),  # >40% = error
         bulk_density = bulk_density,
         water_mm     = moisture_pct / 100 * bulk_density * thick_cm * 10,
         no3_mgkg     = if_else(no3_raw < 1, 0, no3_raw),
         nh4_mgkg     = if_else(nh4_raw < 1, 0, nh4_raw),
         mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10) %>%
  arrange(plot, top_cm)

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
cat("\n1. Layers per point (expect 6) and points (expect 48):\n")
print(layers %>% count(row, bay, name = "n_layers") %>% count(n_layers, name = "points"))

cat("\n2. Missing values, counted explicitly (expect all 0 except sample_date = 288):\n")
print(layers %>% summarise(across(c(plot, treatment_label, sample_date, wet, dry,
                                    moisture_pct, water_mm, no3_mgkg, nh4_mgkg,
                                    mineral_n_kgha), ~ sum(is.na(.x)))))

cat("\n3. Duplicate row/bay/depth (expect 0):\n")
print(sum(duplicated(layers[c("row", "bay", "top_cm")])))

cat("\n4. Treatment label: file vs plot key (expect 0 rows):\n")
print(layers %>% filter(is.na(label_ws) | is.na(treatment_label) |
                          label_ws != treatment_label) %>%
        distinct(row, bay, label_ws, treatment_label))

cat("\n5. Ten highest moisture readings (look for typos):\n")
print(layers %>% arrange(desc(moisture_pct)) %>%
        select(plot, treatment_label, top_cm, wet, dry, moisture_pct) %>% head(10))

cat("\n6. Our water mm vs the file's (expect max difference 0 while tare_g = 0):\n")
print(max(abs(layers$water_mm - layers$mm_file), na.rm = TRUE))

cat("\n7. File's Layer N vs its own mg/kg x BD x cm (expect 2 rows: Row 21 Bay 2, 0-10 and 20-40):\n")
print(layers %>%
        mutate(n_file_calc = (no3_raw + nh4_raw) * bd_file * thick_file / 10) %>%
        filter(abs(n_file - n_file_calc) > 0.01) %>%
        select(plot, row, bay, top_cm, no3_raw, nh4_raw, n_file, n_file_calc))

cat("\n8. N results below 1.0: all should be exactly 0.9 (expect nitrate 124, ammonium 276, others 0):\n")
print(layers %>% summarise(no3_09 = sum(no3_raw == 0.9), nh4_09 = sum(nh4_raw == 0.9),
                           no3_other_lt1 = sum(no3_raw < 1 & no3_raw != 0.9),
                           nh4_other_lt1 = sum(nh4_raw < 1 & nh4_raw != 0.9)))

cat("\n9. Profile totals with NA (expect 0 rows):\n")
print(profile %>% filter(is.na(water_mm_0_100) | is.na(mineral_n_kgha_0_100)))

cat("\n10. Profile summary (expect water 49.9 / 79.3 / 114.6 mm, N 0.0 / 15.8 / 55.9 kg/ha):\n")
print(profile %>% summarise(points = n(),
                            water_min = min(water_mm_0_100), water_mean = mean(water_mm_0_100),
                            water_max = max(water_mm_0_100),
                            n_min = min(mineral_n_kgha_0_100), n_mean = mean(mineral_n_kgha_0_100),
                            n_max = max(mineral_n_kgha_0_100)))

# ---- Step 7: Save the two CSVs (run only after the checks are clean) -------
layers_out <- layers %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         top_cm, bottom_cm, thick_cm, bulk_density, moisture_pct, water_mm,
         n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)

groups_out <- depth_groups %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         depth_group, n_layers, water_mm, mineral_n_kgha)

readr::write_csv(layers_out, file.path(out_dir, "Walpeup_2026_depth_layers_N_and_H2O.csv"))
readr::write_csv(groups_out, file.path(out_dir, "Walpeup_2026_depth_groups_N_and_H2O.csv"))
c(layers = nrow(layers_out), groups = nrow(groups_out))   # expect 288 and 144