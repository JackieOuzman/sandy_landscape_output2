# =============================================================================
# Script:   Walpeup_2024_soil_water.R
# Project:  Sandy Soils II - Output 2 (starting soil N and soil water)
# Site:     SSO2_Walpeup-Pole (Walpeup)
# Year:     2024 (sampling dates: 22 Apr, 14 Aug, 23 Sep)
# Author:   Jackie Ouzman, CSIRO Systems Analysis
# Created:  2026-10-08
#
# Purpose:  Calculate soil water (mm) by depth layer, depth group and profile
#           total for each sampled plot. Soil N, coordinates and zone are
#           added in later steps.
#
# Input:    H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole/
#             2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx
#             - sheets "Soil Water 22.04.24", "Soil Water 14.08.24",
#               "Soil Water 23.09.24" (raw weights)
#             - sheet "Walpeup_output1_ShortID_treatme" (plot / treatment key)
#
# Output:   Not yet written. CSVs are saved once soil N is added:
#             Walpeup_2024_depth_layers_N_and_H2O.csv
#             Walpeup_2024_depth_groups_N_and_H2O.csv
#
# Method:   Moisture (%) = (wet - dry) / (dry - tare) x 100
#             (tare = chip weight in April, bag weight in Aug/Sep)
#           Moisture above 40% is treated as an error and set to NA
#           Soil water (mm) = moisture / 100 x bulk density x layer cm x 10
#           Depth groups 0-20, 20-60, 60-100 cm, cut on the top of each layer
#           Totals are strict: any NA layer makes the group/profile total NA
#
# Assumes:  Bulk density 1.3 g/cm3 (not measured)
#           Dates are taken from the sheet names. The All_long_tidy tab is NOT
#           used because its September dates are wrong (fill-down error).
#           Aug/Sep plots are identified from run + bay via the plot key
#
# Override: Sep 2024, plot C108 (Run 28 Bay 3, RT1_T6) 0-10 cm = 33.1% moisture,
#           set to NA (suspected typo in dry weight). The 23-Sep 0-20 cm group and
#           profile total for C108 are therefore NA.
# =============================================================================



# ---- Step 1: Set up --------------------------------------------------------
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

water_file <- "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole/2. Soil Data and Nutrition/2024/Walpeup Soil Water 2024.xlsx"
bulk_density <- 1.3   # g/cm3, assumed (not measured)

# Find sheets by pattern (the April sheet name has a trailing space)
sheets <- excel_sheets(water_file)
sheet_apr <- sheets[str_detect(sheets, "22\\.04\\.24")]
sheet_aug <- sheets[str_detect(sheets, "14\\.08\\.24")]
sheet_sep <- sheets[str_detect(sheets, "23\\.09\\.24")]
sheet_key <- sheets[str_detect(sheets, "ShortID")]
stopifnot(length(sheet_apr) == 1, length(sheet_aug) == 1,
          length(sheet_sep) == 1, length(sheet_key) == 1)

# ---- Step 2: Plot / treatment key -----------------------------------------
plot_key <- read_excel(water_file, sheet = sheet_key) %>%
  select(plot = Plot, run = row, bay, treatment_label = label,
         treatment_desc = TreatmentDescription)

# ---- Step 3: Read each sampling date into one common shape ----------------
# April: identified by plot, tare = chip weight (this reproduces the sheet's %MOIS)
apr_raw <- read_excel(water_file, sheet = sheet_apr)

# Print any weight cell that is not a plain number (blanks are not flagged)
apr_raw %>%
  select(Plot, Depth, `Chip weight (g)`, `Wet weight (g)`, `Dry weight (g)`) %>%
  filter(if_any(c(`Chip weight (g)`, `Wet weight (g)`, `Dry weight (g)`),
                ~ is.na(suppressWarnings(as.numeric(.x))) & !is.na(.x))) %>%
  print()

apr <- apr_raw %>%
  transmute(sample_date = as.Date("2024-04-22"),
            plot = Plot,
            depth_label = str_trim(Depth),
            wet  = as.numeric(`Wet weight (g)`),
            dry  = as.numeric(`Dry weight (g)`),
            tare = as.numeric(`Chip weight (g)`))

# August / September: identified by run + bay, tare = bag weight.
# Date comes from the sheet name, NOT from the All_long_tidy tab.
read_run_sheet <- function(sheet, date) {
  read_excel(water_file, sheet = sheet) %>%
    transmute(sample_date = as.Date(date),
              run = Run, bay = Bay,
              depth_label = str_remove(str_trim(Depth), "cm"),
              wet = `Wet (g)`, dry = `Dry (g)`, tare = Bag) %>%
    left_join(plot_key, by = c("run", "bay"))
}
aug <- read_run_sheet(sheet_aug, "2024-08-14")
sep <- read_run_sheet(sheet_sep, "2024-09-23")

# Bring April onto the same key so treatment is attached the same way
apr <- apr %>% left_join(plot_key, by = "plot")

# ---- Step 4: Soil water by depth layer ------------------------------------
layers <- bind_rows(apr, aug, sep) %>%
  mutate(site = "Walpeup") %>%
  separate(depth_label, into = c("top_cm", "bottom_cm"), sep = "-", convert = TRUE) %>%
  mutate(thick_cm     = bottom_cm - top_cm,
         moisture_pct = (wet - dry) / (dry - tare) * 100,
         moisture_pct = if_else(moisture_pct > 40, NA_real_, moisture_pct),  # >40% = error
         # Manual override: suspected mistyped dry weight (33.1% vs ~1-2% typical)
         moisture_pct = if_else(sample_date == as.Date("2024-09-23") & plot == "C108" & top_cm == 0,
                                NA_real_, moisture_pct),
         bulk_density = bulk_density,
         water_mm     = moisture_pct / 100 * bulk_density * thick_cm * 10) %>%
  arrange(sample_date, plot, top_cm)

# ---- Step 4b: Soil mineral N (APAL baseline, 22 Apr 2024 only) -------------
n_file <- "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole/APAL soils/2024/Baseline results.xlsx"

strict_sum <- function(x) if (anyNA(x)) NA_real_ else sum(x)

# "<1.0" style results are set to 0 (our convention)
less_than_to_zero <- function(x) {
  x <- as.character(x)
  if_else(str_detect(x, "^\\s*<"), 0, suppressWarnings(as.numeric(x)))
}

# Find the real header row by searching for a known column name
n_top <- read_excel(n_file, sheet = 1, col_names = FALSE, n_max = 30)
n_header_row <- which(apply(n_top, 1, function(r) any(r == "SampleName", na.rm = TRUE)))[1]
stopifnot(!is.na(n_header_row))

n_lab <- read_excel(n_file, sheet = 1, skip = n_header_row - 1,
                    .name_repair = "unique_quiet") %>%
  filter(!is.na(SampleName)) %>%            # drops the units row under the header
  transmute(sample_date = as.Date(SamplingDate),
            plot        = str_match(SampleName, "^FMP_(C\\d+)_")[, 2],
            n_depth     = str_trim(as.character(SampleDepth)),
            no3_mgkg    = less_than_to_zero(`Nitrate - N (2M KCl)`),
            nh4_mgkg    = less_than_to_zero(`Ammonium - N (2M KCl)`))

# Attach N to the water layers. The 60-100 N result applies to both the
# 60-80 and 80-100 water layers, each using its own thickness.
# Mineral N (kg/ha) = (nitrate-N + ammonium-N) x bulk density x layer cm / 10
layers <- layers %>%
  mutate(n_depth = case_when(top_cm < 10 ~ "0-10",
                             top_cm < 20 ~ "10-20",
                             top_cm < 40 ~ "20-40",
                             top_cm < 60 ~ "40-60",
                             TRUE        ~ "60-100")) %>%
  left_join(n_lab, by = c("sample_date", "plot", "n_depth")) %>%
  mutate(mineral_n_kgha = (no3_mgkg + nh4_mgkg) * bulk_density * thick_cm / 10)



# ---- Step 5: Depth groups (cut on top of layer), strict totals ------------
depth_groups <- layers %>%
  mutate(depth_group = case_when(top_cm < 20 ~ "0-20",
                                 top_cm < 60 ~ "20-60",
                                 TRUE        ~ "60-100")) %>%
  group_by(site, sample_date, plot, treatment_label, treatment_desc, depth_group) %>%
  summarise(n_layers = n(),
            water_mm       = strict_sum(water_mm),
            mineral_n_kgha = strict_sum(mineral_n_kgha),
            .groups = "drop")

# Profile total, one row per plot per date
profile <- layers %>%
  group_by(site, sample_date, plot, treatment_label, treatment_desc) %>%
  summarise(n_layers = n(),
            water_mm_0_100       = strict_sum(water_mm),
            mineral_n_kgha_0_100 = strict_sum(mineral_n_kgha),
            .groups = "drop")

# ---- Step 6: Checks (paste the output back) -------------------------------
cat("\n1. Layers per plot per date (expect 6):\n")
print(layers %>% count(sample_date, plot) %>% count(sample_date, n))

cat("\n2. Plots per date (expect 8, 24, 24):\n")
print(layers %>% distinct(sample_date, plot) %>% count(sample_date))

cat("\n3. Missing values (counted explicitly):\n")
print(layers %>% summarise(across(c(plot, treatment_label, wet, dry, tare,
                                    moisture_pct, water_mm), ~ sum(is.na(.x)))))

cat("\n4. Duplicate date/plot/depth (expect 0):\n")
print(sum(duplicated(layers[c("sample_date", "plot", "top_cm")])))

cat("\n5. Ten highest moisture readings (look for typos):\n")
print(layers %>% arrange(desc(moisture_pct)) %>%
        select(sample_date, plot, treatment_label, top_cm, wet, dry, moisture_pct) %>%
        head(10))

cat("\n6. Profile totals with NA (expect 0 rows):\n")
print(profile %>% filter(is.na(water_mm_0_100)))

cat("\n7. Profile water (mm) by date:\n")
print(profile %>% group_by(sample_date) %>%
        summarise(n = n(), min = min(water_mm_0_100, na.rm = TRUE),
                  mean = mean(water_mm_0_100, na.rm = TRUE),
                  max = max(water_mm_0_100, na.rm = TRUE)))

# ---- Step 7: Checks for N (paste the output back) -------------------------
apr_date  <- as.Date("2024-04-22")
apr_plots <- unique(layers$plot[layers$sample_date == apr_date])

cat("\n8. Lab N file: rows, dates, plots (expect 40 rows, 1 date = 2024-04-22, 8 plots):\n")
print(n_lab %>% summarise(rows = n(), n_dates = n_distinct(sample_date),
                          first_date = min(sample_date), n_plots = n_distinct(plot)))
print(n_lab %>% count(plot, name = "layers") %>% count(layers, name = "plots"))

cat("\n9. Lab N duplicates (expect 0) and missing values (expect all 0):\n")
print(sum(duplicated(n_lab[c("sample_date", "plot", "n_depth")])))
print(n_lab %>% summarise(across(everything(), ~ sum(is.na(.x)))))

cat("\n10. Plot mismatches, April water vs lab N (expect both character(0)):\n")
print(setdiff(apr_plots, n_lab$plot))
print(setdiff(n_lab$plot, apr_plots))

cat("\n11. Rows after N join (expect 336) and missing mineral N by date (April 0, Aug 144, Sep 144):\n")
print(nrow(layers))
print(layers %>% group_by(sample_date) %>%
        summarise(rows = n(), n_missing = sum(is.na(mineral_n_kgha))))

cat("\n12. '<' results set to 0 (expect nitrate 2, ammonium 31):\n")
print(n_lab %>% summarise(no3_zero = sum(no3_mgkg == 0, na.rm = TRUE),
                          nh4_zero = sum(nh4_mgkg == 0, na.rm = TRUE)))

cat("\n13. April profile totals (kg N/ha and mm water, 0-100 cm):\n")
print(profile %>% filter(sample_date == apr_date) %>%
        select(plot, treatment_label, mineral_n_kgha_0_100, water_mm_0_100))

# ---- Step 8: Save the two CSVs (run only after the checks are clean) ------
out_dir <- "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole/2. Soil Data and Nutrition/2024"  # <- change if needed

layers_out <- layers %>%
  mutate(year = as.integer(format(sample_date, "%Y"))) %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         top_cm, bottom_cm, thick_cm, bulk_density, moisture_pct, water_mm,
         n_depth, no3_mgkg, nh4_mgkg, mineral_n_kgha)

groups_out <- depth_groups %>%
  mutate(year = as.integer(format(sample_date, "%Y"))) %>%
  select(site, year, sample_date, plot, treatment_label, treatment_desc,
         depth_group, n_layers, water_mm, mineral_n_kgha)

readr::write_csv(layers_out, file.path(out_dir, "Walpeup_2024_depth_layers_N_and_H2O.csv"))
readr::write_csv(groups_out, file.path(out_dir, "Walpeup_2024_depth_groups_N_and_H2O.csv"))

