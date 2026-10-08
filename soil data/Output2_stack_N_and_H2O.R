# ==============================================================================
# Script:   Output2_stack_N_and_H2O.R
# Project:  Sandy Soils II, Output 2
# Purpose:  Stack (row-bind) the per-site-year soil mineral N and soil water files
#           into one flat table, then join site-year metadata.
# Author:   Jackie Ouzman, CSIRO Systems Analysis, Waite Campus
# Created:  2026-10-08
#
# NOTE ON "STACK": these are flat tables, not rasters. Stacking here means
#   appending the rows of each site-year file under each other, so one table
#   holds every site and year. Depth layers stay as rows (top_cm, bottom_cm).
#
# INPUTS
#   Per site-year (written by <Site>_<Year>_N_and_H2O.R scripts), in
#   <site folder>/2. Soil Data and Nutrition/<year>/ :
#     <Site>_<Year>_depth_layers_N_and_H2O.csv
#     <Site>_<Year>_depth_groups_N_and_H2O.csv
# OUTPUTS (saved in the same folder as the metadata workbook: Jackie_processing_etc)
#
# OUTPUTS (saved next to the metadata workbook)
#   Output2_depth_layers_N_and_H2O_stacked.csv
#   Output2_depth_groups_N_and_H2O_stacked.csv
#
# CONVENTIONS (set in the per-site-year scripts, carried through unchanged)
#   Mineral N (kg/ha)  = (NO3 + NH4, mg/kg) x bulk density x layer cm / 10
#   Soil water (mm)    = moisture % / 100 x bulk density x layer cm x 10
#   Bulk density       = 1.3 (assumed)
#   Moisture above 40% is set to NA
#   Depth groups       = 0-20, 20-60, 60-100 cm (cut on the top of each layer)
#   Totals are NA if any layer in the group is NA
#   Results below detection ("<") are set to 0 in our scripts. The source
#   reports and the 2026 compiled sheet use 0.9, so their N is higher.
#
# SITE AND COORDINATES
#   One paddock-level coordinate per site (no per-sample GPS), joined from the
#   metadata workbook. coord_type = "paddock". EPSG 4326 is assumed (to confirm).
#
# ADDING A SITE
#   Add it to site_dirs in Step 1. The site name must match the Site column in
#   the metadata workbook and the prefix of the per-site-year file names.
#
# KNOWN GAPS (fix in the metadata workbook, then rerun)
#   Walpeup: no sample_date for 2025 or 2026; 2025 crop and sowing date unknown;
#            no harvest dates.
# Wharminda: 2025 is N only (no water found), sample_date NA (lab received 5 May 2025).
#              2024 baseline has N only; 2026 pre-sow wet/dry treated as net weights (tray = 0, to confirm).
#   Copeville: sampling events differ by year (see sample_event); 2026 has N at all three events.
#   sample_event (baseline, pre-sow, GS31, GS37, GS65, flowering, harvest) is NA for Walpeup.**
#   Open decision: "<" = 0 versus 0.9.
#
# NOTES
#   (add dated notes here)
# ==============================================================================

# ---- Step 1: setup ----
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)

# Site folders (one per site). Each holds "2. Soil Data and Nutrition/<year>" with the CSVs
site_dirs <- c(Walpeup   = "H:/Output-2/Site-Data/1. SSO2_Walpeup-Pole",
               Copeville = "H:/Output-2/Site-Data/2._SSO2_Copeville-Farley",
               Wharminda = "H:/Output-2/Site-Data/3. SSO2_Wharminda-Masters")

# The metadata workbook (Sites and Site_year tabs)
meta_file <- "H:/Output-2/Site-Data/Jackie_processing_etc/Output2_site_metadata.xlsx"

sites_to_stack <- names(site_dirs)  # add a site to site_dirs above as each one is done
years_to_stack <- c(2024, 2025, 2026)

# ---- Step 2: read and bind the layer and group files ----
# Everything is read as text first so a column that is numeric in one year
# and text in another does not stop the bind. Types are fixed afterwards.
read_one <- function(site, year, kind) {
  f <- file.path(site_dirs[[site]], "2. Soil Data and Nutrition", year,
                 paste0(site, "_", year, "_depth_", kind, "_N_and_H2O.csv"))
  if (!file.exists(f)) { message("MISSING: ", f); return(NULL) }
  read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(site = site, year = as.integer(year), .before = 1)
}

grid <- expand.grid(site = sites_to_stack, year = years_to_stack, stringsAsFactors = FALSE)

layers_all <- pmap(grid, ~ read_one(..1, ..2, "layers")) %>% bind_rows() %>% type_convert(guess_integer = FALSE)
groups_all <- pmap(grid, ~ read_one(..1, ..2, "groups")) %>% bind_rows() %>% type_convert(guess_integer = FALSE)

# ---- Step 3: join the site-year lookup ----
sites_meta <- read_excel(meta_file, sheet = "Sites") %>%
  transmute(site = Site,
            latitude = `Paddock latitude`, longitude = `Paddock longitude`,
            epsg = `EPSG (TO CONFIRM)`, coord_type = `Coord type`,
            closest_town = `Closest town (display)`)

site_year_meta <- read_excel(meta_file, sheet = "Site_year") %>%
  transmute(site = Site, year = as.integer(Year), crop = Crop, cultivar = Cultivar,
            presow_date_1 = as.Date(`Pre-sow soil test date 1`),
            sowing_date = as.Date(`Sowing date (start)`),
            harvest_date = as.Date(`Machine harvest date`))

lookup <- site_year_meta %>% left_join(sites_meta, by = "site")

layers_all <- layers_all %>% left_join(lookup, by = c("site", "year"))
groups_all <- groups_all %>% left_join(lookup, by = c("site", "year"))

# ---- Step 4: first checks (paste this output back) ----
cat("\n--- 1. Rows per site-year (layers, groups) ---\n")
print(layers_all %>% count(site, year)); print(groups_all %>% count(site, year))

cat("\n--- 2. Column names (layers) ---\n");  print(names(layers_all))
cat("\n--- 3. Column names (groups) ---\n");  print(names(groups_all))

cat("\n--- 4. Column types (layers) ---\n");  print(sapply(layers_all, function(x) class(x)[1]))

cat("\n--- 5. Any columns that exist in some years but not others ---\n")
print(layers_all %>% group_by(year) %>%
        summarise(across(everything(), ~ sum(!is.na(.x))), .groups = "drop") %>%
        tidyr::pivot_longer(-year) %>% filter(value == 0) %>% count(name))

cat("\n--- 6. Lookup join: any site-years with no metadata row? ---\n")
print(layers_all %>% distinct(site, year, crop, sowing_date, latitude) %>% arrange(year))
# ---- Step 5: derived column, more checks, save ----
layers_all <- layers_all %>% mutate(days_after_sowing = as.integer(sample_date - sowing_date))
groups_all <- groups_all %>% mutate(days_after_sowing = as.integer(sample_date - sowing_date))

cat("\n--- 7. Duplicate keys (should be 0 rows each) ---\n")
print(layers_all %>% count(site, year, sample_date, plot, top_cm) %>% filter(n > 1))
print(groups_all %>% count(site, year, sample_date, plot, depth_group) %>% filter(n > 1))

cat("\n--- 8. Does each plot keep the same treatment across years? (should be 0 rows) ---\n")
print(groups_all %>% distinct(site, year, plot, treatment_label) %>%
        group_by(site, plot) %>% filter(n_distinct(treatment_label) > 1) %>% arrange(plot, year))
cat("\n--- 9. Plots per site-year ---\n")
print(groups_all %>% group_by(site, year) %>% summarise(n_plots = n_distinct(plot), .groups = "drop"))

cat("\n--- 10. Profile totals by site-year, event and depth group (compare with the single-year scripts) ---\n")
print(groups_all %>% group_by(site, year, sample_event, sample_date, depth_group) %>%
        summarise(water_mm_mean = mean(water_mm, na.rm = TRUE),
                  mineral_n_mean = mean(mineral_n_kgha, na.rm = TRUE),
                  n_na_water = sum(is.na(water_mm)), n_na_n = sum(is.na(mineral_n_kgha)),
                  .groups = "drop"), n = Inf)

# Save the stacked files next to the metadata workbook
out_stack <- dirname(meta_file)
write_csv(layers_all, file.path(out_stack, "Output2_depth_layers_N_and_H2O_stacked.csv"))
write_csv(groups_all, file.path(out_stack, "Output2_depth_groups_N_and_H2O_stacked.csv"))
cat("\nSaved stacked files to:", out_stack, "\n")
