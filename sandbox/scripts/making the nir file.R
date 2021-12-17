
library(here)
devtools::load_all()

# The file paths to all the yield field notes
All_Yield_files     <- list.files(here("sandbox", "data", "yieldfiles_2021"), full.names = TRUE)

# The file paths to all the leadsheet files
All_Leadsheet_files <- list.files(here("sandbox", "data", "leadsheets"), full.names = TRUE)

# Clean the yield files and the leadsheets
Yield_Cleaned      <- clean_yield_files(All_Yield_files)
leadsheets_cleaned <- clean_lead_sheets(All_Leadsheet_files)



# Make a lookup table to map leadsheet test names to yield file test names
yield_lookup <- make_yield_lookup_table(leadsheets_cleaned, Yield_Cleaned)

exclude_tests <- c("HIF 5", "LU 5 Early-1", "LU 5 Early-2")

Yield_Cleaned <- Yield_Cleaned %>%
  filter(!(test %in% exclude_tests & loc == "CAS")) %>%
  mutate(loc = toupper(loc))

# Make the yield nir file using these three data sources
nir_2021_yield <- make_nir_file(leadsheets_cleaned, Yield_Cleaned, yield_lookup, nir_startnumber = 6871) %>%
  rename(Rows = plot,
         cross = code,
         color = rep,
         plant_no = genotype,
         bar_code = nir_code) %>%
  arrange(test, loc, desc(cross), desc(color))

write_csv(nir_2021_yield, here("sandbox", "data", "yield_nir_file.csv"))


yield_progress_report(yieldfiles = NULL, leadsheetfiles = NULL)


Yield_Cleaned %>%
  group_by(test, loc) %>%
  summarise(yield_all_na = all(is.na(yield)),
            sdwt_all_na = all(is.na(sdwt))) %>%
  View()

