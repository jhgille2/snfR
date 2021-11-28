
# The file paths to all the yield field notes
All_Yield_files     <- list.files(here("sandbox", "data", "yieldfiles_2021"), full.names = TRUE)

# The file paths to all the leadsheet files
All_Leadsheet_files <- list.files(here("sandbox", "data", "leadsheets"), full.names = TRUE)

# Clean the yield files and the leadsheets
Yield_Cleaned      <- clean_yield_files(All_Yield_files)
leadsheets_cleaned <- clean_lead_sheets(All_Leadsheet_files)

# Make a lookup table to map leadsheet test names to yield file test names
yield_lookup <- make_yield_lookup_table(leadsheets_cleaned, Yield_Cleaned)

# Make the yield nir file using these three data sources
make_nir_file(leadsheets_cleaned, Yield_Cleaned, yield_lookup)

