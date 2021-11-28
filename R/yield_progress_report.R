#' This function takes the paths to the leadheet files and the yield files and makes a progress
#' report for each test to say what data has been collected for each test out of what data still
#' has to be collected
#'
#' @title Make a progress report for yield trial data collection
#' @param yieldfiles file paths to all the yield field data files
#' @param leadsheetfiles file paths to all the lead sheet files
#' @param yield_name_lookup A lookup table to map the test names used in the leadsheets to those used in the field notes
#' @param has_nir Logical: has nir data been collected yet
#' @param nirfiles File paths to nir data. Can be left as NULL if nir has not been collected yet
yield_progress_report <- function(yieldfiles = NULL, leadsheetfiles = NULL, yield_name_lookup = NULL, has_nir = FALSE, nirfiles = NULL) {

  # clean the yield files and the leadsheet files
  yield_cleaned      <- clean_yield_files(yieldfiles)
  leadsheets_cleaned <- clean_lead_sheets(leadsheetfiles)

  # Make a lookup table for yield trial names if it does not already exist
  if(is.null(yield_name_lookup))
  {
    yield_name_lookup <- make_yield_lookup_table(leadsheets_cleaned, yield_cleaned)
  }

  # recode the trait column of the leadsheets to match the trait names in the yield files
  leadsheet_data_collect <- leadsheets_cleaned %>%
    purrr::pluck("Merged tables", "Data to collect") %>%
    dplyr::mutate(trait = recode(trait,
                                 "fatty acid"   = "fatty_acid",
                                 "flower color" = "fc",
                                 "flower date"  = "flower_date",
                                 "height"       = "ht",
                                 "lodging"      = "lod",
                                 "maturity"     = "md",
                                 "protein/oil"  = "p_o",
                                 "pubescence"   = "pub",
                                 "seed density" = "seed_density",
                                 "seed quality" = "sq",
                                 "swell ratio"  = "swell_ratio"))

}
