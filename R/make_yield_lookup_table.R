#' This function takes the output from the leadsheet and yield file cleaning functions and lets the
#' user create a lookup table to map the test names used in the leadsheets to those used in the
#' yield files. Oftentimes we use longer names in the field note files for
#' each test than in the lead sheets, for example a test may be listed as LU-5E in the leadsheets
#' but as LU-5 Early in the yield files. However, to join the data from the leadsheets to the field data, there has
#' to be a way to map the names to each other. This function takes the cleaned leadsheet and yield file data
#' and then gives the user an interactive worksheet with the lead sheet test names filled and allows
#' the user to enter in their equivalent in the field note files.
#'
#' To use the interactive editor, first select the appropriate test name for the yield files
#' from the dropdown options in the yield file names column, then click on the "sync" button
#' with the two circular arrows near the top of the editor, then click the done button.
#'
#' @title Make a lookup table to join lead sheet test names with field data test names
#' @param cleaned_leadsheets Lead sheet data as output from the clean_lead_sheets function
#' @param cleaned_yield_files Yield file data as output from the clean_yield_files function
make_yield_lookup_table <- function(cleaned_leadsheets = NULL,
                                    cleaned_yield_files = NULL) {

  # The unique names for the yield tests from the cleaned yield field note files
  yieldnames <- cleaned_yield_files$test %>%
    unique() %>%
    sort()

  # The unique test names from the cleaned lead sheet files
  lsheetnames <- cleaned_leadsheets %>%
    purrr::pluck("Merged tables", "Data to collect") %>%
    dplyr::filter(trait == "protein/oil") %>%
    purrr::pluck("test_name")

  lookup_table <- data.frame(lead_sheet_test_names = lsheetnames)

  # Use an interactive editor to say which names from the yield file dataframe match with
  # the names from the leadsheets
  namelookup <- DataEditR::data_edit(lookup_table,
                                     col_bind = "yield_sheet_names",
                                     col_options = list("yield_sheet_names" = yieldnames),
                                     col_edit = TRUE)

  return(namelookup)
}
