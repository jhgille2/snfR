#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param leadsheet_data Leadsheet data that has already been processed with the "clean_lead_sheets" function
#' @param nir_startnumber What NIR number to start on
#' @param yield_data Yield file data that has already been processed with the clean_yield_files function
#' @param yield_lookup A lookup table used to map the test names used in the leadsheets to the test names
#' used in the yield files. This should be a dataframe or tibble where the first column holds the test names that
#' are used in the leadsheets and the second column uses the names that are used in the field note files.
make_nir_file <- function(leadsheet_data = NULL, yield_data = NULL, yield_lookup = NULL, nir_startnumber = 1) {

  # Get the number of reps of protein/oil data needed for each test from the lead sheets
  # and then add the yield sheet test names to this table
  prot_oil_reps <- leadsheet_data %>%
    purrr::pluck("Merged tables", "Data to collect") %>%
    dplyr::filter(trait == "protein/oil") %>%
    dplyr::select(reps_to_measure, test_name) %>%
    dplyr::left_join(yield_lookup, by = c("test_name" = names(yield_lookup)[2]))

  # Join the reps to measure numbers to the full yield table and filter so that only the reps that need
  # protein/oil scores are kept
  yield_joined <- yield_data %>%
    dplyr::select(genotype,
           loc,
           test,
           year,
           rep,
           code,
           plot,
           fc,
           md,
           lod,
           ht,
           yield,
           sdwt) %>%
    dplyr::left_join(prot_oil_reps, by = c("test" = "yield_sheet_names")) %>%
    dplyr::filter(rep <= reps_to_measure) %>%
    dplyr::arrange(test, loc, code, rep)

  # What is the last nir number using the number of rows in the data nd the starting nir number
  nir_lastrow <- nir_startnumber + nrow(yield_joined) - 1

  # Add the nir number to the data and rearrange some columns before also adding the barcode column
  nir_final <- yield_joined %>%
    dplyr::mutate(NIR_No = paste0("NIR-", stringr::str_pad(nir_startnumber:nir_lastrow, 5, side = "left", pad = "0"))) %>%
    dplyr::select(NIR_No,
                  plot,
                  year,
                  test,
                  loc,
                  code,
                  genotype,
                  rep) %>%
    dplyr::mutate(nir_code = paste(year, loc, test, code, genotype, rep, NIR_No, sep = "_"))

  return(nir_final)
}
