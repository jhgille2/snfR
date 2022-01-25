#' This function takes all the nir file exports for a given season and extracts/cleans up the measurement data from the exports. In their
#' original state, the exports have some header information before the measurements themselves. This function keeps just the measurement data
#' and cleans it up to a tidy rectangular dataframe.
#'
#' @title A function to clean NIR export files
#' @param files A vector of file paths to nir csv export files.
#' @param nir_masterfile The file path the nir masterfile
clean_nir_files <- function(files = NULL, nir_masterfile = NULL) {

  # Read in the nir masterfile
  masterfile <- readr::read_csv(nir_masterfile)

  # A function to read in and clean each nir file from the files argument
  clean_nir_export <- function(file, masterfile = masterfile)
  {
    nir_df <- readxl::read_excel(file) %>%
      janitor::clean_names() %>%
      dplyr::mutate(nir_number_extracted = toupper(str_extract(sample_id, regex("nir-[0-9]+", ignore_case = TRUE)))) %>%
      dplyr::select(sample_id, date_time_of_analysis, predicted_moisture_percent, predicted_protein_dry_basis_percent, predicted_oil_dry_basis_percent, nir_number_extracted) %>%
      tidyr::separate(sample_id, into = c("year", "loc", "test", "code", "genotype", "rep", "nir_no"), sep = "_") %>%
      dplyr::mutate(nir_no = nir_number_extracted) %>%
      dplyr::rename(moisture = predicted_moisture_percent,
                    oil_dry_basis = predicted_oil_dry_basis_percent,
                    protein_dry_basis = predicted_protein_dry_basis_percent) %>%
      dplyr::mutate(year = ifelse(str_detect(year, regex("nir", ignore.case = TRUE)), NA, year)) %>%
      dplyr::select(nir_no, date_time_of_analysis, moisture, oil_dry_basis, protein_dry_basis) %>%
      dplyr::left_join(nir_masterfile, by = c("nir_no" = "NIR_No")) %>%
      dplyr::select(test, cross, Rows, color, plant_no, loc, year, moisture, oil_dry_basis, protein_dry_basis) %>%
      dplyr::rename(code = cross, plot = Rows, rep = color, genotype = plant_no)

    return(nir_df)
  }

  # Apply the cleaning function to each nir export and then bind the results together
  CleanedFiles <- purrr::map(files, clean_nir_export) %>%
    purrr::reduce(dplyr::bind_rows)

  return(CleanedFiles)

}
