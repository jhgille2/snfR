#' This function takes all the nir file exports for a given season and extracts/cleans up the measurement data from the exports. In their
#' original state, the exports have some header information before the measurements themselves. This function keeps just the measurement data
#' and cleans it up to a tidy rectangular dataframe.
#'
#' @title A function to clean NIR export files
#' @param files A vector of file paths to nir csv export files.
#' @param nir_masterfile The file path the nir masterfile
#' @param select_FA Logical, should fatty acid columns be included in the data
clean_nir_files <- function(files = NULL, nir_masterfile = NULL, select_FA = FALSE) {

  # Read in the nir masterfile
  masterfile <- nir_masterfile

  # A function to read in and clean each nir file from the files argument
  clean_nir_export <- function(file, nir_lookup = masterfile)
  {
    nir_df <- readxl::read_excel(file) %>%
      janitor::clean_names() %>%
      dplyr::mutate(nir_number_extracted = toupper(stringr::str_extract(sample_id, stringr::regex("nir-[0-9]+", ignore_case = TRUE))))

    if(select_FA){
      nir_df %<>%
        dplyr::select(sample_id,
                      date_time_of_analysis,
                      predicted_moisture_percent,
                      predicted_protein_dry_basis_percent,
                      predicted_oil_dry_basis_percent,
                      predicted_linoleic_acid_dry_basis_percent,
                      predicted_linolenic_acid_dry_basis_percent,
                      predicted_oleic_acid_dry_basis_percent,
                      predicted_palmitic_acid_dry_basis_percent,
                      predicted_stearic_acid_dry_basis_percent,
                      nir_number_extracted) %>%
        dplyr::rename(moisture                 = predicted_moisture_percent,
                      oil_dry_basis            = predicted_oil_dry_basis_percent,
                      protein_dry_basis        = predicted_protein_dry_basis_percent,
                      linoleic_acid_dry_basis  = predicted_linoleic_acid_dry_basis_percent,
                      linolenic_acid_dry_basis = predicted_linolenic_acid_dry_basis_percent,
                      oleic_acid_dry_basis     = predicted_oleic_acid_dry_basis_percent,
                      palmitic_acid_dry_basis  = predicted_palmitic_acid_dry_basis_percent,
                      stearic_acid_dry_basis   = predicted_stearic_acid_dry_basis_percent)

    }else{
      nir_df %<>%
        dplyr::select(sample_id, date_time_of_analysis, predicted_moisture_percent, predicted_protein_dry_basis_percent, predicted_oil_dry_basis_percent, nir_number_extracted) %>%
        dplyr::rename(moisture = predicted_moisture_percent,
                      oil_dry_basis = predicted_oil_dry_basis_percent,
                      protein_dry_basis = predicted_protein_dry_basis_percent)
    }

    nir_df %<>%
      tidyr::separate(sample_id, into = c("year", "loc", "test", "genotype", "code", "plot", "rep", "nir_no"), sep = "_")  %>%
      dplyr::mutate(nir_no = nir_number_extracted) %>%
      dplyr::mutate(year = ifelse(stringr::str_detect(year, stringr::regex("nir", ignore.case = TRUE)), NA, year))

    if(select_FA){
      nir_df %<>%
        select(nir_no,
               date_time_of_analysis,
               moisture,
               oil_dry_basis,
               protein_dry_basis,
               linoleic_acid_dry_basis,
               linolenic_acid_dry_basis,
               oleic_acid_dry_basis,
               palmitic_acid_dry_basis,
               stearic_acid_dry_basis)
    }else{
      nir_df %<>%
        select(nir_no,
               date_time_of_analysis,
               moisture,
               oil_dry_basis,
               protein_dry_basis)
    }

    id_cols <- c("test", "code", "plot", "rep", "genotype", "loc", "year")

    nir_df %<>%
      dplyr::left_join(nir_lookup, by = c("nir_no" = "nir_number")) %>%
      dplyr::relocate(any_of(id_cols), .before = moisture)

    return(nir_df)
  }

  # Apply the cleaning function to each nir export and then bind the results together
  CleanedFiles <- purrr::map(files, clean_nir_export) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::distinct()

  return(CleanedFiles)

}
