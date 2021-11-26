#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param leadsheet_data
#' @param nir_startnumber
make_nir_file <- function(leadsheet_data = NULL, nir_startnumber = 1) {

  # Get just the number of reps of nir data to collect for each test
  nir_reps <- leadsheet_data %>%
    purrr::pluck("Merged tables", "Data to collect") %>%
    dplyr::filter(trait == "protein/oil")

  # Get the locations that each test was grown in
  loc_data <- leadsheet_data %>%
    purrr::pluck("Merged tables", "Plot techniques") %>%
    dplyr::filter(component == "Loc") %>%
    dplyr::select(value, test_name) %>%
    dplyr::rename(Loc = value)

  # Select just the columns with the data needed in the nir file
  # and join this table with the table that has how many nir reps to take
  genotype_data <- leadsheet_data %>%
    purrr::pluck("Merged tables", "Genotypes") %>%
    select(code, entry, pedigree, test_name) %>%
    dplyr::left_join(nir_reps, by = "test_name") %>%
    dplyr::left_join(loc_data, by = "test_name")

  # Repeat each row (genotype) in the table by the number of reps which need to be measured
  genotypes_repeated <- genotype_data[rep(seq(nrow(genotype_data)), genotype_data$reps_to_measure),]

  nir_lastrow <- nir_startnumber + nrow(genotypes_repeated) - 1

  # Add a rep and a nir number column to the table
  genotypes_final <- genotypes_repeated %>%
    dplyr::group_by(test_name, code, Loc) %>%
    dplyr::mutate(rep = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(test_name, Loc, code, rep) %>%
    dplyr::mutate(NIR_No = paste0("NIR-", str_pad(nir_startnumber:nir_lastrow, 5, side = "left", pad = "0")))

  return(genotypes_final)
}
