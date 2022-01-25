#' A function to read in and tidy the nir machine csv output file(s)
#'
#' @title Tidy NIR csv exports into a list of tibbles.
#' @param files A vector of file paths to nir csv export files.
#' @return A merged dataframe with all the NIR data from all the files that were used as input.
clean_nir <- function(files = NULL, sepChar = NULL, codeCols = NULL) {

  # Read in every file from the filepaths in the files argument
  AllFiles <- purrr::map(files, readr::read_csv, skip = 3)

  # A function to clean an individual file
  clean_nir_file <- function(nir_df){

    colnames(nir_df)[1:5] <- c("sample_id", "date", "time", "notes", "parameters")

    nir_df <- nir_df[-c(1:2, nrow(nir_df)), ]

    # Numeric column names
    numcols <- c("year",
                 "code",
                 "rep",
                 "moisture",
                 "protein_dry_basis",
                 "oil_dry_basis")

    # Character columns
    charcols <- c("loc",
                  "test",
                  "genotype",
                  "nir_no")

    nir_df_clean <- janitor::clean_names(nir_df) %>%
      dplyr::select(sample_id, date, time, notes, moisture, protein_dry_basis, oil_dry_basis) %>%
      tidyr::separate(sample_id, into = c("year",
                                          "loc",
                                          "test",
                                          "code",
                                          "genotype",
                                          "rep",
                                          "nir_no"),
                      sep = "_",
                      fill = "left") %>%
      dplyr::mutate(date_time = lubridate::mdy_hm(paste(date, time))) %>%
      dplyr::relocate(date_time, .before = "date") %>%
      dplyr::select(-any_of(c("date", "time"))) %>%
      dplyr::mutate(across(any_of(numcols), as.numeric),
                    across(any_of(charcols), as.character),
                    genotype = stringr::str_squish(genotype))

    return(nir_df_clean)
  }

  CleanedFiles <- purrr::map(AllFiles, clean_nir_file) %>%
    purrr::reduce(dplyr::bind_rows)

  return(CleanedFiles)
}
