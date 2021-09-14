#' A function to read in and tidy the nir machine csv output file(s)
#'
#' @title Tidy NIR csv exports into a list of tibbles.
#' @param files A vector of file paths to nir csv export files.
#' \value{
#' A named list of tibbles.
#' }
clean_nir <- function(files = NULL, sepChar = NULL, codeCols = NULL) {

  # Read in every file from the filepaths in the files argument
  AllFiles <- purrr::map(files, readr::read_csv, col_names = FALSE)

  # A function to clean an individual file
  clean_nir_file <- function(nir_export){

    # The columns that hold the moisture, protein and oil data
    measureCols <- c("Moisture", "Protein Dry basis", "Oil Dry basis")

    # The column numbers where this data is found in the file
    measureIds  <- map_dbl(measureCols, function(x) which(unlist(nir_export[4, ]) == x))

    DataCols <- c(1:3, measureIds)
    DataRows <- c(7:(nrow(nir_export) - 1))

    nir_data <- nir_export[DataRows, DataCols]

    colnames(nir_data) <- c("sample_id",
                            "date",
                            "time",
                            "moisture",
                            "protein_dry_basis",
                            "oil_dry_basis")

    nir_cleaned <- nir_data %>%
      separate(col = "sample_id", into = codeCols, sep = sepChar) %>%
      mutate(across(all_of(c("moisture", "protein_dry_basis", "oil_dry_basis")), as.numeric),
             date_time = as.POSIXct(paste(date, time), format="%m/%d/%Y %H:%M %p")) %>%
      select(-all_of(c("date", "time")))

    return(nir_cleaned)
  }

  CleanedFiles <- purrr::map(AllFiles, clean_nir_file)

  return(CleanedFiles)
}
