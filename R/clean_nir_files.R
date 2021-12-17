#' This function takes all the nir file exports for a given season and extracts/cleans up the measurement data from the exports. In their
#' original state, the exports have some header information before the measurements themselves. This function keeps just the measurement data
#' and cleans it up to a tidy rectangular dataframe.
#'
#' @title A function to clean NIR export files
#' @param code_sep_character The character used to separate different data in the id/code that is scanned from the barcode. A barcode like "Ply_N19-0001_2021" would have "_" for this argument
#' @param code_column_names The different column names for tha data that make up the id/code. A code like "Ply_N19-0001_2021" would have 'c("loc", "genotype", "year")' for this argument
#' @param files File paths to the nir export files to be cleaned/merged
clean_nir_files <- function(code_sep_character = NULL, code_column_names =
                            NULL, files = NULL) {

  # A function to clean a single export df
  clean_export_df <- function(nir_file)
  {
    # Read in the file
    file_df <- read.csv(nir_file, header = FALSE)

    # Find the row in the file where data measurements start,
    # and the last row where the measurements end
    df_first_col <- file_df[, 1] %>%
      unlist() %>%
      as.character()

    measurements_first_row <- which(df_first_col == "Sample ID")
    measurements_last_row  <- nrow(file_df) - 1

    # Isolate the measurement data from the rest of the worksheet
    # Also, only take the sumset of the total columns that have ID information and the
    # protein and oil measurements
    measurement_data <- file_df[measurements_first_row:measurements_last_row, c(1:4, 6:8)]

    # Set column names for the data and remove the first row that previously
    # held (some) of the column names
    colnames(measurement_data) <- c("sample_id",
                                    "date",
                                    "time",
                                    "notes",
                                    "moisture",
                                    "protein_dry_basis",
                                    "oil_dry_basis")

    measurement_data <- measurement_data[-1, ]


    clean_measurement_data <- measurement_data %>%
      dplyr::mutate(date_time = as.POSIXct(paste(date, time), format = "%m/%d/%Y %I:%M %p")) %>%
      dplyr::select(sample_id,
                    date_time,
                    notes,
                    moisture,
                    protein_dry_basis,
                    oil_dry_basis) %>%
      tidyr::separate(sample_id, into = code_column_names, sep = code_sep_character)

    return(clean_measurement_data)
  }

  # Apply this function to each of the nir files in the "files" argument
  all_nir_data <- purrr::map(files, clean_export_df) %>%
    purrr::reduce(dplyr::bind_rows)

  return(all_nir_data)

}
