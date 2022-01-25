#' This function takes takes the individual yield trial workbooks for a season and
#' standardizes their formats and merges them to a single dataframe. Often there are minor issues
#' that prevent the workbooks from being merged directly. This most involves discrepancies between the
#' column names and column types used for the variables that are otherwise the same across the worksheets.
#' For example, one worksheet might have the maturity date encoded as a character in a column named "MD"
#' while another has it encoded as a numeric in a column named "MAT". In this example, the columns would both be renamed
#' to a numeric named "md". This function attempts to solve other similar conflicts and then row binds all the dataframes.
#'
#' @title Standardize yield file formats and merge to a single dataframe
#' @param files A vector of filepaths to the yield files to be standardized/merged
clean_yield_files <- function(files = NULL) {

  # A function to rename columns
  # The yield_lookup table is an internal lookup table.
  rename_yield_dfs <- function(yieldFile, name_lookups = yield_lookup)
  {

    # Clean the column names with the clean_names function with janitor to catch common issues
    names_cleaned <- janitor::clean_names(yieldFile)

    # Use a lookup table to rename columns which often have different names for the same trait
    # so that they all have the same name for the same trait
    rename_matches <- name_lookups$newname[match(names(names_cleaned), name_lookups$oldname)]
    rename_inds    <- which(!is.na(rename_matches))
    rename_values  <- rename_matches[rename_inds]

    names(names_cleaned)[rename_inds] <- rename_values

    return(names_cleaned)
  }

  # A function to set the column types, here just for the character vs numeric columns in the vectors below
  set_yield_coltypes <- function(yieldfile)
  {

    # Column names for numeric, and character columns. Used for converting
    # column types prior to binding dataframes
    char_cols <- c("fc",
                   "genotype",
                   "note1",
                   "note2",
                   "notes",
                   'pub',
                   "test",
                   "id",
                   "loc")

    num_cols <- c("code",
                  "ht",
                  "lod",
                  "md",
                  "plot",
                  "rep",
                  "sdwt",
                  "year",
                  "yield",
                  "sq",
                  "protein_dry_basis",
                  "oil_dry_basis",
                  "p_o",
                  "pro_13_percent_m",
                  "oil_13_percent_m")

    yieldfile %>%
      dplyr::mutate(across(any_of(char_cols), as.character)) %>%
      dplyr::mutate(across(any_of(num_cols), as.numeric))
  }

  # Read in each file, rename the columns, set column types, and merge all the files together
  All_Yield_Dfs <- purrr::map(files, function(x) readxl::read_excel(x) %>%
                                rename_yield_dfs(., yield_lookup) %>%
                                set_yield_coltypes()) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(-tidyr::num_range("x", 1:999))

  return(All_Yield_Dfs)
}
