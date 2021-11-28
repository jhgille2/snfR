#' A function to read in and tidy data from lead sheets generated with the yield test excel macro.
#'
#' @title Tidy lead sheet file content into a list of tibbles.
#' @param files A vector of file paths to lead sheet excel files.
#'
#' @return
#' A named list of tibbles, one element for each leadsheet. The list names are taken from the file names of the leadsheets.
#' Each list element is a named list of lists. The following data is collected from each leadsheet:
#' \itemize{
#' \item \strong{Plot techniques:} Field techniques used for planting the test.
#' \item \strong{Data to collect:} What phenotypes to measure and how many reps for each phenotype.
#' \item \strong{Genotypes:} Code numbers and entry names for the genotypes included in the test along with other relevant identifying information for each genotype.
#' }
#' In the final output these tables can be found for each test in the \strong{Individual tests} element. The \strong{Merged tables} merges each of these three
#' data sources across all the tests to make three combined tables.
#'
clean_lead_sheets <- function(files = NULL) {

  clean_one_sheet <- function(file)
  {
    LeadSheet_df <- readxl::read_excel(path = file,
                               col_names = FALSE)

    # Seperate out the different info sources in the file

    # Test short name
    test_shortname <- LeadSheet_df[1, 1]

    # Test long name
    test_longname <- LeadSheet_df[2, 1]

    # The year
    test_year <- LeadSheet_df[1, 9]

    # Maturity group
    test_mg <- LeadSheet_df[2, 9]

    # Test purpose
    test_purpose <- LeadSheet_df[5, 1]

    # Test locations
    test_locations <- LeadSheet_df[6, ]

    # Plot techniques
    plot_techniques_startRow  <- purrr::map(LeadSheet_df[, 1], function(x) stringr::str_detect(x, "LOCS:")) %>% unlist() %>% which()

    # Find start and end rows of the genotype table
    geno_startRow  <- purrr::map(LeadSheet_df[, 1], function(x) stringr::str_detect(x, "INITIALS")) %>% unlist() %>% which()
    geno_endRow    <- purrr::map(LeadSheet_df[, 1], function(x) stringr::str_detect(x, "Data to be Collected:")) %>% unlist() %>% which()
    test_genotypes <- LeadSheet_df[(geno_startRow + 1):(geno_endRow - 1), 1:8]

    # The plot techniques data
    test_plot_techniques <- LeadSheet_df[plot_techniques_startRow:(geno_startRow-2), 1:8]

    # The data to be collected
    data_collect_startRow <- geno_endRow + 3
    data_collect_endRow   <- data_collect_startRow + 7
    test_data_collect     <- LeadSheet_df[data_collect_startRow:data_collect_endRow, 2:5]

    # Functions to tidy each component
    # Clean the plot techniques
    clean_plot_techniques <- function(df){

      # Special handling for the location row to allow for multiple locations
      loc_row <- df[1, ] %>% unlist()
      df      <- df[-1, ]

      locs <- loc_row[!is.na(loc_row)]
      locs <- locs[2:length(locs)]

      # A tibble with just the locations
      loc_df <- tibble(component = "Loc", value = locs)

      # Split the data into 2 column chunks
      df_list <- list(df[, 1:2], df[, 3:4], df[, 5:6], df[, 7:8])

      # A function to clean up the design component strings
      clean_components <- function(component){
        component %>%
          stringr::str_remove_all("#") %>%
          stringr::str_remove_all(":") %>%
          tolower() %>%
          stringr::str_replace("seed/plot", "seeds per plot") %>%
          stringr::str_squish()
      }

      df_list <- df_list %>%
        purrr::map(., function(x) x %>% dplyr::mutate(across(everything(), as.character)))

      CleanData <- df_list %>%
        purrr::reduce(bind_rows) %>%
        purrr::set_names(c("component", "value")) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(component = purrr::map_chr(component, clean_components)) %>%
        dplyr::bind_rows(loc_df)

      return(CleanData)
    }

    # Cleaning the data to collect
    clean_data_collect <- function(df, cleaned_plot_techniques_df){

      # A function to clean up traits
      clean_trait <- function(trait){
        trait %>%
          tolower() %>%
          stringr::str_replace("100-seed wt.", "sdwt") %>%
          stringr::str_squish()
      }

      # How many reps are in the test
      RepCount <- cleaned_plot_techniques_df %>%
        dplyr::filter(component == "reps") %>%
        purrr::pluck("value")

      # A function to clean up the value column
      clean_value <- function(value, RepCount){
        value %>%
          tolower() %>%
          stringr::str_replace("all", RepCount) %>%
          stringr::str_replace("no", "0") %>%
          stringr::str_replace("yes", RepCount) %>%
          stringr::str_remove(" reps") %>%
          as.numeric()
      }

      CleanData <- dplyr::bind_rows(df[, 1:2], df[, 3:4]) %>%
        purrr::set_names(c("trait", "reps_to_measure")) %>%
        dplyr::mutate(trait = purrr::map_chr(trait, clean_trait),
                      reps_to_measure = purrr::map2_dbl(reps_to_measure, RepCount, clean_value))

      return(CleanData)
    }

    # cleaning the genotypes data frame
    colname_lookup <- c("sdwt" = "x100sdwt")

    clean_genotype_table <- function(df){
      CleanData <- df %>%
        janitor::row_to_names(1) %>%
        janitor::clean_names() %>%
        dplyr::rename(any_of(colname_lookup)) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        dplyr::filter(!is.na(code))

      return(CleanData)
    }

    # Apply these cleaning functions to the plot techniques, data to collect, and
    # the genotype table that were set aside earlier
    test_plot_techniques_table <- clean_plot_techniques(test_plot_techniques)
    test_data_collect_table    <- clean_data_collect(test_data_collect, test_plot_techniques_table)
    test_genotype_table        <- clean_genotype_table(test_genotypes)

    res <- list("Plot techniques" = test_plot_techniques_table,
                'Data to collect' = test_data_collect_table,
                "Genotypes"       = test_genotype_table)

    return(res)

  }

  # Get the names of the files without file extensions
  leadsheet_names <- tools::file_path_sans_ext(basename(files)) %>%
    stringr::str_remove(stringr::regex('lead sheet', ignore_case = T)) %>%
    stringr::str_squish()

  # A function to add the test name to each dataframe
  add_test_name <- function(testdata, testname)
  {
    purrr::map(testdata, function(x) x %>% dplyr::mutate(test_name = testname))
  }

  # Apply this cleaning function to all the files in the "files" argument of the main function
  All_clean_files <- purrr::map(files, clean_one_sheet) %>%
    purrr::set_names(leadsheet_names) %>%
    purrr::map2(., names(.), add_test_name)

  # A function to combine the three tables from each leadsheet into three compete tables to hold the data for the
  # complete set of tests
  pluck_and_merge <- function(testdata, element)
  {
    purrr::map(testdata, function(x) purrr::pluck(x, element)) %>%
      purrr::reduce(dplyr::bind_rows)
  }

  # Merge the tables from each individual test
  all_plot_techniques <- pluck_and_merge(All_clean_files, "Plot techniques")
  all_data_to_collect <- pluck_and_merge(All_clean_files, "Data to collect")
  all_genotype_tables <- pluck_and_merge(All_clean_files, "Genotypes")

  # Make a list to hold the three big tables and then return everything in a final named list
  overall_tables <- list("Plot techniques" = all_plot_techniques,
                         "Data to collect" = all_data_to_collect,
                         "Genotypes"       = all_genotype_tables)

  res_final <- list("Merged tables"    = overall_tables,
                    "Individual tests" = All_clean_files)

  return(res_final)
}


