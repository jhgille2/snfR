library(here)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)

LeadSheet_df <- readxl::read_excel(path = list.files(here("sandbox", "data", "leadsheets"), full.names = TRUE)[[7]],
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
test_plot_techniques <- LeadSheet_df[7:12, 1:8]

# test genotypes

# Find start and end rows of table
geno_startRow  <- map(LeadSheet_df[, 1], function(x) str_detect(x, "INITIALS")) %>% unlist() %>% which()
geno_endRow    <- map(LeadSheet_df[, 1], function(x) str_detect(x, "Data to be Collected:")) %>% unlist() %>% which()
test_genotypes <- LeadSheet_df[(geno_startRow + 1):(geno_endRow - 1), 1:8]

# The data to be collected
data_collect_startRow <- geno_endRow + 3
data_collect_endRow   <- data_collect_startRow + 7
test_data_collect     <- LeadSheet_df[data_collect_startRow:data_collect_endRow, 2:5]

# Functions to tidy each component
# Clean the plot techniques
clean_plot_techniques <- function(df){

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
    map(., function(x) x %>% mutate(across(everything(), as.character)))

  CleanData <- df_list %>%
    purrr::reduce(bind_rows) %>%
    purrr::set_names(c("component", "value")) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(component = map_chr(component, clean_components))

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
      stringr::str_replace("yes", "1") %>%
      stringr::str_remove(" reps") %>%
      as.numeric()
  }

  CleanData <- dplyr::bind_rows(df[, 1:2], df[, 3:4]) %>%
    purrr::set_names(c("trait", "reps_to_measure")) %>%
    dplyr::mutate(trait = map_chr(trait, clean_trait),
                  reps_to_measure = map2_dbl(reps_to_measure, RepCount, clean_value))

  return(CleanData)
}

# cleaning the genotypes data frame
clean_genotype_table <- function(df){
  CleanData <- df %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names() %>%
    dplyr::rename(sdwt = x100sdwt)

  return(CleanData)
}


test_plot_techniques_table <- clean_plot_techniques(test_plot_techniques)
test_data_collect_table    <- clean_data_collect(test_data_collect, test_plot_techniques_table)
test_genotype_table        <- clean_genotype_table(test_genotypes)

