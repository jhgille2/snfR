library(readr)
library(tidyr)
library(dplyr)
library(tidyselect)
library(here)
library(purrr)
library(janitor)

csv_file <- here("sandbox", "data", "nir", "example_csv_export.csv")


csv_df <- read_csv(csv_file, col_names = FALSE)

measureCols <- c("Moisture", "Protein Dry basis", "Oil Dry basis")
measureIds  <- map_dbl(measureCols, function(x) which(unlist(csv_df[4, ]) == x))

DataCols <- c(1:3, measureIds)
DataRows <- c(7:(nrow(csv_df) - 1))

nir_data <- csv_df[DataRows, DataCols]

colnames(nir_data) <- c("sample_id",
                        "date",
                        "time",
                        "moisture",
                        "protein_dry_basis",
                        "oil_dry_basis")

sepChar <- "_"
codeCols <- c("year", "loc", "pop", "code", "genotype", "rep", "nir_no")

nir_data %>%
  separate(col = "sample_id", into = codeCols, sep = sepChar) %>%
  mutate(across(all_of(c("moisture", "protein_dry_basis", "oil_dry_basis")), as.numeric),
         date_time = as.POSIXct(paste(date, time), format="%m/%d/%Y %H:%M %p")) %>%
  select(-all_of(c("date", "time")))
