#' This function creates a set of nested folders to hold the data for a breeding season. The structure is set up to match
#' what has been used in the past and to work with the output folders that are requested by the yield file macro. The main folders
#' created store data for the planting, crossing, harvesting/processing, and NIR data collection stages of the breeding program.
#'
#' @title Create a new directory to hold all the files for a new breeding season
#' @param startdir A file path to the directory where the new directory will be created in
#' @param season_name The name of the season that will be used to name the uppermost directory folder
#' @examples
#' \code{create_season_directory(startdir = "C:/breeding program directory", season_name = "2021")}
create_season_directory <- function(startdir = NULL, season_name = NULL) {

  # Path to the starting folder for the season
  uppermost_dir <- paste0(startdir, "/", season_name)

  dir.create(uppermost_dir)

  # Paths to folders to hold the harvest info, nir, planting info, and crossing block
  planting_info  <- paste0(uppermost_dir, "/", season_name, " Planting info")
  crossing_block <- paste0(uppermost_dir, "/", season_name, " AxB")
  harvest_info   <- paste0(uppermost_dir, "/", season_name, " Harvest info/")
  nir_data       <- paste0(uppermost_dir, "/", season_name, " NIR")

  all_main_folders <- c(planting_info,
                        crossing_block,
                        harvest_info,
                        nir_data)

  purrr::map(all_main_folders, dir.create)

  planting_info_folders <- c("Maps", "Planting labels", "Rando files")
  for(i in 1:length(planting_info_folders))
  {
    dir.create(paste0(planting_info, "/", season_name, " ", planting_info_folders[[i]]))
  }

  nir_folders <- c("Single plants", "Plant rows", "Yield")
  for(i in 1:length(nir_folders))
  {
    dir.create(nir_data, "/", nir_folders[[i]])
  }

}
