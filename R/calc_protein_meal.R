#' This function takes the seed protein and seed oil content at a 13% moisture basis and
#' returns the meal protein content.
#'
#' @title Calculate protein meal.
#' @param oil_13_percent The seed oil content at 13\% moisture basis.
#' @param protein_13_percent The seed protein content at 13\% moisture basis.
#' @references Pantalone, V., & Smallwood, C. (2018) Registration of 'TN11-5102' soybean cultivar with high yield and high protein meal. Journal of Plant Registrations, 12:304-308
#' @return Meal protein content
calc_protein_meal <- function(oil_13_percent = NULL, protein_13_percent = NULL) {

  meal_protein <- (protein_13_percent/(1-(oil_13_percent/100)))/0.92

  return(meal_protein)

}
