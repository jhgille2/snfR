#' This function just takes a value and multiplies it by 0.87. This is used
#' to convert protein and oil measurements from a dry b
#'
#' @title Convert to 13\% moisture content
#' @param dry_basis_pct The value of some observation, typically seed protein or oil
#' content on a dry basis.
#' @value The value of some observation on a 13\% moisture content basis.
convert_to_13_pct <- function(dry_basis_pct = NULL) {

  return(dry_basis_pct*0.87)

}
