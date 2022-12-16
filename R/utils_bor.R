
#' Extract the shorthand lake name from a string
#'
#' @param x \code{chr}
#'
#' @return \code{chr}
#' @export
#'
#' @examples
#' bor_nm2chr("Mead Annual Pool Elevation")

bor_nm2chr <- function(x) {
  trimws(tolower(stringr::str_extract(x, stringr::regex(UU::regex_or(c("mead", "powell", "navajo", "fontenelle", "havasu", "mohave", "blue mesa", "flaming gorge")), ignore_case = TRUE))))
}
