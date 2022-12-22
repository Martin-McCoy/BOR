
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

#' Update `powell_mead_elevations` table
#' @description A table of key elevations pertaining to Lake's Mead & Powell
#' @return \code{file}


update_powell_mead_elevations <- function() {
  virgaUtils::google_auth()
  powell_mead_elevations <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1lRtySzf-BPS-mUxjTPYX4Aky9x0yiJOJt_AHpgVpins/edit#gid=1728077509",
    sheet = "Powell & Mead thresholds",
    col_types = glue::glue_collapse(c(
      Reservoir = "c",
      `Elevation Description` = "c",
      Operator = "c",
      `Elevation (ft)` = "n",
      `Current RiverViz Elevs.` = "c",
      `Proposed RiverViz Elevs.` = "c"
    )))
  file <- "R/obj_powell_mead_elevations.R"
  UU::mkpath(file, mkfile = TRUE)
  write("#' @export", file)
  dump("powell_mead_elevations", file, append = TRUE)
}
