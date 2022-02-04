.num_cols <- c("ResCapacity(AF)" = "Reservoircapacity", "Capacity(MW)", "AnnualGeneration(MWh)" = "Annualgeneration(MWh)")

#' Update the local version of the tables found at \href{list of dams in the colorado river system}{https://en.wikipedia.org/wiki/List_of_dams_in_the_Colorado_River_system}
#'
#' @param .url \code{(chr)} The wikipedia url

update_crb_dams <- function(.url = "https://en.wikipedia.org/wiki/List_of_dams_in_the_Colorado_River_system") {
  if (curl::has_internet()) {
    crb_dams <- xml2::read_html(.url) |>
      rvest::html_elements("table.wikitable") |>
      purrr::map(rvest::html_table) |>
      rlang::set_names(c("Main Stem", "Upper Basin", "Lower Basin")) |>
      purrr::map(~dplyr::select(dplyr::mutate(.x, dplyr::across(dplyr::any_of(.num_cols), to_num)), - dplyr::all_of(.num_cols[c(1,3)])))
    dump("crb_dams", file.path("R", "crb_dams.R"))
    nms <- do.call(UU::common_names, crb_dams)
    crb_dams_full <- rlang::exec(dplyr::bind_rows, !!!purrr::map(crb_dams, `[`, nms), .id = "region")
    dump("crb_dams_full", file.path("R", "crb_dams_full.R"))
  } else
    rlang::warn("No internet, can't update.")

}

#' Look up reservoir or dam
#'
#' @param dam \code{(chr)} Name of dam
#' @param reservoir \code{(chr)} Name of reservoir
#'
#' @return \code{(tbl)}
#' @export
#'
#' @examples
#' crb_dam_reservoir("Blue Mesa")
crb_dam_reservoir <- function(dam = NULL, reservoir = NULL) {
  value <- c(dam = dam, reservoir = reservoir)
  val <-  switch(names(value), dam = "Name", reservoir = "Reservoir",  rlang::abort("Must supply either `dam` or `reservoir`"))
  var <- rlang::sym(val)
  dplyr::filter(crb_dams_full, agrepl(!!rlang::sym(names(value)), !!var, ignore.case = TRUE)) |>
    dplyr::select(Name, Reservoir, dplyr::everything())
}


to_num <- function(x) {
  as.numeric(stringr::str_replace_all(stringr::str_extract(x, "[\\d\\,]+"), "\\,", ""))
}
