update_crb_dams <- function(.url = "https://en.wikipedia.org/wiki/List_of_dams_in_the_Colorado_River_system") {
  if (curl::has_internet()) {
    crb_dams <- xml2::read_html(.url) |>
      rvest::html_elements("table.wikitable") |>
      purrr::map(rvest::html_table) |>
      rlang::set_names(c("Main Stem", "Upper Basin", "Lower Basin"))
    dump("crb_dams", file.path("R", "crb_dams.R"))
    nms <- do.call(UU::common_names, crb_dams)
    crb_dams_full <- rlang::exec(dplyr::bind_rows, !!!purrr::map(crb_dams, `[`, nms), .id = "region")
    dump("crb_dams_full", file.path("R", "crb_dams_full.R"))
  } else
    rlang::warn("No internet, can't update.")

}

crb_dam_reservoir <- function(dam = NULL, reservoir = NULL) {
  value <- c(dam = dam, reservoir = reservoir)
  val <-  switch(names(value), dam = "Name", reservoir = "Reservoir",  rlang::abort("Must supply either `dam` or `reservoir`"))
  var <- rlang::sym(val)
  dplyr::filter(crb_dams_full, agrepl(!!rlang::sym(names(value)), !!var, ignore.case = TRUE)) |>
    dplyr::select(Name, Reservoir, dplyr::everything())
}
