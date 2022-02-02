
.base_url <- "https://www.usbr.gov/projects/index.php"
net_check <- function(abort = TRUE) {
  if (!curl::has_internet())
    rlang::exec(purrr::when(abort, . ~ rlang::abort, ~ rlang::warn), "This function needs an active internet connection to function.")
}


update_dam_options <- function() {
  net_check()
  .dam_opts <- rvest::html_elements(xml2::read_html(.base_url), xpath = '//option[contains(text(), "Select a dam")]/following-sibling::option') |>
  {\(x) {rlang::set_names(rvest::html_attr(x, "value"), stringr::str_replace(rvest::html_text(x), "\\`", "'"))}}()
  dump(".dam_opts", "R/dam_opts.R")
}


#' Fuzzy search for a dam name or view all
#' @param name \code{(chr)} of the dam. **Optional** leave blank to return all options
#' @return \code{(named chr)} with the url named according to the dam
dam_select <- function(name) {
  if (!missing(name)) {
    out <- .dam_opts[agrep(name, names(.dam_opts), ignore.case = TRUE)]
    if (length(out) > 1 && interactive()) {
      out <- out[menu(names(out))]
    } else if (length(out) > 1) {
      rlang::warn(paste0("Multiple matches:\n",paste0(names(out), collapse = "\n"), "\nSelected ", names(out)[1]))
      out <- out[1]
    }
    out
  } else
    out <- .dam_opts
  out
}

#' Get details on a dam
#' @description See `dam_select` for available dams
#' @param dam \code{(chr)} Name of the dam to lookup
#'
#' @return \code{(list)} of tables
#' \itemize{
#'   \item{\code{General}}
#'   \item{\code{Dimensions}}
#'   \item{\code{Hydraulics & Hydrology}}
#' }
#' @export
#'
#' @examples
#' dam_tables("Blue Mesa")
dam_tables <- function(dam) {
  net_check()
  .path <- dam_select(dam)
  .h <- xml2::read_html(httr::modify_url(.base_url, path = .path))
  out <- purrr::map(rlang::set_names(c("General", "Dimensions", "Hydraulics & Hydrology")), ~{
    el <- rvest::html_elements(.h, xpath = glue::glue("//h4[contains(text(), '{.x}')]/following-sibling::table[1]"))
    if (UU::is_legit(el))
      out <- rvest::html_table(el)[[1]]
    else
      out <- NULL
  }) |> purrr::compact()
  out
}
