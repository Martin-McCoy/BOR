.index_htm <- NULL
.base_url <- "https://www.usbr.gov/projects/index.php"
index_htm <- function() {
  if (!curl::has_internet())
    rlang::abort("This function needs an active internet connection to function.")
  if (is.null(get0(".index_htm", envir = rlang::ns_env("BORdata")))) {
    index_htm <- xml2::read_html(.base_url)
    rlang::env_binding_unlock(rlang::ns_env("BORdata"))
    assignInNamespace(".index_htm", index_htm, "BORdata")
    rlang::env_binding_lock(rlang::ns_env("BORdata"))
  } else {
    index_htm <- .index_htm
  }
  index_htm
}

index_htm()

#' All available dams for lookup
#' @return \code{(chr)} of all dam names

dam_options <- function() rvest::html_elements(index_htm(), xpath = '//option[contains(text(), "Select a dam")]/following-sibling::option') |> rvest::html_text()

dam_select <- function(dam) {
  out <- dam_options()[agrep(dam, dam_options(), ignore.case = TRUE)]
  if (length(out) > 1 && interactive()) {
    out <- menu(out)
  } else if (length(out) > 1) {
    rlang::warn(paste0("Multiple matches:\n",paste0(out, collapse = "\n"), "\nSelected ", out[1]))
    out <- out[1]
  }
  out
}

#' Get details on a dam
#' @description See `dam_options` for available dams
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
  if (missing(dam))
    .txt <- "Select a dam"
  else
    .txt <- dam_select(dam)
  href <- rvest::html_element(index_htm(), xpath = glue::glue('//option[contains(text(), "{.txt}")]')) |>
    rvest::html_attr("value")
  if (curl::has_internet()) {
    .h <- xml2::read_html(httr::modify_url(.base_url, path = href))
    out <- purrr::map(rlang::set_names(c("General", "Dimensions", "Hydraulics & Hydrology")), ~{
      el <- rvest::html_elements(.h, xpath = glue::glue("//h4[contains(text(), '{.x}')]/following-sibling::table[1]"))
      if (!is.na(el))
        out <- rvest::html_table(el)[[1]]
      else
        out <- NULL

    }) |> purrr::compact()
  } else {
    out <- NULL
    rlang::warn(glue::glue("No internet. Cannot fetch tables for {dam}"))
  }
  out
}
