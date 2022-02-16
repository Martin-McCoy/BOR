
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
      out <- out[menu(names(out), title = paste0(name, " matches:"))]
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
#' @param dam \code{(chr)} Name of the dam to lookup. If not supplied, all tables will be returned
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
dam_info <- function(dam) {
  if (missing(dam))
    all_dams
  else
    all_dams[[names(dam_select(dam))]]
}

#' Retrieve dam Dimensions from a Dam Table
#'
#' @param dam_table \code{(tbl)} from which to pull the metrics
#' @param dim_regex \code{(chr)} regex on which to match the dimension name
#' @inheritParams stringr::regex
#'
#' @return \code{(named vector)}
#' @export
dam_dim <- function(dam_table, dim_regex, ignore_case = TRUE) {
  dplyr::filter(dam_table, stringr::str_detect(!!rlang::sym(names(dam_table)[1]), stringr::regex(UU::regex_or(dim_regex), ignore_case = ignore_case))) |>
    {\(x) {rlang::set_names(dplyr::pull(x, Value), dplyr::pull(x, !!rlang::sym(names(dam_table)[1])))}}()
}

#' Parse downloaded dam data rds files
#'
#' @return Generates _R/all_dams.R_
dam_table_parse <- function() {
  .files <- UU::list.files2(file.path("data", "dams"))
  all_dams <- purrr::map(.files, readRDS)
  # Transform Dimensions to numeric
  all_dams <- purrr::imap(all_dams, ~{
    if (!is.null(.x$Dimensions)) {
      .Dimensions <- rlang::set_names(.x$Dimensions, c("Dimension", "Value")) |>
        dplyr::mutate(Unit = trimws(stringr::str_extract(Value, "[\\s[:alpha:]\\;\\,\\(\\)\\-]+$")),
                      Value = as.numeric(stringr::str_remove_all(stringr::str_extract(Value, "^\\s?[\\d\\,\\.]+"), "\\,")))
      #browser(expr = anyNA(.x$Dimensions, TRUE) || .y == "Hoover Dam")
      .x$Dimensions <- .Dimensions
    }
    if (!is.null(.x$General))
      .x$General <- rlang::set_names(.x$General,c("Item", "Value"))
    if (!is.null(.x$`Hydraulics & Hydrology`))
      .x$`Hydraulics & Hydrology` <- rlang::set_names(.x$`Hydraulics & Hydrology`,c("Metric", "Value"))

    .x
  })
  dump("all_dams", "R/all_dams.R")
}

#' Update info rds for all dams
#'
#' @param dams \code{(chr)} named character of url paths
#'
#' @return All saved rds files in _data/dams_ and an updated _R/all_dams.R_
dam_table_update <- function(dams = dam_select()) {
  net_check()
  i <- 1
  .pb <- cli::cli_progress_bar(name = "Dam: ", type = "tasks", total = length(dams), format = "{cli::pb_bar} {pb::status} {cli::pb_current}/{cli::pb_total} {cli::pb_percent}")
  while (i <= length(dams)) {
    cli::cli_progress_update(id = .pb, status = names(dams)[i], set = i)
    out <- dam_fetch(dams[i])
    if (!UU::is_error(out)) {
      if (!is.null(out)) {
        saveRDS(out, file.path("data", "dams", paste0(names(dams)[i], ".rds")))
      }
      i <- i + 1
    }
    Sys.sleep(1)
  }
  dam_table_parse()
}

#' Fetches dam data from BOR website
#'
#' @param path \code{(chr)} path to dam info page
#'
#' @return General, Dimensions, Hydraulics & Hydrology tables

dam_fetch <- function(path) {
  try({
    .url <- httr::modify_url(.base_url, path = path)
    .url = url(.url, "rb")
    .h <- xml2::read_html(.url)
    close(.url)
     purrr::map(rlang::set_names(c("General", "Dimensions", "Hydraulics & Hydrology")), ~{
      el <- rvest::html_elements(.h, xpath = glue::glue("//h4[contains(text(), '{.x}')]/following-sibling::table[1]"))
      if (UU::is_legit(el))
        out <- rvest::html_table(el)[[1]]
      else
        out <- NULL
    }) |> purrr::compact()
  })
}
