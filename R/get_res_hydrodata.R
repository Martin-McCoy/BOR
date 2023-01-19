
#' Get Reservoir Hydrodata from BoR Pseudo-API
#'
#' @description Grab any of the reservoir data from the BoR Hydrodata API if you know the
#' number corresponding to a given reservoir and the number corresponding to a
#' given datatype. The function also allows you to grab a specific day, or the
#' entire historic/current data if no date is specified.
#'
#' @param reservoir_num \code{(num)} numeric value corresponding to a given reservoir
#' @param data_type_num \code{(num)} numeric value correspond to a given data type
#' @param date \code{(chr)} date to filter for in YYYY-MM-DD format
#'
#' @return a two-column tibble of returned data
#' @export

#' @examples
#' get_res_data(919, 49) # get Powell Elevation
get_res_data = function(reservoir_num,
                        data_type_num,
                        date = NULL,
                        most_recent = FALSE) {

  # generate url
  url = glue::glue('https://www.usbr.gov/uc/water/hydrodata/reservoir_data/{reservoir_num}/csv/{data_type_num}.csv')

  # request
  req = httr2::request(url)

  # perform request
  resp = httr2::req_perform(req)

  # get data
  raw_data = resp |>
    httr2::resp_body_string() |>
    readr::read_csv()

  # filtering by date
  if (!is.null(date)) {
    filtered_data = raw_data |>
      dplyr::filter(datetime == date)

    return(filtered_data)

  }

  if (most_recent == TRUE) {

    filtered_data = utils::tail(raw_data, n = 1)

    return(filtered_Data)

  } else {

    return(raw_data)

  }

}
