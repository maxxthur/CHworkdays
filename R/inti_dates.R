#' Title
#'
#' @param start
#' @param end
#'
#' @return
#'
#'
#' @examples
init_dates <- function(start, end) {

  start_date <- lubridate::dmy(start)

  end_date <- lubridate::dmy(end)

  n_days <- lubridate::interval(start_date, end_date)/lubridate::days(1)

  start_date + lubridate::days(0:n_days)
}
