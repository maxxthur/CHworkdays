#' Title
#'
#' @param days
#'
#' @return
#'
#'
#' @examples
federal_states_days <- function(Y) {
 fed_hols <- fixed %>%
    dplyr::filter(federal_holiday)

  states_years <- expand.grid(level = unique(fed_hols$level), year = Y)

  fed_hols %>%
    dplyr::left_join(states_years, by = "level") %>%
    dplyr::mutate(date = paste(day, year, sep = "-")) %>%
    dplyr::mutate(date = lubridate::dmy(date)) %>%
    dplyr::select(date, level)
}
