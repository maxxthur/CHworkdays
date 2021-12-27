#' Title
#'
#' @param Y
#'
#' @return
#'
#'
#' @examples
fixed_state_holidays <- function(Y) {
  state_hols <- read_data_ch()[[1]] %>%
    dplyr::filter(!federal_holiday & holiday)

  states_years <- expand.grid(level = unique(state_hols$level), year = Y)

  state_hols %>%
    dplyr::left_join(states_years, by = "level") %>%
    dplyr::mutate(date = paste(day, year, sep = "-")) %>%
    dplyr::mutate(date = lubridate::dmy(date)) %>%
    dplyr::select(date, level)
}
