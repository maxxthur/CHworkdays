#' Title
#'
#' @param eastern
#'
#' @return
#'
#'
#' @examples
easter_deduct_federal <- function(eastern_ext) {
  fixed_holidays <- eastern %>%
    dplyr::filter(federal_holiday) %>%
    dplyr::distinct(Datum)
  easter_m <- data.frame(Datum = "Ostermontag", date = eastern_ext + 1)
  easter_f <- data.frame(Datum = "Karfreitag", date = eastern_ext - 2)
  ch <- data.frame(Datum = "Christi Himmelfahrt", date = eastern_ext + 39)
  pm <- data.frame(Datum = "Pfingstmontag", date = eastern_ext + 50)


  fixed_holidays %>%
    dplyr::left_join(easter_m, by = "Datum") %>%
    dplyr::left_join(easter_f, by = "Datum") %>%
    dplyr::left_join(ch, by = "Datum") %>%
    dplyr::left_join(pm, by = "Datum") %>%
    tidyr::pivot_longer(cols = -Datum,
                        names_to = "col",
                        values_to = "date") %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::select(date) %>%
    dplyr::mutate(level = "federal")
}

#' Title
#'
#' @param eastern
#'
#' @return
#' @export
#'
#' @examples
easter_deduct_state <- function(eastern_ext) {
  holidays <- eastern %>%
    dplyr::filter(holiday) %>%
    dplyr::select(Datum, level)

  easter_m <- data.frame(Datum = "Ostermontag", date = eastern_ext + 1)
  easter_f <- data.frame(Datum = "Karfreitag", date = eastern_ext - 2)
  ch <- data.frame(Datum = "Christi Himmelfahrt", date = eastern_ext + 39)
  pm <- data.frame(Datum = "Pfingstmontag", date = eastern_ext + 50)

  holidays %>%
    dplyr::left_join(easter_m, by = "Datum") %>%
    dplyr::left_join(easter_f, by = "Datum") %>%
    dplyr::left_join(ch, by = "Datum") %>%
    dplyr::left_join(pm, by = "Datum") %>%
    tidyr::pivot_longer(cols = -c(Datum, level),
                        names_to = "col",
                        values_to = "date") %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::select(date, level)

}
