library(magrittr)

read_data_ch <- function() {
  fixed <- readxl::read_excel(path = "~/GitHub/Holidays_CH.xlsx", sheet = 1) %>%
    tidyr::pivot_longer(cols = - Datum,
                        names_to = "level",
                        values_to = "holiday") %>%
    dplyr::mutate(holiday = dplyr::case_when(
      holiday %in% c("A", "B", "C", "D", "c") ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(Datum = as.character(Datum)) %>%
    dplyr::mutate(day = paste(substr(Datum, 9, 10), substr(Datum, 6, 7), sep = "-")) %>%
    dplyr::group_by(Datum) %>%
    dplyr::mutate(federal_holiday = mean(holiday)) %>%
    dplyr::mutate(federal_holiday = dplyr::case_when(
               federal_holiday == 1 ~ TRUE,
               TRUE ~ FALSE
             )) %>%
    dplyr::ungroup() %>%
    dplyr::select(-Datum)

  easter <- readxl::read_excel(path = "~/GitHub/Holidays_CH.xlsx", sheet = 2) %>%
    tidyr::pivot_longer(cols = - Datum,
                        names_to = "level",
                        values_to = "holiday") %>%
    dplyr::mutate(holiday = dplyr::case_when(
      holiday %in% c("A", "B", "C", "D", "c") ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(Datum = as.character(Datum)) %>%
    dplyr::group_by(Datum) %>%
    dplyr::mutate(federal_holiday = mean(holiday)) %>%
    dplyr::mutate(federal_holiday = dplyr::case_when(
      federal_holiday == 1 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::ungroup()

  list(fixed, easter)
}

fixed <- read_data_ch()[[1]]
eastern <- read_data_ch()[[2]]

usethis::use_data(fixed, eastern, internal = TRUE, overwrite = TRUE)
