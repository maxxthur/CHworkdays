#' Title
#'
#' @param Y
#'
#' @return
#'
#'
#' @examples
gauss_easter <- function(Y) {
  a = Y %% 19
  b = Y %% 4
  c = Y %% 7
  d = (19 * a + 24) %% 30
  e = (2 * b + 4 * c + 6 * d + 5) %% 7
  OT = 22 + d + e
  OM = rep(3, length(OT))

  for(i in 1:length(OT)) {
    if(OT[i] > 31) {
      OT[i] = d[i] + e[i] - 9
      OM[i] = 4
    }

    if(OT[i] == 26 & OM[i] == 4) {
      OT[i] = 19
    }

    if(OT[i] == 25 & OM[i] == 4 & d[i] == 28 & e[i] == 6 & a[i] > 10) {
      OT[i] = 18
    }
  }

  data.frame(day = OT, month = OM, year = Y) %>%
    dplyr::mutate(date = lubridate::dmy(paste(day, month, year, sep = "-"))) %>%
    .$date

}
