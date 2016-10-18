#' @title Generates the APACHE Metabolic Failure score
#'
#' @description
#' Generates the APACHE Metabolic Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("pH") := sample(seq(6.0,8.0,0.01), 200, replace = T)]
#' ddata[, ("HCO3 - ABG / VBG") := sample(seq(5,50,0.1), 200, replace = T)]
#' system.time(gen_apache_mf(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_mf")]
#'
#' @export


gen_apache_mf <- function(dt, window = c(0,24)) {
  #  =============================
  #  = APACHE - Metabolic Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the output
  apache_mf <- "apache_mf"
  w_apache_mf <- "w_apache_mf"

  dt[, (w_apache_mf) := 0]

  # APACHE = 0
  dt[(`pH - ABG / VBG`   > c(7.32)), (w_apache_mf) := 0]
  dt[is.na(`pH - ABG / VBG`)  & (`HCO3 - ABG / VBG` > c(21))  , (w_apache_mf) := 0]

  # APACHE = 1
  dt[(`pH - ABG / VBG`   > c(7.49)), (w_apache_mf) := 1]
  dt[is.na(`pH - ABG / VBG`)  & (`HCO3 - ABG / VBG` > c(31))  , (w_apache_mf) := 1]

  # APACHE = 2
  dt[ (`pH - ABG / VBG`   < c(7.33)), (w_apache_mf) := 2]
  dt[is.na(`pH - ABG / VBG`)  & (`HCO3 - ABG / VBG` < c(22))  , (w_apache_mf) := 2]

  # APACHE = 3
  dt[(`pH - ABG / VBG`   < c(7.25)) | (`pH - ABG / VBG`   > c(7.59)), (w_apache_mf) := 3]
  dt[(is.na(`pH - ABG / VBG`)) & (((`HCO3 - ABG / VBG` < c(18)))  | (`HCO3 - ABG / VBG` > c(40))) , (w_apache_mf) := 3]

  # APACHE = 4
  dt[ (`pH - ABG / VBG`   < c(7.16)) | (`pH - ABG / VBG`   > c(7.69)), (w_apache_mf) := 4]
  dt[(is.na(`pH - ABG / VBG`)) & ((`HCO3 - ABG / VBG` < c(16))  | (`HCO3 - ABG / VBG` > c(51))) , (w_apache_mf) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_mf) := max(w_apache_mf, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_mf := NULL]

}

