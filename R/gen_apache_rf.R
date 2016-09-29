#' @title Generates the APACHE Respiratory Failure score
#'
#' @description
#' Generates the APACHE Respiratory Failure score;
#' \describe{ This function use the results of gen_grad to calculate the APACHE score for respiratory failure component}
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' ddata$"grad_rf" <- c(sample(seq(10,50,0.1), 200, replace = T))
#' ddata <- as.data.table(ddata)
#' ddata[,("Inspired fraction of oxygen") := sample(seq(0.21,1,0.01), 200, replace = T)]
#' ddata[, ("time") := sample(seq(1,72,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("PaO2/FiO2 ratio") := sample(seq(4,40,1), 200, replace = T)]
#' ddata[, ("PaO2 - ABG") := sample(seq(4,90,1), 200, replace = T)]
#' gen_apache_rf(ddata, window = c(0,24), format = "dataItem")
#' ddata[time %between% c(0,24), .N, by = c("site", "episode_id", "apache_rf")]
#'
#' @export


gen_apache_rf <- function(dt, window, format = "dataItem") {
  #  =============================
  #  = APACHE - Respiratory Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires either fiO2 component and alveolo arterial gradient of oxygen.

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Non-standard evaluation
  apache_rf <- "apache_rf"
  w_apache_rf <- "w_apache_rf"

  # Prioritize the value to take into account for the fiO2
  switch(format, dataItem =  {fio2_p <- "Inspired fraction of oxygen"
                              fio2_pp <- "PaO2/FiO2 ratio"
                              PaO2  <- "PaO2 - ABG"},
                 NHICcode =     {fio2_p <- "NIHR_HIC_ICU_0150"
                              fio2_pp <- "NIHR_HIC_ICU_0913"
                              PaO2  <- "NIHR_HIC_ICU_0132"},
                 shortName = {fio2_p <- "fiO2"
                              fio2_pp <- "pf_ratio"
                              PaO2 <- "~"}
  )

  # Set Default value
  dt[, (w_apache_rf) := 0]

  # APACHE = 0
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) > c(9.3))  , (w_apache_rf) := 0]
  dt[(get(fio2_p) > 0.49) & (grad_rf < c(26.7)), (w_apache_rf) := 0]
  dt[(get(fio2_pp)/get(PaO2) < 0.5)  & (get(PaO2) > c(9.3))  , (w_apache_rf) := 0]
  dt[ (get(fio2_pp)/get(PaO2) > 0.49) & (grad_rf < c(26.7)), (w_apache_rf) := 0]


  # APACHE = 1
  dt[(get(fio2_p) < 0.5) & (get(PaO2) < c(9.3)), (w_apache_rf) := 1]
  dt[(get(PaO2) / get(fio2_pp) < 0.5) & (get(PaO2) < c(9.3)), (w_apache_rf) := 1]

  # APACHE = 2
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(26.6)), (w_apache_rf) := 2]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(26.6)), (w_apache_rf) := 2]

  # APACHE = 3
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) < c(8.1))    , (w_apache_rf) := 3]
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(46.4)), (w_apache_rf) := 3]
  dt[(get(PaO2) / get(fio2_pp) < 0.5)  & (get(PaO2) < c(8.1)), (w_apache_rf) := 3]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(46.4)), (w_apache_rf) := 3]

  # APACHE = 4
  dt[(get(fio2_p) < 0.5)  & (get(PaO2) < c(7.3)) , (w_apache_rf) := 4]
  dt[(get(fio2_p) > 0.49) & (grad_rf > c(66.3)), (w_apache_rf) := 4]
  dt[(get(PaO2) / get(fio2_pp) < 0.5)  & (get(PaO2) < c(7.3)) , (w_apache_rf) := 4]
  dt[(get(PaO2) / get(fio2_pp) > 0.49) & (grad_rf > c(66.3)), (w_apache_rf) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_rf) := max(w_apache_rf, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_rf := NULL]

}

