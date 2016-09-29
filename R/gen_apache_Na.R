#' @title Generates the APACHE Sodium score
#'
#' @description
#' Generates the APACHE Sodium score;
#'
#' @import data.table
#' @param dt data.table containing physiological data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("Sodium") := sample(seq(100,180,1), 200, replace = T)]
#' system.time(gen_apache_Na(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_Na")]
#'
#' @export

gen_apache_Na <- function(dt, window, format = "dataItem") {
  #  ===================
  #  = APACHE - Sodium =
  #  ===================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires Sodium concentration [Na] in mmol/l

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_Na
  apache_Na <- "apache_Na"
  w_apache_Na <- "w_apache_Na"

  # Prioritize the value to take into account for the temperature
  switch(format, dataItem =  {Na <- "Sodium"},
                 NHICcode =     {Na <- "NIHR_HIC_ICU_0168"},
                 shortName = {Na <- "sodium"}
  )


  # Update based on conditions
  # Order of conditions is IMPORTANT

  # APACHE = 0
  dt[(get(Na) > c(129)), (w_apache_Na) := 0]

  # APACHE = 1
  dt[(get(Na) > c(149)), (w_apache_Na) := 1]

  # APACHE = 2
  dt[(get(Na) < c(130))  | (get(Na) > c(154)), (w_apache_Na) := 2]

  # APACHE = 3
  dt[(get(Na) < c(120))  | (get(Na) > c(159)), (w_apache_Na) := 3]

  # APACHE = 4
  dt[(get(Na) < c(111))  | (get(Na) > c(179)), (w_apache_Na) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_Na) := max(w_apache_Na, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_Na := NULL]

}

