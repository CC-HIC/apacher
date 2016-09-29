#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
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
#' ddata[, ("White cell count") := sample(seq(2,50,0.1), 200, replace = T)]
#' system.time(gen_apache_wbc(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_wbc")]
#'
#' @export


gen_apache_wbc <- function(dt, window, format = "dataItem") {
  #  ===================================
  #  = APACHE - White Blood Cell Count =
  #  ===================================
  # appending _ to var wbcmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_wbc
  apache_wbc <- "apache_wbc"
  w_apache_wbc <- "w_apache_wbc"

  # Prioritize the value to take into account for leucocytes count
  switch(format, dataItem =  {wbc <- "White cell count"},
         NHICcode =     {wbc <- "NIHR_HIC_ICU_0182"},
         shortName = {wbc <- "White cell"}
  )


  # Update based on conditions
  # Order of conditions is IMPORTANT

  # APACHE = 0
  dt[(get(wbc) > c(2.999)),   (w_apache_wbc) := 0]

  # APACHE = 1
  dt[(get(wbc) > c(14.999)), (w_apache_wbc) := 1]

  # APACHE = 2
  dt[(get(wbc) < c(3.000)) | (get(wbc) > c(19.999)), (w_apache_wbc) := 2]

  # APACHE = 4
  dt[(get(wbc) < c(1.000)) | (get(wbc) > c(39.999)), (w_apache_wbc) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_wbc) := max(w_apache_wbc, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_wbc := NULL]


}

