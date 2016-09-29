#' @title Generates the APACHE Heart Rate score
#'
#' @description
#' Generates the APACHE Heart Rate score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#'@examples
#' ddata <- NULL
#' hr <- "Heart rate"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("Heart rate") := sample(seq(30,120,1), 200, replace = T)]
#' system.time(gen_apache_hr(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_hr")]
#'
#' @export


gen_apache_hr <- function(dt, window, format = "dataItem") {
  #  =============================
  #  = APACHE - Heart Rate =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_hr
  apache_hr <- "apache_hr"
  w_apache_hr <- "w_apache_hr"

  # Prioritize the value to take into account for the heart rate
  switch(format, dataItem =  {hr_p <- "Heart rate"},
                 NHICcode =  {hr_p <- "NIHR_HIC_ICU_0108"},
                 shortName = {hr_p <- "hrate"}
  )


  # Update based on conditions
  # Order of conditions is IMPORTANT

  dt[, (w_apache_hr) := 0]

  # APACHE = 0
  dt[get(hr_p) %between% c(70,109), (w_apache_hr) := 0]

  # APACHE = 2
  dt[(get(hr_p) %between% c(55,69)) | (get(hr_p) %between% c(110,139)), (w_apache_hr) := 2]

  # APACHE = 3
  dt[(get(hr_p) %between% c(40,54)) | (get(hr_p) %between% c(140,179)), (w_apache_hr) := 3]

  # APACHE = 4
  dt[(get(hr_p) < c(40)) | (get(hr_p) > c(179)), (w_apache_hr) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_hr) := max(w_apache_hr, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_hr := NULL]

}

