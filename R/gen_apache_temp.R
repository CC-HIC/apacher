#' @title Generates the APACHE Temperature score
#'
#' @description
#' Generates the APACHE Temperature score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' temperature <- "Temperature - Central"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[ , ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[ , (temperature) := sample(seq(35,45,0.1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' system.time(gen_apache_temp(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_temp")]
#'
#' @export

gen_apache_temp <- function(dt, window, format = "dataItem") {
  #  =============================
  #  = APACHE - Temperature =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the output
  apache_temp <- "apache_temp"
  w_apache_temp <- "w_apache_temp"

  # Prioritize the value to take into account for the temperature
  switch(format, dataItem =  {temp_p <- "Temperature - Central"
                              temp_pp <- "Temperature - Non-central"},
                 NHICcode =  {temp_p <- "NIHR_HIC_ICU_0141"
                              temp_pp <- "NIHR_HIC_ICU_0142"},
                 shortName = {temp_p <- "temperature_central"
                              temp_pp <- "temperature_non_central"}
         )


  if (temp_p %in% names(dt) & !temp_pp %in% names(dt)){
    dt[, d_temp := get(temp_p)]
  }

  if (!temp_p %in% names(dt) & temp_pp %in% names(dt)){
    dt[, d_temp := get(temp_pp)]
  }

  if (temp_p %in% names(dt) & temp_pp %in% names(dt)){
    dt[, d_temp := get(temp_pp)]
    dt[!is.na(get(temp_p)), d_temp := get(temp_p)]
  }



  # Update based on conditions
  # Order of conditions is IMPORTANT

  #Set the Default Value
  dt[, (w_apache_temp) := 0]

  # APACHE = 4
  dt[(d_temp < c(30)) | (d_temp > c(40)), (w_apache_temp) := 4]

  # APACHE = 3
  dt[(d_temp %between% c(30,31.9)) | (d_temp %between% c(39,40)), (w_apache_temp) := 3]

  # APACHE = 2
  dt[d_temp %between% c(32,33.9), (w_apache_temp) := 2]

  # APACHE = 1
  dt[(d_temp %between% c(34,35.9)) | (d_temp %between% c(38.5,38.9)), (w_apache_temp) := 1]

  # APACHE = 0
  dt[d_temp %between% c(36,38.4), (w_apache_temp) := 0]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_temp) := max(w_apache_temp, na.rm = T), by = c("site", "episode_id")]
  dt[, (w_apache_temp) := NULL]

}

