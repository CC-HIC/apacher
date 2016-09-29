#' @title Generates the APACHE Glasgow Coma Scale score
#'
#' @description
#' Generates the APACHE gcs score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' gcs <- "GCS - total"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[ , ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[ , (gcs) := sample(seq(3,15,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' system.time(gen_apache_gcs(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_gcs")]
#'
#' @export


gen_apache_gcs <- function(dt, window, format = "dataItem") {
  #  ===============================
  #  = APACHE - Glasgow Coma Scale =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_gcs
  apache_gcs <- "apache_gcs"
  w_apache_gcs <- "w_apache_gcs"

  # Prioritize the value to take into account for Glasgow Coma Scale Score
  switch(format, dataItem =  {gcs <- "GCS - total"},
         NHICcode =     {gcs <- "NIHR_HIC_ICU_0156"},
         shortName = {gcs <- "gcs"}
  )


  # Update based on conditions
  # Order of conditions is IMPORTANT

  # APACHE = 15- GCS
  dt[,   (w_apache_gcs) := round(15 - get(gcs),0)]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_gcs) := max(w_apache_gcs, na.rm = T), by = c("site", "episode_id")]
  dt[, (w_apache_gcs) := NULL]

}

