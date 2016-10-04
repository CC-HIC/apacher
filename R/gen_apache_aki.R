#' @title Generates the APACHE Acute Kidney Injury score
#'
#' @import data.table
#' @param dt data.table containing physiological data
#' @param crrt Strings. Name of the Binary vector containing the assessment of prior chronic renal insufficiency.
#' @param window Numeric.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("Creatinine") := sample(seq(20,1000,11), 200, replace = T)]
#' ddata[, crrt := sample(c(0,1), 200, replace = T)]
#' system.time(gen_apache_aki(ddata, crrt = ddata[, crrt], window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_aki")]
#'
#' @export




gen_apache_aki <- function(dt, crrt, window = c(0,24)) {
  #  ================================
  #  = APACHE - Acute Kidney Injury =
  #  ================================
  # appending _ to var akimes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_aki
  apache_aki <- "apache_aki"
  w_apache_aki <- "w_apache_aki"

  # Update based on conditions
  # Order of conditions is IMPORTANT

  dt[, (w_apache_aki) := 0]

  # APACHE = 0
  dt[`Creatinine` < c(54), (w_apache_aki) := 0]

  # APACHE = 1
  dt[`Creatinine` %between% c(55, 129), (w_apache_aki) := 1]

  # APACHE = 2
  dt[`Creatinine` > c(129), (w_apache_aki) := 2]

  # APACHE = 3
  dt[`Creatinine` > c(169), (w_apache_aki) := 3]

  # APACHE = 4
  dt[`Creatinine` > c(304), (w_apache_aki) := 4]

  # crrt
  dt[get(crrt) < 1, (w_apache_aki) := (w_apache_aki)*2]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_aki) := max(w_apache_aki, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_aki := NULL]

}

