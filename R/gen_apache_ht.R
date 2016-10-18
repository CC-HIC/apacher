#' @title Generates the APACHE Hematocrit score
#'
#' @description
#' Generates the APACHE ht score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' d_ht <- "d_ht"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[ , ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[ , (d_ht) := sample(seq(0.10,0.7,0.01), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' system.time(gen_apache_ht(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_ht")]
#'
#' @export

gen_apache_ht <- function(dt, window = c(0,24)) {
  #  =======================
  #  = APACHE - Hematocrit =
  #  =======================
  # appending _ to var htmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_ht
  apache_ht <- "apache_ht"
  w_apache_ht <- "w_apache_ht"


  if (!"d_ht" %in% names(dt)){
   stop("Haematocrit variable unavailable, see gen_haemo for mor informations")
  }


  # Update based on conditions
  # Order of conditions is IMPORTANT

  dt[, (w_apache_ht) := 0]

  # APACHE = 0
  dt[d_ht > c(29), (w_apache_ht) := 0]

  # APACHE = 1
  dt[(d_ht > c(45.9)), (w_apache_ht) := 1]

  # APACHE = 2
  dt[(d_ht < c(30)) | (d_ht > c(49.9)), (w_apache_ht) := 2]

  # APACHE = 4
  dt[(d_ht < c(20)) | (d_ht > c(59.9)), (w_apache_ht) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_ht) := max(w_apache_ht, na.rm = T), by = c("site", "episode_id")]
  dt[, (w_apache_ht) := NULL]

}

