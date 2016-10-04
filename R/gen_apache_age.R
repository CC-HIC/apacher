#' @title Generates the APACHE Age
#'
#' @description
#' Generates the APACHE age score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#'
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[ , ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[ , (age) := sample(seq(0,99,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' system.time(gen_apache_age(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_age")]
#'
#' @export


gen_apache_age <- function(dt, window = c(0,24)) {
  #  ===============================
  #  = APACHE - Glasgow Coma Scale =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_gcs
  apache_age <- "apache_age"
  w_apache_age <- "w_apache_age"

  # Update based on conditions
  # Order of conditions is IMPORTANT

  # # transform strings as datetime object
  #
  # as.posixct(data[, `Date of birth`])
  #
  # data[, `Date of birth`]
  #
  dt[, (w_apache_age) := 0]

  # APACHE = 0
  dt[(`age` < 44), (w_apache_age) := 0]

  # APACHE = 2
  dt[(`age` %between% c(45, 54)), (w_apache_age) := 2]

  # APACHE = 3
  dt[(`age` %between% c(55, 64)), (w_apache_age) := 3]

  # APACHE = 5
  dt[(`age` %between% c(65, 74)), (w_apache_age) := 5]

  # APACHE = 6
  dt[(`age` > 74), (w_apache_age) := 6]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_age) := max(w_apache_age, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_age := NULL]
}

