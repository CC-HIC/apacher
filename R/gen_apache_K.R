#' @title Generates the APACHE Potassium score
#'
#' @description
#' Generates the APACHE Potassium score;
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
#' ddata[, ("Potassium") := sample(seq(2,10,0.1), 200, replace = T)]
#' system.time(gen_apache_K(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_K")]
#' @export

gen_apache_K <- function(dt, window = c(0,24)) {
  #  ======================
  #  = APACHE - Potassium =
  #  ======================
  # appending _ to var Kmes for readability and to ensure uses scoped version
  # requires Potassium concentration [K] in mmol/l

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_K
  apache_K <- "apache_K"
  w_apache_K <- "w_apache_K"

  # Update based on conditions
  # Order of conditions is IMPORTANT


  dt[, (w_apache_K) := 0]

  # APACHE = 1
  dt[(`Potassium` > c(2.9)), (w_apache_K) := 1]

  # APACHE = 0
  dt[(`Potassium` > c(3.4)), (w_apache_K) := 0]

  # APACHE = 2
  dt[(`Potassium` < c(3))  | (`Potassium` > c(5.4)), (w_apache_K) := 2]

  # APACHE = 3
  dt[(`Potassium` > c(5.9)), (w_apache_K) := 3]

  # APACHE = 4
  dt[(`Potassium` < c(2.5))  | (`Potassium` > c(6.9)), (w_apache_K) := 4]

  # Calculate APACHE score for time window
  dt[time %between% window, (apache_K) := max(w_apache_K, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_K := NULL]
}

