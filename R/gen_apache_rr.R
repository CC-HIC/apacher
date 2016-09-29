#' @title Generates the APACHE Respiratory Rate score
#'
#' @description
#' Generates the APACHE Respiratory Rate score. This function needs the result of the gen_q_rr helper function.
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' ddata$time <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[,("q_rr") := sample(seq(6,60,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' gen_apache_rr(ddata, window = c(0,24), format = "dataItem")
#' ddata[time %between% c(0,24), .N, by = c("site", "episode_id", "apache_rr")]
#'
#' @export



gen_apache_rr <- function(dt, window) {
  #  =============================
  #  = APACHE - Respiratory Rate =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming the apache_rr item
  apache_rr <- "apache_rr"
  d_apache_rr <- "d_apache_rr"

  if (!"q_rr" %in% names(dt)){
    stop("q_rr hasn't been found. Run gen_q_rr function!")}




  # Order of conditions is IMPORTANT

  # Set the Default Value
  dt[, (d_apache_rr) := 0]

  # APACHE = 1
  dt[q_rr %between% c(25,34), (d_apache_rr) := 1]

  # APACHE = 0
  dt[ q_rr %between% c(12,24), (d_apache_rr) := 0]

  # APACHE = 2
  dt[q_rr %between% c(10,11), (d_apache_rr) := 2]

  # APACHE = 3
  dt[q_rr %between% c(6,9) | (q_rr %between% c(35,49)), (d_apache_rr) := 3]

  # APACHE = 4
  dt[(q_rr < c(6)) | (q_rr > c(49)), (d_apache_rr) := 4]


  # Replace the maximal APACHE rr for the window period by patient
  dt[time %between% window ,  (apache_rr) := max(d_apache_rr, na.rm = T), by = c("site", "episode_id")]

  # Erase the d_apache_rr item
  dt[, (d_apache_rr) := NULL]

}

