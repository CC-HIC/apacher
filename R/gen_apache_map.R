#' @title Generates the APACHE Arterial Pressure score
#'
#' @description
#' Generates the APACHE Arterial Pressure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' map <- "Mean arterial blood pressure - Art BPMean arterial blood pressure"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, (map) := sample(seq(30,120,1), 200, replace = T)]
#' system.time(gen_apache_map(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_map")]
#'
#' @export


gen_apache_map <- function(dt, window = c(0,24)) {
  #  ==============================
  #  = APACHE - Arterial Pressure =
  #  ==============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the output
  apache_map <- "apache_map"
  w_apache_map <- "w_apache_map"

  # Prioritize the value of MAP taken for APACHE calculation
  if ("Mean arterial blood pressure - Art BPMean arterial blood pressure" %in% names(dt) & "Mean arterial blood pressure - NBPMean arterial blood pressure" %in% names(dt)){
    dt[!is.na(`Mean arterial blood pressure - Art BPMean arterial blood pressure`),
       d_map := `Mean arterial blood pressure - Art BPMean arterial blood pressure`]
    dt[is.na(`Mean arterial blood pressure - Art BPMean arterial blood pressure`),
       d_map := `Mean arterial blood pressure - NBPMean arterial blood pressure`]
  }

  if ("Mean arterial blood pressure - Art BPMean arterial blood pressure" %in% names(dt) & !"Mean arterial blood pressure - NBPMean arterial blood pressure" %in% names(dt)){
    dt[, d_map := `Mean arterial blood pressure - Art BPMean arterial blood pressure`]
  }

  if (!"Mean arterial blood pressure - Art BPMean arterial blood pressure" %in% names(dt) & "Mean arterial blood pressure - NBPMean arterial blood pressure" %in% names(dt)){
    dt[, d_map := `Mean arterial blood pressure - NBPMean arterial blood pressure`]
  }

  if ("Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure" %in% names(data) | "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure`" %in% names(data)){
    dt[is.na(d_map), d_map := gen_map(`Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure`, `Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure`) ]
  }

  if ("Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure" %in% names(dt) | "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure" %in% names(dt)){
    dt[is.na(d_map), d_map := gen_map(`Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure`, `Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure`) ]
  }



  # Update based on conditions
  # Order of conditions is IMPORTANT

  dt[, (w_apache_map) := 0]

  # APACHE = 4
  dt[(d_map < c(50)) | (d_map > c(159)), (w_apache_map) := 4]

  # APACHE = 3
  dt[d_map %between% c(130,159), (w_apache_map) := 3]

  # APACHE = 2
  dt[(d_map %between% c(50,69)) | (d_map %between% c(110,129)), (w_apache_map) := 2]

  # APACHE = 0
  dt[d_map %between% c(70,109), (w_apache_map) := 0]


  # Calculate APACHE score for time window
  dt[time %between% window, (apache_map) := max(w_apache_map, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_map := NULL]

}

