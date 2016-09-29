#' @title Generates the APACHE Arterial Pressure score
#'
#' @description
#' Generates the APACHE Arterial Pressure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' map <- "Mean arterial blood pressure - Art BPMean arterial blood pressure"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, (map) := sample(seq(30,120,1), 200, replace = T)]
#' system.time(gen_apache_map(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_map")]
#'
#' @export


gen_apache_map <- function(dt, window, format = "dataItem") {
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

  # Prioritize the value to take into account for map or calculate it from gen_map
  switch(format, dataItem =  {map_mean_art <- "Mean arterial blood pressure - Art BPMean arterial blood pressure"
                              map_mean_cuff <- "Mean arterial blood pressure - NBPMean arterial blood pressure"
                              map_syst_art <- "Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure"
                              map_sys_cuff <- "Systolic Arterial blood pressure - NBPSystolic Arterial blood pressure"
                              map_dia_art <- "Diastolic arterial blood pressure - Art BPDiastolic arterial blood pressure"
                              map_dia_cuff <- "Diastolic arterial blood pressure - NBPDiastolic arterial blood pressure"},
                 NHICcode =     {map_mean_art <- "NIHR_HIC_ICU_0110"
                              map_mean_cuff <- "NIHR_HIC_ICU_0111"
                              map_syst_art <- "NIHR_HIC_ICU_0112"
                              map_syst_cuff <- "NIHR_HIC_ICU_0113"
                              map_dia_art <- "NIHR_HIC_ICU_0114"
                              map_dia_cuff <- "NIHR_HIC_ICU_0115"},
                 shortName = {map_mean_art <- "bp_m_a"
                              map_mean_cuff <- "bp_m_ni"
                              map_syst_art <- "bp_sys_a"
                              map_sys_cuff <- "bp_sys_ni"
                              map_dia_art <- "bp_dia_a"
                              map_dia_cuff <- "bp_dia_ni"}
  )


  if (map_mean_art %in% names(dt) & map_mean_cuff %in% names(dt)){
    dt[!is.na(get(map_mean_art)), d_map := get(map_mean_art)]
    dt[is.na(get(map_mean_art)), d_map := get(map_mean_cuff)]
  }

  if (map_mean_art %in% names(dt) & !map_mean_cuff %in% names(dt)){
    dt[, d_map := get(map_mean_art)]
  }

  if (!map_mean_art %in% names(dt) & map_mean_cuff %in% names(dt)){
    dt[, d_map := get(map_mean_cuff)]
  }

  if (map_syst_art %in% names(data) | map_dia_art %in% names(data)){
    dt[is.na(d_map), d_map := gen_map(get(map_syst_art), get(map_dia_art)) ]
  }

  if (map_sys_cuff %in% names(dt) | map_dia_cuff %in% names(dt)){
    dt[is.na(d_map), d_map := gen_map(get(map_sys_cuff), get(map_dia_cuff)) ]
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

