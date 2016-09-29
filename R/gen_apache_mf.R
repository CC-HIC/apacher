#' @title Generates the APACHE Metabolic Failure score
#'
#' @description
#' Generates the APACHE Metabolic Failure score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#' @param format String. The format chosen for data items. Could be "dataItem", "shortName" or "NHICcode".
#' See relabel_cols for more informations.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("pH") := sample(seq(6.0,8.0,0.01), 200, replace = T)]
#' ddata[, ("HCO3 - ABG / VBG") := sample(seq(5,50,0.1), 200, replace = T)]
#' system.time(gen_apache_mf(ddata, window = c(0,24), format = "dataItem"))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_mf")]
#'
#' @export


gen_apache_mf <- function(dt, window, format = "dataItem") {
  #  =============================
  #  = APACHE - Metabolic Failure =
  #  =============================
  # appending _ to var names for readability and to ensure uses scoped version

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the output
  apache_mf <- "apache_mf"
  w_apache_mf <- "w_apache_mf"

  # Prioritize the value to take into account for the Metabolic Failure
  switch(format, dataItem =  {ph <- "pH - ABG / VBG"
                              hco3 <- "HCO3 - ABG / VBG"},
                 NHICcode =     {ph <- "NIHR_HIC_ICU_0136"
                              hco3 <- "NIHR_HIC_ICU_0138"},
                 shortName = {ph <- "ph_abg_vbg"
                              if (!ph %in% names(data)){
                                stop("Unable to derive metabolic component. Verify the availability of the item, or try to convert the shortnames to dataItem or NIHH code")
                              }
                 }
  )

  if (!ph %in% names(data)) {ph <- hco3}




  # Update based on conditions
  # Order of conditions is IMPORTANT

  # APACHE = 0
  if (format != "shortName"){
    dt[(get(ph)   > c(7.32)), (apache_mf) := 0]
  }
  dt[is.na(apache_mf)  & (get(hco3) > c(21))  , (apache_mf) := 0]

  # APACHE = 1
  dt[(get(ph)   > c(7.49)), (apache_mf) := 1]
  dt[is.na(apache_mf)  & (get(hco3) > c(31))  , (apache_mf) := 1]

  # APACHE = 2
  if (format != "shortName"){
    dt[ (get(ph)   < c(7.33)), (apache_mf) := 2]
  }
  dt[is.na(apache_mf)  & (get(hco3) < c(22))  , (apache_mf) := 2]

  # APACHE = 3
  if (format != "shortName"){
    dt[(get(ph)   < c(7.25)) | (get(ph)   > c(7.59)), (apache_mf) := 3]
  }
  dt[(is.na(apache_mf)) & (((get(hco3) < c(18)))  | (get(hco3) > c(40))) , (apache_mf) := 3]

  # APACHE = 4
  if (format != "shortName"){
    dt[ (get(ph)   < c(7.16)) | (get(ph)   > c(7.69)), (apache_mf) := 4]
  }
  dt[(is.na(apache_mf)) & ((get(hco3) < c(16))  | (get(hco3) > c(51))) , (apache_mf) := 4]



  # Calculate APACHE score for time window
  dt[time %between% window, (apache_mf) := max(w_apache_mf, na.rm = T), by = c("site", "episode_id")]
  dt[, w_apache_mf := NULL]

}

