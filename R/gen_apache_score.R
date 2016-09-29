#' @title Generates the APACHE Score
#'
#' @description
#' Generates the APACHE score from the calculated variables;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("apache_map") := sample(seq(1,4,1), 200, replace = T)]
#' ddata[, ("apache_hr") := sample(seq(1,4,1), 200, replace = T)]
#' ddata[, ("apache_ht") := sample(seq(1,4,1), 200, replace = T)]
#' ddata[, ("apache_rr") := sample(seq(1,4,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' gen_apache_score(ddata, window = c(0,24))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_score")]
#'
#'
#' @export

gen_apache_score <- function(dt, window) {
  #  ===============================
  #  =        APACHE - main        =
  #  ===============================
  # appending _ to var gcsmes for readability and to ensure uses scoped version
  # requires known or unknown chronic kidney disease status

  # library(data.table)
  # data.table changes the object in place unless you use dt1 <- copy(dt)
  # so passing data.tables via function is actually just passing a reference

  # Naming  the apache_score
  apache_score <- "apache_score"

  # Define the fields requested for full computation
  apache <- c("apache_age", "apache_chronic", "apache_rr", "apache_temp", "apache_map", "apache_hr", "apache_mf",
     "apache_gcs", "apache_wbc", "apache_ht", "apache_aki", "apache_K", "apache_Na", "apache_rf")

  # Display a warning if fields are missing
  if (length(which(is.na(match(apache, names(dt))))) > 0 ){
    warning( paste("Fields are missing for complete apache computation:", apache[which(is.na(!match(apache, names(dt))))], "\n"))
    apache <- apache[which(!is.na(match(apache, names(dt))))]
  }


  # Update based on conditions
  # Order of conditions is IMPORTANT

  # APACHE = sum of 11 physiologic variables + comorbidity variable + age variable
    # Set a reference value for APACHE score
    dt[, apache_score := 0]

    # Add each column to apache score:
    for (i in 1:length(apache)){
      dt[time %between% window , (apache_score) := (apache_score + get(apache[i])), by= c("site", "episode_id")]
    }

}

