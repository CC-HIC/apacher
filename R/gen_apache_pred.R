#' @title Generates the predicted mortality frome the APACHE score
#'
#' @description
#' Generates the predicted mortality using the formula given by Knaus et al.
#' \describe{This function needs:}{
#'    \itemize{
#'      \item The results of the apache_score function
#'      \item A weight for the primary diagnostic for admission
#'      \item A Medical or Post-operative condition for admission}}
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param window Numerical.Vector delimiting boundaries for time-window.
#'
#' @examples
#' ddata <- NULL
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("apache_score") := sample(seq(0,60,1), 200, replace = T)]
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("weight") := sample(seq(-0.5, 1.75, 0.01), 200, replace = T), by = c("site", "episode_id")]
#' ddata[, ("emergent") := sample(c("Medical", "Post-operative"), 200, replace = T), by = c("site", "episode_id")]
#' gen_apache_pred(ddata, window = c(0,24))
#' ddata[time %between% c(0,24), .N, by = c("site", "episode_id", "risk")]
#'
#' @export

gen_apache_pred <- function(dt, window){

  # Naming  the apache_score
  apache_pred <- "apache_pred"

  # Display a warning if fields are missing
  if (!match("apache_score", names(dt)) != F ){
    stop( paste("?Please, compute APACHE score First"))
  }

  if (!match("weight", names(dt)) != F ){
    stop( paste("?Please, report diagnoses wheight on the dataset"))
  }

  if (!match("emergent", names(dt)) != F ){
    stop( paste("?Please, label the admission data by post-operative or medical"))
  }


  # The predicted mortality is calculated according to the following equation:
  # ln(R/(1-R)) = -3.157 + (apache_score * 0.146) + (0.603 for emergent surgery) + Diagnostic category weight
  dt[time %between% window & `emergent` == "Medical" , "R/1-R" := exp(-3.157 + `apache_score` * 0.146 + `weight`)]
  dt[time %between% window & `emergent` == "Post-operative" , "R/1-R" := exp(-3.157 + `apache_score` * 0.146 + 0.603 + `weight`)]

  dt[time %between% window , "risk" := (`R/1-R`/(1+`R/1-R`))]

}


