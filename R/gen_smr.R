#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#'
#'
#' @examples
#' # system.time(gen_apache_wbc(ddata, format = "dataItem"))
#' # table(ddata$apache_wbc, useNA="always")


#' @export


gen_smr <- function(dt, pred, observed, group){

  smr <- "smr"

  # Prioritize the value to take into account for the acute kidney injury
  dt[, smr := (sum(get(observed), na.rm = T)/length(which(!is.na(get(observed))))/mean(get(pred)[which(!is.na(get(observed)))], na.rm = T)), by = group]
  res <- matrix(NA, ncol = 2, nrow = 1, dimnames = list("smr", c(levels(as.factor(data[, get(group)])))))
  res[1, ] <- dt[ , round(unique(smr), 2), by = get(group)][, V1]
  return(res)
}


