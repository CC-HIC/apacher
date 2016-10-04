#' @title Generates the APACHE White Blood Cell Count score
#'
#' @description
#' Generates the APACHE wbc score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param format Strings. The format that have been chosen by the users at naming the fields of the datatable.
#' see relabelcols for more informations.
#'
#' @examples
#' # system.time(gen_apache_wbc(ddata, format = "dataItem"))
#' # table(ddata$apache_wbc, useNA="always")


#' @export


gen_smr <- function(dt, pred, observed, group){

  smr <- "smr"

  # Prioritize the value to take into account for the acute kidney injury
  dt[, smr := (sum(get(observed), na.rm = T)/length(which(!is.na(get(observed))))/get(pred)), by = group]

}


