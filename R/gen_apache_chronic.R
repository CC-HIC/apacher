#' @title Generates the APACHE Chronic Health score
#'
#' @description
#' Generates the APACHE Chronic Health score; requires 15 comorbidity variables as inputs. A warning is
#' displayed in case of missing fields
#' \describe{This function requires the output of the helper function gen_comorbidity, and transform it into binary variable}
#' \describe{The function needs two fields into the datatable, related to the type of admission and the classification of surgery.
#' Items have to be coded by the Yaml dictionnary as follow: }
#' \describe{- Admission}{
#'    \itemize{
#'      \item L: unplanned Local admission ;
#'      \item U: Unplanned transfer in ;
#'      \item P: Planned transfer in ;
#'      \item S: planned local Surgical admission ;
#'      \item M: planned local Medical admission ;
#'      \item R: Repatriation.}}
#'  \describe{- Surgery}{
#'    \itemize{
#'      \item M: Emergency ;
#'      \item U: Urgent ;
#'      \item S: Scheduled ;
#'      \item L: eLective}}
#'
#' @import data.table
#' @param dt data.table containing physiology data
#'
#' @examples
#' dt <- NULL
#' dt$"Admission type" <- c(sample(c("L", "U", "P", "S", "M", "R"), 200, replace = T))
#' dt$"classification of surgery" <- c(sample(c("M", "U", "S", "L"), 200, replace = T))
#' dt$"d_comorbidity" <- c(sample(c(0, 1, 2, 3, 4, NA), 200, replace = T))
#' dt <- as.data.table(dt)
#' gen_apache_chronic(dt)
#' dt[, .N, by = b_comorbidity]
#' dt[, .N, by = apache_chronic]
#'
#' @export

gen_apache_chronic <- function(dt) {
  #  =========================================================
  #  = APACHE - Chronic Health Assessment and Admission Type =
  #  =========================================================
  # appending _ to var names for readability and to ensure uses scoped version
  # requires - At least one condition among:
  #                 - Chronic Heart Failure : NYHA 4
  #                 - Hepatic Disease : Biopsy Proven Cirrhosis ;
  #                                     Documented Portal Hypertension ;
  #                                     Upper Gastro-Intestinal Bleedings due to Portal Hypertension ;
  #                                     Hepatic encephalopathy or comatose episode.
  #                 - Chroncic respiratory insufficiency: Restrictive or Obstructive or Vascular Disease
  #                                                       with severe impairement of physical activities ;
  #                                                       Secondary Polycythemia ;
  #                                                       Documented Chronic Hypoxia or Hypercarbia ;
  #                                                       Severe Pulmonary Hypertension or
  #                                                       Long-Term Oxygenotherapy.
  #                 - Chronic Renal Replacement Therapy
  #                 - Imunosuppression :  Immunosuppressive Therapy ;
  #                                       Chemotherapy ;
  #                                       Radiotherapy ;
  #                                       Long Lasting Corticotherapy ;
  #                                       Oncohaematologic Disease ;
  #                                       AIDS.
  #            - AND one of the following criteria for admission type:
  #                 - 2: + Elective Surgery
  #                 - 5: + Medical Reason for Admission || + Emergent Surgery


  # Name the input/output
  b_comorbidity <- "b_comorbidity"
  apache_chronic <- "apache_chronic"

  # Update based on conditions
  # Order of conditions is IMPORTANT

  if ("d_comorbidity" %in% names(dt)){
    dt[, (b_comorbidity) := 0]
    dt[!is.na(`d_comorbidity`) & `d_comorbidity` > 0, (b_comorbidity) := 1]
    dt[!is.na(`d_comorbidity`) & `d_comorbidity` == 0, (b_comorbidity) := 0]
    dt[is.na(`d_comorbidity`), (b_comorbidity) := NA]
  }else{
    stop("derived comorbidity field is requested")
  }




  # APACHE = 0
  dt[, (apache_chronic) := 0]

  # APACHE = 2
  dt[b_comorbidity > 0 & (`Admission type` %in% c("S") | `classification of surgery` %in% c("S") | `classification of surgery` %in% c("L")) ,
     (apache_chronic) := 2]

  # APACHE = 5
  dt[b_comorbidity > 0 & (`Admission type` %in% c("M") | `Admission type` %in% c("L") | `classification of surgery` %in% c("U") | `classification of surgery` %in% c("M")),
     (apache_chronic) := 5]

  # APACHE = NA
  dt[is.na(`b_comorbidity`), (apache_chronic) := NA]

}

