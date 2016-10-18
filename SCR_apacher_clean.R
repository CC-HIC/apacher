
# author: Arthur Le Gall
# date: 2016-05-23
# subject: Calculate APACHE II score

# Readme
# ======
# See issue https://github.com/UCL-HIC/paper-brc/issues/15

# Function should be passed a (one row per time-point per episode/patient 2d data)
# Function will expect that the data has been _pre_ aggregated


# Todo
# ====
#
# -- To create functions to calculate separately each components of the APACHE severity score:   %%% DONE
#     -- Hemodynamic Failure          %%% DONE
#     -- Respiratory Failure          %%% DONE
#     -- Metabolic Failure            %%% DONE
#     -- Renal Failure                %%% DONE
#     -- Neurologic Failure           %%% DONE
#     -- Respiratory rate             %%% DONE
#     -- Leucocytes count             %%% DONE
#     -- Heart rate                   %%% DONE
#     -- Temperature                  %%% DONE
#     -- Sodium                       %%% DONE
#     -- Potassium                    %%% DONE
#
# -- To create function to calculate Chronic Health Status:                                      %%% DONE
#
# -- To create function to take into account the age in the calculation:                         %%% IN PROGRESS
#     -- Age should be derived from a previous work.
#
# -- To create function to calculate the predicted mortality :
#
# -- To create Helper Functions:
#     -- To create an helper function for the comorbidity score calculation:                     %%% FUNCTIONNAL
#           - Plan to include a sum of comorbidity and a binary comorbidity
#           - IMPROVEMENTS: To include warnings if mandatory fields are missing
#
#     -- To create a function to prioritize the field to choose for computation:                 %%% DONE
#           - Respiratory Rate                                                                   %%% DONE
#           - Metabolic Failure
#
#     -- To create a funcion to determine the parameter to use to calculate rf score             %%% FUNCTIONNAL
#         - Depending on the FiO2
#                 - FiO2 < 0.5 : PaO2 is the considered variable
#                 - Fio2 >= 0.5 : Alveolo-arterial gradient [ Grad.(A-a) ] is the considered variable
#         - IMPROVEMENTS: To rewrite variables inside the function to make it more readable     %%% DONE
#
#     -- To create a function to determine the parameter to use to calculate metabolic failure  %%% DONE
#         - Depending on the availability of the pH variable
#
#     -- To create a function to determine the parameter to use to calculate renal failure      %%% DONE
#           - Depending on the AKI presence or absence
#
# -- To test each function to track some error in the apache score calculation                  %%% DONE
#
# -- To clean the SCR_APACHE.R script to make it readable into the safe heaven
#




# Log
# ===
# 2016-05-10
# - file created


# ----------------------------------------------------------------------------------------------------------
# Items and function needed for calculation of APACHE score:
# ===
# 2016-05-23

# Functions and Libraries ----------------------------------------------------------------------------------
library(ccdata)
library(ccfun)
library(devtools)
library(assertthat)
library(apacher)
library(BlandAltmanLeh)

# Path and setup -------------------------------------------------------------------------------------------
# Save the workspace
wdbackup <- getwd()

# Defining the Path to Find the Yaml file
DictionaryPath <- "G:/UCLH/paper-brc/data/"
Dictionary <- "ANALYSIS_APACHE.yaml"

# Defining the Path to Find the Data Environment
DataPath <- "G:/UCLH/paper-brc/data/"
DataName <- "delta_num.Rdata"

# Naming the Datatable
data <- "dt"

# Renaming column of the DataTable
Renaming <- "yes"
ColInput <- "NHICcode"
ColOutput <- "dataItem"


# Dataset Management-----------------------------------------------------------------------------------------
# Loading environment
if (!exists("ccd_delta_num")){
  load(paste(DataPath,DataName, sep = ""))
}

# Creating the DataTable
data <- create2dclean(ccd_delta_num, paste(DictionaryPath,Dictionary, sep = ""), nchunks=5)

# Free memory space
rm(ccd_delta_num)
gc()

# Translating the DataTable
ifelse(Renaming == "yes", relabel_cols(data,ColInput ,ColOutput), relabel_cols(data,ColInput ,ColInput))


# Vector of Comorbidities
Comorbidities             <- c("Portal hypertension",
                               "Heaptic encephalopathy",
                               "Very severe cardiovascular disease",
                               "Chronic myelogenous /lymphocytic leukaemia",
                               "Steroid treatment",
                               "Acute myeloid/lymphocytic leukaemia or myeloma",
                               "Severe respiratory disease",
                               "Congenital immunohumoral or cellular immune deficiency state",
                               "Chronic renal replacement therapy",
                               "Biopsy proven cirrhosis",
                               "Home ventilation",
                               "Radiotherapy",
                               "HIV/AIDS",
                               "Lymphoma",
                               "Chemotherapy (within the last 6months) steroids alone excluded")

# Transform strings data as numeric data in the fields requested for chronic evaluation
data[,  (Comorbidities) := lapply(data[, Comorbidities, with = F], as.numeric)]

# APACHE Chronic
data <- gen_comorbidity(dt = data, fields = Comorbidities)
data <- gen_apache_chronic(dt = data)

# APACHE Age
age <- "age"
data <- data[time %between% c(0,24), (age):= rnorm(length(site), mean = 60, sd = 15), by = c("site", "episode_id", "apache_chronic")]
data <- gen_apache_age(dt = data, window = c(0,24))

# APACHE Respiratory rate
data <- gen_q_rr(dt = data, qual = c(5,60))
data <- gen_apache_rr(dt = data, window = c(0,24))

# APACHE Temperature
data <- gen_apache_temp(dt = data, window = c(0,24))

# APACHE Mean Arterial Pressure
data <- gen_apache_map(dt = data, window = c(0,24))

# APACHE Heart rate
data <- gen_apache_hr(dt = data, window = c(0,24))

# APACHE Respiratory failure
data <- gen_grad(dt = data)
data <- gen_apache_rf(dt = data, window = c(0,24))

# APACHE Metabolic failure
data <- gen_apache_mf(dt = data, window = c(0,24))

# APACHE Sodium
data <- gen_apache_Na(dt = data, window = c(0,24))

# APACHE Potassium
data <- gen_apache_K(dt = data, window = c(0,24))

# APACHE Acute Kidney Injury
data <- gen_apache_aki(dt = data, crrt = "Chronic renal replacement therapy", window = c(0,24))

# APACHE Hematocrit
gen_haemo(dt = data, mcch = NULL)
data <- gen_apache_ht(dt = data, window = c(0,24))

# APACHE Leucocytes
data <- gen_apache_wbc(dt = data, window = c(0,24))

# APACHE Glasgow Coma Scale
data <- gen_apache_gcs(dt = data, window = c(0,24))

# Global Apache Score
gen_apache_score(dt = data)

# Summarize the table
data <- reduce(dt = data, window = c(0,24))

# Predicted Mortality
  # Load ICNARC Dictionnary
    Path <- "G:/UCLH/SavedCodeArthur/dc.ap2.csv"
    apache <- as.data.table(read.csv(Path, sep = ",", header = T, na.strings = "", stringsAsFactors = F))

  # To format diagnosis as dictionnary format
    diagfirst <- strsplit(data[,`Primary reason for admission to your unit`], "[.]")
    diagfirst <- lapply(diagfirst, function(x){x <- as.character(as.numeric(x))
    x <- paste(x, collapse = ".")})
    first_diag <- "first_diag"
    data[, (first_diag) := unlist(diagfirst)]

  # To merge apache table by icnarc diagnosis
    data <- merge(x = data, y = apache, by.x = "first_diag", by.y = "icnarc.dc", all.x = T, all.y = F)

  # To specify the weights for apache score regarding the nature of the admission, and the nature emergent of the surgery
    data[(`adm.type` %in% c("Surgical") & `classification of surgery` %in% c("M", "U")), `:=`(`emergent_surgery` = "Yes")]
    data[(`adm.type` %in% c("Surgical") & `classification of surgery` %in% c("S", "L")), `:=`(`emergent_surgery` = "No")]
    data[(`adm.type` %in% c("Medical")), `:=`(`emergent_surgery` = "No")]
    data[(`emergent_surgery` == "No" &
            `Primary reason for admission to your unit` %in% grep("^1.",`Primary reason for admission to your unit`, value = T) &
            `classification of surgery` %in% c("M", "U")), `:=`(`emergent_surgery` = "Yes")]
    data[(is.na(`emergent_surgery`) &
            `Primary reason for admission to your unit` %in% grep("^1.",`Primary reason for admission to your unit`, value = T) &
            `classification of surgery` %in% c("M", "U")), `:=`(`emergent_surgery` = "Yes")]
    data[(is.na(`emergent_surgery`) &
            `Primary reason for admission to your unit` %in% grep("^1.",`Primary reason for admission to your unit`, value = T) &
            `classification of surgery` %in% c("S", "L")), `:=`(`emergent_surgery` = "No")]
    data[(is.na(`emergent_surgery`) &
            (`Primary reason for admission to your unit` %in% grep("^2.",`Primary reason for admission to your unit`, value = T))), `:=`(`emergent_surgery` = "No")]

    data[`Primary reason for admission to your unit` %in% grep("^1.",`Primary reason for admission to your unit`, value = T), weight := `AP2.coef.nonsurg`]
    data[`Primary reason for admission to your unit` %in% grep("^2.",`Primary reason for admission to your unit`, value = T), weight := `AP2.coef.surg`]
    data[, weight := as.numeric(weight)]

  # To calculate Risk of death
    data <- gen_apache_pred(dt = data, window = c(0,24))
  # To prepare comparison between calculated and given risk of death
    data[, `APACHE II Probability` := `APACHE II Probability`*0.01]

# To create the SMR variable
  # To transform the dead or alive variable into binary variable
    data[`Dead or alive on discharge` != "D" & `Dead or alive on discharge` != "NA", status:= 0]
    data[`Dead or alive on discharge` == "D", status:= 1]

  # To define groups to compare
    Haemo <- "Haemo"
    data[grep("^2.9", `Primary reason for admission to your unit`), (Haemo) := 1]
    data[!grep("^2.9", `Primary reason for admission to your unit`), (Haemo) := 0]
    data[is.na(`Haemo`), (Haemo) := 0]


#######################################
#   Statistics on those data

# 1- Descriptive

#     Plot the distributions

# Apache Score Calculated vs Given
h = hist(data[, apache_score])
h$density = h$count/sum(h$count)*100
plot(h, freq = F, ylab = "Percentage", ylim = c(0,80), xlab = "Apache Score", new = T, main = "Frequency according to Calculated Apache Score")

h = hist(data[, `APACHE II Score`])
h$density = h$count/sum(h$count)*100
plot(h, freq = F, ylab = "Percentage", ylim = c(0,80), xlab = "Apache Score", new = T, main = "Frequency according to Given Apache Score")

# Predicted death Calculated vs Given
h = hist(data[, `risk`])
h$density = h$count/sum(h$count)*100
plot(h, freq = F, ylab = "Percentage", ylim = c(0,80), xlab = "Predicted Risk", new = T, main = "Frequency according to Calculated Risk of death")

h = hist(data[, `APACHE II Probability`])
h$density = h$count/sum(h$count)*100
plot(h, freq = F, ylab = "Percentage", ylim = c(0,80), xlab = "Predicted Risk", new = T, main = "Frequency according to Given Risk of death")

# Relationship between Calculated and Given APACHE score
y = data[, apache_score]
x = data[, `APACHE II Score`]
red  <- (y - min(y, na.rm = T)) / (max(y, na.rm = T) - min(y, na.rm = T))
red <- ifelse(is.na(red), 0, red)

par(fig=c(0, 0.6, 0, 0.8), new=F)
plot(x = data[, apache_score], y = data[, `APACHE II Score`], ylab="Given APACHE score",
     xlab="Calculated APACHE score", col=rgb(red, 0.5, 0.25, 0.8), pch = 19, xlim = c(0,60), ylim = c(0,60))
model1 <- lm(data[, apache_score] ~ data[, `APACHE II Score`])
abline(model1, lty = 2, lwd = 2, col = "darkgrey")
abline(v = data[, median(`apache_score`, na.rm = T)], lty = 2, lwd = 1, col = "darkgrey")
abline(h = data[, median(`APACHE II Score`, na.rm = T)], lty = 2, lwd = 1, col = "darkgrey")

par(fig=c(0, 0.6, 0.35, 1), new=TRUE)
boxplot(data[, `apache_score`], horizontal=TRUE, axes=F, width = 1.5, ylim = c(0, 60))
par(fig=c(0.35, 0.8, 0, 0.8),new=TRUE)
boxplot(data[, `APACHE II Score`], axes=F, width = 1.5, ylim = c(0, 60))
res <- matrix(NA, ncol = 2, nrow = 1, dimnames = list(c("model"), c("a", "b")))
res[1,] <- c(round(coef(model1)[[2]], 2), round(coef(model1)[[1]], 2))


# Concordance between given and calculated APACHE score
# Bland & Altman
par(mfrow = c(1,1))
bland.altman.plot(data[, apache_score], data[, `APACHE II Score`], graph.sys = "ggplot2", xlim = c(0,60), ylim = c(0,60))

# Comparison between Calculated and given APACHE Score
res <- matrix(NA, ncol = 3, nrow  = 2, dimnames = list(c("APACHE", "Risk"), c("Calculated", "Given", "pval")))
res[1, ] <- c(paste(round(median(data[, apache_score], na.rm = T),0), " [", summary(data[, apache_score])[[2]],
                    " - ", summary(data[, apache_score])[[5]], "] ", sep = ""),
              paste(round(median(data[, `APACHE II Score`], na.rm = T),0), " [", summary(data[, `APACHE II Score`])[[2]],
                    " - ", summary(data[, `APACHE II Score`])[[5]], "] ", sep = ""),
              paste(round(wilcox.test(data[, `APACHE II Score`], data[, apache_score])[[3]], 3))
)
res[2, ] <- c(paste(round(median(data[, risk], na.rm = T), 2), " [", summary(data[, risk])[[2]],
                    " - ", summary(data[, risk])[[5]], "] ", sep = ""),
              paste(round(median(data[, `APACHE II Probability`], na.rm = T), 2), " [", round(summary(data[, `APACHE II Probability`])[[2]],2),
                    " - ", round(summary(data[, `APACHE II Probability`])[[5]], 2), "] ", sep = ""),
              paste(round(wilcox.test(data[, `APACHE II Probability`], data[, risk])[[3]], 3))
)


# Work on Heamo and non Heamo population

# Description of Heamo vs non Heamo
# APACHE
boxplot(data[, apache_score]~data[, Haemo], outline = F,
        names = c("Control", "Hematology"), ylab = "Calculated Apache Score", )
title ("APACHE according to the diagnostic" )

boxplot(data[, risk]~data[, Haemo], outline = F,
        names = c("Control", "Hematology"), ylab = "Calculated Risk", )
title ("Prerdicted Death according to the diagnostic" )

boxplot(data[, `APACHE II Score`]~data[, Haemo], outline = F,
        names = c("Control", "Hematology"), ylab = "Given Apache Score", ylim = c(0,40) )
title ("APACHE according to the diagnostic")

boxplot(data[, `APACHE II Probability`]~data[, Haemo], outline = F,
        names = c("Control", "Hematology"), ylab = "Given Risk", ylim = (c(0,1)))
title ("Predicted Death according to the diagnostic" )

res <- matrix(NA, ncol = )
# Standardized mortality ratio between Hamo and non Haemo
# Using APACHE Calculation
res <- gen_smr(dt = data, pred= "risk", observed = "status", group = "Haemo")

# Using Given APACHE
res <- gen_smr(dt = data, pred= "APACHE II Probability", observed = "status", group = "Haemo")














