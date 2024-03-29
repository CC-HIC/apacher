% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_apache_mf.R
\name{gen_apache_mf}
\alias{gen_apache_mf}
\title{Generates the APACHE Metabolic Failure score}
\usage{
gen_apache_mf(dt, window = c(0, 24))
}
\arguments{
\item{dt}{data.table containing physiology data}

\item{window}{Numerical.Vector delimiting boundaries for time-window.}
}
\description{
Generates the APACHE Metabolic Failure score;
}
\examples{
ddata <- NULL
ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
ddata <- as.data.table(ddata)
ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
ddata[, ("pH") := sample(seq(6.0,8.0,0.01), 200, replace = T)]
ddata[, ("HCO3 - ABG / VBG") := sample(seq(5,50,0.1), 200, replace = T)]
system.time(gen_apache_mf(ddata, window = c(0,24)))
ddata[time \%between\% c(0,24), .N, by = c("site","episode_id", "apache_mf")]

}

