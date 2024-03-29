% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_apache_hr.R
\name{gen_apache_hr}
\alias{gen_apache_hr}
\title{Generates the APACHE Heart Rate score}
\usage{
gen_apache_hr(dt, window = c(0, 24))
}
\arguments{
\item{dt}{data.table containing physiology data}

\item{window}{Numerical.Vector delimiting boundaries for time-window.}
}
\description{
Generates the APACHE Heart Rate score;
}
\examples{
ddata <- NULL
hr <- "Heart rate"
ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
ddata <- as.data.table(ddata)
ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
ddata[, ("Heart rate") := sample(seq(30,120,1), 200, replace = T)]
system.time(gen_apache_hr(ddata, window = c(0,24)))
ddata[time \%between\% c(0,24), .N, by = c("site","episode_id", "apache_hr")]

}

