% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_apache_ht.R
\name{gen_apache_ht}
\alias{gen_apache_ht}
\title{Generates the APACHE Hematocrit score}
\usage{
gen_apache_ht(dt, window = c(0, 24))
}
\arguments{
\item{dt}{data.table containing physiology data}

\item{window}{Numerical.Vector delimiting boundaries for time-window.}
}
\description{
Generates the APACHE ht score;
}
\examples{
ddata <- NULL
d_ht <- "d_ht"
ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
ddata <- as.data.table(ddata)
ddata[ , ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
ddata[ , (d_ht) := sample(seq(0.10,0.7,0.01), 200, replace = T)]
ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
system.time(gen_apache_ht(ddata, window = c(0,24)))
ddata[time \%between\% c(0,24), .N, by = c("site","episode_id", "apache_ht")]

}

