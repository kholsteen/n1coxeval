% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate-time-transform.R
\name{generate_time_transform}
\alias{generate_time_transform}
\title{Generate the time transformation g() and g^-1()}
\usage{
generate_time_transform(weibull_shape = 2, beta = NULL,
  median_event_time = NULL, x.list = NULL, verbose = FALSE)
}
\arguments{
\item{weibull_shape}{(numeric) Weibull distribution shape parameter; shape > 1
means hazard is increasing, 1 = constant, and <1 is decreasing.}

\item{beta}{(numeric vector) Coefficient vector, used to generate the weibull
scale parameter}

\item{median_event_time}{(integer) target median days between events
(time-to-event)}

\item{x.list}{(list of tbls) Output from generate_pred_wrapper, need when
use_weibull = TRUE, to generate the weibull scale parameter}

\item{verbose}{(Boolean) print calculated Weibull scale (TRUE) or not (FALSE)}
}
\value{
list with two components: functions `g.inv` and `g`
}
\description{
As described in Hendry (2013) and Montez-Rath (2017),
generate a function `g` and its inverse. The inverse represents the process
of interest as the cumulative baseline hazard.
}
\note{
Relying on the Weibull scale calculation from Montez-Rath (2017) did
not work well for our parameters. Figured out approximate method for this
specific case but future versions will need to update and systematize to
make the generated median event times reliable in other parameter settings.
}
\references{
Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.

Montez-Rath ME, Kapphahn K, Mathur MB, Mitani AA, Hendry DJ, Desai M. Guidelines for generating right-censored outcomes from a Cox model extended to accommodate time-varying covariates. J Mod Appl Stat Methods. 2017;16:86-106.
}
\keyword{internal}
