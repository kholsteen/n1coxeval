% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data_cox.R
\name{generate_data_cox}
\alias{generate_data_cox}
\title{Generate data based on Cox PH with time-varying covariates}
\usage{
generate_data_cox(n.subj = 1, n.vars = 10, var.types, beta = rep(0,
  n.vars), rho = rep(0, n.vars), freq = rep(0.5, n.vars),
  max.n.days = NA, n.events = 50, t.min = 0, t.max = 60,
  median.days.btwn.events = 7, wb.shape = 1,
  round_event_times = FALSE, verbose = FALSE)
}
\arguments{
\item{n.subj}{number of subjects to generate}

\item{n.vars}{number of predictor variables to generate}

\item{var.types}{types of predictor variables to generate}

\item{beta}{(numeric vector) true coefficient values. Length `n.vars`}

\item{rho}{(numeric vector) autocorrelation coefficient (unused)}

\item{freq}{(numeric vector) frequency of dichotomous var}

\item{max.n.days}{(integer)}

\item{n.events}{(integer) number of outcome events to generate}

\item{t.min}{(integer) minimum time-to-event}

\item{t.max}{(integer) maximum time-to-event (must be greater than `t.min`)}

\item{median.days.btwn.events}{(integer) target median days between events
(time-to-event)}

\item{wb.shape}{Weibull shape parameter}

\item{round_event_times}{(Boolean) TRUE = round event times up to the nearest
integer, FALSE = keep exact event times}

\item{verbose}{(Boolean) TRUE = print progress, FALSE = print nothing}
}
\value{
dataset for analysis
}
\description{
Main function to generate the dataset based on Cox PH model.
            Algorithm from Hendry (2014) with guidelines from Montez-Rath (2017)
}
\references{
Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.

Montez-Rath ME, Kapphahn K, Mathur MB, Mitani AA, Hendry DJ, Desai M. Guidelines for generating right-censored outcomes from a Cox model extended to accommodate time-varying covariates. J Mod Appl Stat Methods. 2017;16:86-106.
}
\keyword{internal}
