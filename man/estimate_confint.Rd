% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_confint.R
\name{estimate_confint}
\alias{estimate_confint}
\title{Profile Likelihood Confidence Interval}
\usage{
estimate_confint(mod, varname, data = NA)
}
\arguments{
\item{mod}{Model object (of class `coxph` or `coxphf`)}

\item{varname}{(string) Predictor variable}

\item{data}{(tbl) dataset used in estimation; needed for profile likelihood
interval with `coxph`.}
}
\description{
Firth model returns confidence interval from original estimation;
other models (estimated with `coxph`) have profile CI separately estimated.
}
\keyword{internal}
