% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_input_path.R
\name{check_input_path}
\alias{check_input_path}
\title{Checking correctness of parameter describing input file's location}
\usage{
check_input_path(file, paramName = "")
}
\arguments{
\item{file}{value of a parameter that should be checked}

\item{paramName}{optionally name of a parameter (to provide informative
error messages)}
}
\value{
Function returns \code{TRUE} (if no problems are detected).
}
\description{
Simple routine developed to check correctness of parameters
that should desribe paths to input files
}
\details{
Function throws an error if it detects problem.
}
