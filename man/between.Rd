\name{between}
\alias{between}
\title{Find Values betwwen upper and lower bounds}
\description{Finds values between provided upper and lower bounds}
\usage{between(in.data, ge = NULL, lt = NULL, gt = NULL, le = NULL)}
\arguments{
  \item{in.data}{Numeric vector to locate values that lie within specified range}
  \item{ge}{Numeric value indicating greater than or equal to lower bound}
  \item{lt}{Numeric value indicating less than upper bound}
  \item{gt}{Numeric value indicating greater than lower bound}
  \item{le}{Numeric value indicating less than or equal to upper bound}
}
\value{A logical vector the same length as the input, where TRUE indicates whether or not the value falls between the upper and lower bounds}
\author{Timothy R. Koscik <timkoscik+rmisc@gmail.com>}