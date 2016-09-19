\name{ci.se}
\alias{ci.se}
\title{Calculate confidence interval of mean}
\description{Calculate the confidence interval of the mean based on the standard error}
\usage{ci.se(data.in, conf.level, verbose = 0)}
\arguments{
  \item{data.in}{Data on which to calculate confidence intervals. Can be vector, data frame or a list of vectors and datarames}
  \item{conf.level}{Confidence interval, alpha}
  \item{verbose}{Logical indicating whether or not to display results in console.}
}
\value{
A matrix containing columns for: n, sample.mean, std.dev, std.error, conf.level, error.margin, sample.mean - error.margin, sample.mean + error.margin
}
\author{Timothy R. Koscik <timkoscik+rmisc@gmail.com>}