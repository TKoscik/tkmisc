\name{power.pro}
\alias{power.pro}
\title{Power proportion Method for correcting variables}
\description{}
\usage{
power.pro(df, var.adj, var.pro, a.start = 1, b.start = 1, restart.ab = TRUE, max.restart=1000, if.fail=c(1,0), rescale = TRUE, save.params = FALSE)
}
\arguments{
  \item{df}{Input dataframe containing variables to adjust and the variable to be used for correction}
  \item{var.adj}{A character vector indicating the names of the variables to adjust}
  \item{var.pro}{A string indicating which variable to use for the adjustment}
  \item{a.start}{A number indicating the starting value for the alpha parameter (linear term)}
  \item{b.start}{A number indicating the starting value for the beta parameter (beta term)}
  \item{restart.ab}{A logical value indicating whether or not to retry fitting the model with randomized starting values. Alpha and Beta values are independently randomized to be between 0 and 2 following a unifromdistribution, e.g., runif(1,0,2)}
  \item{max.restart}{A numeric value indicating the number of refitting attempts to make with random values (default = 1000)}
  \item{if.fail}{A numeric vector of length 2, with values to substitute for alpha and beta (in order), if fitting fails. if.fail=c(1,0) [default] results in no correction being applied. Alternatively if.fail=c(0,1) results in a linear correction}
  \item{rescale}{A logical indicating whether to rescale the power proportioned values back to the range of the original data, default=TRUE. Values are rescaled by normalizing adjusted values to their mean and standard deviation then rescaling them by the original mean and standard deviation}
  \item{save.params}{Default=FALSE, if false output is the original dataframe with adjusted values. If true, the output is a list with the adjusted dataframe, alpha and beta fit parameters, starting values and iterations.}
}
\value{If save.params=FALSE, output is the original dataframe with adjusted values. If save.params=TRUE, the output is a list with the adjusted dataframe, alpha and beta fit parameters, starting values and iterations}
\description{
For each variable, we fit a non-linear model where variables are predicted by both linear and power terms of the predictor, using the formula: y ~ alpha * x ^ beta. Initial starting values for alpha and beta are set to 1 (i.e., representing a linear relationship with a slope of 1). If values for alpha and beta are not fit, alpha and beta can be re-initialized using randomized starting points for each. Randomized starting values are selected from a uniform distribution between 0 and 2, if needed, and this randomization of the start values is repeated until the model was successfully fit (or max.restats is reached). After finding values of alpha and beta for each variable, variables are adjusted by dividing by ICV^beta. Finally, adjusted variables are rescaled to the original units by normalizing adjusted variables by their mean and standard deviation then rescaling them by the original mean and standard deviation.

Liu D, Johnson HJ, Long JD, Magnotta VA, Paulsen JS. The power-proportion method for intracranial volume correction in volumetric imaging analysis. Frontiers in neuroscience. 2014 Nov 6;8:356.
}
\author{
  Timothy R. Koscik <timkoscik+tkmisc@gmail.com>
}
\examples{}