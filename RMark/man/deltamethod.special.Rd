\name{deltamethod.special}
\alias{deltamethod.special}
\title{Compute delta method variance for sum, cumsum, prod and cumprod functions}
\usage{
  deltamethod.special(function.name, mean, cov, ses = TRUE)
}
\arguments{
  \item{function.name}{Quoted character string of either
  "sum", "cumsum", "prod" or "cumprod"}

  \item{mean}{vector of estimates used in the function}

  \item{cov}{variance-covariance matrix of the estimates}

  \item{ses}{if TRUE it returns a vector of estimated
  standard errors of the function of the estimates and if
  FALSE it returns the variance-covariance matrix}
}
\value{
  either a vector of standard errors (ses=TRUE) or a
  variance-covariance matrix (ses=FALSE)
}
\description{
  This function computes the delta method std errors or v-c
  matrix for a sum, cumsum (vector of cummulative sums),
  prod (product), or cumprod(vector of cummulative
  products) of a set of estimates.
}
\details{
  This function computes the delta method std errors or v-c
  matrix for a sum, cumsum (vector of cummulative sums),
  prod (product), or cumprod(vector of cummulative
  products).  It uses the function deltamethod from the msm
  package and constructs the necessary formula for these
  special cases. It will load the msm pacakge but assumes
  that it has already been installed. See the msm
  documentation for a complete description on how the
  deltamethod function works.  If ses=TRUE, it returns a
  vector of std errors for each of functions of the
  estimates contained in mean.  If ses=F, then it returns a
  v-c matrix for the functions of the estimates contained
  in mean.  cov is the input v-c matrix of the estimates.
}
\examples{
\donttest{
#
# The following are examples only to demonstrate selecting different
# model sets for adjusting chat and showing model selection table.
# It is not a realistic analysis.
#
  data(dipper)
  mod=mark(dipper,model.parameters=list(Phi=list(formula=~time)))
  rr=get.real(mod,"Phi",se=TRUE,vcv=TRUE)
  deltamethod.special("prod",rr$estimates$estimate[1:6],rr$vcv.real)
  deltamethod.special("cumprod",rr$estimates$estimate[1:6],rr$vcv.real,ses=FALSE)
  deltamethod.special("sum",rr$estimates$estimate[1:6],rr$vcv.real)
  deltamethod.special("cumsum",rr$estimates$estimate[1:6],rr$vcv.real,ses=FALSE)
}
}
\author{
  Jeff Laake
}
\keyword{utility}

