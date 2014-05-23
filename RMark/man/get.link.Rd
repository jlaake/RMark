\name{get.link}
\alias{get.link}
\title{Compute sets of link values for real parameters}
\usage{
get.link(model, parameter, beta = NULL, design = NULL, data = NULL,
  vcv = FALSE)
}
\arguments{
  \item{model}{MARK model object}

  \item{parameter}{type of parameter in model (character)
  (e.g.,"Phi")}

  \item{beta}{values of beta parameters for computation of
  link values}

  \item{design}{a numeric design matrix with any covariate
  values filled in with numerical values}

  \item{data}{covariate data to be averaged for estimates
  if design=NULL}

  \item{vcv}{if TRUE computes and returns the v-c matrix of
  the subset of the link values}
}
\value{
estimates: If \code{vcv=TRUE}, a list is returned with
elements \code{vcv.link} and the dataframe
\code{estimates}. If \code{vcv=FALSE}, only the estimates
dataframe is returned which has the same structure as in
\code{\link{get.real}}.
}
\description{
Computes link values for real parameters for a particular
type of parameter (parameter) and returns in a table
(dataframe) format.
}
\details{
This function is very similar to \code{\link{get.real}}
except that it provides estimates of link values before
they are transformed to real estimates using the
inverse-link.  Also, the value is always a dataframe for
the estimates and design data and optionally a
variance-covariance matrix. See \code{\link{get.real}} for
further details about the arguments.
}
\author{
Jeff Laake
}
\seealso{
\code{\link{compute.link}},\code{\link{get.real}}
}
\keyword{utility}

