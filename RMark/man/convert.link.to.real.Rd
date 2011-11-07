\name{convert.link.to.real}
\alias{convert.link.to.real}
\title{Convert link values to real parameters}
\usage{
  convert.link.to.real(x, model = NULL, links = NULL, fixed
  = NULL)
}
\arguments{
  \item{x}{Link values to be converted to real parameters}

  \item{model}{MARK model object}

  \item{links}{vector of character strings specifying links
  to use in computation of reals}

  \item{fixed}{vector of fixed values for real parameters
  that are needed for calculation of reals from mlogits
  when some are fixed}
}
\value{
  vector of real parameter values
}
\description{
  Computes real parameters from link values
}
\details{
  Computation of the real parameter from the link value is
  relatively straightforward for most links and the
  function \code{\link{inverse.link}} is used.  The only
  exception is parameters that use the \code{mlogit} link
  which requires the transformation across sets of
  parameters.  This is a convenience function that does the
  necessary work to convert from link to real for any set
  of parameters.  The appropriate links are obtained from
  \code{model$links} unless the argument \code{links} is
  specified and they will over-ride those in \code{model}.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{inverse.link}},\code{\link{compute.real}}
}
\keyword{utility}

