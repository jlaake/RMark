\name{model.average}
\alias{model.average}
\alias{model.average.default}
\title{Compute model averaged estimates}
\arguments{
  \item{x}{is either a list with a prescribed structure as
  defined in \code{\link{model.average.list}} or a
  \code{marklist} as described in
  \code{\link{model.average.marklist}}}

  \item{...}{additional arguments passed to specific
  functions}
}
\value{
  The structure of the returned value depends on which
  function is called.
}
\description{
  A generic function to compute model averaged estimates
  and their standard errors or variance-covariance matrix.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{model.average.marklist}},\code{\link{model.average.list}}
}
\keyword{utility}

