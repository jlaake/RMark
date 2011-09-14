\name{run.models}
\alias{run.models}
\title{Runs a set of MARK models}
\usage{
  run.models(model.list = NULL, type = NULL, save = TRUE,
  ...)
}
\arguments{
  \item{model.list}{either a vector of model names or NULL
  to run all MARK models possibly of a particular
  \code{type}}

  \item{type}{either a model type (eg "CJS", "Burnham" or
  "Barker") or NULL for all types}

  \item{save}{if TRUE, the R data directory is saved (i.e.,
  \code{save.image()}) between analyses to enable
  interruption without losing analyses that have already
  been run}

  \item{...}{any additional parameters to be passed to
  \code{\link{run.mark.model}}}
}
\value{
  None; models are stored in parent frame.
}
\description{
  Runs either a collection of models as defined in
  \code{model.list} or runs all defined MARK object models
  in the frame of the calling function with no
  \code{output} (\code{model.list=NULL}) or just those of a
  particular type (e.g., \code{type="CJS"})
}
\details{
  The model names in \code{model.list} must be in the frame
  of the function that calls \code{run.models}. If
  \code{model.list=NULL} or the MARK models are collected
  from the frame of the calling function (the parent). If
  \code{type} is specified only the models of that type
  (e.g., "CJS") are run. In each case the models are run
  and saved in the parent frame.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{collect.model.names}},
  \code{\link{run.mark.model}}
}
\keyword{utility}

