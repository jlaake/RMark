\name{collect.model.names}
\alias{collect.model.names}
\title{Collect names of MARK model objects from list of R objects (internal
function)}
\usage{
  collect.model.names(lx, type = NULL, warning = TRUE)
}
\arguments{
  \item{lx}{vector of R object names from parent.frame(2)}

  \item{type}{either NULL (for all types) or a character
  model type (eg "CJS")}

  \item{warning}{if TRUE warning given when models of
  different types are collected}
}
\value{
  model.list: a vector of \code{mark} model names
}
\description{
  Either names of all \code{mark} model objects
  (\code{type=NULL}) or names of \code{mark} model objects
  of a specific type (\code{type}) are extracted from a
  vector of R objects (\code{lx}) that was collected from
  the parent environment (frame) of the function that calls
  \code{collect.model.names}. Thus, it is two frames back
  (parent.frame(2)).
}
\details{
  If \code{type=NULL} then the names of all objects of
  \code{class(x)[1]="mark"} in \code{lx} are returned.  If
  \code{type} is specified, then the names of all objects
  of \code{class(x)=c("mark",type)} in \code{lx} are
  returned.

  This function was written with the intention that it
  would be called from other functions ( e.g.,
  \code{\link{collect.models}}, \code{\link{run.models}})
  but it will work if called directly (e.g.,
  \code{collect.model.names( lx=ls())}). While this
  function returns a vector of model names,
  \code{\link{collect.models}} returns a list of model
  objects.  The latter can be used to easily create a list
  of models created in a function to be used as a return
  value without listing all the names of the functions.  It
  uses \code{collect.model.names} to perform that function.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{collect.models}}, \code{\link{run.models}},
  \code{\link{model.table}}
}
\keyword{utility}

