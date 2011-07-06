\name{setup.model}
\alias{setup.model}
\title{Defines model specific parameters (internal use)}
\description{
Compares \code{model}, the name of the type of model (eg "CJS") to the list of acceptable models to
determine if it is supported and then creates some global fields specific to that type of model that are
used to modify the operation of the code.
}
\usage{
setup.model(model, nocc, mixtures = 1)
}
\arguments{
  \item{model}{name of model type (must be in vector \code{valid.models})}
  \item{nocc}{length of capture history string}
  \item{mixtures}{number of mixtures}
}
\details{
In general, the structure of the different types of models (e.g., "CJS","Recovery",...etc) are very similar
with some minor exceptions.  This function is not intended to be called directly by the user but it is documented
to enable other models to be added.  This function is called by other functions to validate and setup 
model specific parameters.  For example, for live/dead models, the length of the capture history is twice
the number of capture occasions and the number of time intervals equals the number of capture occasions because
the final interval is included with dead recoveries.  Whereas, for recapture models, the length of the capture
history is the number of capture occasions and the number of time intervals is 1 less than the number of 
occasions.  This function validates that the model is valid and sets up some parameters specific to the model
that are used in the code.
}
\value{
 model.list - a list with following elements
  \item{etype}{encounter type string for MARK input; typically same as model}
  \item{nocc}{number of capture occasions}
  \item{num}{number of time intervals relative to number of occasions (0 or -1)}
  \item{mixtures}{number of mixtures if any}
  \item{derived}{logical; TRUE if model produces derived estimates}
}
\author{Jeff Laake}
\seealso{ \code{\link{setup.parameters}}, \code{\link{valid.parameters}} }
\keyword{utility}
