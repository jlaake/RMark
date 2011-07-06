\name{store}
\alias{store}
\alias{restore}
\title{Store models externally or restore to workspace from external storage}
\description{
Stores/restores all mark model objects in a marklist either to or from external storage.
}
\usage{
store(x)
restore(x)
}
\arguments{
  \item{x}{marklist of models}
}
\details{
For \code{store}, each mark model is stored externally and the object in the list is replaced with the
filename of the object. \code{restore} does the opposite of storing the saved external object into the
marklist and then deleting the saved file.
}
\value{
A modified marklist to replace the previous marklist specified as the argument.
}
\author{Jeff Laake}
\keyword{utility}
