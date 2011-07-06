\name{PIMS}
\alias{PIMS}
\title{Display PIM for a parameter}
\description{Extract PIMS for a particular parameter and display either the full PIM structure
or the simplified PIM structure.}
\usage{
PIMS(model, parameter, simplified=TRUE,use.labels=TRUE)}
\arguments{
  \item{model}{mark model object}
  \item{parameter}{character string of a particular type of parameter in the model (eg "p","Phi","pent","S")}
  \item{simplified}{if TRUE show simplified PIM structure; otherwise show full structure}
  \item{use.labels}{if TRUE, uses time and cohort labels for columns and rows respectively}
}
\value{
None
}
\author{Jeff Laake}
\seealso{\code{\link{make.design.data}} }
\examples{
data(dipper)
results=mark(dipper)
PIMS(results,"Phi")
PIMS(results,"Phi",simplified=FALSE)
}
\keyword{utility}
