\name{release.gof}
\alias{release.gof}
\title{Runs RELEASE for goodness of fit test}
\description{
Creates input file for RELEASE with the specified data, runs RELEASE and extracts the
summary results for TEST2 and TEST3. Output file is named Releasennn.tmp where nnn is an increasing
numeric value to create a unique filename.}
\usage{
release.gof(data,invisible=TRUE,title="Release gof",view=FALSE)
}
\arguments{
  \item{data}{processed RMark data}
  \item{invisible}{if TRUE, RELEASE run window is hidden from view}
  \item{title}{title for output}
  \item{view}{if TRUE, shows release output in a viewer window}
}
\value{
results: a dataframe giving chi-square, degrees of freedom and P value
for TEST2, TEST3 and total of tests
}
\examples{
data(dipper)
dipper.processed=process.data(dipper,groups=("sex"))
# The following is commented out because it does not run
# under check for some reason; very weird error lurking somewhere
#release.gof(dipper.processed)
}
\author{Jeff Laake}
\keyword{utility}
