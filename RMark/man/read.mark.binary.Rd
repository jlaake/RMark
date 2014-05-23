\name{read.mark.binary}
\alias{read.mark.binary}
\alias{read.mark.binary.linux}
\title{Reads binary file output from MARK and returns a list of the results}
\usage{
read.mark.binary(filespec)
}
\arguments{
  \item{filespec}{Filename specification for binary output
  file from MARK;named here as markxxx.vcv}
}
\value{
List of estimates, se, lcl, ucl and var-cov matrices for
beta, real and derived estimates \item{beta}{Dataframe for
beta parameters containing estimates, se, lcl, ucl}
\item{beta.vcv}{variance-covariance matrix for beta
estimates} \item{real}{Dataframe for real parameters
containing estimates, se, lcl, ucl}
\item{real.vcv}{variance-covariance matrix for real
estimates} \item{derived}{Dataframe for derived parameters
(if any) containing estimates, se, lcl, ucl}
\item{derived.vcv}{variance-covariance matrix for derived
estimates (if any)}
}
\description{
Window and linux versions to read binary files created by
MARK
}
\author{
Jeff Laake
}
\seealso{
\code{\link{extract.mark.output}}
}
\keyword{utility}

