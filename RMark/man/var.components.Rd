\name{var.components}
\alias{var.components}
\title{Variance components estimation}
\description{Computes estimated effects, standard errors and process variance for a set of estimates }
\usage{
var.components(theta,design,vcv, LAPACK=TRUE) }
\arguments{
  \item{theta}{vector of parameter estimates}
  \item{design}{design matrix for combining parameter estimates}
  \item{vcv}{estimated variance-covariance matrix for parameters}
  \item{LAPACK}{argument passed to call to \code{qr} for qr decomposition and inversion}
}
\details{
Computes estimated effects, standard errors and process variance for a set of estimates using the
method of moments estimator described by Burnham and White (2002). The \code{design} matrix
specifies the manner in which the estimates (\code{theta}) are combined.  The number
of rows of the design matrix must match the length of \code{theta}.  To get a mean
estimate use a column matrix of 1's (e.g., \code{design=matrix(1,ncol=1,nrow=length(theta))}.
The function returns a list with the estimates of the coefficients for the design matrix (\code{beta})
with one value per column in the design matrix and the variance-covariance matrix (\code{vcv.beta}) for the
\code{beta} estimates. The process variance is returned as \code{sigma}.
}

\value{A list with the following elements
\item{sigma}{process variance estimate}
\item{beta}{dataframe with estimates and standard errors of betas for design}
\item{vcv.beta}{variance-covariance matrix for beta}
}
\author{Jeff Laake}
\references{
BURNHAM, K. P. and G. C. WHITE. 2002. Evaluation of some random effects methodology applicable to bird ringing data. 
Journal of Applied Statistics 29: 245-264.
}
\examples{
data(dipper)
md=mark(dipper,model.parameters=list(Phi=list(formula=~time)))
zz=get.real(md,"Phi",vcv=TRUE)
z=zz$estimates$estimate[1:6]
vcv=zz$vcv.real
var.components(z,design=matrix(rep(1,length(z)),ncol=1),vcv) 
}
