\name{var.components}
\alias{var.components}
\title{Variance components estimation}
\usage{
  var.components(theta, design, vcv, alpha = 0.05,
    upper = 10 * max(vcv), LAPACK = TRUE)
}
\arguments{
  \item{theta}{vector of parameter estimates}

  \item{design}{design matrix for combining parameter
  estimates}

  \item{vcv}{estimated variance-covariance matrix for
  parameters}

  \item{upper}{upper limit for process variance}

  \item{LAPACK}{argument passed to call to \code{qr} for qr
  decomposition and inversion}
}
\value{
  A list with the following elements \item{sigmasq}{process
  variance estimate and confidence interval; estimate may
  be <0} \item{sigma}{sqrt of process variance; set to o if
  sigmasq<0} \item{beta}{dataframe with estimates and
  standard errors of betas for design}
  \item{betarand}{dataframe of shrinkage estimates}
  \item{vcv.beta}{variance-covariance matrix for beta}
  \item{GTrace}{trace of matrix G}
}
\description{
  Computes estimated effects, standard errors and process
  variance for a set of estimates
}
\details{
  Computes estimated effects, standard errors and process
  variance for a set of estimates using the method of
  moments estimator described by Burnham and White (2002).
  The \code{design} matrix specifies the manner in which
  the estimates (\code{theta}) are combined.  The number of
  rows of the design matrix must match the length of
  \code{theta}.  To get a mean estimate use a column matrix
  of 1's (e.g.,
  \code{design=matrix(1,ncol=1,nrow=length(theta))}. The
  function returns a list with the estimates of the
  coefficients for the design matrix (\code{beta}) with one
  value per column in the design matrix and the
  variance-covariance matrix (\code{vcv.beta}) for the
  \code{beta} estimates. The process variance is returned
  as \code{sigma}.
}
\examples{
\donttest{
data(dipper)
md=mark(dipper,model.parameters=list(Phi=list(formula=~time)))
md$results$AICc
zz=get.real(md,"Phi",vcv=TRUE)
z=zz$estimates$estimate[1:6]
vcv=zz$vcv.real
varc=var.components(z,design=matrix(rep(1,length(z)),ncol=1),vcv)
df=md$design.data$Phi
shrinkest=data.frame(time=1:6,value=varc$betarand$estimate)
df=merge(df,shrinkest,by="time")
md=mark(dipper,model.parameters=list(Phi=list(formula=~time,fixed=list(index=df$par.index,value=df$value))),adjust=FALSE)
npar=md$results$npar+varc$GTrace
md$results$lnl+2*(npar + (npar*(npar+1))/(md$results$n-npar-1))
}
}
\author{
  Jeff Laake; Ben Augustine
}
\references{
  BURNHAM, K. P. and G. C. WHITE. 2002. Evaluation of some
  random effects methodology applicable to bird ringing
  data.  Journal of Applied Statistics 29: 245-264.
}

