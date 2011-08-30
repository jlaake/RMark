\name{model.average.list}
\alias{model.average.list}
\title{Compute model averaged estimates of real parameters from a list structure
for estimates}
\usage{
  \method{model.average}{list}(x, revised=TRUE,...)
}
\arguments{
  \item{x}{a list containing the following elements: 1)
  \code{estimate} - a vector or matrix of estimates, 2)a
  vector of model selection criterion value named
  \code{AIC,AICc,QAIC,QAICc} or a \code{weight} variable
  that sums to 1 across models, and 3) a vector or matrix
  named \code{se} which give the model-specific standard
  errors for each estimate or a list of matrices named
  \code{vcv} which give the model-specific
  variance-covariance matrices.}

  \item{revised}{if TRUE it uses eq 6.12 from Burnham and
  Anderson (2002) for model averaged se; otherwise it uses
  eq 4.9}

  \item{...}{additional arguments passed to specific
  functions}
}
\value{
  A list containing elements: \item{estimate}{vector of
  model-averaged estimates} \item{se}{vector of
  unconditional standard errors (square root of
  unconditional variance estimator)}
  \item{vcv}{model-averaged variance-covariance matrix if
  \code{vcv} was specified input list}
}
\description{
  A generic function to compute model averaged estimates
  and their standard errors or variance-covariance matrix
}
\details{
  If a single estimate is being model-averaged then
  \code{estimate} and \code{se} are vectors with an entry
  for each model.  However, if there are several estimatee
  being averaged then both \code{estimate} and \code{se}
  should be matrices in which the estimates for each model
  are a row in the matrix.  Regardless, if \code{vcv} is
  specified it should be a list of matrices and in the case
  of a single estimate, each matrix is 1x1 containing the
  estimated sample-variance but that would be rather
  useless and \code{se} should be used instead.

  If the list contains an element named
  \code{AIC},\code{AICc},\code{QAIC}, or \code{QAICc}, then
  the minimum value is computed and subtracted to compute
  delta values relative to the minimum. These are then
  converted to Akaike weights which are
  \code{exp(-.5*delta)} and these are normalized to sum to
  1.  If the list does not contain one of the above values
  then it should have a variable named \code{weight}.  It
  is normalized to 1.  The model-averaged estimates are
  computed using equation 4.1 of Burnham and Anderson
  (2002).

  If the contains a matrix named \code{vcv}, then a
  model-averaged variance-covariance matrix is computed
  using formulae given on page 163 of Burnham and Anderson
  (2002).  If there is no element named \code{vcv} then
  there must be an element \code{se} which contains the
  model-specific estimates of the standard errors.  The
  unconditional standard error for the model-averaged
  estimates is computed using equation 4.9 of Burnham and
  Anderson (2002) if if \code{revised=FALSE}; otherwise it
  uses eq 6.12.
}
\examples{
# Create a set of models from dipper data
data(dipper)
run.dipper=function()
{
dipper$nsex=as.numeric(dipper$sex)-1
mod1=mark(dipper,groups="sex",
model.parameters=list(Phi=list(formula=~sex)))
mod2=mark(dipper,groups="sex",
model.parameters=list(Phi=list(formula=~1)))
mod3=mark(dipper,groups="sex",
model.parameters=list(p=list(formula=~time),
Phi=list(formula=~1)))
dipper.list=collect.models()
return(dipper.list)
}
dipper.results=run.dipper()
# Extract indices for first year survival from
#  Females (group 1) and Males (group 2)
Phi.indices=extract.indices(dipper.results[[1]],
"Phi",df=data.frame(group=c(1,2),row=c(1,1),col=c(1,1)))
# Create a matrix for estimates
estimate=matrix(0,ncol=length(Phi.indices),
nrow=nrow(dipper.results$model.table))
# Extract weights for models
weight=dipper.results$model.table$weight
# Create an empty list for vcv matrices
vcv=vector("list",length=nrow(dipper.results$model.table))
# Loop over each model in model.table for dipper.results
for (i in 1:nrow(dipper.results$model.table))
{
# The actual model number is the row number for the model.table
model.numbers= as.numeric(row.names(dipper.results$model.table))
# For each model extract those real parameter values and their
# vcv matrix and store them
x=covariate.predictions(dipper.results[[model.numbers[i]]],
data=data.frame(index=Phi.indices))
estimate[i,]=x$estimates$estimate
vcv[[i]]=x$vcv
}
# Call model.average using the list structure which includes
#  estimate, weight and vcv list in this case
model.average(list(estimate=estimate,weight=weight,vcv=vcv))
#
# Now get same model averaged estimates using model.average.marklist
# Obviously this is a much easier approach and what would be used
# if all you are doing is model averaging real parameters in the model.
# The other form is more useful for model averaging
# functions of estimates of the real parameters (eg population estimate)
#
mavg=model.average(dipper.results,"Phi",vcv=TRUE)
print(mavg$estimates[Phi.indices,])
print(mavg$vcv.real[Phi.indices,Phi.indices])
}
\author{
  Jeff Laake
}
\references{
  BURNHAM, K. P., AND D. R. ANDERSON. 2002. Model selection
  and multimodel inference. A practical
  information-theoretic approach. Springer, New York.
}
\seealso{
  \code{\link{model.average.marklist}}
}
\keyword{utility}

