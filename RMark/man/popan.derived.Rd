\name{popan.derived}
\alias{popan.derived}
\alias{popan.Nt}
\alias{popan.NGross}
\title{Computes some derived abundance estimates for POPAN models}
\description{
Computes estimates, standard errors, confidence intervals and var-cov matrix for population size 
of each group at each occasion and the sum across groups by occasion for POPAN models. If a \code{marklist} is provided
the estimates are model averaged.
}
\usage{
popan.derived(x,model,revised=TRUE, normal=TRUE, N=TRUE, 
              NGross=TRUE, drop=FALSE)
popan.Nt(Phi,pent,Ns,vc,time.intervals)
}
\arguments{
  \item{x}{processed data list resulting from \code{\link{process.data}}}
  \item{model}{a single mark POPAN model or a \code{marklist} of POPAN models}
  \item{revised}{if TRUE, uses revised version of model averaged standard error eq 6.12; otherwise uses eq 4.9 of Burnham and Anderson (2002)}
  \item{Phi}{interval-specific survival estimates for each group}
  \item{pent}{occasion-specific prob of entry estimates (first computed by subtraction) for each group}
  \item{Ns}{group specific super-population estimate}
  \item{vc}{variance-covariance matrix of the real parameters}
  \item{normal}{if TRUE, uses confidence interval based on normal distribution; otherwise, uses log-normal}
  \item{N}{if TRUE, will return abundance estimates by group and occasion and total by occasion}
  \item{NGross}{if TRUE, will return gross abundance estimate per group}  
  \item{drop}{if TRUE, models with any non-positive variance for betas are dropped}
  \item{time.intervals}{vector of time interval values}
}
\details{
\code{popan.derived} computes all of the real parameters using \code{\link{covariate.predictions}}
and handles all of the computation using \code{popan.Nt}.  Description for functions \code{popan.Nt} and \code{popan.NGross} are 
given here for completeness but it is not intended that they be called directly.  

If a \code{model} is a \code{marklist} of models, the values returned by \code{popan.derived} are model averaged using 
model weights in the \code{model.table}; otherwise, it returns the values for the specified model. 
}
\value{
\code{popan.derived} returns a list with the following elements depending on the values of \code{N} and \code{NGross}:
\preformatted{
N          - dataframe of estimates by group and occasion and se, 
                 lcl,ucl and group/occasion data
N.vcv      - variance-covariance matrix of abundance estimates in N
Nbyocc     - dataframe of estimates by occasion (summed across groups) 
                 and se, lcl,ucl and occasion data
Nbyocc.vcv - variance-covariance matrix of abundance estimates in Nbyocc
NGross     - dataframe of gross abundance estimates by group and se, 
                 lcl,and ucl 
NGross.vcv - variance-covariance matrix of NGross abundance estimates 
}

\code{popan.Nt} returns a list with the following elements:
\preformatted{
N          - dataframe of estimates by group and occasion and se, lcl,ucl 
                 and group/occasion data
N.vcv      - variance-covariance matrix of abundance estimates in N
}

\code{popan.NGross} returns a list with the following elements:
\preformatted{
NGross     - vector of gross abundance estimates by group
vcv        - variance-covariance matrix of abundance estimates in NGross
}


}
\author{Jeff Laake}
\references{BURNHAM, K. P., AND D. R. ANDERSON. 2002. Model selection and multimodel inference. A practical information-theoretic approach.
Springer, New York.
}
\examples{
# Example
data(dipper)
dipper.processed=process.data(dipper,model="POPAN",groups="sex")
run.dipper.popan=function()
{
dipper.ddl=make.design.data(dipper.processed)
Phidot=list(formula=~1)
Phitime=list(formula=~time)
pdot=list(formula=~1)
ptime=list(formula=~time)
pentsex.time=list(formula=~time)
Nsex=list(formula=~sex)
#
# Run assortment of models
#
dipper.phisex.time.psex.time.pentsex.time=mark(dipper.processed,
     dipper.ddl,model.parameters=list(Phi=Phidot,p=ptime,
     pent=pentsex.time,N=Nsex),invisible=FALSE,adjust=FALSE)
dipper.psex.time.pentsex.time=mark(dipper.processed,dipper.ddl,
     model.parameters=list(Phi=Phitime,p=pdot,
     pent=pentsex.time,N=Nsex),invisible=FALSE,adjust=FALSE)
#
# Return model table and list of models
#
return(collect.models() )
}
dipper.popan.results=run.dipper.popan()
popan.derived(dipper.processed,dipper.popan.results)
}
\keyword{utility}
