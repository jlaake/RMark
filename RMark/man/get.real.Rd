% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.real.R
\name{get.real}
\alias{get.real}
\title{Extract or compute sets of real parameters}
\usage{
get.real(
  model,
  parameter,
  beta = NULL,
  se = FALSE,
  design = NULL,
  data = NULL,
  vcv = FALSE,
  show.fixed = TRUE,
  expand = FALSE,
  pim = TRUE
)
}
\arguments{
\item{model}{MARK model object}

\item{parameter}{type of parameter in model (character) (e.g.,"Phi")}

\item{beta}{values of beta parameters for computation of real parameters}

\item{se}{if TRUE uses table format and extracts se and confidence
intervals, if FALSE uses PIM format with estimates only}

\item{design}{a numeric design matrix with any covariate values filled in
with numerical values}

\item{data}{covariate data to be averaged for estimates if design=NULL}

\item{vcv}{if TRUE computes and returns the v-c matrix of the subset of the
real parameters}

\item{show.fixed}{if TRUE fixed values are returned rather than NA in place
of fixed values}

\item{expand}{if TRUE, returns vcv matrix for unique parameters and only 
simplified unique parameters if FALSE}

\item{pim}{if TRUE and se=FALSE, returns a list of estimates in PIM format}
}
\value{
estimates: if \code{se=FALSE and Beta=NULL}, a matrix of estimates
or list of matrices for more than one group, and if \code{se=TRUE or beta=is
not NULL and vcv=FALSE} a dataframe of estimates with attached design data.
If \code{vcv=TRUE}, a list is returned with elements \code{vcv.real} and the
dataframe \code{estimates} as returned with \code{se=TRUE}.
}
\description{
Extracts or computes real parameters for a particular type of parameter
(parameter) and returns in either a table (dataframe) format or in PIM
format.
}
\details{
This function is called by \code{\link{summary.mark}} to extract(from
\code{model$results$real}) sets of parameters for display.  But, it can also
be useful to compute particular sets of real parameters for output,
manipulation or plotting etc.  It is closely related to
\code{\link{compute.real}} and it uses that function when it computes
(rather than extracts) real parameters. It provides an easier way to
extract/compute real estimates of a particular type (\code{parameter}).

The real parameter estimates are computed when either 1) \code{model$chat} >
1, 2) \code{design}, \code{data}, or \code{beta} are specified with
non-NULL values for those arguments, or 3) vcv=TRUE.  If none of the above
hold, then the estimates are extracted.

If \code{se=FALSE} and estimates are shown in triangular or square PIM
structure depending on the model and parameter.  For triangular, the lower
half of the matrix is shown as NA (not applicable in this case). If
\code{se=TRUE}, the estimate, standard error and confidence interval for the
real parameters with the accompanying design data are combined into a
dataframe.

If the model contains individual covariates, there are 3 options for
specifying the covariate values that are used for the real estimates.  If
neither \code{design} nor \code{data} are specified, then the real estimates
are computed with the average covariate values used in the MARK output.
This is what is done in the call from \code{\link{summary.mark}}.
Alternatively, the argument \code{design} can be given a numeric design
matrix for the model with the covariates given specific values.  This can be
done with \code{\link{find.covariates}} and \code{\link{fill.covariates}}.
Finally, a quicker approach is to specify a dataframe for \code{data} which
is averaged for the numeric covariate values and these are automatically
filled into the design matrix of the model with calls to
\code{\link{find.covariates}} and \code{\link{fill.covariates}} from
\code{\link{compute.real}} which is used for computation.  The second and
third options are essentially the same but creating the completed design
matrix will be quicker if multiple calls are made with the same completed
design matrix for different parameters.  The dataframe for \code{data} can
contain a single entry to specify certain values for computation.
}
\examples{

data(example.data)
pregion=list(formula=~region)
PhiAge=list(formula=~Age)
mod=mark(example.data,model.parameters=list(p=pregion,Phi=PhiAge),
 groups=c("sex","age","region"),age.var=2,initial.ages=c(0,1,2),threads=1,delete=TRUE)
# extract list of Phi parameter estimates for all groups in PIM format 
Phi.estimates=get.real(mod,"Phi")  
# print out parameter estimates in triangular PIM format
for(i in 1:length(Phi.estimates))  
{
  cat(names(Phi.estimates)[i],"\n")
  print(Phi.estimates[[i]]$pim,na.print="")
}
require(plotrix)
#extract parameter estimates of capture probability p with se and conf intervals
p.table=get.real(mod,"p",se=TRUE) 
print(p.table[p.table$region==1,])  # print values from region 1
estimates=by(p.table$estimate,p.table$region,mean)
lcl=by(p.table$lcl,p.table$region,mean)
ucl=by(p.table$ucl,p.table$region,mean)
plotCI(c(1:4),estimates,ucl-estimates,estimates-lcl,xlab="Region",
         ylab="Capture probability",
		ylim=c(.5,1),main="Capture probability estimates by region")


}
\seealso{
\code{\link{summary.mark}},\code{\link{compute.real}}
}
\author{
Jeff Laake
}
\keyword{utility}
