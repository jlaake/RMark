\name{summary.mark}
\alias{coef.mark}
\alias{print.summary.mark}
\alias{summary.mark}
\title{Summary of MARK model parameters and results}
\usage{
\method{summary}{mark}(object,...,se=FALSE,vc=FALSE,showall=TRUE,show.fixed=FALSE,brief=FALSE)
       \method{coef}{mark}(object,...)
       \method{print}{summary.mark}(x,...)
}
\arguments{
  \item{object}{a MARK model object}

  \item{se}{if FALSE the real parameter estimates are
  output in PIM format (eg. triangular format); if TRUE,
  they are displayed as a list with se and confidence
  interval}

  \item{vc}{if TRUE the v-c matrix of the betas is
  included}

  \item{showall}{if FALSE it only returns the values of
  each unique parameter value}

  \item{show.fixed}{if FALSE, each fixed value given NA;
  otherwise the fixed real value is used. If se=TRUE,
  default for show.fixed=TRUE}

  \item{brief}{if TRUE, does not show real parameter
  estimates}

  \item{...}{additional non-specified argument for S3
  generic function}

  \item{x}{list resulting from call to \code{summary}}
}
\value{
A list with each of the summarized objects that depends on
the argument values. Only the first 4 are given if it is a
summary of a model that has not been run. \item{model}{type
of model (e.g., CJS)} \item{title}{user define title if
any} \item{model.name}{descriptive name of fitted model}
\item{call}{call to make.mark.model used to construct the
model} \item{npar}{number of fitted parameters}
\item{lnl}{-2xLog Likelihood value} \item{npar}{Number of
parameters (always the number of columns in design matrix)}
\item{chat}{Value of over-dispersion constant if not equal
to 1} \item{npar.unadjusted}{number of estimated parameters
from MARK if different than npar } \item{AICc}{Small sample
corrected AIC using npar; named qAICc if chat not equal to
1} \item{AICc.unadjusted}{Small sample corrected AIC using
npar.unadjusted; prefix of q if chat not equal to 1}
\item{beta}{dataframe of beta parameters with estimate, se,
lcl, ucl} \item{vcv}{variance-covariance matrix for beta}
\item{reals}{list of lists, dataframes or matrices
depending on value of se and the type of model (triangular
versus square PIMS) (see details above)}
}
\description{
Creates a summary object of either a MARK model input or
model output which includes number of parameters, deviance,
AICc, the beta and real parameter estimates and optionally
standard errors, confidence intervals and
variance-covariance matrices.  If there are several groups
in the data, the output is structured by group.
}
\details{
The structure of the summary of the real parameters depends
on the type of model and the value of the argument
\code{se} and \code{showall}.  If \code{se=F} then only the
estimates of the real parameters are shown and they are
summarized the result element \code{reals} in PIM format.
The structure of \code{reals} depends on whether the PIMS
are upper triangular ("Triang") or a row ("Square" although
not really square). For the upper triangular format, the
values are passed back as a list of matrices where the list
is a list of parameter types (eg Phi and p) and within each
type is a list for each group containing the pim as an
upper triangular matrix containing the real parameter
estimate.  For square matrices, \code{reals} is a list of
matrices with a list element for each parameter type, but
there is not a second list layer for groups because in the
returned matrix each group is a row in the matrix of real
estimates.  If \code{se=TRUE} then estimates, standard
error (se), lower and upper confidence limits (lcl, ucl)
and a "Fixed" indicator is passed for each real parameter.
If the pims for the model were simplified to represent the
unique real parameters (unique rows in the design matrix),
then it is possible to restict the summary to only the
unique parameters with \code{showall=FALSE}.  This argument
only has an affect if \code{se=TRUE}. If
\code{showall=FALSE}, \code{reals} is returned as a
dataframe of the unique real parameters specified in the
model.  This does not mean they will all have unique values
and it includes all "Fixed" real parameters and any real
parameters that cannot be simplified in the case of
parameters such as "pent" in POPAN or "Psi" in
"Multistrata" that use the multinomial logit link. Use of
\code{showall=FALSE} is of limited use but provided for
completeness.  In most cases the default of
\code{showall=TRUE} will be satisfactory.  In this case,
\code{reals} is a list of dataframes with a list element
for each parameter type.  The dataframe contains the
estimate, se,lcl, ucl,fixed and the associated default
design data for that parameter (eg time,age, cohort etc).
The advantage of retrieving the reals in this format is
that it is the same regardless of the model, so it enables
model averaging the real parameters over different models
with differing numbers of unique real parameters.
}
\author{
Jeff Laake
}
\keyword{utility}

