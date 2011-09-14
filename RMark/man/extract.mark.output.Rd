\name{extract.mark.output}
\alias{extract.mark.output}
\title{Extract results from MARK output file (internal use)}
\usage{
  extract.mark.output(out, model, adjust, realvcv = FALSE,
  vcvfile)
}
\arguments{
  \item{out}{output from MARK analysis
  (\code{model$output})}

  \item{model}{mark model object}

  \item{adjust}{if TRUE, adjusts number of parameters
  (npar) to number of columns in design matrix, modifies
  AIC and records both}

  \item{realvcv}{if TRUE the vcv matrix of the real
  parameters is extracted and stored in the model results}

  \item{vcvfile}{name of vcv file output}
}
\value{
  result: list of extracted output elements
  \item{lnl}{-2xLog-likelihood} \item{deviance}{Difference
  between saturated model and lnl} \item{npar}{Number of
  model parameters} \item{AICc}{Small-sample corrected AIC
  value using npar and n} \item{npar.unadjusted}{Number of
  model parameters as reported by MARK if npar was
  adjusted} \item{AICc.unadjusted}{Small-sample corrected
  AIC value using npar.unadjusted and n} \item{n}{Effective
  sample size reported by MARK; used in AICc calculation}
  \item{beta}{Dataframe of beta parameters with fields:
  estimate, se, lcl, ucl} \item{real}{Dataframe of real
  parameters with fields: estimate, se, lcl, ucl}
  \item{derived.vcv}{variance-covariance matrix for derived
  parameters if any} \item{covariate.values}{dataframe with
  fields Variable and Value which are the covariate names
  and value used for real parameter estimates in the MARK
  output} \item{singular}{indices of beta parameters that
  are non-estimable or at a boundary}
  \item{real.vcv}{variance-covariance matrix for real
  parameters (simplified) if realvcv=TRUE}
}
\description{
  Extracts the lnl, AICc, npar, beta and real estimates and
  returns a list of these results for inclusion in the
  \code{mark} object. The elements \code{beta} and
  \code{real} are dataframes with fields
  estimate,se,lcl,ucl. This function was written for
  internal use and is called by
  \code{\link{run.mark.model}}. It is documented here for
  more advanced users that might want to modify the code or
  adapt for their own use.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{run.mark.model}}
}
\keyword{utility}

