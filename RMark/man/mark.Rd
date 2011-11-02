\name{mark}
\alias{mark}
\title{Interface to MARK for fitting capture-recapture models}
\usage{
  mark(data, ddl = NULL, begin.time = 1, model.name = NULL,
  model = "CJS", title = "", model.parameters = list(),
  initial = NULL, design.parameters = list(), right = TRUE,
  groups = NULL, age.var = NULL, initial.ages = 0, age.unit
  = 1, time.intervals = NULL, nocc = NULL, output = TRUE,
  invisible = TRUE, adjust = TRUE, mixtures = 1, se =
  FALSE, filename = NULL, prefix = "mark", default.fixed =
  TRUE, silent = FALSE, retry = 0, options = NULL, brief =
  FALSE, realvcv = FALSE, delete = FALSE, external = FALSE,
  profile.int = FALSE, chat = NULL, reverse = FALSE)
}
\arguments{
  \item{data}{Either the raw data which is a dataframe with
  at least one column named ch (a character field
  containing the capture history) or a processed dataframe}

  \item{ddl}{Design data list which contains a list element
  for each parameter type; if NULL it is created}

  \item{begin.time}{Time of first capture(release)
  occasion}

  \item{model.name}{Optional name for the model}

  \item{model}{Type of c-r model (eg CJS, Burnham, Barker)}

  \item{title}{Optional title for the MARK analysis output}

  \item{model.parameters}{List of model parameter
  specifications}

  \item{initial}{Optional vector of named or unnamed
  initial values for beta parameters or previously run
  model object}

  \item{design.parameters}{Specification of any grouping
  variables for design data for each parameter}

  \item{right}{if TRUE, any intervals created in
  design.parameters are closed on the right and open on
  left and vice-versa if FALSE}

  \item{groups}{Vector of names factor variables for
  creating groups}

  \item{age.var}{Optional index in groups vector of a
  variable that represents age}

  \item{initial.ages}{Optional vector of initial ages for
  each age level}

  \item{age.unit}{Increment of age for each increment of
  time}

  \item{time.intervals}{Intervals of time between the
  capture occasions}

  \item{nocc}{number of occasions for Nest model; either
  time.intervals or nocc must be specified for this model}

  \item{output}{If TRUE produces summary of model input and
  model output}

  \item{invisible}{If TRUE, window for running MARK is
  hidden}

  \item{adjust}{If TRUE, adjusts npar to number of cols in
  design matrix, modifies AIC and records both}

  \item{mixtures}{number of mixtures for heterogeneity
  model}

  \item{se}{if TRUE, se and confidence intervals are shown
  in summary sent to screen}

  \item{filename}{base filename for files created by
  MARK.EXE. Files are named filename.*.}

  \item{prefix}{base filename prefix for files created by
  MARK.EXE; for example if prefix="SpeciesZ" files are
  named "SpeciesZnnn.*"}

  \item{default.fixed}{if TRUE, real parameters for which
  the design data have been deleted are fixed to default
  values}

  \item{silent}{if TRUE, errors that are encountered are
  suppressed}

  \item{retry}{number of reanalyses to perform with new
  starting values when one or more parameters are singular}

  \item{options}{character string of options for Proc
  Estimate statement in MARK .inp file}

  \item{brief}{if TRUE and output=TRUE then a brief summary
  line is given instead of a full summary for the model}

  \item{realvcv}{if TRUE the vcv matrix of the real
  parameters is extracted and stored in the model results}

  \item{delete}{if TRUE the output files are deleted after
  the results are extracted}

  \item{external}{if TRUE the mark object is saved
  externally rather than in the workspace; the filename is
  kept in its place}

  \item{profile.int}{if TRUE will compute profile intervals
  for each real parameter; or you can specify a vector of
  real parameter indices}

  \item{chat}{value of chat used for profile intervals}

  \item{reverse}{if set to TRUE, will reverse timing of
  transition (Psi) and survival (S) in Multistratum models}
}
\value{
  model: a MARK object containing output and extracted
  results. It is a list with the following elements
  \item{data}{name of the processed data frame}
  \item{model}{type of analysis model (see list above)}
  \item{title}{title used for analysis}
  \item{model.name}{descriptive name of model}
  \item{links}{vector of link function(s) used for
  parameters, one for each row in design matrix or only one
  if all parameters use the same function}
  \item{mixtures}{number of mixtures in Pledger-style
  closed capture-recapture models} \item{call}{call to
  make.mark.model used to construct the model}
  \item{parameters}{a list of parameter descriptions
  including the formula, pim.type, link etc.}
  \item{model.parameters}{the list of parameter
  descriptions used in the call to mark; this is used only
  by \code{rerun.mark}} \item{time.intervals}{Intervals of
  time between the capture occasions}
  \item{number.of.groups}{number of groups defined in the
  data} \item{group.labels}{vector of labels for the
  groups} \item{nocc}{number of capture occasions}
  \item{begin.time}{single time of vector of times (if
  different for groups) for the first capture occasion}
  \item{covariates}{vector of covariate names (as strings)
  used in the model} \item{fixed}{dataframe of parameters
  set at fixed values; \code{index} is the parameter index
  in the full parameter structure and \code{value} is the
  fixed value for the real parameter}
  \item{design.matrix}{design matrix used in the input to
  MARK.EXE} \item{pims}{list of pims used for each
  parameter including any group or strata designations;
  each parameter in list is denoted by name and within each
  parameter one or more sub-lists represent groups and
  strata if any} \item{design.data}{design data used to
  construct the design matrix} \item{strata.labels}{labels
  for strata if any} \item{mlogit.list}{structure used to
  simplify parameters that use mlogit links}
  \item{simplify}{list containing \code{pim.translation}
  which translate between all different and simplified
  pims, \code{real.labels} which are labels for real
  parameters for full (non-simplified) pim structure and
  \code{links} the link function names for the full
  parameter structure} \item{output}{base portion of
  filenames for input,output, vc and residual files output
  from MARK.EXE} \item{results}{List of values extracted
  from MARK ouput} \tabular{lll}{ \tab \code{lnl} \tab
  -2xLog Likelihood value \cr \tab \code{npar} \tab Number
  of parameters (always the number of columns in design
  matrix) \cr \tab \code{npar.unadjusted} \tab number of
  estimated parameters from MARK if different than npar \cr
  \tab \code{n} \tab effective sample size \cr \tab
  \code{AICc} \tab Small sample corrected AIC using npar
  \cr \tab \code{AICc.unadjusted} \tab Small sample
  corrected AIC using npar.unadjusted \cr \tab \code{beta}
  \tab data frame of beta parameters with estimate, se,
  lcl, ucl \cr \tab \code{real} \tab data frame of real
  parameters with estimate, se, lcl, ucl and fixed \cr \tab
  \code{beta.vcv} \tab variance-covariance matrix for beta
  \cr \tab \code{derived} \tab dataframe of derived
  parameters if any \cr \tab \code{derived.vcv} \tab
  variance-covariance matrix for derived parameters if any
  \cr \tab \code{covariate.values} \tab dataframe with
  fields \code{Variable} and \code{Value} \cr \tab \tab
  which are the covariate names and value used for real
  parameter \cr \tab \tab estimates in the MARK output\cr
  \tab \code{singular} \tab indices of beta parameters that
  are non-estimable or at a boundary \cr \tab
  \code{real.vcv} \tab variance-covariance matrix for real
  parameters (simplified) if realvcv=TRUE \cr }
  \item{chat}{over-dispersion constant; if not present
  assumed to be 1}
}
\description{
  Fits user specified models to various types of
  capture-recapture data by creating input file and running
  MARK software and retrieving output
}
\details{
  This function acts as an interface to the FORTRAN program
  MARK written by Gary White
  (\url{http://www.cnr.colostate.edu/~gwhite/mark/mark.htm}).
  It creates the input file for MARK based on
  user-specified sub-models (\code{model.parameters}) for
  each of the parameters in the capture-recapture model
  being fitted to the data. It runs MARK.EXE (see note
  below) and then imports the text output file and binary
  variance-covariance file that were created.  It extracts
  output values from the text file and creates a list of
  results that is returned as part of the list (of class
  mark) which is the return value for this function.

  The following are the MARK capture-recapture models that
  are currently supported for argument \code{model}:
  \tabular{ll}{ \code{model} \tab Selection in MARK \cr
  \code{CJS} \tab Recaptures only\cr \code{Recovery} \tab
  Recoveries only\cr \code{Burnham} \tab Both(Burnham)\cr
  \code{Barker} \tab Both(Barker)\cr \code{Pradel} \tab
  Pradel recruitment only\cr \code{Pradsen} \tab Pradel
  survival and seniority\cr \code{Pradlambda} \tab Pradel
  survival and lambda\cr \code{Pradrec} \tab Pradel
  survival and recruitment\cr \code{LinkBarker} \tab
  Available only in change data type as Link-Barker\cr
  \code{Closed} \tab Closed - no heterogeneity\cr
  \code{HetClosed} \tab Closed with heterogeneity\cr
  \code{FullHet} \tab Closed with full heterogeneity\cr
  \code{Huggins} \tab Huggins with no heterogeneity\cr
  \code{HugHet} \tab Huggins with heterogeneity\cr
  \code{HugFullHet} \tab Huggins with full heterogeneity\cr
  \code{POPAN} \tab POPAN\cr \code{Jolly} \tab Burnham
  formulation for original Jolly-Seber model\cr
  \code{Known} \tab Known - known fate data (e.g,
  radio-tracking)\cr \code{Multistrata} \tab Multistrata -
  CJS model with strata\cr \code{Robust} \tab Robust design
  with Closed models for secondary periods with no
  heterogeneity\cr \code{RDHet} \tab Robust design with
  Closed models for secondary periods with heterogeneity\cr
  \code{RDFHet} \tab Robust design with Closed models for
  secondary periods with full heterogeneity\cr
  \code{RDHuggins} \tab Robust design with Huggins models
  for secondary periods with no heterogeneity\cr
  \code{RDHHet} \tab Robust design with Huggins models for
  secondary periods with heterogeneity\cr \code{RDHFHet}
  \tab Robust design with Huggins models for secondary
  periods with full heterogeneity\cr \code{Nest} \tab Nest
  survival\cr \code{Occupancy} \tab Site occupancy
  modelling\cr \code{OccupHet} \tab Site occupancy
  modelling with mixture model for heterogeneity\cr
  \code{RDOccupEG} \tab Robust design site occupancy
  modelling; single Psi, espsilon, and gamma\cr
  \code{RDOccupPE} \tab Robust design site occupancy
  modelling; mutliple Psi and espsilon\cr \code{RDOccupPG}
  \tab Robust design site occupancy modelling; mutliple Psi
  and gamma\cr \code{RDOccupHetEG} \tab Robust design site
  occupancy modelling with heterogeneity; single Psi,
  espsilon, and gamma\cr \code{RDOccupHetPE} \tab Robust
  design site occupancy modelling with heterogeneity;
  mutliple Psi and espsilon\cr \code{RDOccupHetPG} \tab
  Robust design site occupancy modelling with
  heterogeneity; mutliple Psi and gamma\cr
  \code{OccupRNPoisson} \tab Royle-Nichols Poisson site
  occupancy modelling\cr \code{OccupRNNegBin} \tab
  Royle-Nichols Negative Binomial site occupancy
  modelling\cr \code{OccupRPoisson} \tab Royle count
  Poisson site occupancy modelling\cr \code{OccupRNegBin}
  \tab Royle count Negative Binomial site occupancy
  modelling\cr \code{MSOccupancy} \tab Multi-state site
  occupancy modelling\cr }

  The function mark is a shell that calls 5 other functions
  in the following order as needed: 1)
  \code{\link{process.data}}, 2)
  \code{\link{make.design.data}}, 3)
  \code{\link{make.mark.model}}, 4)
  \code{\link{run.mark.model}}, and 5)
  \code{\link{summary.mark}}. A MARK model can be fitted
  with this function (\code{mark}) or by calling the
  individual functions that it uses.  The calling arguments
  for \code{mark} are a compilation of the calling
  arguments for each of the functions it calls (with some
  arguments renamed to avoid conflicts). If data is a
  processed dataframe (see \code{\link{process.data}}) then
  it expects to find a value for \code{ddl}.  Likewise, if
  the data have not been processed, then \code{ddl} should
  be NULL.  This dual calling structure allows either a
  single call approach for each model or alternatively for
  the data to be processed and the design data (\code{ddl})
  to be created once and then a whole series of models can
  be analyzed without repeating those steps.

  For descriptions of the arguments \code{data},
  \code{begin.time}, \code{groups}, \code{age.var},
  \code{initial.ages}, \code{age.unit},
  \code{time.intervals} and \code{mixtures} see
  \code{\link{process.data}}.

  For descriptions of \code{ddl},
  \code{design.parameters}=\code{parameters}, and
  \code{right}, see \code{\link{make.design.data}}.

  For descriptions of \code{model.name} , \code{model},
  \code{title},\code{model.parameters}=\code{parameters} ,
  \code{default.fixed} , \code{initial}, \code{options},
  see \code{\link{make.mark.model}}.

  And finally, for descriptions of arguments
  \code{invisible}, \code{filename} and \code{adjust},see
  \code{\link{run.mark.model}}.

  \code{output},\code{silent}, and \code{retry} are the
  only arguments specific to mark.  \code{output} controls
  whether a summary of the model input and output are
  given(if \code{output=TRUE}). \code{silent} controls
  whether errors are shown when fitting a model.
  \code{retry} controls the number of times a model will be
  refitted with new starting values (uses 0) when some
  parameters are determined to be non-estimable or at a
  boundary. The latter is the only time it makes sense to
  retry with new starting values but MARK cannot discern
  between these two instances. The indices of the beta
  parameters that are "singular" are stored in
  \code{results$singular}.
}
\note{
  It is assumed that MARK.EXE is located in directory
  "C:/Program Files/Mark".  If it is in a different
  location set the variable MarkPath to the directory
  location. For example, seting MarkPath="C:/Mark/" at the
  R prompt will assign run "c:/mark/mark.exe" to do the
  analysis.  If you have chosen a non-default path for
  Mark.exe, MarkPath needs to be defined for each R
  session.  It is easiest to do this assignment
  automatically by putting the MarkPath assignment into
  your .First function which is run each time an R session
  is initiated.  In addition to MarkPath, the variable
  MarkViewer can be assigned to a program other than
  notepad.exe (see \code{\link{print.mark}}).
}
\examples{
data(dipper)
dipper.Phidot.pdot=mark(dipper)
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{make.mark.model}},
  \code{\link{run.mark.model}},
  \code{\link{make.design.data}},
  \code{\link{process.data}}, \code{\link{summary.mark}}
}
\keyword{models}

