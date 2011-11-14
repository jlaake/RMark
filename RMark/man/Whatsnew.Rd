\name{Whatsnew}
\alias{Whatsnew}
\title{Summary of changes by version}
\description{
  A good place to look for new changes.  Often I'll add
  changes here but don't always get to it in the
  documentation for awhile.  They are ordered from newest
  to oldest.
}
\details{
  Version 2.0.9 (8 Nov 2011) \itemize{ \item Patch was made
  to \code{\link{make.mark.model}} to fix bug in PIM
  creation for a multi-session model and there was just 1
  session. Thanks to Erin Roche for helping to identify
  this bug. \item Patch was made to
  \code{\link{make.mark.model}} to fix bug in handling of
  mlogits for pi and Omega parameters with more than one
  group. These parameters were introduced with the
  RDMSMisClass and other new models that were recently
  added. \item Additional changes were made to
  \code{\link{export.MARK}} to re-fix changes for robust
  and nest survival model export to MARK. \item A function
  \code{\link{mark.wrapper.parallel}} written by Eldar
  Rakimberdiev provides a parallel processing version of
  mark.wrapper.  See the example in the help for the
  function.  The parallel version is functionally the same
  and can be used in place of \code{mark.wrapper} to run
  sequentially or in parallel. It does not include the run
  argument however. \item A bug was fixed in
  \code{\link{get.real}} which caused an R error for a
  triangular PIM that had only a single entry. Thanks to
  Amanda Goldberg for discovering and reporting this error.
  } Version 2.0.8 (7 Oct 2011) \itemize{ \item Both
  \code{\link{setup.model}} and
  \code{\link{setup.parameters}} were re-written to use
  data files models.txt and parameters.txt to define models
  and parameters which should make it easier to add new
  models.  The latter function is now much simpler and
  smaller. \item Model RDOccupEG now allows sharing Epsilon
  and Gamma parameters.  Epsilon is the dominant parameter
  which gets the share=TRUE argument. See
  \code{\link{RDOccupancy}} for an example. Thanks to Jake
  Ivan for an example of what was needed. \item To avoid
  confusion, the arguments \code{component} and
  \code{component.name} were removed from the parameter
  specification because these have not been required since
  v1.3 when full support for individual covariates were
  included.  Likewise the argument \code{covariates} in
  \code{\link{mark}} and \code{\link{make.mark.model}} was
  removed because it was only needed to support the
  component approach. \item \code{\link{export.MARK}} was
  modified so that if all individual covariates are output,
  it excludes factor covariates. \item Many more models
  were added to those supported in RMark. Now 92 of the 137
  models in MARK are supported. See MarkModels.pdf in the
  RMark directory of your R library to see which models are
  supported (in red). Most of the remaining unsupported
  models are versions with mis-identification error and
  they are not shown in MarkModels.pdf. \item Previously
  RMark stored the input file in a temporary file
  Markxxx.tmp. Using a common filename caused problems when
  more than one model were spawned to different CPUs, so
  now it uses a random temporary file name. It also no
  longer uses common file markxxx.vcv. Thanks to Glenn
  Stauffer for testing these changes. \item Sessions are
  now labelled using value of session time rather than
  numerically from 1 to largest session number in robust
  designs.  Thanks to Tommy Garrison for this suggestion.
  \item A bug was fixed in \code{\link{get.real}} which
  caused incorrect assignments of fixed parameter values in
  the unusual case where a fixed parameter had a non-zero
  design matrix row. \item \code{\link{mark.wrapper}} was
  modified so it returns a list of the models that were
  constructed if run==FALSE.  Thanks to Eldar Rakhimberdiev
  for the suggestion and code. \item Code was added to
  \code{\link{extract.mark.output}} to extract deviance
  degrees of freedom.  Thanks again to Eldar for
  contributing this code. \item A bug was fixed in
  \code{\link{make.mark.model}} that prevented use of sin
  link on within session parameters in a robust design
  model. Thanks to Tommy Garrison for reporting this bug.
  \item A bug was fixed in \code{\link{make.mark.model}}
  which prevented the use of time varying covariates with
  shared parameters. Thanks to Andre Breton for reporting
  this bug. \item simplify argument was removed from
  functions because I have not found a reason not to
  simplify and I have not been testing code with
  simplify=FALSE. } Version 2.0.7 (25 August 2011)
  \itemize{ \item Change to \code{\link{make.mark.model}}
  to fix bug in which mlogits were incorrectly assigned in
  ORDMS model when both Psi and pent used mlogit links.
  Thanks to Glenn Stauffer for identifying and tracking
  down this error.  \item Fixed \code{\link{export.chdata}}
  and \code{\link{export.MARK}} so Nest survival models can
  be exported.  Also changed default of argument
  ind.covariates to "all" which will use all individual
  covariates in the data in the file sent to MARK.  Thanks
  to Jay Rotella for his help. \item Made change to
  \code{\link{process.data}} and \code{\link{mark}} to
  include a new argument reverse, which if set to TRUE with
  model="Multistratum" will reverse the timing of
  transition and survival. See \code{\link{mstrata}} for an
  example of a reverse multistratum model. \item Made
  change to \code{\link{make.design.data}} to allow for
  zero time intervals in non-robust design model.  This was
  needed to allow use of the reverse time structure in
  multi-state models. In addition, for the reverse
  multistate model the function now adds an occasion (occ)
  field to the design data because the time field will be
  constant when with a 0 time interval.  The row names of
  the design matrix and real parameters was extended for
  this model to include occasion (o) and occasion cohort
  (oc) to create unique labels because cohort and time are
  not unique with 0 time intervals.  } Version 2.0.6 (1
  July 2011) \itemize{ \item Change to MR resight examples
  so the output does not appear in notepad which was
  causing problem with check on CRAN submission } Version
  2.0.5 (29 June 2011) \itemize{ \item Order of arguments
  for \code{\link{model.average.marklist}} were switched
  incorrectly such that ... was the second argument.  This
  has been fixed to the original format where ... is at the
  end.  This resulted in the value of any arguments other
  than the first to be ignored unless specifically named
  e.g., \code{parameter="Phi"}. Thanks to Rod Towell for
  reporting this error. \item Additional changes were made
  to .First.lib to 1) examine any MarkPath setting, 2) look
  in C:/program files/mark or c:/program files (x86)/mark,
  3) or to search the path.  If MARK executable cannot be
  found in any of those ways then a warning is issued that
  the user needs to set MarkPath to the path specification
  for the location of the MARK executable.  Thanks to Bryan
  Wright for help with tracking down a bug.  \item A bug in
  \code{\link{add.design.data}} was fixed where the
  function gave incorrect results in pim.type was anything
  other than "all".  Thanks to Jeff Hostetler for reporting
  this error. \item The mark-resight models PoissonMR,
  LogitNormalMR, and IELogitNormalMR were added.  This
  required some changes to
  \code{\link{make.mark.model}},\code{\link{make.design.data}}
  and \code{\link{compute.design.data}} and the addition of
  an argument \code{counts} to \code{\link{process.data}}
  to provide mark-resight count data that are not in the
  capture history format. The format for \code{counts} is a
  named list of vectors (one group) or matrices (each group
  is a row) where the names of the list elements are
  specific to the model. Currently there is no checking to
  make sure these are named correctly. Some of these models
  are very sensitive to starting values; thus the use of
  initial values in the examples.  Thanks to Brett
  McClintock for his help incorporating these models. }
  Version 2.0.4 (1 June 2011) \itemize{ \item Change made
  to .First.lib to check for availability of mark software
  that depends on operating system. It now provides a
  warning rather than stopping package attachment.  This
  allows the user to set MarkPath to some location outside
  of default location Program Files or Program Files (x86)
  without changing path which requires administrator
  privilege on Windows. \item Change made to
  \code{\link{run.mark.model}} to use shQuote because link
  to mark.exe was not working in some cases.  } Version
  2.0.3 (17 May 2011) \itemize{ \item Now requires R 2.13
  to use path.package function \item Change made to
  .First.lib to check for availability of mark software
  that depends on operating system. } Version 2.0.2 (16 May
  2011) \itemize{ \item MarkPath no longer needs to be set
  if mark.exe is no longer in the default location
  (c:/Program Files/mark) but is specified in the path. The
  code now uses the R function Sys.which to find the
  correct location for mark.exe, if it is contained in the
  Path. \item Code for crm models was removed from RMark
  and moved to a different R package called marked that is
  under-development.  This removes the FORTRAN code and
  accompanying dll and some functions and help files that
  were extraneous to RMark capabilities. There is still
  some code in some functions for crm that could be removed
  at some point.  None of this matters to those who use
  RMark for its original purpose as an interface to
  mark.exe. \item Many superficial changes were made to
  code so it could be posted on CRAN.  Three changes that
  may be noticed by users involved renaming
  deriv.inverse.link, summary.ch and
  merge.design.covariates to deriv_inverse.link,
  summary_ch, and merge_design.covariates.  These names
  conflicted with the generic functions deriv, summary and
  merge.  It is only the last 2 that you may have in your
  scripts and you will have to rename them. The deprecated
  function merge.occasion.data was removed. Sorry for any
  inconvenience. \item The dependency on Hmisc for the
  examples was removed by replacing errbar with plotCI. }
  Version 2.0.1 (21 Feb 2011) \itemize{ \item Made a change
  to \code{\link{run.mark.model}} to handle output
  filenames that exceeded mark9999. \item Added an argument
  \code{prefix} in \code{\link{mark}},
  \code{\link{run.mark.model}} and \code{\link{cleanup}}.
  Like other parameters for \code{\link{mark}}, it can also
  be used in \code{\link{mark.wrapper}} as one of the ...
  arguments. Previously the mark files have always been
  named "marknnn.*". By specifying \code{prefix} you can
  now create sets of models with different prefixes. For
  example, \code{prefix="cu"} would result in
  cu001.*(cu001.out, cu001.inp, etc), cu002.* etc.  This
  provides the ability to name files to do things like
  naming them based on the species being analyzed.  In
  general, there is no need to work with these files
  directly because the filename.* is stored with each
  \code{mark} object and the various R functions use that
  link to provide the information from the files. If you
  use prefixes other than "mark", you'll need to call call
  \code{\link{cleanup}} with each prefix to remove unused
  files.  See \code{\link{run.mark.model}} for an example
  that shows use of the prefix argument to split the dipper
  data into separate analyses for each sex.  Note that use
  of prefix was not mandatory here to separate the analyses
  but it provided a useful example.  \item Additional
  usefulness has been coded for argument \code{initial} for
  assigning initial values to beta parameters.  Previously,
  the options were either a vector of the same length as
  the new model to be run or a previously run model in a
  \code{mark} object from which equivalent betas are
  extracted based on their names in the design matrix.  Now
  if the vector contains names for the elements they will
  be matched with the new model like with the model option
  for initial.  If any betas in the new model are not
  matched, they are assigned 0 as their initial value, so
  the length of the \code{initial} vector no longer needs
  to match the number of parameters in the new model as
  long as the elements are named. The names can be
  retrieved either from the column names of the design
  matrix or from \code{rownames(x$results$beta)} where
  \code{x} is the name of the \code{mark} object.  \item
  Using the feature above, I added a new argument
  \code{use.initial} to \code{\link{mark.wrapper}}.  If
  \code{use.initial=TRUE}, prior to running a model it
  looks for the first model that has already been run (if
  any) for each parameter formula and constructs an
  \code{initial} vector from that previous run. For
  example, if you provided 5 models for p and 3 for Phi in
  a CJS model, as soon as the first model for p is run, in
  the subsequent 2 models with different Phi models, the
  initial values for p are assigned based on the run with
  the first Phi model.  At the outset this seemed like a
  good idea to speed up execution times, but from the one
  set of examples I ran where several parameters were at
  boundaries, the results were discouraging because the
  models converged to a sub-optimal likelihood value than
  the runs using the default initial values.  I've left
  this option in but set its default value to FALSE. \item
  A possibly more useful argument and feature was added to
  \code{\link{mark.wrapper}} in the argument
  \code{initial}.  Previously, you could use
  \code{initial=model} and it would use the estimates from
  that model to assign initial values for any model in the
  set defined in \code{\link{mark.wrapper}}. Now I've
  defined \code{initial} as a specific argument and it can
  be used as above but you can also use it to specify a
  \code{marklist} of previously run models. When you do
  that, the code will lookup each new model to be run in
  the set of models specified by \code{initial} and if it
  finds one with the matching name then it will use the
  estimates for any matching parameters as initial values
  in the same way as \code{initial=model} does.  The model
  name is based on concatenating the names of each of the
  parameter specification objects.  To make this useful,
  you'll want to adapt to an approach that I've started to
  use of naming the objects something like p.1,p.2 etc
  rather than naming them something like p.dot, p.time as
  done in many of the examples.  I've found that using
  numeric approach is much less typing and cumbersome
  rather than trying to reflect the formula in the name. By
  default, the formula is shown in the model selection
  results table, so it was a bit redundant.  Now where I
  see this being the most benefit.  Individual covariate
  models tend to run rather slowly. So one approach is to
  run the sequence of models (eg results stored in
  initial_marklist), including the set of formulas with all
  of the variables other than individual covariates.  Then
  run another set with the same numbering scheme, but
  adding the individual covariates to the formula and using
  \code{initial=initial_marklist} That will work if each
  parameter specification has the same name (eg.,
  p.1=list(formula=~time) and then
  p.1=list(formula=~time+an_indiv_covariate)).  All of the
  initial values will be assigned for the previous run
  except for any added parameters (eg. an_indiv_covariate)
  which will start with a 0 initial value. \item I added a
  new function \code{\link{search.output.files}} to the set
  of utility functions. This can be useful to search all of
  the output files in a \code{marklist} for a specific
  string like "numerical convergence suspect" or just
  "WARNING".  The function returns the model numbers in the
  \code{marklist} that contain that string in the output
  file. \item Further changes were needed to
  \code{\link{popan.derived}} to handle data that are
  summarized (i.e. frequency of capture history >1). Thanks
  to Carl Schwarz for reporting and finding the change
  needed. \item A bug was fixed in \code{create.dm} was
  fixed so it would return a matrix instead of a vector
  with the formula ~1 } Version 2.0.0 (14 Jan 2011)
  \itemize{ \item The packages msm, Hmisc, nlme, plotrix
  are now explicitly required to install RMark.  These were
  used in examples or for specialty functions, but to avoid
  problems these must be installed as well. \item Added
  example for "CRDMS" model that was created by Andrew
  Paul. See \code{\link{crdms}}. \item Change was made for
  gamma link in v1.9.3 was only made to Pradel model and
  not Pradsen like it stated.  Both now correctly use the
  logit link as the default for gamma.  Thanks to Gina
  Barton for bringing this to my attention. \item Change
  was made in \code{\link{make.mark.model}} that prevented
  use of groups with Nest survival models.  Thanks to Jeff
  Warren for bringing this to my attention.  \item Change
  was made in \code{\link{make.design.data}} because
  re-ordering of parameters caused issues with the CRDMS
  model because the \code{subtract.stratum} were not being
  set for \code{Psi}.  } Version 1.9.9 (2 Nov 2010)
  \itemize{ \item This version was built with R 2.12 and
  will not work with earlier versions of R.  It contains
  both 32 and 64 bit versions and R will automatically
  ascertain which to use. \item Parameter ordering for some
  models (RDHet, RDFullHet, RDHHet, RDHFHet, OccupHet,
  RDOccupHetPE, RDOccupHetPG, RDOccupHetEG, MSOccupancy,
  ORDMS, and CRDMS) had to be changed such that the models
  could be imported into the MARK interface. This change
  can influence any code you have written for those models
  if you specified parameter indices because the ordering
  of the parameters were changed. For example, see change
  in example for \code{\link{RDOccupancy}} to use
  indices=c(1) from c(10).  Thanks to Gary White for
  helping me work this out.  \item Modified code in
  \code{\link{extract.mark.output}} to handle cases with
  more than 9999 real parameters because MARK outputs ****
  when it exceeds 9999. } Version 1.9.8 (15 Sept 2010)
  \itemize{ \item Added \code{model="CRDMS"} with
  parameters S, Psi, N, p, and c for closed robust design
  multi-state models \item Patched
  \code{\link{popan.derived}} which produced incorrect
  abundance estimates with unequal time intervals. Thanks
  to Andy Paul for finding this error and testing for me.
  \item Patched \code{\link{export.MARK}} which failed for
  robust design models.  Thanks to Dave Hewitt and Gary
  White for discovering and isolating the problem. }
  Version 1.9.7 (14 April 2010) \itemize{ \item Added
  \code{model="Brownie"} with parameters S and f which is
  the Brownie et al. parameterization of the recovery
  model. "Recovery only" (in RMark) \code{model="Recovery"}
  which is also encounter type "dead" in MARK uses the
  Seber parameterization with parameters S and r which is
  also used in the models for live and dead encounter
  models. \item Added \code{model="MSLiveDead"} with
  parameters S, r, Psi and p.  It is the multistate version
  of the Burnham model in which F=1. \item Added
  \code{\link{compute.Sn}} to utility functions for
  computation of natural survival from total survival when
  all harvest is reported. Patched \code{\link{nat.surv}}
  which was incorrectly rejecting based on model type.
  \item Added \code{\link{var.components.reml}} to provide
  an alternate variance components estimation using REML or
  maximum likelihood. It allows a random component that is
  not iid which is all that \code{\link{var.components}}
  can do. \item Replaced all T/F values with TRUE/FALSE to
  avoid conflicts with objects named TRUE or FALSE. }
  Version 1.9.6 (1 February 2010) \itemize{ \item Writing
  the \code{\link{popan.derived}} function has led me down
  all sorts of paths.  I had to make one small change to
  this function to handle externally saved models.
  However, various changes listed below were brought on by
  using this function with a relatively large POPAN model.
  \item Most importantly I discovered an error in
  computations of real parameters with an mlogit link in
  which some of the real parameters involved in the mlogit
  link were fixed. This is NOT a problem if you simply used
  the real parameter values extracted from MARK; however,
  if you were using either \code{\link{compute.real}}
  (model.average uses this function) or
  \code{\link{covariate.predictions}} to compute those real
  parameters (with an mlogit link) then they may be
  incorrect.  This would have been apparent because their
  value would have changed relative to the original values
  extracted from the MARK output. Correcting this error
  involved changes in \code{\link{compute.real}},
  \code{\link{convert.link.to.real}} and
  \code{\link{covariate.predictions}} to correct the real
  parameter estimates and their standard errors.  I've not
  found it in the MARK documentation but deduced that if
  you use the mlogit link and fix real parameters it uses
  those fixed real parameter values in the calculation. A
  simple example will make it clear.  Consider pent for 5
  occasions where the first is computed by subtraction and
  you then have 4 real parameters.  Let's assume that the
  3rd and fourth parameters were fixed to 0.  Then the real
  parameters are calculated as follows:
  pent2=exp(beta2)/(1+
  exp(beta2)+exp(beta3)+exp(0)+exp(0)),
  pent3=exp(beta3)/(1+
  exp(beta2)+exp(beta3)+exp(0)+exp(0)), pent4=0, pent5=0
  and pent1=1-pent2-pent3-0-0 (in this case). Obviously you
  would not want to fix any real parameters to be >1, <0 or
  to have the sum to be <0 or >1. This structure also had
  implications on how the standard error was calculated.
  \item In addition the coding was made more efficient in
  \code{\link{covariate.predictions}} for the case where
  \code{data(index=somevector)} is used without any data
  entries for covariate values in the design matrix.  While
  the task performed with that use of the function could be
  done with \code{\link{compute.real}}, it is useful to
  have the capability in
  \code{\link{covariate.predictions}} as well because it
  will then model average over the listed set of
  parameters. The previous approach to coding was
  inefficient and led to very large matrices that were
  unnecessary and could cause failure with insufficient
  memory for large analyses.  \item Calculation of NGross
  was added to \code{\link{popan.derived}} and logical
  arguments \code{N} and \code{NGross} were added to
  control what was computed in the call. In addition,
  argument \code{drop} was added which is passed to
  \code{\link{covariate.predictions}} to control whether
  models are dropped when variance of betas are not all
  positive.  \item \code{\link{var.components}} was
  modified to use qr matrix inversion. The returned value
  for beta is now a dataframe that includes the std errors
  which are extracted from the vcv matrix. Also, if the
  design matrix only uses a portion of the vcv matrix, the
  appropriate rows and columns are now extracted. Prior to
  this change, the standard errors would have been
  unreliable if the design matrix didn't use the entire set
  of thetas and vcv matrix. } Version 1.9.5 (4 December
  2009) \itemize{ \item A bug in
  \code{\link{covariate.predictions}} was fixed that would
  assign fixed values incorrectly if the parameter indices
  were specified in anything but ascending order.  The
  error would have been obvious to anyone that may have
  encountered it because estimated parameters would likely
  have been assigned a fixed value.  In most cases indices
  would be passed in order if they were selected from the
  design data unless indices were chosen from more than one
  parameter type.  I discovered it using the
  \code{\link{popan.derived}} function I added in v1.9.4
  because it requests indices for multiple parameters in a
  single function call.  } Version 1.9.4 (6 November 2009)
  \itemize{ \item Note that this version was built with R
  2.10 which no longer supports compiled help files
  (chtml). If you were using compiled help files to get
  help with RMark, you'll need to switch to regular html
  files by using options(help_type="html") in R.  You can
  put this command in your RProfile.site file so it is set
  up that way each time you start R. The functionality is
  the same but it is not as pretty.  You can find the index
  (what used to be in a window on the left) as a link at
  the bottom of each help page. \item Changed
  \code{\link{export.MARK}} so it will not allow selection
  of a project name that would over-write an existing .inp
  file. \item Added the function
  \code{\link{popan.derived}} which for POPAN models
  computes derived abundance estimates by group and
  occasion and sum of group abundances for each occasion.
  For some reason RMark is unable to extract all of the
  derived parameters from the MARK binary file for POPAN
  models.  This function provides the derived abundance
  estimates and their var-cov matrix and adds the abundance
  estimate sum across groups for each occasion which is not
  provided by MARK. Note that by default confidence
  intervals are based on a normal distribution to match the
  output of MARK, but if you want log-normal intervals use
  \code{normal=FALSE}. \item The
  \code{\link{model.average.list}} and
  \code{\link{model.average.marklist}} functions were
  modified to use revised estimator for the unconditional
  standard error (eq 6.12 of Burnham and Anderson (2002))
  which is now the default in MARK.  To use eq 4.9 (the
  prior formula) set the argument \code{revised=FALSE}.
  \item Fixed bug in \code{\link{model.average.list}} which
  in some cases failed when the list of var-cov matrices
  were specified. \item Code in
  \code{\link{model.average.marklist}} was changed to set
  standard error to 0 if the variance is negative.  The
  same is done in the var-cov matrix for the variance and
  any corresponding covariances.  The results from RMark
  will now match the model average results from MARK for
  this case.  It is not entirely clear that this is the
  best approach when ill-fitted models are included.  \item
  Changed code in \code{\link{cleanup}} to handle case in
  which a model did not run. \item Changed use of
  \code{grep} and \code{regexpr} in
  \code{\link{convert.inp}} and
  \code{\link{extract.mark.output}} to accomodate change in
  R.2.10.  The code should work in earlier versions of R
  but if not update to R2.10. \item Created a function
  \code{\link{adjust.value}} and kept special case of
  \code{adjust.chat}.  For any field other than \code{chat}
  it will adjust the value in \code{model$results}.  As an
  example, to adjust the effective sample size (ESS) use
  \code{model.list=adjust.value("n",value,model.list)}
  where value is replaced with the ESS you want to use. As
  part of the change \code{\link{model.table}} was changed
  to recompute AICc.  } Version 1.9.3 (24 September 2009)
  \itemize{ \item Default link function for Gamma in the
  Pradel seniority model had been incorrectly set to log
  and has now been changed to logit to restrict it to be a
  probability. \item A bug was fixed in
  \code{\link{compute.real}} and
  \code{\link{covariate.predictions}} in which confidence
  intervals were being incorrectly scaled by c (chat
  adjustment) instead of sqrt(c).  The reported standard
  errors were correctly using sqrt(c) and only the
  confidence intervals for the real predictions were too
  large.  Simply re-running the prediction computations for
  a model will provide the correct results. There is no
  reason to re-run the models. Also this in no way affects
  model selection. \item A related bug was fixed in
  \code{\link{compute.real}} and
  \code{\link{covariate.predictions}} which created invalid
  confidence intervals for real parameters if a probability
  link other than logit was used and a single type of link
  was used for all real parameters (e.g., sin).  } Version
  1.9.2 (10 August 2009) \itemize{ \item Added the function
  \code{\link{export.MARK}} which creates a .Rinp, .inp and
  optionally renames one or more output files for import to
  MARK. The July 2009 version of MARK now contains a
  File/RMARK Import menu item which will automatically
  create the MARK project using the information in these
  files.  This prevents problems that have been encountered
  in creating MARK projects with RMark output because the
  data/group structures are setup exactly in MARK as they
  were in RMark. See \code{\link{export.MARK}} for an
  example and instructions.  \item Fixed a problem in
  \code{\link{make.design.data}} which prevented use of
  \code{remove.unused} with unequal time intervals and more
  than one group. \item At least one person has encountered
  a problem with a very large number of parameters in which
  RMark created the input file with PIMs written in
  exponential notation for the larger indices.  MARK will
  not accept that format and it will fail. The solution to
  this is to set the R option scipen to a positive number.
  Start with options(scipen=1) and increase if necessary. }
  Version 1.9.1 (2 June 2009) \itemize{ \item Fixed a
  problem \code{\link{make.design.data}} which was not
  using \code{begin.time} to label the session values \item
  Made a change in \code{\link{export.chdata}} like the
  change in \code{\link{make.mark.model}} to accomodate
  change with release of version R2.9.0. \item Made a
  change in \code{\link{process.data}} so that
  \code{strata.labels} can be specified for Multistrata
  designs like with ORDMS so an unobserved strata can be
  included. \item A warning was added to the help file for
  \code{\link{export.chdata}} and
  \code{\link{export.model}} so it is clear that the MARK
  database must be created correctly and with the .inp file
  created by \code{\link{export.chdata}} from the processed
  data that was used to create the models that are being
  exported.  This is to ensure that the group structure is
  setup such that the assumed model structure for groups
  matches the model structure setup in the .inp file. }
  Version 1.9.0 (30 April 2009) \itemize{ \item Fixed a bug
  in \code{\link{summary.mark}} which occasionally produced
  erroneous results with \code{showall=FALSE}. \item Made a
  change in \code{\link{make.mark.model}} to accomodate
  change with release of version R2.9.0. \item RMark now
  requires R version 2.8.1 or higher. } Version 1.8.9 (9
  March 2009) \itemize{ \item Changed
  \code{\link{model.average.marklist}} and
  \code{\link{covariate.predictions}} to set NaN or Inf
  results in v-c matrix to 0 to cope with poorly determined
  models.  Also, for each function the dropping of models
  is now restricted to cases in which there are negative
  variances for the betas being used in the averaged
  parameter estimates. Unused betas are ignored.  For
  example, if \code{\link{model.average}} is called with
  \code{parameter="Phi"}, then the model will only be
  dropped if there is a negative variance for one of the
  betas associated with "Phi". \item In
  \code{\link{var.components}} the tolerance value
  (\code{tol}) in the call to \code{uniroot} was reduced to
  1e-15 which should provide better estimates of the
  process variance when it is small.  Previously a process
  variance less than 1e-5 would be treated as 0. \item Made
  changes to \code{\link{cleanup}},
  \code{\link{coef.mark}}, and
  \code{\link{make.mark.model}} to accomodate externally
  saved model objects (\code{external=TRUE}). } Version
  1.8.8 (5 December 2008) \itemize{ \item An error was
  fixed in \code{\link{make.time.factor}} which created
  incorrect assignments when only some of the time
  dependent variables contained a "." for occasions with no
  data. \item Patched \code{\link{compute.design.data}}
  which was not creating the design data in the same order
  as the PIM construction for the newly added \code{ORDMS}
  model. \item Generalized section of code in
  \code{\link{make.mark.model}} to handle \code{mlogit}
  structure for \code{ORDMS} model. \item Fixed a bug in
  \code{\link{process.data}} in which the initial ages were
  not correctly assigned in some situations with multiple
  grouping variables.  Note that it is always a good idea
  to examine the design data after it is created to make
  sure it is structured properly because it relates the
  data and model structure via the grouping variables and
  the pre-defined variables (ie age, time etc), While I've
  done a lot of testing, I have certainly not tried every
  possible example and there is always the potential for an
  error to occur in a circumstance that I've not
  encountered. } Version 1.8.7 (13 November 2008) \itemize{
  \item An argument \code{common.zero} was added to
  function \code{\link{make.design.data}} and
  \code{\link{compute.design.data}}.  It can be set to TRUE
  to make the \code{Time} variable have a common time
  origin of \code{begin.time} which is useful for shared
  parameters like \code{p} and \code{c} in closed capture
  and similar models.  \item The function
  \code{\link{read.mark.binary}} was patched to work with
  the newer versions of MARK.EXE since 1 Oct 2008. \item
  The model type \code{ORDMS} for open robust design
  multi-state models was added. An example data set will be
  added at a later date after further testing has been
  completed. \item Some patches were made to fix some
  aspects of profile intervals and to fix adjustment by
  chat in \code{\link{summary.mark}} when \code{showall=F}.
  The notation for profile intervals is now included in the
  field \code{model$results$real$note} where \code{model}
  is the name of a mark model.  Previously an incomplete
  notation was kept in \code{model$results$real$fixed} but
  that field is now used exclusively to denote fixed
  parameters.  It is important to realize that profile
  intervals computed by MARK are only found in
  \code{model$results$real$note} and are not changed by a
  \code{chat} adjustment unless the model is re-run. None
  of the intervals computed by \code{RMark} and displayed
  by \code{\link{summary.mark}} are profile intervals. }
  Version 1.8.6 (28 October 2008) \itemize{ \item A bug in
  an error message for \code{initial.ages} in
  \code{\link{process.data}} was fixed. \item A new
  function \code{\link{var.components}} was added to
  provide variance components capability as in the MARK
  interface except that shrinkage estimators are not
  computed currently.  \item Fixed parameter values are now
  being reported correctly by
  \code{\link{covariate.predictions}}. Also over-dispersion
  (c>1) was not being included in the variances for
  parameters except those using the mlogit link.  \item
  Some utility functions were added including
  \code{\link{pop.est}},\code{\link{nat.surv}}, and
  \code{\link{extract.indices}}. \item The function
  \code{\link{model.average}} has been changed to a generic
  function. Currently it supports 2 classes: 1) list, and
  2) marklist.  The latter was the original
  \code{model.average} which has been renamed
  \code{\link{model.average.marklist}} and the first
  argument has been renamed \code{x} instead of
  \code{model.list} to match the standard generic function
  approach.  The previous syntax \code{model.average(...)}
  will work as long as the usage does not name the first
  agument as in the example
  \code{model.average(model.list=dipper.results,...)}.  The
  list formulation (\code{\link{model.average.list}}) was
  created to enable a generic model averaging of estimates
  instead of just real parameter estimates from a
  \code{mark} model.  It could be used with any set of
  estimates, model weights and estimates of precision.
  \item A change is needed to
  \code{\link{read.mark.binary}} to accomodate the change
  to mark.exe with the version dated 1 Oct 2008. Some data
  types (notably Nest survival) may not work with the new
  version of mark.exe.  Working with Gary to make the
  patch. If you need an older version of mark.exe contact
  me. } Version 1.8.5 (8 October 2008) \itemize{ \item A
  bug in \code{\link{process.data}} was fixed that
  prevented use of a dataframe contained in a list while
  using the \code{groups} argument. \item Profile intervals
  on the real parameters can now be obtained from MARK
  using the arguments \code{profile.int} and optionally
  \code{chat} in \code{\link{mark}}. The argument
  \code{profile.int} can be set to \code{TRUE} and a
  profile interval will be constructed for all real
  parameters, or a vector of parameter indices can be
  specified to restrict the profiling to certain
  parameters.  The value specified by \code{chat} is passed
  to MARK for over-dispersion. \item References to cjs, js
  etc have been removed from here because this code was
  removed 5/11/11. \item Yet another fix to
  \code{\link{summary.ch}} which gave incorrect results for
  the number recaptured at least once when \code{marray=F}
  and the data contained non-unity values for \code{freq}.
  } Version 1.8.4 (29 August 2008) \itemize{ \item A
  generic function \code{\link{coef.mark}} was added to
  extract the table of betas from the model with the
  expression \code{coef(model)} where \code{model} is a
  \code{mark} model that has been run and contains output.
  The table includes standard errors and confidence
  intervals. \item An argument \code{brief} was added to
  \code{\link{summary.mark}}. If \code{brief=TRUE} the real
  parameters are not included in the summary. \item
  References to cjs, js etc have been removed from here
  because this code was removed 5/11/11. \item A bug in
  \code{\link{summary.ch}} was fixed.  It would produce
  erroneous results when the data contained a non-constant
  \code{freq} field. Results with the default of
  \code{freq=1} were fine. \item The function
  \code{\link{adjust.chat}} and its help file were changed
  such that it was clear that a \code{model.list} argument
  was needed. } Version 1.8.3 (25 July 2008) \itemize{
  \item For robust design models, an error trap was added
  to \code{\link{process.data}} to make sure that the
  capture history length matches the specification for the
  \code{time.intervals}.  This error was already trapped
  for non-robust models. \item Fixed an error in
  \code{\link{make.mark.model}} that prevented interaction
  model of session/time-specific individual covariates in a
  robust design model. \item Fixed an error in
  \code{\link{process.data}} so that the field \code{freq}
  is optional for nest survival data sets. \item
  \code{\link{print.mark}} was modified to add an argument
  \code{input} which if set to \code{input=TRUE} will have
  the MARK input file be displayed rather than the output
  file. Also, \code{wait=FALSE} was set in the system
  command which means the viewer window will be opened and
  you can carry on with R.  Before you had to close the
  viewer window before proceeding with R. \item An example
  \code{\link{RDOccupancy}} provided by Bret Collier was
  added for the Robust Occupancy model which shows the use
  of session and time-varying individual covariates in a
  robust design model. } Version 1.8.2 (26 June 2008)
  \itemize{ \item \code{\link{summary.ch}} was modified to
  allow missing cohorts (no captures/recaptures) for an
  occasion and to fix a bug in which \code{bygroup=FALSE}
  did not work when groups were defined. \item To avoid
  running out of memory, an argument \code{external} has
  been added to \code{\link{collect.models}},
  \code{\link{mark}}, \code{\link{rerun.mark}}, and
  \code{\link{run.mark.model}}.  As with all arguments of
  \code{\link{mark}}, \code{external} can also be set in
  \code{\link{mark.wrapper}}.  Likewise, \code{external}
  can also be set in \code{\link{run.models}} and it is
  passed to \code{\link{run.mark.model}}. The default is
  \code{external=FALSE} but if it is set to \code{TRUE}
  then the mark model object is saved in an external file
  with an extension \code{.rda} and the same base filename
  as its matching MARK output files. The mark object in the
  workspace is a character string which is the name of the
  file with the saved image (e.g., "mark001.rda").  If
  \code{external=TRUE} with \code{\link{mark.wrapper}} then
  the resulting marklist contains a list entry for each
  mark model which is only the filename and then the last
  entry is the \code{model.table}.  All of the functions
  recognize the dual nature of the mark object (i.e.,
  filename or mark object) in the workspace.  So even if
  the mark object only contains the filename, functions
  like \code{\link{print.mark}} or
  \code{\link{summary.mark}} will work.  However, if you
  have used \code{external=TRUE} and you want to look at
  part of a mark object without using one of the functions,
  then use the function \code{load.model}. Whereas, before
  you may have typed \code{mymark$results}, if you use
  \code{external=TRUE}, you would replace the above with
  \code{load.model(mymark)$results}. \item Functions
  \code{\link{store}} and \code{\link{restore}} were
  created to \code{store} externally and \code{restore}
  models from external storage into the R workspace.  They
  work on a \code{marklist} and are only needed to
  \code{store} externally existing marklist models or ones
  originally created with \code{external=FALSE} or to
  \code{restore} if you change your mind and decide to keep
  them in the R workspace.  \item Error in setup for robust
  design occupancy models with more than 2 primary sessions
  was fixed.  The error resulted in mark.exe crashing.
  \item The concept of time-varying individual covariates
  has been expanded to include robust design models which
  have both primary (session) and secondary (time)
  occasion-specific data.  For a robust design, a
  time-varying individual covariate can be either
  session-dependent or session-time dependent.  As an
  example, if there are 3 primary sessions and each has 4
  secondary occasions, then the individual covariates can
  be named x1,x2,x3 to be primary session-dependent or
  named x11,x12,x13,x14,x21,x22,x23,x24,x31,x32,x33,x34.
  The value of x can be any name for the covariate.  In the
  formula only the base name is used (e.g., \code{~x}) and
  RMark fills in the individual covariate names that it
  finds that match either the session or session-time
  individual covariates. } Version 1.8.1 (19 May 2008)
  \itemize{ \item Added function \code{\link{summary.ch}}
  to provide summaries of the capture history data
  (resighting matrices and m-arrays). It will not work with
  all types of models at present.  It will work with CJS
  and Jolly-type models. \item Added argument
  \code{model.name} to \code{\link{model.table}} to be able
  to use alternate names in the model table. It can use
  either the model name with each mark object which uses a
  formula notation (the current approach) or it can use the
  name of the R object containing the mark model
  (\code{model.name=FALSE}). See \code{\link{model.table}}
  for an example. Also, the help file for
  \code{model.table} was updated to reflect the code
  changes implemented in version 1.7.3. \item Code in
  \code{\link{mark.wrapper}} was modified to output the
  number of columns and column names of the design matrix
  for each model if \code{run=FALSE}.  This allows a check
  of each of the columns included in the model.  By
  reviewing these you can assess whether the model was
  constucted as you intended.  If there is any question you
  can either use \code{\link{model.matrix}} or
  \code{\link{make.mark.model}} to examine the design
  matrix more thoroughly. \item A bug in the new function
  \code{\link{merge.design.covariates}} was fixed in which
  \code{merge} was sorting the design data which does
  obvious bad things.  Adding \code{sort=FALSE} does not
  appear to mean that the data frame is left in its
  original order. To prevent this, the dataframe is forced
  to remain in its original order by adding a sequence
  field for re-sorting after the merge. } Version 1.8.0 (8
  May 2008) \itemize{ \item Fixed a bug in
  \code{\link{model.average}} that caused it to fail and
  issue an error when any of the models included a time
  dependent covariate in the parameter being averaged.
  \item Added \code{\link{merge.design.covariates}} which
  is meant to replace \code{merge.occasion.data}. This new
  function allows covariates to be assigned by \code{time},
  \code{time} and \code{group}, or just \code{group}. It
  also uses a simplified list of arguments and works with
  individual design dataframes rather than the entire ddl.
  It uses the R function \code{\link{merge}} which can be
  used on its own to merge design covariates into the
  design data.  You can use \code{\link{merge}} directly as
  this function only checks for some common mistakes before
  it calls \code{merge} and it handles reassignemnt of row
  names in the case were design data have been deleted.  An
  example, where you might want to use \code{merge} instead
  of this function would be situations where the design
  data are not just group, time or group-time specific.
  For example, if groups were specified by two different
  factor variables say initial age and region and the
  design covariates were only region-specific. It would be
  more efficient to use \code{merge} directly rather than
  this function which would require an entry for each group
  which would be each pairing of inital age and region.  If
  you use \code{\link{merge}} and you deleted design data
  prior to merging, save the row.names, merge and then
  reassign the row.names. \item An argument \code{run} was
  added to \code{\link{mark.wrapper}}.  If set to FALSE,
  then it will run through each set of models in
  \code{model.list} and try to build each model but does
  not attempt to run it.  This is useful to check for and
  fix any errors in the formula before setting off a large
  run. If you use \code{run=FALSE} do not include arguments
  that are meant to be passed to
  \code{\link{run.mark.model}} like \code{adjust}. }
  Version 1.7.9 (7 April 2008) \itemize{ \item
  \code{\link{make.design.data}} was fixed so that
  \code{remove.unused=T} will work properly when different
  \code{begin.time} values are specified for each group. }
  Version 1.7.8 (12 March 2008) \itemize{ \item Changed the
  default link for N to log in the
  \code{\link{setup.parameters}} for the \code{HetClosed}
  and \code{FullHet}. It was incorrectly set to logit which
  created incorrect estimates to be computed in
  \code{\link{model.average}} because MARK forces the log
  link for N regardless of what is set in the input file. }
  Version 1.7.7 (6 March 2008) \itemize{ \item Supressed
  warning message that occurred with code to check the
  validity of the sin link in
  \code{\link{make.mark.model}}. \item Fixed a couple of
  bugs in \code{\link{covariate.predictions}} that
  prevented it from working for some cases after including
  code for the sin link. \item Added function
  \code{\link{release.gof}} to construct the RELEASE
  goodness of fit test and extract the TEST2 and TEST3
  final chi-square, df and P-values. } Version 1.7.6 (26
  Feb 2008) \itemize{ \item \code{\link{make.mark.model}}
  was modified to change the capitalization of the link
  functions and to remove all spaces after "=" in the input
  file for mark.exe. These differences were preventing the
  MARK interface from fully importing the model. Although
  the model would be imported and could be run inside the
  MARK interface, median c-hat would not run and would give
  an error stating "Invalid Link" for any model imported
  from RMark.  Now transfering a model from RMark to the
  MARK interface is fully functional (I hope).  If you want
  to import an output file that was created with a prior
  version of RMark without re-running it, use a text editor
  on the output file and remove any spaces before and after
  an = sign.  Then change the capitalization of the links
  to "Logit", "MLogit", "Log", "LogLog", "CLogLog",
  "Identity". \item The sin link is now supported if the
  formula for the parameter generates an identity matrix
  for the parameter. For example, if you use ~-1+time
  instead of ~time then the resulting design matrix will be
  an identity for time.  Likewise, for interactions use
  ~-1+group:time instead of ~group*time.  If you select the
  sin link and the resulting design matrix is not an
  identity for the parameter, an error will be given and
  the run will stop. \item To match the output from MARK,
  the confidence intervals for real parameters using any
  0-1 link including loglog,cloglog,logit and sin are now
  computed using the logit transformation.  For previous
  versions this will only affect any results that were
  using loglog and cloglog. Previously, it was using the
  chosen link to compute the se and the interval endpoints.
  The latter is still used for the log and identity links
  which are not bounded in 0-1. \item The model "Jolly" was
  added to the supported list of models. Parameters include
  Phi,p,Lambda,N.  It is not a particularly numerically
  stable model and often will not converge. Use of
  options="SIMANNEAL" in call to \code{\link{mark}} is
  recommended for better convergence.  It will take much
  longer to converge but is mroe reliable. } Version 1.7.5
  (24 Jan 2008) \itemize{ \item \code{\link{model.average}}
  was modified to ignore any models that did not run and
  either had no attached output file or no results. \item
  \code{\link{read.mark.binary}} and
  \code{\link{extract.mark.output}} were modified to
  extract and store the real.vcv matrix (var-cov matrix of
  the simplified real parameters) in the mark object if
  realvcv=TRUE. The default is realvcv=FALSE. This argument
  has been added to functions \code{\link{mark}},
  \code{\link{run.mark.model}} and
  \code{\link{rerun.mark}}. \item An argument delete has
  also been added to \code{\link{mark}} and
  \code{\link{run.mark.model}}.  The default value is FALSE
  but if set to TRUE it deletes all output files created by
  MARK after extracting the results.  This is most useful
  for simulations that could easily create thousands of
  output files and after extracting the results the model
  objects are no longer needed. This is just a convenience
  to replace the need to call \code{\link{cleanup}}. }
  Version 1.7.4 (10 Jan 2008) \itemize{ \item A bug in
  \code{\link{make.mark.model}} was fixed.  It was
  preventing creation of individual (site) covariate models
  for parameters with only a single parameter (single
  index) in certain circumstances like Psi1 in the
  MSOccupancy model. \item The fix to
  \code{merge.occasion.data} in version 1.7.1 did not work
  when design data had been deleted.  That has been
  remedied. \item Various functions with some operating
  specific calls have been modified so they will work on
  either Windows or Linux.  Thus, the there is a single
  file for all source/help for both operating systems in
  RMarkSource.zip. It can be downloaded to either Windows
  or Linux to build the package. You need to build the
  package for Linux but not for Windows. For Windows, you
  only need RMark.zip which contains the pre-built package
  which only needs to be installed. Currently, with Linux
  the variable MarkPath is ignored and mark.exe is assumed
  to be in the path.  Also, for Linux the default for
  MarkViewer is "pico" (an editor on some Linux machines).
  This can be modified in \code{\link{print.mark}} or by
  setting MarkViewer to a different value.  The one Linux
  specific function is \code{read.mark.binary.linux}.  The
  function \code{\link{extract.mark.output}} calls either
  \code{read.mark.binary.linux} or
  \code{\link{read.mark.binary}} depending on the operating
  system. A Linux version of mark.exe (32 or 64 bit) can be
  obtained from Evan. } Version 1.7.3 (4 Jan 2008)
  \itemize{ \item In working with the occupancy models, it
  became apparent that it would be useful to have a new
  function called \code{\link{make.time.factor}} which
  creates time-varying dummy variables from a time-varying
  factor variable.  An example is given using observer with
  the occupancy dataset \code{\link{weta}} from the
  MacKenzie et al Occupancy modelling book. \item To match
  the results in the book, I added arguments \code{use.AIC}
  and \code{use.lnl} to function \code{\link{model.table}}
  to construct a results table with AIC rather than AICc
  and -2LnL values. The latter is more useful with a mix of
  models some using individual covariates and others not.
  \item A modification was made to
  \code{\link{make.mark.model}} with the \code{MSOccupancy}
  model to fix the name of the added data for parameter
  \code{p1} when \code{share=TRUE} to be \code{p2}. For an
  example which uses p2 to construct an additive model, see
  \code{\link{NicholsMSOccupancy}}. } Version 1.7.2 (20 Dec
  2007) \itemize{ \item In changing code for the occupancy
  models, a brace was misplaced which prevented the nest
  survival models from working. This has been fixed.  Also,
  the example code for \code{\link{mallard}} and
  \code{\link{killdeer}} was modified to exclude the calls
  to process the input file.  This enables use of the
  function \code{example()} to run the example code (e.g.
  \code{example(mallard)}). From now on as I add examples
  they are being included in my test set to avoid this type
  of problem in the future. } Version 1.7.1 (14 Dec 2007)
  \itemize{ \item If you update with this version of RMark
  make sure to update MARK also, so you get the fixes for
  some of the occupancy models. \item A minor bug was fixed
  in function \code{merge.occasion.data} that created
  duplicate row names and prevented the design data from
  being used in a model. \item Thirteen different occupancy
  models were added. Models in the following list use the
  designation from MARK: \code{Occupancy, OccupHet,
  RDOccupEG, RDOccupPE, RDOccupPG, RDOccupHetEG,
  RDOccupHetPE, RDOccupHetPG,OccupRNPoisson},
  \code{OccupRNNegBin,OccupRPoisson,OccupRNegBin,MSOccupancy}.
  \code{Het} means it uses the Pledger mixture and those
  with \code{RD} are the robust design models. The 2 letter
  designations for the RD models are shorthand for the
  parameters that are estimated.  For \code{EG}, Psi,
  Epsilon, and Gamma are estimated, for \code{PE} gamma is
  dropped and for \code{PG}, Epsilon is dropped.  For the
  latter 2 models, Psi can be estimated for each primary
  occasion. The last 5 models include the Royle/Nichols
  count (RPoisson) and presence (RNPoisson) models and the
  multi-state occupancy model.  See
  \code{\link{salamander}} for an example of
  \code{Occupancy, OccupHet}, \code{\link{Donovan.7}} for
  an example of \code{OccupRNPoisson,OccupRNNegBin},
  \code{\link{Donovan.8}} for an example of
  \code{OccupRPoisson,OccupRNegBin}, see
  \code{\link{RDSalamander}} for an example of the robust
  design models and \code{\link{NicholsMSOccupancy}} for an
  example of \code{MSOccupancy}. \code{\link{salamander}}
  data. \item The functions \code{\link{create.model.list}}
  and \code{\link{mark.wrapper}} were modified so that a
  list of parameters can be used to loop.  This is useful
  in the situation with shared parameters such as \code{p1}
  and \code{p2} in the \code{MSOccupancy} model,
  \code{closed} models etc. See \code{p1.p2.different.dot}
  in \code{\link{NicholsMSOccupancy}} for an example.  It
  can also be useful if the model definitions are linked
  conceptually (e.g., when one parameter is time dependent,
  the other should also be time dependent). \item The "."
  value in an encounter history is now acceptable to RMark
  and gets passed to MARK for interpretation as a missing
  value. \item \code{\link{print.marklist}} was fixed to
  show the model table properly after a c-hat adjustment
  was made. The change in the code in version 1.6.5 to add
  parameter specific values to the model table had the
  side-effect of dropping the model name if c-hat was
  adjusted. } Version 1.7.0 (7 Nov 2007) \itemize{ \item A
  function \code{\link{deltamethod.special}} for
  computation of delta method variances of some special
  functions was added.  It uses the function
  \code{deltamethod} from the package \code{msm}.  You need
  to install the package \code{msm} from CRAN to use it.
  \item A more complete example (\code{\link{mallard}})
  created by Jay Rotella was added for the nest survival
  model. His script provides a nice tutorial for RMark and
  the utility of R to provide a wide-open capability to
  calculate/plot etc with the results. It also demonstrates
  the advantages of scripting in R to document your
  analysis and enable it to be repeated.  Before you use
  his tutorial you need to install the package plotrix from
  CRAN. At a later date, Jay has said he will add some
  additional examples to demonstrate use of the
  \code{deltamethod} function to create variances for
  functions of the results from MARK. \item Various changes
  were made to help files.  A more complete description of
  \code{\link{cleanup}} was given to tie into
  \code{\link{mallard}} example. } Version 1.6.9 (10 Oct
  2007) \itemize{ \item Nest survival model was added to
  list of MARK models supported by RMark. See
  \code{\link{killdeer}} for an example.  Note that the
  data structure for nest models is completely different
  from the standard capture history so the functions
  \code{import.chdata}, \code{export.chdata} and
  \code{convert.inp} do not work with nest data structure.
  \item Slight change was made to
  \code{\link{run.mark.model}} and
  \code{\link{print.mark}}to accomodate change in R 2.6.0.
  } Version 1.6.8 (2 Oct 2007) \itemize{ \item Changes were
  made to \code{merge.occasion.data} to enable group and
  time-specific design covariates to be added to the design
  data. \item Change was made to
  \code{\link{setup.parameters}} to use a log-link for N in
  the closed-capture models.  MARK forces that link for N
  but the change was needed for \code{\link{model.average}}
  which does the inverse-link computation.  Note that the
  reported N in \code{\link{model.average}} is actually f0
  (number not seen).  To get the correct values for N
  simply add M_t+1 (unique number captured) to f0.  That is
  the way MARK computes N.  The std error and confidence
  interval is on f0 such that the lower ci on N will never
  be less than M_t+1. \item An error was fixed in the
  output of \code{\link{model.average}}.  When you selected
  a specific parameter, it was giving a UCL which was a
  copy from one of the models and not the UCL from the
  model averaging.  If you didn't specify \code{vcv=T} it
  only showed the errant UCL and if you did specify
  \code{vcv=T} then it showed the correct LCL and UCL but
  then added the errant UCL in a column. This occurred
  because it was adding covariate data for the specific
  parameter and was shifted a column because of a change in
  1.6.1.  } Version 1.6.7 (7 Aug 2007) \itemize{ \item
  Changes were made in \code{\link{print.mark}},
  \code{\link{print.summary.mark}} and
  \code{\link{compute.design.data}} to acommodate changes
  in V2.5.1 of R. When upgrading versions of R problems may
  occur if RMark was built with an earlier version of R.
  The version of R that was used to build RMark is listed
  on the screen each time it is loaded with library(RMark)
  \code{This is RMark 1.6.7 Built: R 2.5.1;
  i386-pc-mingw32; 2007-08-07 09:00:33; windows} \item The
  help file for \code{\link{import.chdata}} was expanded to
  clarify the differences between it and
  \code{\link{convert.inp}} and the use of the \code{freq}
  field. } Version 1.6.6 (14 May 2007) \itemize{ \item
  Function \code{\link{make.mark.model}} was fixed so that
  the real label indices were properly written when
  \code{simplify=FALSE} is used. \item Function
  \code{\link{make.mark.model}} was also changed to remove
  the parameter simplification for \code{mlogit} parameters
  that was added in v1.4.5.  I mistakenly assumed that the
  \code{mlogit} parameters were setup such that the
  normalization to sum to 1 was done will all the real
  parameters in the set (i.e., all PSI for a single
  stratum).  In fact, the \code{mlogit} values are only
  specified for the unique real parameters so if there is
  any simplification and the sum of the probabilities is
  close to 1 (excluding subtraction value) the values will
  not be properly constrained.  For example, with the
  \code{\link{mstrata}} data if the problem was constrained
  such that PSI from AtoB was equal to AtoC, it is still
  necessary to have these as separate real parameters and
  constrain them with the design matrix. As it turns out,
  with the \code{\link{mstrata}} example it does not matter
  because the problem is such that the sum of Psi for AtoB
  and AtoC is not close to 1 (same for other strata) and
  any link will work. This change will only be noticeable
  in situations in which the constraint matters (i.e., the
  probability for the subtraction parameter is near 0).
  The change back to non-simplification for \code{mlogit}
  parameters may increase execution times because the
  design matrix size has been increased.  Previous users of
  the Multistrata design will see very little difference in
  there results if they only used models containing
  stratum:tostratum because that will create an
  all-different PIM within each \code{mlogit} set. When I
  ran the \code{\link{mstrata}} examples with this version
  and compared them to v1.6.5 the results were different
  but they were differences in the 5th or smaller decimal
  point due to differences in numerical optimization. }
  Version 1.6.5 ( 3 May 2007) \itemize{ \item Function
  \code{\link{model.table}} was modified to include
  parameter formula fields in the \code{model.table}
  dataframe of a \code{marklist}. Previously only the
  \code{model.name} was included which is a concatenation
  of the individual parameter formulas. The additional
  fields allows extracting the model table results based on
  one of the parameter formulas or to create a matrix of
  model AICc or other values with rows as one parameter and
  colums as the other.  See \code{\link{model.table}} for
  an example. \item Function \code{\link{process.data}} was
  modified such that factor variables used for grouping
  retain the ordering of the factor levels in the data
  file. Previously they would revert back to default
  ordering and the re-leveling would also have to be
  repeated on the design data also. \item An argument
  \code{brief} was added to \code{\link{mark}} to control
  amount of summary output. \item Fixed a bug in
  \code{\link{get.real}} that prevented computation for
  models without the stored covariate values. \item Added
  code to \code{\link{make.mark.model}} that prevents
  constructing models with empty rows in the design matrix
  unless the parameter is fixed.  For example, if you were
  to try ~-1+Time for the dipper data, it will fail now
  because there is no value for the intercept (Time=0).
  \item Function \code{\link{mark.wrapper}} outputs the
  model name to the screen before running the model which
  helps associate any error messages to the model if
  \code{output=F}. } Version 1.6.4 (7 March 2007) \itemize{
  \item A new function \code{\link{export.model}} was
  created to copy the output files into the naming
  convention needed to append them into a MARK .dbf/.fpt
  database so they can be used with the MARK interface
  features. This is useful to be able to use some of the
  features not contained in RMark such as median c-hat and
  variance component estimation. To create a MARK database,
  first use \code{\link{export.chdata}} to create a .inp
  file to pass the data into MARK.  Start MARK and use
  File/New to create a new database.  Select the
  appropriate Data Type (model in RMark) and fill in the
  appropriate values for encounter occasions etc.  For the
  Encounter Histories File Name, select the file you
  created with \code{\link{export.chdata}}. Once you have
  created the database in the Program MARK interface, click
  on the Browse menu item and then Output/Append and select
  the output file(s) (i.e those with a Y.tmp) that you
  exported with \code{\link{export.model}}.  Note that this
  will not work with output files run with versions of
  RMark prior to this one because the MARK interface will
  give a parse error for the design matrix. To get around
  that you can edit the output file and remove the spaces
  in the line with the design matrix header.  For example,
  it should look as follows \code{design matrix
  constraints=7 covariates=7} without spaces around the =
  sign. \item The minor change described above was made in
  the input file with spacing on the design matrix line to
  enable proper appending of the output into a MARK
  .dbf/.fpt database. \item The function
  \code{\link{cleanup}} was modified to delete all
  mark*.tmp files.  Do not use \code{cleanup} until you
  have appended any exported models. \item An argument
  \code{use.comments} was added to
  \code{\link{import.chdata}} to enable comment fields to
  be used as row names in the data frame.  A comment is
  indicated as in MARK with /* comment */.  They can be
  anywhere in the record but they must be unique and they
  can not have a column header (field name). \item Function
  \code{\link{create.model.list}} was modified such that it
  only includes lists with a \code{formula} element.  This
  prevents collecting other objects that are named
  similarly but are not model defintions. } Version 1.6.3
  (5 March 2007) \itemize{ \item A minor change in
  \code{\link{make.mark.model}} and
  \code{\link{find.covariates}} was made to accommodate use
  of the same covariate in different formulas (e.g. Phi and
  p). Previous code worked except any call to
  \code{\link{get.real}} would fail. Previously a duplicate
  of the covariate was entered in the data file to MARK.
  Now only a single copy is passed. \item An argument
  \code{default} has been added to the model definition
  (\code{parameters} in \code{\link{make.mark.model}} and
  \code{model.parameters} in \code{\link{mark}}). The
  argument sets the default value for parameters
  represented by design data that have been deleted. \item
  Checks were added in \code{\link{make.mark.model}} to
  fail if any of the individual covariates used are either
  factor variables or contain NAs. Both could fail in the
  MARK.EXE run but the error message would be less obvious.
  Factor variables can work as an individual covariate, if
  the levels are numeric.  But it was easier to exclude all
  factor variables from being individual covariates.  They
  can easily be converted to a continuous version (e.g.
  Blackduck$BirdAge=as.numeric(Blackduck$BirdAge)-1). The
  code for the \code{\link{Blackduck}} was changed to make
  BirdAge a continuous rather than factor variable.  Factor
  variables can still be used to define groups and then
  used in the formula.  They just can't be used as
  individual covariates. This change was made because a
  factor variable was in the data but not defined in
  \code{groups} and when it was used in the formula it
  would create a float error in MARK.EXE and that would be
  confusing and hard to track down. } Version 1.6.2 (28 Feb
  2007) \itemize{ \item The fix in 1.6.1 to avoid the
  incorrect design matrix was not sufficiently general and
  created a parse error in R if you attempted to use any
  design data covariates that were created with a cut
  function to create factor variables by binning a
  variable.  This has been corrected in this version. \item
  The code in \code{\link{read.mark.binary}} has been
  changed to skip over the v-c matrix for the derived
  parameters if it is not found in the file.  This was
  causing an error with the PRADREC model type. } Version
  1.6.1 (17 Jan 2007) \itemize{ \item An important bug was
  fixed in \code{\link{make.mark.model}} in which an
  incorrect design matrix would be created if you used two
  individual covariates in the same formula whereby one of
  the covariate names was contained within the other.  For
  example, if you used ~mass+mass2 where mass2=mass^2, it
  would actually create a design matrix with columns mass
  product(mass,mass2) which would be the model mass+mass^3.
  This happened due to the way the code identified columns
  where it needed to replace dummy values with individual
  covariate names. Since mass was contained in mass2 it
  added mass to the column as a product. The code now does
  exact matching so the error can no longer occur. \item An
  argument \code{indices} was added to the function
  \code{\link{model.average}} which enables restricting the
  model averaging to a specific set of parameters as
  identified by the all-different parameter indices.  This
  is most useful in large models with many different
  indices such that memory limitations are encountered in
  constructing the variance-covariance matrix of the real
  parameters.  For example, with a CJS analysis of data
  with 18 groups and 26 years of data, the number of
  parameter indices exceeds 22,000.  Even by restricting
  the parameters to either Phi or p with the
  \code{parameter} argument there are still 11,000 which
  would attempt to create a matrix containing 11,000 x
  11,000 elements which can exceed the memory limit. In
  most cases, there are far fewer unique parameters and
  this argument allows you to select which parameters to
  average. \item Time-varying covariates are no longer
  needed for all times if the formula is correctly written
  to exclude them in the resulting design matrix.
  \code{\link{make.mark.model}} still reports missing
  time-varying covariates but will continue to try and fit
  the model but if the missing variables are used in the
  design matrix the model will fail.  As an example
  consider a time varying covariate x for recapture times
  1990 to 1995.  The code expects to find variables x1990,
  x1991, x1992, x1993, x1994, x1995. However, lets say that
  the values are only known for 1993-1995.  If you define a
  variable I'll call recap in the design data which has a
  value 1 for 1993-1995 and a value 0 for 1990-1992 then if
  you use the formula ~recap:x the resulting design matrix
  will only use the known variables for 1993-1995 but you
  will still be warned that the other values (x1990 -
  x1992) are missing. \item A bug was fixed in
  \code{\link{extract.mark.output}} which prevented it from
  obtaining more than the last mean covariate value from
  the MARK output. \item \code{\link{fill.covariates}} was
  modified such that only a partial list of covariate
  values need to be specified with \code{data} and the
  remainder are filled in with default values depending on
  argument \code{usemean}. \item The output from
  \code{\link{summary.mark}} was modified for real
  parameters when \code{se=T} to include
  \code{all.diff.index} to provide the indices of each real
  parameter in the all-different PIM structure.  They are
  useful to restrict \code{\link{covariate.predictions}}
  and \code{\link{model.average}} to a specific set of real
  parameters. \item A new function
  \code{\link{covariate.predictions}} was created to
  compute real parameter values for multiple covariate
  values and their variance-covariance matrix. It will also
  model average those values if a marklist is passed to the
  function. Two examples from chapter 12 of Cooch and White
  are provided to give examples of models with individual
  covariates and the use of this function. \item The
  default value of \code{vcv} in
  \code{\link{model.average}} has been changed to
  \code{FALSE}. } Version 1.6.0 (27 Nov 2006) \itemize{
  \item A bug was fixed in \code{\link{PIMS}} which
  prevented it from working with Multistrata models. \item
  Bugs were fixed in \code{\link{make.design.data}} which
  prevented use of argument \code{remove.unused=T} with
  Multistrata models and also for any type of model when
  there were no grouping variables. \item Bugs were fixed
  in \code{\link{process.data}} which gave incorrect
  ordering of intial ages if the factor variable for the
  age group was numeric and more than two digits. Also, the
  number of groups in the data was not correct if the
  number of loss on capture records exceeded the number
  without loss on capture within a group. \item Bugs were
  fixed in \code{\link{setup.parameters}} and
  \code{\link{setup.model}} that prevented use of the
  Barker model and that reported an erroneous list of model
  names when an incorrect type of model was selected. }
  Version 1.5.9 (26 June 2006) \itemize{ \item A bug was
  fixed in \code{\link{convert.inp}} which prevented the
  code from working with groups and two or more covariates.
  Note that there are limitations to this function which
  may require some minor editing of the file.  The
  limitations have been added to the help file
  (\code{\link{convert.inp}}). } Version 1.5.8 (22 June
  2006) \itemize{ \item Argument \code{options} was added
  to \code{\link{mark}} and \code{\link{make.mark.model}}
  with a default NULL value. It is simply a character
  string that is tacked onto the \code{Proc Estimate}
  statement for the MARK .inp file.  It can be used to
  request options such as NoStandDM (to not standardize the
  design matrix) or SIMANNEAL (to request use of the
  simulated annealing optimization method) or any existing
  or new options that can be set on the estimate proc.
  \item A bug in \code{\link{model.table}} was fixed so it
  would accomodate the change from v1.3 to a marklist in
  which the model.table was switched to the last entry in
  the list. \item A bug in \code{\link{summary.mark}} was
  fixed so it would properly display QAICc when chat > 1.
  \item Function \code{\link{adjust.chat}} was modified
  such that it returns a marklist with each model having a
  new chat value and the model.table is adjusted for the
  new chat value. \item Function
  \code{\link{adjust.parameter.count}} was modfied so it
  returns the mark model object rather than using eval to
  modify the object in place.  The latter does not work
  with models in a marklist and calls made within
  functions. } Version 1.5.7 (8 June 2006) \itemize{ \item
  Argument \code{data} was added to function
  \code{\link{model.average}} to enable model averaging
  parameters at specific covariate values rather than the
  mean value of the observed data.  An example is given in
  the help file. \item Argument \code{parameter} of
  function \code{\link{model.average}} now has a default of
  NULL and if it is not specified then all of the real
  parameters are model averaged rather than those for a
  particular type of parameter (eg p or Psi). \item A bug
  was fixed in function \code{\link{compute.real}} that
  caused the function to fail for computations of
  \code{Psi}. } Version 1.5.6 (6 June 2006) \itemize{ \item
  \code{print.summary.mark} was modified so fixed
  parameters are noted. \item Argument \code{show.fixed}
  was added to \code{\link{summary.mark}} to control
  whether fixed parameters are shown as NA (FALSE) or as
  the value at which they were fixed.  If \code{se=T} the
  default is \code{show.fixed=T} otherwise
  \code{show.fixed=F}.  The latter is most useful in
  displaying values in PIM format (without std errors), so
  fixed values are displayed as blanks instead of NA. \item
  Argument \code{links} was added to
  \code{\link{convert.link.to.real}} and the default value
  for argument \code{model} is now NULL.  One or the other
  must be given. If the value for \code{links} is given
  then they are used in place of the links specified in the
  \code{model} object.  This provides for additional
  flexibility in changing link values for computation (eg
  use of log with mlogit). \item Argument \code{drop} was
  added to \code{\link{model.average}}.  If
  \code{drop=TRUE} (the default), then any model with one
  or more non-positive (0 or negative) variances is not
  used in the model averaging. \item An error in
  computation of the v-c matrix of mlogit link values in
  \code{compute.links.from.reals} was fixed. This did not
  affect confidence intervals for real parameters (eg Psi)
  in \code{model.average} because it uses the logit
  transformation for confidence intervals on real
  parameters that use mlogit link (eg Psi). \item
  \code{\link{get.real}} was unable to extract a single
  parameter value(eg constant Phi model).  This was fixed.
  \item The argument \code{parm.indices} was removed from
  the functions \code{\link{compute.real}} and
  \code{\link{convert.link.to.real}} because the subsetting
  can be done easily with the complete results returned by
  the functions.  This changed the examples in
  \code{\link{fill.covariates}}. \item
  \code{\link{compute.real}} and subsequently
  \code{\link{get.real}} return a field \code{fixed} when
  se=TRUE that denotes whether a real parameter is a fixed
  parameter or an estimated parameter at a boundary which
  is identified by having a standard error=0. } Version
  1.5.5 (1 June 2006) \itemize{ \item \code{model} has been
  deleted from the arguments in TransitionMatrix.  It was
  only being used to ascertain whether the model was a
  Multistrata model.  This is now determined more
  accurately by looking for the presence of
  \code{tostratum} in the argument \code{x} which is a
  dataframe created for \code{Psi} from the function
  \code{\link{get.real}}. The function also works with the
  estimates dataframe generated from
  \code{\link{model.average}}. See help for
  \code{\link{TransitionMatrix}} for an example. \item An
  argument \code{vcv} was added to function
  \code{\link{model.average}}.  If the argument is TRUE
  (the default value) then the var-cov matrix of the model
  averaged real parameters is computed and returned and the
  confidence intervals for the model averaged parameters
  are constructed. Models with non-positive variances for
  betas are reported and dropped from model averaging and
  the weights are renormalized for the remaining models.
  \item A new function
  \code{\link{compute.links.from.reals}} was added to the
  library to transform real parameters to its link space.
  It has 2 functions both related to model averaged
  estimates. Firstly, it is used to transform model
  averaged estimates so the normal confidence interval can
  be constructed on the link values and then
  back-transformed to real space.  The second function is
  to enable parametric bootstrapping in which the error
  distbution is assumed to be multivariate normal for the
  link values. From a single model, the link values are
  easily constructed from the betas and design matrix so
  this function is not needed.  But for model averaging
  there is no equivalent because the real parameters are
  averaged over a variety of models with the same real
  parameter structure but differing design structures.
  This function allows for link values and their var-cov
  matrix to be created from the model averaged real
  estimates. } Version 1.5.4 (30 May 2006) \itemize{ \item
  In function \code{\link{mark}} an argument \code{retry}
  was added to enable the analysis to be re-run up to the
  number of times specified.  An analysis is only re-run if
  there are "singular" beta parameters which means that
  they are either non-estimable (confounded) or they are at
  a boundary. Beginning with this version,
  \code{\link{extract.mark.output}} was modified such that
  the singular parameters identified by MARK are extracted
  from the output (if any) and the indices for the beta
  parameters are stored in the list element
  \code{model$results$singular}. The default value for
  \code{retry} is 0 which means it will not retry.  When
  the model is re-run the initial values are set to the
  values at the completion of the last run except for the
  "singular" parameters which are set to 0.  Using
  \code{retry} will not help if the parameters are
  non-estimable.  However, if the parameters are at a
  boundary because the optimization "converged" to a
  sub-optimal set of parameters, then setting \code{retry}
  to 1 or a suitably small value will often help it find
  the MLEs by moving away from the boundary. If the
  parameters are estimable and setting \code{retry} does
  not work, then it may be better to set new initial
  parameters by either specifying their values or using a
  model with similar parameters that did converge. \item A
  new function \code{\link{rerun.mark}} was created to
  simplify the process of refitting models with new
  starting values when the models were initially created
  with \code{\link{mark.wrapper}} which runs a list of
  models by using all combinations of the formulas defined
  for the various parameters in the model.  Thus,
  individual calls to \code{mark} are not constructed by
  the user and re-running an analysis from the resulting
  list would require constructing those calls. The argument
  \code{model.parameters} is now stored in the model object
  and it is used by this new function to avoid constructing
  calls to rerun the analysis.  With this new function you
  only need to specify the model list element to be
  refitted, the processed dataframe, the design data and
  the model list element (or different model) to be used
  for initial values. See \code{\link{rerun.mark}} for an
  example. \item To make \code{\link{rerun.mark}} a viable
  approach for all circumstances, the functions
  \code{\link{mark.wrapper}} and \code{\link{model.table}}
  were modified such that models that fail to converge at
  the outset (i.e., does not provide estimates in the
  output file) are stored in the model list created by the
  former function and they are reported as models that did
  not run and are skipped in the model.table by the latter
  function.  This enables a failed model to be reanalyzed
  with \code{\link{rerun.mark}} using another model that
  converged for starting values. } Version 1.5.3 (25 May
  2006) \itemize{ \item In function \code{\link{get.real}}
  a fix was made to accommodate constant pims and a warning
  is given if the v-c matrix for the betas has non-positive
  variances. \item In function
  \code{\link{make.mark.model}}, the argument
  \code{initial} can now be a single value which is then
  assigned as the initial value for all betas.  I have
  found this useful for POPAN models.  For some models I
  have run, the models fail to converge in MARK with the
  default initial values it uses (I believe it uses
  \code{initial=0}). I have had better luck using
  \code{initial=1}.  By allowing the use of a single value
  you can use the same generic starting value for each
  model without figuring out the number of betas in each
  model. Also note that you can specify another model that
  has already been run to use as initial values for a new
  model and it will match parameter values. \item A bad bug
  was fixed in \code{\link{cleanup}} which was
  unfortunately deleting files containing "out", "inp",
  "res" or "vcv" rather than those having these as
  extensions.  This happened without your knowledge if you
  chose ask=FALSE.  Good thing I had a backup.  Anyhow, I
  have now restricted it to files that are named by RMark
  with markxxxx.inp etc where xxxx is a numeric value.
  Thus if you assign your own basefile name for output
  files you'll have to delete them manually.  Better safe
  than sorry. } Version 1.5.2 (18 May 2006) \itemize{ \item
  Two new functions were added in this version.
  \code{\link{convert.inp}} converts a MARK encounter
  history input file to an RMark dataframe.  This will be
  particularly useful for those folks who have already been
  using MARK. Instead of converting and importing their
  data with \code{\link{import.chdata}} they can use the
  \code{\link{convert.inp}} to import their .inp file
  directly.  It can also be used to directly import any of
  the example .inp files that accompany MARK and the MARK
  electronic book
  (\url{http://www.phidot.org/software/mark/docs/book/}).
  The second new function is only useful for tutorials and
  for first time users trying to understand the way RMark
  works.  The function \code{\link{PIMS}} displays the full
  PIM structure or the simplified PIM structure for a
  parameter in a model.  The user does not directly
  manipulate PIMS in RMark and they are essentially
  transparent to the user but for those with MARK
  experience being able to look at the PIMS may help with
  the transition. } Version 1.5.1 (11 May 2006) \itemize{
  \item Functions \code{\link{compute.link}} and
  \code{\link{get.link}} were added to compute link values
  rather than the parameter estimates. \item A function
  \code{\link{convert.link.to.real}} was added to convert
  link estimates to real parameter estimates. Previously a
  similar internal function was used within
  \code{compute.real} but to provide more flexibility it
  was put into a separate function. \item An argument
  \code{beta} was added to \code{\link{get.real}} to enable
  it to be changed in the computation of the real
  parameters rather than always using the values in
  \code{results$beta}. \item A function
  \code{\link{TransitionMatrix}} was added to create a
  transition matrix for the Psi values.  It is provided for
  all strata including the \code{subtract.stratum}.
  Standard errors and confidence intervals can also be
  returned. \item \code{\link{make.mark.model}} was
  modified to include \code{time.intervals} as an element
  in the mark object. } Version 1.5.0 (9 May 2006)
  \itemize{ \item If output file already exists user is
  given option to create mark model from existing files.
  Only really useful if a bug occurs (which occurred to me
  from 1.4.9 changes) and once fixed any models already run
  can be brought into R by running the same model over and
  specifying the existing base \code{filename}.  Base
  \code{filename} values are no longer prefixed with MRK to
  enable this change. \item On occasion MARK will complete
  the analysis but fail to create the v-c matrix and v-c
  file.  The code has been modified to skip over the file
  if it is missing and output a warning. \item Two new
  functions have been added to ease handling of marklist
  objects. \code{\link{merge.mark}} merges an unspecified
  number of marklist and mark model objects into a new
  marklist with an optional model.table.
  \code{\link{remove.mark}} can be used to remove mark
  models from a marklist.  See \code{\link{dipper}} for
  examples of each function. \item Various changes were
  made to functions that compute real parameter estimates,
  their standard errors, confidence intervals and
  variance-covariance matrix.  The functions that were
  changed include
  \code{\link{compute.real}},\code{\link{find.covariates}},\code{\link{get.real}},\code{\link{fill.covariates}}.
  For examples, see help for latter two functions. }
  Version 1.4.9 (3 May 2006) \itemize{ \item Argument
  \code{initial} of \code{\link{make.mark.model}} was not
  working after model simplification was added in v1.2.
  This was modified to select initial values from the model
  based on names of design matrix columns rather than
  column contents which have different numbers of rows
  depending on the simplification. \item
  \code{\link{extract.mark.output}} was fixed to extract
  the correct -2LnL from the output file in situations in
  which initial values were specified. } Version 1.4.8 (25
  April 2006) \itemize{ \item Argument \code{silent} was
  added to \code{\link{mark}} and
  \code{\link{mark.wrapper}} with a default value of
  \code{FALSE}.  This overcomes the problem described above
  in 1.4.7. \item Code was added to
  \code{\link{collect.model.names}} to prevent it from
  tripping up when files contain an asterisk which R uses
  for special names. \item Use of T and F was properly
  changed to TRUE and FALSE in various functions to prevent
  errors when T or F are R objects. \item Code for naming
  files was modified to avoid problems when more than 999
  analyses were run in the same directory. \item Bug in
  setting fixed parameters with argument
  \code{fixed=list(index=,value=)} was corrected. \item
  Argument \code{remove.intercept} was added to parameter
  definition to force removal of intercept in designs with
  nested factor interactions with additional factor
  variables (e.g.,
  \code{Psi=list(formula=~sex+stratum:tostratum,remove.intercept=TRUE)}).
  } Version 1.4.7 (10 April 2006) \itemize{ \item An error
  was fixed in the \code{Psi} simplification code. Note
  that with the fix in 1.4.2 to trap errors, a side effect
  is that non-trapped errors that occur in the R code will
  now fail without any error messages.  If the error occurs
  in making the model, then the model will not be run, but
  you will not receive a message that the model failed.  I
  may have to make the error trapping a user-settable
  option to provide better error tracking. } Version 1.4.6
  (7 April 2006) \itemize{ \item Assurance code was added
  to test that the mlogits were properly assigned.  An
  error message will be given if there has been any
  unforeseen problem created by the simplification.  This
  eliminates any need for the user to check them as
  described under 1.4.5 above. } Version 1.4.5 (6 April
  2006) \itemize{ \item For multistrata models, the code
  for creating the mlogit links for Psi was not working
  properly if there was more than one group.  This was
  fixed in this version. \item Simplification of the PIMS
  has now been extended to include mlogit parameters.  That
  was not a trivial exercise and while I feel confident it
  is correct, double check the assignment of mlogit links
  for complex models, as I have not checked many examples
  at present.  Within a stratum, the corresponding elements
  for Psi for each of the tostratum (movement from stratum
  to each of the other strata excluding the
  \code{subtract.stratum}) should have the same mlogit(xxx)
  value such that it can properly compute the value for
  \code{subtract.stratum} by subtraction. } Version 1.4.4
  (4 April 2006) \itemize{ \item By including the test on
  model failure, errors that would stop program were not
  being displayed.  This has been fixed in this version.
  \item An error was fixed in using time-varying covariates
  when some of the design data had been deleted. } Version
  1.4.3 (30 March 2006) \itemize{ \item Problem with pop up
  window has been fixed.  It will no longer appear if the
  model does not converge but the model will show as having
  failed. \item An error was fixed in extracting output
  from the MARK output file when for some circumstances the
  label for beta parameters included spaces.  This now
  works properly. } Version 1.4.2 (14 March 2006) \itemize{
  \item Errors in the FORTRAN code were preventing
  completion of large batch jobs.  Now these errors are
  caught and models that fail are reported and skipped
  over.  Unfortunately, it does require user intervention
  to close the popup window.  Make sure you select Yes to
  close the window especially if you use the default
  \code{invisible=FALSE} such that the window does not
  appear.  If you select No, you will not able to close the
  window and R will hang. \item A new list element was
  added to \code{parameters} in
  \code{\link{make.design.data}} for parameters such as
  \code{Psi} to set the value of \code{tostratum} that is
  computed by subtraction. The default is to compute the
  probabilitity of remaining in the stratum.  The following
  is an example with strata A to D and setting A to be
  computed by subtraction for each stratum: \cr
  ddl=make.design.data(data.processed, \cr
  parameters=list(Psi=list(pim.type="constant",subtract.stratum=c("A","A","A","A")),
  \cr
  p=list(pim.type="constant"),S=list(pim.type="constant")))
  } Version 1.4.1 (11 March 2006) \itemize{ \item A value
  "constant" was added for the argument \code{pim.type}.
  Note that \code{pim.type} is only used for triangular
  PIMS. See \code{\link{make.design.data}} \item Some code
  changes were made to \code{\link{make.mark.model}} which
  lessen time to create the MARK input file for large
  models. \item Function \code{\link{add.design.data}} was
  modified to accomodate robust design and deletion of
  design data; this was missed in v1.4 changes. \item
  \code{model.name} argument in \code{\link{mark}} and
  \code{\link{make.mark.model}} was not working.  This was
  fixed. } Version 1.4 (9 March 2006) \itemize{ \item
  Robust design models added. See \code{\link{robust}} for
  an example. \item Function \code{\link{cleanup}} was
  modified so warning messages/errors do not occur if no
  models/files are found. \item Parameters in the design
  matrix are now ordered in the same consistent
  arrangement.  In prior versions they were arranged based
  on their order in the argument call. \item Argument
  \code{right} was added to \code{\link{make.design.data}},
  \code{\link{add.design.data}} and in
  \code{design.parameters} of \code{\link{make.mark.model}}
  to control whether bins are inclusive on the right
  (default).  The \code{\link{robust}} example uses this
  argument in a call to \code{\link{mark}}. } Version 1.3
  (22 Feb 2006) \itemize{ \item Time varying covariates can
  now be included in the model formula. See
  \code{\link{make.mark.model}} for details. \item New
  model types for Known (Known-fate) and Multistrata (CJS
  with different strata) were added. See
  \code{\link{Blackduck}} and \code{\link{mstrata}} for
  examples. \item Specific rows of the design data can now
  be removed for parameters that should not be estimated.
  Default fixed values can be assigned.  The function
  \code{\link{make.design.data}} now accepts an argument
  \code{remove.unused} which can be used to automatically
  remove unused design data for nested models. It's
  behavior is also determined by the new argument
  \code{default.fixed} in \code{\link{make.mark.model}}.
  \item \code{\link{summary.mark}} now produces a summary
  object and \code{\link{print.summary.mark}} prints the
  summary object. Changes were made to output when
  \code{se=T}. \item A new function
  \code{merge.occasion.data} was created to add occasion
  specific covariates to the design data. \item New
  functions \code{\link{mark.wrapper}} and
  \code{\link{create.model.list}} were created to automate
  running models from a set of model specifications for
  each model parameter. \item The argument
  \code{begin.time} in \code{\link{process.data}} can now
  be a vector to enable a different beginning time for each
  group. \item An argument \code{pim.type} was added to
  parameter specification to enable using pims with time
  structure for data sets with a single release cohort for
  CJS. See \code{\link{make.design.data}} \item Model lists
  created with \code{\link{collect.models}} are now given
  the class "marklist" which is used with
  \code{\link{cleanup}} and \code{\link{print.marklist}}
  (see \code{\link{print.mark}}). \item The function
  \code{\link{collect.models}} now places the model.table
  at the end of the returned list such that each model
  number in model.table is now the element number in the
  returned list. Previously it was 1+ that number. \item
  Input, output, v-c and residual results files from MARK
  are now stored in the directory containing the
  \code{.Rdata} workspace.  They are numbered consecutively
  and the field \code{output} contains the base filename.
  The function \code{\link{cleanup}} was created to delete
  files that are no longer linked to \code{mark} or
  \code{marklist} objects. \item Model averaged estimates
  and standard errors of real parameters can be obtained
  with the function \code{\link{model.average}}. } Version
  1.2 (4 Oct 2005) \itemize{ \item By default the PIM
  structure is simplified to use the fewest number of
  unique parameters.  This reduces the size of the design
  matrix and should reduce run times. \item The above
  change was made in some versions still numbered 1.1, but
  it contained an error that caused the links command for
  MARK to be constructed incorrectly. \item \code{adjust}
  argument has been added to \code{\link{collect.models}}
  to enable control of number of parameters and resulting
  AIC values. \item \code{model.list} in
  \code{\link{model.table}} and \code{\link{adjust.chat}}
  can now also be a list of models created by
  \code{\link{collect.models}} which allows operating on
  sets of models. }
}
\author{
  Jeff Laake
}

