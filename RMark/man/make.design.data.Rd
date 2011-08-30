\name{make.design.data}
\alias{make.design.data}
\title{Create design dataframes for MARK model specification}
\arguments{
  \item{data}{Processed data list; resulting value from
  process.data}

  \item{parameters}{Optional list containing a list for
  each type of parameter (list of lists); each parameter
  list is named with the parameter name (eg Phi); each
  parameter list can contain vectors named
  age.bins,time.bins and cohort.bins \tabular{ll}{
  \code{subtract.stratum} \tab a vector of strata letters
  (one for each strata) \cr \tab that specifies the
  tostratum that is computed by subtraction \cr \tab for
  mlogit parameters like Psi\cr \code{age.bins} \tab bins
  for binning ages\cr \code{time.bins} \tab bins for
  binning times\cr \code{cohort.bins} \tab bins for binning
  cohorts\cr \code{pim.type} \tab either "all" for all
  different, "time" for column time structure, or \cr \tab
  "constant" for all values the same within the PIM\cr}}

  \item{remove.unused}{If TRUE, unused design data are
  deleted; see details below}

  \item{right}{If TRUE, bin intervals are closed on the
  right}

  \item{common.zero}{if TRUE, uses a common begin.time to
  set origin (0) for Time variable defaults to FALSE for
  legacy reasons but should be set to TRUE for models that
  share formula like p and c with the Time model}
}
\value{
  The function value is a list of data frames. The list
  contains a data frame for each type of parameter in the
  model (e.g., Phi and p for CJS).  The names of the list
  elements are the parameter names (e.g., Phi). The
  structure of the dataframe depends on the calling
  arguments and the model & data structure as described in
  the details above.
}
\description{
  For each type of parameter in the analysis model (e.g, p,
  Phi, r), this function creates design data based on the
  parameter index matrix (PIM) in MARK terminology. Design
  data relate parameters to the sampling and data
  structures; whereas \code{data} relate to the
  object(animal) being sampled. Design data relate
  parameters to time, age, cohort and group structure.  Any
  variables in the design data can be used in formulas to
  specify the model in \code{\link{make.mark.model}}.
}
\details{
  After processing the data, the next step is to create the
  design data for building the models which is done with
  this function.  The design data are different than the
  capture history data that relates to animals.  The types
  of parameters and design data are specific to the type of
  analysis.  For example, consider a CJS analysis that has
  parameters Phi and p.  If there are 4 occasions, there
  are 3 cohorts and potentially 6 different Phi and 6
  different p parameters for a single group.  The format
  for each parameter information matrix (PIM) in MARK is
  triangular.  RMark uses the all different formulation for
  PIMS by default, so the PIMs would be \preformatted{ Phi
  p 1 2 3 7 8 9 4 5 10 11 6 12 } If you chose
  pim.type="time" for each parameter in "CJS", then the
  PIMS are structured as \preformatted{ Phi p 1 2 3 4 5 6 2
  3 5 6 3 6 } That structure is only useful if there is
  only a single release cohort represented by the PIM.  If
  you choose this option and there is more than one cohort
  represented by the PIM then it will restrict the possible
  set of models that can be represented.

  Each of these parameters relates to different times,
  different cohorts (time of initial release) and different
  ages (at least in terms of time since first capture).
  Thus we can think of a data frame for each parameter that
  might look as follows for Phi for the all different
  structure: \preformatted{ Index time cohort age 1 1 1 0 2
  2 1 1 3 3 1 2 4 2 2 0 5 3 2 1 6 3 3 0 } With this design
  data, one can envision models that describe Phi in terms
  of the variables time, cohort and age.  For example a
  time model would have a design matrix like:
  \preformatted{ Int T2 T3 1 1 0 0 2 1 1 0 3 1 0 1 4 1 1 0
  5 1 0 1 6 1 0 1 } Or a time + cohort model might look
  like \preformatted{ Int T2 T3 C2 C3 1 1 0 0 0 0 2 1 1 0 0
  0 3 1 0 1 0 0 4 1 1 0 1 0 5 1 0 1 1 0 6 1 0 1 0 1 } While
  you could certainly develop these designs manually within
  MARK, the power of the R code rests with the internal R
  function \code{\link{model.matrix}} which can take the
  design data and create the design matrix from a formula
  specification such as \code{~time} or \code{~time+cohort}
  alleviating the need to create the design matrix
  manually.  While many analyses may only need age, time or
  cohort, it is quite possible to extend the kind of design
  data, to include different functions of these variables
  or add additional variables such as effort. One could
  consider design data for p as follows: \preformatted{
  Index time cohort age effort juvenile 7 1 1 1 10 1 8 2 1
  2 5 0 9 3 1 3 15 0 10 2 2 1 5 1 11 3 2 2 15 0 12 3 3 1 15
  1 } The added columns represent a time dependent
  covariate (effort) and an age variable of juvenile/adult.
  With these design data, it is easy to specify different
  models such as \code{~time}, \code{~effort},
  \code{~effort+age} or \code{~effort+juvenile}.

  With the simplest call:

  \code{ddl=make.design.data(proc.example.data)}

  the function creates default design data for each type of
  parameter in the model as defined by
  \code{proc.example.data$model}.  If
  \code{proc.example.data} was created with the call in the
  first example of \code{\link{process.data}}, the model is
  "CJS" (the default model) so the return value is a list
  with 2 data frames: one for Phi and one for p. They can
  be accessed as \code{ddl$Phi} (or \code{ddl[["Phi"]]})
  and \code{ddl$p} (or \code{ddl[["p"]]}) or as
  \code{ddl[[1]]} and \code{ddl[[2]]} respectively.  Using
  the former notation is easier and safer because it is
  independent of the ordering of the parameters in the
  list. For this example, there are 16 groups and each
  group has 21 Phi parameters and 21 p parameters. Thus,
  there are 336 rows (parameters) in the design data frame
  for both Phi and p and thus a total of 772 parameters.

  The default fields in each dataframe are \code{group},
  \code{cohort}, \code{age}, \code{time}, \code{Cohort},
  \code{Age}, and \code{Time}. The first 4 fields are
  factor variables, whereas \code{Cohort}, \code{Age} and
  \code{Time} are numeric covariate versions of
  \code{cohort}, \code{age}, and \code{time} shifted so the
  first value is always zero. In addition, for closed
  capture heterogeneity models a factor variable
  \code{mixture} is included. If \code{groups} were created
  in the call to \code{\link{process.data}}, then the
  factor variables used to create the \code{groups} are
  also included in the design data for each type of
  parameter.  If one of the grouping variables is an age
  variable it is named \code{initial.age.class} to
  recognize explicitly that it represents a static initial
  age and to avoid naming conflicts with \code{age} and
  \code{Age} variables which represent dynamic age
  variables of the age of the animal through time.  Non-age
  related grouping variables are added to the design data
  using their names in \code{data}.  For example if
  \code{proc.example.data} is defined using the first
  example in \code{process.data}, then the fields
  \code{sex}, \code{initial.age.class} (in place of
  \code{age} in this case), and \code{region} are added to
  the design data in addition to the \code{group} variable
  that has 16 levels. The levels of the \code{group}
  variable are created by pasting (concatenating) the
  values of the grouping factor in order.  For example, M11
  is sex=M, age class=1 and region=1.

  By default, the factor variables \code{age}, \code{time},
  and \code{cohort} are created such that there is a factor
  level for each unique value.  By specfying values for the
  argument \code{parameters}, the values of \code{age},
  \code{time}, and \code{cohort} can be binned (put into
  intervals) to reduce the number of levels of each factor
  variable. The argument \code{parameters} is specified as
  a list of lists. The first level of the list specifies
  the parameter type and the second level specifies the
  variables (\code{age}, \code{time}, or \code{cohort})
  that will be binned and the cutpoints (endpoints) for the
  intervals.  For example, if you expected that survival
  may change substantially to age 3 (i.e. first 3 years of
  life) but remain relatively constant beyond then, you
  could bin the ages for survival as 0,1,2,3-8.  Likewise,
  as well, you could decide to bin time into 2 intervals
  for capture probability in which effort and expected
  capture probability might be constant within each
  interval.  This could be done with the following call
  using the argument \code{parameters}:

  \preformatted{ddl=make.design.data(proc.example.data,
  parameters=list(Phi=list(age.bins=c(0,0.5,1,2,8)),
  p=list(time.bins=c(1980,1983,1986))))}

  In the above example, \code{age} is binned for Phi but
  not for p; likewise \code{time} is binned for p but not
  for Phi. The bins for age were defined as 0,0.5,1,2,8
  because the intervals are closed ("]" - inclusive) on the
  right by default and open ("(" non-inclusive) on the
  left, except for the first interval which is closed on
  the left.  Had we used 0,1,2,8, 0 and 1 would have been
  in a single interval.  Any value less than 1 and greater
  than 0 could be used in place of 0.5. Alternatively, the
  same bins could be specified as:

  \preformatted{ddl=make.design.data(proc.example.data,
  parameters=list(Phi=list(age.bins=c(0,1,2,3,8)),
  p=list(time.bins=c(1980,1984,1986))),right=FALSE)}

  To create the design data and maintain flexibility, I
  recommend creating the default design data and then
  adding other binned variables with the function
  \code{\link{add.design.data}}.  The 2 binned variables
  defined above can be added to the default design data as
  follows:

  \preformatted{ ddl=make.design.data(proc.example.data)
  ddl=add.design.data(proc.example.data,ddl,parameter="Phi",type="age",
  bins=c(0,.5,1,2,8),name="agebin")
  ddl=add.design.data(proc.example.data,ddl,parameter="p",type="time",
  bins=c(1980,1983,1986),name="timebin") }

  Adding the other binned variables to the default design
  data, allows models based on either time, timebin, or
  Time for p and age, agebin or Age for Phi. Any number of
  additional binned variables can be defined as long as
  they are given unique names. Note that R is case-specific
  so \code{~Time} specifies a model which is a linear trend
  over time ((e.g. Phi(T) or p(T) in MARK) whereas
  \code{~time} creates a model with a different factor
  level for each \code{time} in the data (e.g. Phi(t) or
  p(t) in MARK) and \code{~timebin} creates a model with 2
  factor levels 1980-1983 and 1984-1986.

  Some circumstances may require direct manipulation of the
  design data to create the needed variable when simple
  binning is not sufficient or when the design data is a
  variable other than one related to \code{time},
  \code{age}, \code{cohort} or \code{group} (e.g., effort
  index).  This can be done with any of the vast array of R
  commands.  For example, consider a situation in which
  1983 and 1985 were drought years and you wanted to
  develop a model in which survival was different in
  drought and non-drought years.  This could be done with
  the following commands:

  \code{ddl$Phi$drought=0}

  \code{ddl$Phi$drought[ddl$phi$time==1983 |
  ddl$Phi$time==1985]= 1}

  The first command creates a variable named drought in the
  Phi design data and initializes it with 0.  The second
  command changes the drought variable to 1 for the years
  1983 and 1985. The single brackets [] index a data frame,
  matrix or vector.  In this case \code{ddl$Phi$drought} is
  a vector and \code{ddl$Phi$time==1983 |
  ddl$Phi$time==1985} selects the values in which time is
  equal (==) to 1983 or ("|") 1985.  A simpler example
  might occur if we want to create a function of one of the
  continuous variables.  If we wanted to define a model for
  p that was a function of age and age squared, we could
  add the age squared variable as:

  \code{ddl$p$Agesq=ddl$p$Age^2}

  Any of the fields in the design data can be used in
  formulae for the parameters.  However, it is important to
  recognize that additional variables you define and add to
  the design data are specific to a particular type of
  parameter (e.g., Phi, p, etc). Thus, in the above
  example, you could not use Agesq in a model for Phi
  without also adding it to the Phi design data.  As
  described in \code{\link{make.mark.model}}, there is
  actually a simpler way to add simple functions of
  variables to a formula without defining them in the
  design data.

  The above manipulations are sufficient if there is only
  one or two variables that need to be added to the design
  data.  If there are many covariates that are
  time(occasion)-specific then it may be easier to setup a
  dataframe with the covariate data and use
  \code{\link{merge_design.covariates}}.

  The fields that are automatically created in the design
  data depends on the model.  For example, with models such
  as "POPAN" or any of the "Pradel" models, the PIM
  structure is called square which really means that it is
  a single row and all the rows are the same length for
  each group.  Thus, rectangular or row may have been a
  better descriptor.  Regardless, in this case there is no
  concept of a cohort within the PIM which is equivalent to
  a row within a triangular PIM for "CJS" type models.
  Thus, for parameters with "Square" PIMS the cohort (and
  Cohort) field is not generated.  The cohort field is also
  not created if \code{pim.type="time"} for "Triangular"
  PIMS, because that structure has the same structure for
  each row (cohort) and adding cohort effects would be
  inappropriate.

  For models with "Square" PIMS or \code{pim.type="time"}
  for "Triangular" PIMS, it is possible to create a cohort
  variable by defining the cohort variable as a covariate
  in the capture history data and using it as a variable
  for creating groups.  As with all grouping variables, it
  is added to the design data.  Now the one caution with
  "Square" PIMS is that they are all the same length.
  Imagine representing a triangular PIM with a set of
  square PIMS with each row being a cohort.  The resulting
  set of PIMS is now rectangular but the lower portion of
  the matrix is superfluous because the parameters
  represent times prior to the entry of the cohort,
  assuming that the use of cohort is to represent a birth
  cohort.  This is only problematic for these kinds of
  models when the structure accomodates age and the concept
  of a birth cohort.  The solution to the problem is to
  delete the design data for the superfluous parameters
  after is is created.  For example, let us presume that
  you used cohort with 3 levels as a grouping variable for
  a model with "Square" PIMS which has 3 occasions.  Then,
  the PIM structure would look as follows for Phi:
  \preformatted{ Phi 1 2 3 4 5 6 7 8 9 } If each row
  represented a cohort that entered at occasions 1,2,3 then
  parameters 4,7,8 are superfluous or could be thought of
  as representing cells that are structural zeros in the
  model because no observations can be made of those
  cohorts at those times.

  After creating the design data, the unneeded rows can be
  deleted with R commands or you can use the argument
  \code{remove.unused=TRUE}.  As an example, a code segment
  might look as follows if \code{chdata} was defined
  properly: \preformatted{
  mydata=process.data(chdata,model="POPAN",groups="cohort")
  ddl=make.design.data(mydata) ddl$Phi=ddl$Phi[-c(4,7,8),]
  } If cohort and time were suitably defined an easier
  solution especially for a larger problem would be
  \preformatted{
  ddl$Phi=ddl$Phi[as.numeric(ddl$Phi$time)>=as.numeric(ddl$Phi$cohort),]
  } Which would only keep parameters in which the time is
  the same or greater than the cohort.  Note that time and
  cohort would be factor variables and < and > do not make
  sense which is the reason for the \code{as.numeric} which
  translates the factor to a numeric ordering of factors
  (1,2,...) but not the numeric value of the factor level
  (e.g., 1987,1998).  Thus, the above assumes that both
  time and cohort have the same factor levels. The design
  data is specific to each parameter, so the unneeded
  parameters need to be deleted from design data of each
  parameter.

  However, all of this is done automatically by setting the
  argument \code{remove.unused=TRUE}.  It functions
  differently depending on the type of PIM structure.  For
  models with "Triangular" PIMS, unused design data are
  determined based on the lack of a release cohort.  For
  example, if there were no capture history data that
  started with 0 and had a 1 in the second position
  ("01.....") that would mean that there were no releases
  on occasion 2 and row 2 in the PIM would not be needed so
  it would be removed from the design data.  If
  \code{remove.unused=TRUE} the design data are removed for
  any missig cohorts within each group. For models with
  "Square" PIMS, cohort structure is defined by a grouping
  variable.  If there is a field named "cohort" within the
  design data, then unused design data are defined to occur
  when time < cohort.  This is particularly useful for age
  structured models which define birth cohorts.  In that
  case there will be sampling times prior to the birth of
  the cohort which are not relevant and should be treated
  as "structural zeros" and not as a zero based on
  stochastic events.

  If the design data are removed, when the model is
  constructed with \code{\link{make.mark.model}}, the
  argument \code{default.fixed} controls what happens with
  the real parameters defined by the missing design data.
  If \code{default.fixed=TRUE}, then the real parameters
  are fixed at values that explain with certainty the
  observed data (e.g., p=0).  That is necessary for models
  with "Square" PIMS (eg, POPAN and Pradel models) that
  include each capture-history position in the probability
  calculation.  For "Triangular" PIMS with "CJS" type
  models, the capture(encounter) history probability is
  only computed for occasions past the first "1", the
  release. Thus, when a cohort is missing there are no
  entries and the missing design data are truly superfluous
  and \code{default.fixed=FALSE} will assign the missing
  design data to a row in the design matrix which has all
  0s.  That row will show as a real parameter of (0.5 for a
  logit link) but it is not included in any parameter count
  and does not affect any calculation. The advantage in
  using this approach is that the R code recognizes these
  and displays blanks for these missing parameters, so it
  makes for more readable output when say every other
  cohort is missing. See \code{\link{make.mark.model}} for
  more information about deleted design data and what this
  means to development of model formula.

  For design data of "Multistrata" models, additional
  fields are added to represent strata.  A separate PIM is
  created for each stratum for each parameter and this is
  denoted in the design data with the addition of the
  factor variable \code{stratum} which has a level for each
  stratum.  In addition, for each stratum a dummy variable
  is created with the name of the stratum
  (\code{strata.label})and it has value 1 when the
  parameter is for that stratum and 0 otherwise.  Using
  these variables with the interaction operator ":" in
  formula allows more flexibility in creating model
  structure for some strata and not others.  All
  "Multistrata" models contain "Psi" parameters which
  represent the transitions from a stratum to all other
  strata.  Thus if there are 3 strata, there are 6 PIMS for
  the "Psi" parameters to represent transition from A to B,
  A to C, B to A, B to C, C to A and C to B.  The "Psi"
  parameters are represented by multimonial logit links and
  the probability of remaining in the stratum is determined
  by substraction.  To represent these differences, a
  factor variable \code{tostratum} is created in addition
  to \code{stratum}. Likewise, dummy variables are created
  for each stratum with names created by pasting "to" and
  the strata label (e.g., toA, toB etc). Some examples of
  using these variables to create models for "Psi" are
  given in \code{\link{make.mark.model}}.
}
\examples{
data(example.data)
proc.example.data=process.data(example.data)
ddl=make.design.data(proc.example.data)
ddl=add.design.data(proc.example.data,ddl,parameter="Phi",type="age",
bins=c(0,.5,1,2,8),name="agebin")
ddl=add.design.data(proc.example.data,ddl,parameter="p",type="time",
bins=c(1980,1983,1986),name="timebin")
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{process.data}},\code{\link{merge_design.covariates}},
  \code{\link{add.design.data}},
  \code{\link{make.mark.model}},
  \code{\link{run.mark.model}}
}
\keyword{utility}

