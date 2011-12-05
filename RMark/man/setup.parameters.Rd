\name{setup.parameters}
\alias{setup.parameters}
\title{Setup parameter structure specific to model (internal use)}
\usage{
  setup.parameters(model, parameters = list(), nocc = NULL,
    check = FALSE, number.of.groups = 1)
}
\arguments{
  \item{model}{type of model ("CJS", "Burnham" etc)}

  \item{parameters}{list of model parameter specifications}

  \item{nocc}{number of occasions (value only specified if
  needed)}

  \item{check}{if TRUE only the vector of parameter names
  is returned \code{par.list}}

  \item{number.of.groups}{number of groups defined for
  data}
}
\value{
  The return value depends on the argument \code{check}. If
  it is TRUE then the return value is a vector of the names
  of the parameters used in the specified type of model.
  For example, if \code{model="CJS"} then the return value
  is \code{c("Phi","p")}.  This is used by the function
  \code{\link{valid.parameters}} to make sure that
  parameter specifications are valid for the model (i.e.,
  specifying recovery rate r for "CJS" would give an
  error).  If the function is called with the default of
  \code{check=FALSE}, the function returns a list of
  parameter specifications which is a modification of the
  argument \code{parameters} which adds parameters not
  specified and default values for all types of parameters
  that were not specified. The list length and names of the
  list elements depends on the type of model. Each element
  of the list is itself a list with varying numbers of
  elements which depend on the type of parameter although
  some elements are the same for all parameters.  Below the
  return value list is shown generically with parameters
  named p1,...,pk.  \tabular{ll}{ \code{p1} \tab List of
  specifications for parameter 1 \cr \code{p2} \tab List of
  specifications for parameter 2 \cr . \tab \cr .  \tab \cr
  .  \tab \cr \code{pk} \tab List of specifications for
  parameter k \cr }

  The elements for each parameter list all include:
  \tabular{ll}{ \code{begin} \tab 0 or 1; beginning time
  for the first \cr \tab parameter relative to first
  occasion \cr \code{num} \tab 0 or -1; number of
  parameters relative to \cr \tab number of occassions \cr
  \code{type} \tab type of PIM structure; either "Triang"
  or "Square" \cr \code{formula} \tab formula for parameter
  model (e.g., \code{~time}) \cr \code{link} \tab link
  function for parameter (e.g., \code{"logit"}) \cr }

  and may include: \tabular{ll}{ \code{share} \tab only
  valid for p in closed capture models; \cr \tab if TRUE p
  and c models shared \cr \code{mix} \tab only valid for
  closed capture heterogeneity \cr \tab models; if TRUE
  mixtures are used \cr \code{rows} \tab only valid for
  closed capture heterogeneity models \cr \code{fixed} \tab
  fixed values specified by user and \cr \tab not used
  modified in this function \cr }
}
\description{
  Defines list of parameters used in the specified type of
  model (\code{model}) and adds default values for each
  parameter to the list of user specified values (eg
  formula, link etc) defined in the call to
  \code{\link{make.mark.model}}
}
\details{
  The primary difference in setting up models for MARK is
  the number and types of parameters that are included in
  the model.  This function sets up the list of parameters
  used in the model and defines values for each parameter
  that affect how the PIM and design data are structured in
  the input file for program MARK.  Some of the values of
  the parameter list are user specified such as
  \code{formula}, \code{link},\code{fixed} so this function
  only adds to the list of values that are not specified by
  the user.  That is, it takes the input argument
  \code{parameters} and adds list elements for parameters
  not specified by the user and adds default values for
  each type of parameter and then returns the modified
  list. The structure of the argument \code{parameters} and
  the return value of this function are the same as the
  structure of the argument \code{parameters} in
  \code{\link{make.mark.model}} and argument
  \code{model.parameters} in \code{\link{mark}}.  They are
  lists with an element for each type of parameter in the
  model and the name of each list element is the parameter
  name (e.g., "p", "Phi","S", etc).  For each parameter
  there are a list of values (e.g., formula, link, num etc
  as defined below).  Thus \code{parameters} is a list of
  lists.
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{setup.model}},\code{\link{valid.parameters}}
}
\keyword{utility}

