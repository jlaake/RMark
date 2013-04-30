\name{run.mark.model}
\alias{run.mark.model}
\title{Runs analysis with MARK model using MARK.EXE}
\usage{
  run.mark.model(model, invisible = FALSE, adjust = TRUE,
    filename = NULL, prefix = "mark", realvcv = FALSE,
    delete = FALSE, external = FALSE, threads = -1,
    ignore.stderr = FALSE)
}
\arguments{
  \item{model}{MARK model created by
  \code{\link{make.mark.model}}}

  \item{invisible}{if TRUE, exectution of MARK.EXE is
  hidden from view}

  \item{adjust}{if TRUE, adjusts number of parameters
  (npar) to number of columns in design matrix, modifies
  AIC and records both}

  \item{filename}{base filename for files created by
  MARK.EXE. Files are named filename.*.}

  \item{prefix}{base filename prefix for files created by
  MARK.EXE; the files are named prefixnnn.*}

  \item{realvcv}{if TRUE the vcv matrix of the real
  parameters is extracted and stored in the model results}

  \item{delete}{if TRUE the output files are deleted after
  the results are extracted}

  \item{external}{if TRUE the mark object is saved
  externally rather than in the workspace; the filename is
  kept in its place}

  \item{threads}{number of cpus to use with mark.exe if
  positive or number of cpus to remain idle if negative}

  \item{ignore.stderr}{If set TRUE, messages from mark.exe
  are suppressed; they are automatically suppressed with
  Rterm}
}
\value{
  model: MARK model object with the base filename stored in
  \code{output} and the extracted \code{results} from the
  output file appended onto list; see \code{\link{mark}}
  for a detailed description of a \code{mark} object.
}
\description{
  Passes input file from model (\code{model$input}) to
  MARK, runs MARK, gets \code{output} and extracts relevant
  values into \code{results} which is appended to the
  \code{mark} model object.
}
\details{
  This is a rather simple function that initiates the
  analysis with MARK and extracts the output. An analysis
  was split into two functions
  \code{\link{make.mark.model}} and \code{run.mark.model}
  to allow a set of models to be created and then run
  individually or collectively with
  \code{\link{run.models}}.  By default, the execution of
  MARK.EXE will appear in a separate window in which the
  progress can be monitored. The window can be suppressed
  by setting the argument \code{invisible=TRUE}. The
  function returns a \code{mark} object and it should be
  assigned to the same object to replace the original model
  (e.g., \code{mymodel=run.mark.model(mymodel)}). The
  element \code{output} is the base filename that links the
  objects to the output files stored in the same directory
  as the R workspace.  To removed unneeded output files
  after deleting mark objects in the workspace, see
  \code{\link{cleanup}}. \code{results} is a list of
  specific output values that are extracted from the
  output. In extracting the results, the number of
  parameters can be adjusted (\code{adjust=TRUE}) to match
  the number of columns in the design matrix, which assumes
  that it is full rank and that all of the parameters are
  estimable and not confounded.  This can be useful if that
  assumption is true, because on occasion MARK.EXE will
  report an incorrect number of parameters in some cases in
  which the parameters are at boundaries (e.g., 0 or 1 for
  probabilities).  If the true parameter count is neither
  that reported by MARK.EXE nor the number of columns in
  the design matrix, then it can be adjusted using
  \code{\link{adjust.parameter.count}}.

  If \code{filename} is assigned a value it is used to
  specify files with those names. This is most useful to
  capture output from a model that has already been run.
  If it finds the files with those names already exists, it
  will ask if the results should be extracted from the
  files rather than re-running the models.
}
\examples{
\donttest{
data(dipper)
for(sex in unique(dipper$sex))
{
	x=dipper[dipper$sex==sex,]
	x.proc=process.data(x,model="CJS")
	x.ddl=make.design.data(x.proc)
	Phi.dot=list(formula=~1)
	Phi.time=list(formula=~time)
	p.dot=list(formula=~1)
	p.time=list(formula=~time)
	cml=create.model.list("CJS")
	x.results=mark.wrapper(cml,data=x.proc,ddl=x.ddl,prefix=sex)
	assign(paste(sex,"results",sep="."),x.results)
}
rm(Male.results,Female.results,x.results)
cleanup(ask=FALSE,prefix="Male")
cleanup(ask=FALSE,prefix="Female")
}
}
\author{
  Jeff Laake
}
\seealso{
  \code{\link{make.mark.model}}, \code{\link{run.models}},
  \code{\link{extract.mark.output}},
  \code{\link{adjust.parameter.count}}, \code{\link{mark}},
  \code{\link{cleanup}}
}
\keyword{model}

