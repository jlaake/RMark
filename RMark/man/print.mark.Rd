\name{print.mark}
\alias{print.mark}
\alias{print.marklist}
\title{Print MARK objects}
\usage{
\method{print}{mark}(x,...,input=FALSE)
       \method{print}{marklist}(x,...)
}
\arguments{
  \item{x}{mark model object; or list of mark model objects
  created with \code{\link{collect.models}}}

  \item{...}{additional non-specified argument for S3
  generic function}

  \item{input}{if TRUE, prints mark input file; otherwise
  the output file}
}
\value{
None
}
\description{
If print is for a mark model, it displays MARK output file
or input file with MarkViewer (notepad.exe by default) so
it can be viewed. If print is for a marklist, it displays
the model.table if it exists. To display the output for a
mark model contained in a list, simply type the list value
(e.g., typing mymarklist[[2]] will display output for the
second model). The function \code{print.marklist} was
created to avoid accidental typing of the model list which
would call \code{print.mark} for each of the models.
}
\details{
If the model has been run (\code{model$output} exists) the
output file stored in the directory as identified by the
basefile name (\code{model$output}) and the suffix ".out"
is displayed with a call to \code{MarkViewer}. If
\code{input} is set to \code{TRUE} then the MARK input file
is displayed instead. By default the \code{MarkViewer} is
notepad but any program can be used in its place that
accepts the filename as the first argument. For example
setting \code{MarkViewer="wp"} will use wordperfect
(wp.exe) as long as wp.exe is in the search path.
\code{MarkViewer} must be set during each R session, so it
is best to include it in your \code{.First} function to
change it permanently.  Since \code{print.mark} is the
generic function to print \code{mark} objects you can use
it by just typing the name of a \code{mark} object at the R
prompt and it will call \code{print.mark}. For example, if
\code{mod} is a \code{mark} object then typing \code{mod}
is the same as \code{print.mark(mod)}
}
\author{
Jeff Laake
}
\seealso{
\code{\link{summary.mark}}
}
\keyword{utility}

