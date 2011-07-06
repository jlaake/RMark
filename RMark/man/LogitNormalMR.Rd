\name{LogitNormalMR}
\alias{LogitNormalMR}
\docType{data}
\title{
Example of LogitNormal Mark-Resight model
}
\description{
Data and example illustrating LogitNormal Mark-Resight model. 
}
\usage{data(LogitNormalMR)}
\format{
  A data frame with 35 observations on the following variable.
  \describe{
    \item{\code{ch}}{a character vector}
  }
}
\examples{
data(LogitNormalMR)
logitNor.proc=process.data(LogitNormalMR,model="LogitNormalMR",
		counts=list("Unmarked seen"=c(96,68,59),
				    "Marked Unidentified"=c(0,0,0,0,1,1,1,0,0,3,0,1)),
			         time.intervals=c(0,0,0,1,0,0,0,1,0,0,0))
logitNor.ddl=make.design.data(logitNor.proc)
mod=mark(logitNor.proc,logitNor.ddl)
summary(mod)
}
\keyword{datasets}
