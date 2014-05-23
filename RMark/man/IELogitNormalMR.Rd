\docType{data}
\name{IELogitNormalMR}
\alias{IELogitNormalMR}
\title{Example of Immigration-Emigration LogitNormal Mark-Resight model}
\format{A data frame with 34 observations on the following variable.
\describe{ \item{ch}{a character vector} }}
\description{
Data and example illustrating Immigration-Emigration
LogitNormal Mark-Resight model
}
\examples{
\donttest{
data(IELogitNormalMR)
IElogitNor.proc=process.data(IELogitNormalMR,model="IELogitNormalMR",
	counts=list("Marked Superpopulation"=c(28, 29, 30, 30, 30, 33, 33, 33, 33, 34, 34, 34),
	"Unmarked Seen"=c(264, 161, 152, 217, 217, 160, 195, 159, 166, 152, 175, 190),
	"Marked Unidentified"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
		         time.intervals=c(0,0,0,1,0,0,0,1,0,0,0))
IElogitNor.ddl=make.design.data(IElogitNor.proc)
mod1=mark(IElogitNor.proc,IElogitNor.ddl,
		model.parameters=list(p=list(formula=~-1+session),
				              sigma=list(formula=~session),
							  alpha=list(formula=~-1+session:time),
							  Nstar=list(formula=~session),
							  Nbar=list(formula=~session)))
summary(mod1)

mod2=mark(IElogitNor.proc,IElogitNor.ddl,
		  model.parameters=list(p=list(formula=~-1+session:time),
							  sigma=list(formula=~session),
							  alpha=list(formula=~-1+session:time),
							  Nstar=list(formula=~session),
							  Nbar=list(formula=~session)),
							  initial=mod1)
summary(mod2)
}
}
\keyword{datasets}

