\name{Poisson_twoMR}
\alias{Poisson_twoMR}
\docType{data}
\title{
Example of Poisson Mark-Resight model
}
\description{
Data and example illustrating Poisson Mark-Resight model with 2 groups and one occasion.
}
\usage{data(Poisson_twoMR)}
\format{
  A data frame with 93 observations on the following 2 variables.
  \describe{
    \item{\code{ch}}{a character vector}
    \item{\code{pg}}{a factor with levels \code{group1} \code{group2}}
  }
}
\examples{
data(Poisson_twoMR)
pois.proc=process.data(Poisson_twoMR,model="PoissonMR",groups="pg",
		counts=list("Unmarked Seen"=matrix(c(1237,588),nrow=2,ncol=1),
				    "Marked Unidentified"=matrix(c(10,5),nrow=2,ncol=1),
					"Known Marks"=matrix(c(60,0),nrow=2,ncol=1)))
pois.ddl=make.design.data(pois.proc)
mod=mark(pois.proc,pois.ddl,
		model.parameters=list(alpha=list(formula=~1),
				              U=list(formula=~-1+group),
				              sigma=list(formula=~1,fixed=0)),
	 	                      initial=c(0.9741405 ,0.0000000 ,6., 5.))
summary(mod)	 	                 
}
\keyword{datasets}
