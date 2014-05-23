\docType{data}
\name{brownie}
\alias{brownie}
\title{San Luis Valley mallard data}
\format{A data frame with 108 observations on the following 5 variables.
\describe{ \item{ch}{a character vector containing the encounter history of
each bird} \item{freq}{frequency of that encounter history} \item{ReleaseAge}{the age of the bird when it was
released}}}
\description{
A recovery data set for mallards in San Luis Valley
Colorado
}
\details{
This is a data set that accompanies program MARK as an
example for Brownie and Seber recovery model.  In those
input files it is in a summarized format. Here it is in the
LD encounter history format. The data can be stratified
using ReleaseAge (Adult, Young) as a grouping variable.

Note that in the MARK example the variable is named Age.
In the R code, the fields "age" and "Age" have specific
meanings in the design data related to time since release.
These will override the use of a field with the same name
in the individual covariate data, so the names "time",
"Time", "cohort", "Cohort", "age", and "Age" should not be
used in the individual covariate data with possibly the
exception of "cohort" which is not defined for models with
"Square" PIMS such as POPAN and other Jolly-Seber type
models.
}
\examples{
# brownie=import.chdata("brownie.inp",field.types=c("n","f"))
\donttest{
 data(brownie)
# default ordering of ReleaseAge is alphabetic so it is
# Adult, Young which is why initial.ages=c(1,0)
# Seber Recovery
br=process.data(brownie,model="Recovery",groups="ReleaseAge",age.var=1,initial.ages=c(1,0))
br.ddl=make.design.data(br,parameters=list(S=list(age.bins=c(0,1,10)),
                                           r=list(age.bins=c(0,1,10))),right=FALSE)
mod=mark(br,br.ddl,model.parameters=list(S=list(formula=~-1+age:time,link="sin"),
                                           r=list(formula=~-1+age:time,link="sin")))
# Brownie Recovery
br=process.data(brownie,model="Brownie",groups="ReleaseAge",age.var=1,initial.ages=c(1,0))
br.ddl=make.design.data(br,parameters=list(S=list(age.bins=c(0,1,10)),
                               f=list(age.bins=c(0,1,10))),right=FALSE)
mod=mark(br,br.ddl,model.parameters=list(S=list(formula=~-1+age:time,link="sin"),
                               f=list(formula=~-1+age:time,link="sin")))
mod=mark(br,br.ddl,model.parameters=list(S=list(formula=~-1+age,link="sin"),
                               f=list(formula=~-1+age,link="sin")))
#Random effects Seber recovery
br=process.data(brownie,model="REDead",groups="ReleaseAge",age.var=1,initial.ages=c(1,0))
br.ddl=make.design.data(br,parameters=list(S=list(age.bins=c(0,1,10)),
                                       r=list(age.bins=c(0,1,10))),right=FALSE)
mod=mark(br,br.ddl,model.parameters=list(S=list(formula=~age),r=list(formula=~age)))
#Pledger Mixture Seber recovery
br=process.data(brownie,model="PMDead",groups="ReleaseAge",
                           mixtures=3,age.var=1,initial.ages=c(1,0))
br.ddl=make.design.data(br,parameters=list(S=list(age.bins=c(0,1,10)),
                            r=list(age.bins=c(0,1,10))),right=FALSE)
mod=mark(br,br.ddl,model.parameters=list(pi=list(formula=~mixture),
                     S=list(formula=~age+mixture),r=list(formula=~age)))
br=process.data(brownie,model="PMDead",groups="ReleaseAge",
                     mixtures=2,age.var=1,initial.ages=c(1,0))
br.ddl=make.design.data(br,parameters=list(S=list(age.bins=c(0,1,10)),
                      r=list(age.bins=c(0,1,10))),right=FALSE)
mod=mark(br,br.ddl,model.parameters=list(pi=list(formula=~age),
                      S=list(formula=~age+mixture),r=list(formula=~age)))
}
}
\keyword{datasets}

