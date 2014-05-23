\docType{data}
\name{RDSalamander}
\alias{RDSalamander}
\title{Robust design salamander occupancy data}
\format{A data frame with 40 observations (sites) on the following 2
variables.  \describe{ \item{ch}{a character vector containing the presence
(1) and absence (0) with 2 primary occasions with 48 and 31 visits to the
site} \item{freq}{frequency of sites (always 1)} }}
\description{
A robust design occupancy data set for modelling
presence/absence data for salamanders.
}
\details{
This is a data set that I got from Gary White which is
suppose to be salamander data collected with a robust
design.
}
\examples{
\donttest{
fit.RDOccupancy=function()
{
   data(RDSalamander)
   occ.p.time.eg=mark(RDSalamander,model="RDOccupEG",
      time.intervals=c(rep(0,47),1,rep(0,30)),
      model.parameters=list(p=list(formula=~session)),threads=2)
   occ.p.time.pg=mark(RDSalamander,model="RDOccupPG",
      time.intervals=c(rep(0,47),1,rep(0,30)),
      model.parameters=list(Psi=list(formula=~time),
      p=list(formula=~session)),threads=2)
   occ.p.time.pe=mark(RDSalamander,model="RDOccupPE",
      time.intervals=c(rep(0,47),1,rep(0,30)),
      model.parameters=list(Psi=list(formula=~time),
      p=list(formula=~session)),threads=2)
return(collect.models())
}
RDOcc=fit.RDOccupancy()
print(RDOcc)
}
}
\keyword{datasets}

