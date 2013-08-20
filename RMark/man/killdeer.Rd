\docType{data}
\name{killdeer}
\alias{killdeer}
\title{Killdeer nest survival example data}
\format{A data frame with 18 observations on the following 6 variables.
\describe{ \item{id}{a MARK comment field with a nest id}
\item{FirstFound}{the day the nest was first found} \item{LastPresent}{the
last day that chicks were present} \item{LastChecked}{the last day the nest
was checked} \item{Fate}{the fate of the nest; 0=hatch and 1 depredated}
\item{Freq}{the frequency of nests with this data; usually 1} }}
\description{
  A data set on killdeer that accompanies MARK as an
  example analysis for the nest survival model.
}
\details{
  This is a data set that accompanies program MARK as an
  example for nest survival. The data structure for the
  nest survival model is completely different from the
  capture history structure used for most MARK models.  To
  cope with these data you must import them into a
  dataframe using R commands and assign the specific
  variable names shown above. The id and Freq fields are
  optional.  Freq is assumed to be 1 if not given.  You
  cannot import the MARK .inp file structure directly into
  R without some manipulation.  Also note that
  \code{\link{import.chdata}} and \code{\link{convert.inp}}
  do NOT work for nest survival data. In the examples
  section below, the first section of code provides an
  example of converting the killdeer.inp file into a
  dataframe for RMark.

  If your dataframe contains a variable AgeDay1, which is
  the age of the nest on the first occasion then you can
  use a variable called NestAge which will create a set of
  time-dependent covariates named NestAge1,NestAge2
  ...NestAge(nocc-1) which will provide a way to
  incorporate the age of the nest in the model.  This was
  added because the age covariate in the design data for S
  assumes all nests are the same age and is not
  particularly useful. This effect could be incorporated by
  using the add() function in the design matrix but RMark
  does not have any capability for doing that and it is
  easier to create a time-dependent covariate to do the
  same thing.
}
\examples{
\donttest{
# EXAMPLE CODE FOR CONVERSION OF .INP TO NECESSARY DATA STRUCTURE
# read in killdeer.inp file
#killdeer=scan("killdeer.inp",what="character",sep="\\n")
# strip out ; and write out all but first 2 lines which contain comments
#write(sub(";","",killdeer[3:20]),"killdeer.txt")
# read in as a dataframe and assign names
#killdeer=read.table("killdeer.txt")
#names(killdeer)=c("id","FirstFound","LastPresent","LastChecked","Fate","Freq")
#
# EXAMPLE CODE TO RUN MODELS CONTAINED IN THE MARK KILLDEER.DBF
data(killdeer)
# produce summary
summary(killdeer)
# Define function to run models that are in killdeer.dbf
# You must specify either the number of occasions (nocc) or the time.intervals
# between the occasions.
run.killdeer=function()
{
   Sdot=mark(killdeer,model="Nest",nocc=40)
   STime=mark(killdeer,model="Nest",
       model.parameters=list(S=list(formula=~I(Time+1))),nocc=40,threads=2)
   STimesq=mark(killdeer,model="Nest",
       model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2))),nocc=40,threads=2)
   STime3=mark(killdeer,model="Nest",
      model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2)+I((Time+1)^3))),
                   nocc=40,threads=2)
   return(collect.models())
}
# run defined models
killdeer.results=run.killdeer()
}
}
\keyword{datasets}

