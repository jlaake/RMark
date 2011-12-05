\docType{data}
\name{NicholsMSOccupancy}
\alias{NicholsMSOccupancy}
\title{Multi-state occupancy example data}
\format{A data frame with 40 records for 54 observations (sites) on the
following 2 variables.  \describe{ \item{ch}{a character vector containing
the presence (state 1), presence (state 2), and absence (0) for each visit
to the site, and a "." if the site was not visited} \item{freq}{frequency of
sites with that history} }}
\description{
  An occupancy data set for modelling multi-state data
  (0,1,2).
}
\details{
  This is a data set from Nichols et al (2007).
}
\examples{
# To create the data file use:
# NicholsMSOccupancy=convert.inp("NicholsMSOccupancy.inp")
#
# Create a function to fit the 12 models in Nichols et al (2007).
do.MSOccupancy=function()
{
#  Get the data
   data(NicholsMSOccupancy)
# Define the models; default of Psi1=~1 and Psi2=~1 is assumed
   # p varies by time but p1t=p2t
   p1.p2equal.by.time=list(formula=~time,share=TRUE)
   # time-invariant p p1t=p2t=p1=p2
   p1.p2equal.dot=list(formula=~1,share=TRUE)
   #time-invariant p1 not = p2
   p1.p2.different.dot=list(p1=list(formula=~1,share=FALSE),p2=list(formula=~1))
   # time-varying p1t and p2t
   p1.p2.different.time=list(p1=list(formula=~time,share=FALSE),p2=list(formula=~time))
   # delta2 model with one rate for times 1-2 and another for times 3-5;
   #delta2 defined below
   Delta.delta2=list(formula=~delta2)
   Delta.dot=list(formula=~1)  # constant delta
   Delta.time=list(formula=~time) # time-varying delta
# Process the data for the MSOccupancy model
   NicholsMS.proc=process.data(NicholsMSOccupancy,model="MSOccupancy")
# Create the default design data
   NicholsMS.ddl=make.design.data(NicholsMS.proc)
# Add a field for the Delta design data called delta2.  It is a factor variable
# with 2 levels: times 1-2, and times 3-5.
   NicholsMS.ddl=add.design.data(NicholsMS.proc,NicholsMS.ddl,"Delta",
     type="time",bins=c(0,2,5),name="delta2")
# Create a list using the 4 p modls and 3 delta models (12 models total)
   cml=create.model.list("MSOccupancy")
# Fit each model in the list and return the results
   return(mark.wrapper(cml,data=NicholsMS.proc,ddl=NicholsMS.ddl))
}
# Call the function to fit the models and store it in MSOccupancy.results
MSOccupancy.results=do.MSOccupancy()
# Print the model table for the results
print(MSOccupancy.results)
# Adjust model selection by setting chat=1.74
MSOccupancy.results=adjust.chat(chat=1.74,MSOccupancy.results)
# Print the adjusted model selection results table
print(MSOccupancy.results)
#
# To fit an additive model whereby p1 and p2 differ by time and p2 differs from
# p1 a constant amount on the logit scale, use
#
# p varies by time logit(p1t)=logit(p2t)+constant
p1.plust.p2.by.time=list(formula=~time+p2,share=TRUE)
}
\references{
  Nichols, J. D., J. E. Hines, D. I. MacKenzie, M. E.
  Seamans, and R. J. Gutierrez.  2007. Occupancy estimation
  and modeling with multiple states and state uncertainty.
  Ecology 88:1395-1400.
}
\keyword{dataset}
\keyword{datasets}

