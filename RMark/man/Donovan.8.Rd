% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RMark-package.R
\docType{data}
\name{Donovan.8}
\alias{Donovan.8}
\title{Exercise 8 example data}
\format{
A data frame with 20 observations (sites) on the following 2
variables.  \describe{ \item{ch}{a character vector containing the counts
for each of 5 visits to the site} \item{freq}{frequency of sites (always 1)}
}
}
\description{
An example occupancy data set used as exercise 8 in the occupancy website
developed by Donovan and Hines.
}
\details{
This is a data set from exercise 8 of Donovan and Hines occupancy web site
(\url{https://blog.uvm.edu/tdonovan-vtcfwru/occupancy-estimation-and-modeling-ebook/}).
In MARK, it uses 2 digits to allow a count of 0 to 99 at each site, so the
history has 10 digits for 5 visits (occasions).
}
\examples{
\donttest{
# This example is excluded from testing to reduce package check time
# Donovan.8 can be created with
# Donovan.8=convert.inp("Donovan.8.inp")
do.exercise.8=function()
{
  data(Donovan.8)
# Results agree with the values on the website.
  Donovan.8.poisson=mark(Donovan.8,model="OccupRPoisson",invisible=FALSE,threads=2,delete=TRUE)
# The following model was not in exercise 8. The NegBin model does 
# better if it is initialized with the r and lambda from the poisson.
  Donovan.8.negbin=mark(Donovan.8,model="OccupRNegBin",
    initial=Donovan.8.poisson,invisible=FALSE,threads=2,delete=TRUE)
  return(collect.models())
}
exercise.8=do.exercise.8()
# Remove # to see output
# print(exercise.8)
}

}
\keyword{datasets}
