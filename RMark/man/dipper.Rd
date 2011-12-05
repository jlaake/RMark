\docType{data}
\name{dipper}
\alias{dipper}
\title{Dipper capture-recapture data}
\format{A data frame with 294 observations on the following 2 variables.
\describe{ \item{ch}{a character vector containing the encounter history of
each bird} \item{sex}{the sex of the bird: a factor with levels
\code{Female} \code{Male}} }}
\source{
  Lebreton, J.-D., K. P. Burnham, J. Clobert, and D. R.
  Anderson. 1992. Modeling survival and testing biological
  hypotheses using marked animals: case studies and recent
  advances. Ecol. Monogr. 62:67-118.
}
\description{
  A capture-recapture data set on European dippers from
  France that accompanies MARK as an example analysis using
  the CJS and POPAN models.  The dipper data set was
  orginally described as an example by Lebreton et al
  (1992).
}
\details{
  This is a data set that accompanies program MARK as an
  example for CJS and POPAN analyses.  The data can be
  stratified using sex as a grouping variable.  The
  functions \code{run.dipper}, \code{run.dipper.alternate},
  \code{run.dipper.popan} defined below in the examples
  mimic the models used in the dbf file that accompanies
  MARK. Note that the models used in the MARK example use
  PIM coding with the sin link function which is often
  better at identifying the number of estimable parameters.
  The approach used in the R code uses design matrices and
  cannot use the sin link and is less capable at counting
  parameters.  These differences are illustrated by
  comparing the results of \code{run.dipper} and
  \code{run.dipper.alternate} which fit the same set of
  "CJS" models.  The latter fits the models with
  constraints on some parameters to achieve identifiability
  and the former does not. Although it does not influence
  the selection of the best model it does infleunce
  parameter counts and AIC ordering of some of the less
  competitive models. In using design matrices it is best
  to constrain parameters that are confounded (e.g., last
  occasion parameters in Phi(t)p(t) CJS model) when
  possible to achieve more reliable counts of the number of
  estimable parameters.  See
  \code{\link{adjust.parameter.count}} for more dicussion
  on this point.

  Note that the covariate "sex" defined in dipper has
  values "Male" and "Female".  It cannot be used directly
  in a formula for MARK without using it do define groups
  because MARK.EXE will be unable to read in a covariate
  with non-numeric values.  By using \code{groups="sex"} in
  the call the \code{\link{process.data}} a factor "sex"
  field is created that can be used in the formula.
  Alternatively, a new covariate could be defined in the
  data with say values 0 for Female and 1 for Male and this
  could be used without defining groups because it is
  numeric.  This can be done easily by translating the
  values of the coded variables to a numeric variable.
  Factor variables are numbered 1..k for k levels in
  alphabetic order.  Since Female < Male in alphabetic
  order then it is level 1 and Male is level 2.  So the
  following will create a numeric sex covariate.

  \preformatted{
  dipper$numeric.sex=as.numeric(dipper$sex)-1 }

  See \code{\link{export.chdata}} for an example that
  creates a .inp file for MARK with sex being used to
  describe groups and a numeric sex covariate.
}
\examples{
data(dipper)
dipper.model=mark(dipper)
run.dipper=function()
{
#
# Process data
#
dipper.processed=process.data(dipper,groups=("sex"))
#
# Create default design data
#
dipper.ddl=make.design.data(dipper.processed)
#
# Add Flood covariates for Phi and p that have different values
#
dipper.ddl$Phi$Flood=0
dipper.ddl$Phi$Flood[dipper.ddl$Phi$time==2 | dipper.ddl$Phi$time==3]=1
dipper.ddl$p$Flood=0
dipper.ddl$p$Flood[dipper.ddl$p$time==3]=1
#
#  Define range of models for Phi
#
Phidot=list(formula=~1)
Phitime=list(formula=~time)
Phisex=list(formula=~sex)
Phisextime=list(formula=~sex+time)
Phisex.time=list(formula=~sex*time)
PhiFlood=list(formula=~Flood)
#
#  Define range of models for p
#
pdot=list(formula=~1)
ptime=list(formula=~time)
psex=list(formula=~sex)
psextime=list(formula=~sex+time)
psex.time=list(formula=~sex*time)
pFlood=list(formula=~Flood)
#
# Run assortment of models
#
dipper.phidot.pdot          =mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phidot,p=pdot))
dipper.phidot.pFlood      	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phidot,p=pFlood))
dipper.phidot.psex        	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phidot,p=psex))
dipper.phidot.ptime       	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phidot,p=ptime))
dipper.phidot.psex.time		=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phidot,p=psex.time))
dipper.phitime.ptime      	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phitime, p=ptime))
dipper.phitime.pdot       	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phitime,p=pdot))
dipper.phitime.psex		=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phitime,p=psex))
dipper.phitime.psex.time	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phitime,p=psex.time))
dipper.phiFlood.pFlood    	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=PhiFlood, p=pFlood))
dipper.phisex.pdot        	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex,p=pdot))
dipper.phisex.psex        	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex,p=psex))
dipper.phisex.psex.time        	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex,p=psex.time))
dipper.phisex.ptime       	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex,p=ptime))
dipper.phisextime.psextime	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisextime,p=psextime))
dipper.phisex.time.psex.time	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex.time,p=psex.time))
dipper.phisex.time.psex 	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex.time,p=psex))
dipper.phisex.time.pdot		=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex.time,p=pdot))
dipper.phisex.time.ptime	=mark(dipper.processed,dipper.ddl,
                 model.parameters=list(Phi=Phisex.time,p=ptime))
#
# Return model table and list of models
#
return(collect.models() )
}

dipper.results=run.dipper()

run.dipper.alternate=function()
{
#
# Process data
#
dipper.processed=process.data(dipper,groups=("sex"))
#
# Create default design data
#
dipper.ddl=make.design.data(dipper.processed)
#
# Add Flood covariates for Phi and p that have different values
#
dipper.ddl$Phi$Flood=0
dipper.ddl$Phi$Flood[dipper.ddl$Phi$time==2 | dipper.ddl$Phi$time==3]=1
dipper.ddl$p$Flood=0
dipper.ddl$p$Flood[dipper.ddl$p$time==3]=1
#
#  Define range of models for Phi
#
Phidot=list(formula=~1)
Phitime=list(formula=~time)
Phitimec=list(formula=~time,fixed=list(time=6,value=1))
Phisex=list(formula=~sex)
Phisextime=list(formula=~sex+time)
Phisex.time=list(formula=~sex*time)
PhiFlood=list(formula=~Flood)
#
#  Define range of models for p
#
pdot=list(formula=~1)
ptime=list(formula=~time)
ptimec=list(formula=~time,fixed=list(time=7,value=1))
psex=list(formula=~sex)
psextime=list(formula=~sex+time)
psex.time=list(formula=~sex*time)
psex.timec=list(formula=~sex*time,fixed=list(time=7,value=1))
pFlood=list(formula=~Flood)
#
# Run assortment of models
#
dipper.phidot.pdot          =mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phidot,p=pdot))
dipper.phidot.pFlood      	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phidot,p=pFlood))
dipper.phidot.psex        	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phidot,p=psex))
dipper.phidot.ptime       	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phidot,p=ptime))
dipper.phidot.psex.time		=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phidot,p=psex.time))
dipper.phitime.ptimec      	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phitime, p=ptimec))
dipper.phitime.pdot       	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phitime,p=pdot))
dipper.phitime.psex		=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phitime,p=psex))
dipper.phitimec.psex.time	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phitimec,p=psex.time))
dipper.phiFlood.pFlood    	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=PhiFlood, p=pFlood))
dipper.phisex.pdot        	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex,p=pdot))
dipper.phisex.psex        	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex,p=psex))
dipper.phisex.psex.time        	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex,p=psex.time))
dipper.phisex.ptime       	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex,p=ptime))
dipper.phisextime.psextime	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisextime,p=psextime),adjust=FALSE)
dipper.phisex.time.psex.timec	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex.time,p=psex.timec))
dipper.phisex.time.psex 	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex.time,p=psex))
dipper.phisex.time.pdot		=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex.time,p=pdot))
dipper.phisex.time.ptimec	=mark(dipper.processed,dipper.ddl,
                  model.parameters=list(Phi=Phisex.time,p=ptimec))
#
# Return model table and list of models
#
return(collect.models() )
}
dipper.results.alternate=run.dipper.alternate()
#
# Merge two sets of models into a single model list and include the
# initial model as a demo for merge.mark
#
dipper.cjs=merge.mark(dipper.results,dipper.results.alternate,dipper.model)
dipper.cjs
#
# next delete some of the models to show how this is done with remove.mark
#
dipper.cjs=remove.mark(dipper.cjs,c(2,4,9))
dipper.cjs

run.dipper.popan=function()
{
#
# Process data
#
dipper.processed=process.data(dipper,model="POPAN",group="sex")
#
# Create default design data
#
dipper.ddl=make.design.data(dipper.processed)
#
# Add Flood covariates for Phi and p that have different values
#
dipper.ddl$Phi$Flood=0
dipper.ddl$Phi$Flood[dipper.ddl$Phi$time==2 | dipper.ddl$Phi$time==3]=1
dipper.ddl$p$Flood=0
dipper.ddl$p$Flood[dipper.ddl$p$time==3]=1
#
#  Define range of models for Phi
#
Phidot=list(formula=~1)
Phitime=list(formula=~time)
Phisex=list(formula=~sex)
Phisextime=list(formula=~sex+time)
Phisex.time=list(formula=~sex*time)
PhiFlood=list(formula=~Flood)
#
#  Define range of models for p
#
pdot=list(formula=~1)
ptime=list(formula=~time)
psex=list(formula=~sex)
psextime=list(formula=~sex+time)
psex.time=list(formula=~sex*time)
pFlood=list(formula=~Flood)
#
#  Define range of models for pent
#
pentsex.time=list(formula=~sex*time)
#
#  Define range of models for N
#
Nsex=list(formula=~sex)
#
# Run assortment of models
#
dipper.phisex.time.psex.time.pentsex.time=mark(dipper.processed,dipper.ddl,
model.parameters=list(Phi=Phisex.time,p=psex.time,pent=pentsex.time,N=Nsex),
invisible=FALSE,adjust=FALSE)
dipper.phisex.time.psex.pentsex.time=mark(dipper.processed,dipper.ddl,
model.parameters=list(Phi=Phisex.time,p=psex,pent=pentsex.time,N=Nsex),
invisible=FALSE,adjust=FALSE)
#
# Return model table and list of models
#
return(collect.models() )
}

dipper.popan.results=run.dipper.popan()
}
\keyword{dataset}
\keyword{datasets}

