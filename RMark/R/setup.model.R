"setup.model" <-
function(model,nocc,mixtures=1)
{
#
# setup.model - defines list of acceptable models and creates some global fields for the model
#
# Arguments:
# 
#   model    - name of model (must be in valid.models)
#   nocc     - length of capture history string
#   mixtures - number of mixtures
#
# Value: 
#
#   model.list - a list with following elements
#                  etype - encounter type string; typically same as model name
#                  nocc  - number of capture occasions
#                  num   - number of time intervals relative to number of occasions (0 or -1)
#                  mixtures - number of mixtures if any
#                  derived - TRUE if model produces derived parameters
#
#  10 Jan 06; added known fate
#  11 Jan 06; added multistrata
#           ; robust design models
#   5 Oct 07; added Nest
#   9 Dec 07; added Occupancy and OccupHet and robust versions
#   14 Feb 07; added Jolly
#   12 Sept 08; added cjs and js R models
#   3 Mar 10; added Brownie; note ORDMS added year earlier
#   Aug 10; added CRDMS; MsLiveDead done in between
#   June 11; added Mark-resight models
#
  valid.models=c("CJS","Recovery","Burnham","Barker","POPAN","Pradel","Pradrec","LinkBarker","Pradsen","Pradlambda",
                 "Closed","HetClosed","FullHet","Huggins","HugHet","HugFullHet","Known","Multistrata","Robust",
                 "RDHet","RDFullHet","RDHuggins","RDHHet","RDHFHet","Nest","Occupancy","OccupHet",
                 "RDOccupEG","RDOccupPE","RDOccupPG","RDOccupHetEG","RDOccupHetPE","RDOccupHetPG",
                 "OccupRPoisson","OccupRNegBin","OccupRNPoisson","OccupRNNegBin","MSOccupancy","Jolly",
                 "ORDMS","Brownie","MSLiveDead","CRDMS","LogitNormalMR","PoissonMR","IELogitNormalMR")
  stype=rep("mark",length(valid.models))
  num=c(-1,0,0,0,rep(-1,12),0,-1,rep(0,9),rep(-1,6),0,0,0,0,-1,-1,0,0,0,0,-1,-1,-1) # of intervals relative to nocc
  divisor=c(1,2,2,2,rep(1,12),2,rep(1,16),2,2,1,1,1,1,1,2,2,1,1,2,1) # to compute nocc from length of ch
  default.mixtures=c(rep(1,11),2,2,1,2,2,1,1,1,2,2,1,2,2,1,1,2,1,1,1,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
  valid.types=c("Live","Dead","Both","Barker","POPAN","Pradel","Pradrec","LinkBarker","Pradsen","Pradlambda",
                "Closed","HetClosed","FullHet","Huggins","HugHet","HugFullHet","Known","Multistrata","Robust",
                 "RDHet","RDFullHet","RDHuggins","RDHHet","RDHFHet","Nest","Occupancy","OccupHet",
                 "RDOccupEG","RDOccupPE","RDOccupPG","RDOccupHetEG","RDOccupHetPE","RDOccupHetPG",
                 "OccupRPoisson","OccupRNegBin","OccupRNPoisson","OccupRNNegBin","MSOccupancy","Jolly",
                 "ORDMS","Brownie","MSLiveDead","CRDMS","LogitNormalMR","PoissonMR","IELogitNormalMR")
  etype= match(model,valid.models)
  derived=c(rep(FALSE,13),rep(TRUE,3),TRUE,FALSE,rep(TRUE,7),FALSE,FALSE, rep(TRUE,11),FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE)
  robust=c(rep(FALSE,18),rep(TRUE,6),rep(FALSE,3),rep(TRUE,6),rep(FALSE,6),TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,TRUE)
  closed=c(rep(FALSE,10),rep(TRUE,6),rep(FALSE,26),TRUE,TRUE,TRUE,TRUE)
  occupancy=c(rep(FALSE,25),rep(TRUE,13),FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)
  if(is.na(etype))
     stop("Invalid type of model = ",model," Valid types are\n", paste(valid.models,collapse=" "))
  if(mixtures==1) mixtures=default.mixtures[etype]
  return(list(etype=valid.types[etype],nocc=nocc/divisor[etype],num=num[etype],mixtures=mixtures,
               derived=derived[etype],robust=robust[etype],closed=closed[etype],occupancy=occupancy[etype],stype=stype[etype]))
}
