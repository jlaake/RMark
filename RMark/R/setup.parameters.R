"setup.parameters" <-
function(model,parameters=list(),nocc=NULL,check=FALSE,number.of.groups=1)
# ----------------------------------------------------------------------------------------
#  setup.parameters  - fills in value for begin and num for each parameter type depending
#                      on the type of c-r model. num defines number of parameters relative to
#                      number of occasions.  begin defines the first occasion which is relevant
#                      to the parameter
#
#  Arguments:
#    model      - type of model ("CJS", "Burnham" etc)
#    parameters - list of model parameter specifications
#    nocc       - number of occasions (value only specified if needed)
#    check      - default is FALSE; if TRUE it only returns list of parameter names
#
#  Value:
#    parameters - updated list of model parameter specifications with new fields added
#
#
#  10 Jan 06; added pim.type to list for triang structured parameters
# ----------------------------------------------------------------------------------------
{
#
#  Create valid parameter list depending on model.
#
   if(model=="CJS" | model=="cjs")par.list=c("Phi","p")
   if(model=="Recovery") par.list=c("S","r")
   if(model=="Brownie") par.list=c("S","f")
   if(model=="Burnham") par.list=c("S","p","r","F")
   if(model=="MSLiveDead") par.list=c("S","p","Psi","r")
   if(model=="Barker") par.list=c("S","p","r","R","RPrime","F","FPrime")
   if(model=="POPAN" | model=="js") par.list=c("Phi","p","pent","N")
   if(model=="Pradel") par.list=c("Gamma","p")
   if(model=="Pradrec" | model=="LinkBarker") par.list=c("Phi","p","f")
   if(model=="Pradsen") par.list=c("Phi","p","Gamma")
   if(model=="Pradlambda") par.list=c("Phi","p","Lambda")
   if(model=="Closed") par.list=c("p","c","N")
   if(model=="HetClosed") par.list=c("pi","p","N")
   if(model=="FullHet") par.list=c("pi","p","c","N")
   if(model=="Huggins") par.list=c("p","c")
   if(model=="HugHet") par.list=c("pi","p")
   if(model=="HugFullHet") par.list=c("pi","p","c")
   if(model=="Known") par.list=c("S")
   if(model=="Multistrata") par.list=c("S","p","Psi")
   if(model=="BaseRobust") par.list=c("S","GammaDoublePrime","GammaPrime")
   if(model=="Robust") par.list=c("S","GammaDoublePrime","GammaPrime","p","c","N")
   if(model=="RDHet") par.list=c("S","GammaDoublePrime","GammaPrime","pi","p","N")
   if(model=="RDFullHet") par.list=c("S","GammaDoublePrime","GammaPrime","pi","p","c","N")
   if(model=="RDHuggins") par.list=c("S","GammaDoublePrime","GammaPrime","p","c")
   if(model=="RDHHet") par.list=c("S","GammaDoublePrime","GammaPrime","pi","p")
   if(model=="RDHFHet") par.list=c("S","GammaDoublePrime","GammaPrime","pi","p","c")
   if(model=="Nest") par.list=c("S")
   if(model=="Occupancy") par.list=c("p","Psi")
   if(model=="OccupHet") par.list=c("pi","p","Psi")
   if(model=="RDOccupEG") par.list=c("Psi","Epsilon","Gamma","p")
   if(model=="RDOccupPE") par.list=c("Psi","Epsilon","p")
   if(model=="RDOccupPG") par.list=c("Psi","Gamma", "p")
   if(model=="RDOccupHetEG") par.list=c("Psi","Epsilon","Gamma","pi","p")
   if(model=="RDOccupHetPE") par.list=c("Psi","Epsilon","pi","p")
   if(model=="RDOccupHetPG") par.list=c("Psi","Gamma","pi","p")
   if(model=="OccupRNPoisson") par.list=c("r","Lambda")
   if(model=="OccupRNNegBin") par.list=c("r","Lambda","VarAdd")
   if(model=="OccupRPoisson") par.list=c("r","Lambda")
   if(model=="OccupRNegBin") par.list=c("r","Lambda","VarAdd")
   if(model=="MSOccupancy") par.list=c("Psi1","Psi2","p1","p2","Delta")
   if(model=="Jolly") par.list=c("Phi","p","Lambda","N")
   if(model=="ORDMS") par.list=c("S","Psi","pent","Phi","p")
   if(model=="CRDMS") par.list=c("S","Psi","p","c","N")
   if(model=="LogitNormalMR") par.list=c("p","sigma","N")   
   if(model=="PoissonMR") par.list=c("alpha","sigma","U","Phi","GammaDoublePrime","GammaPrime")
   if(model=="IELogitNormalMR") par.list=c("p","sigma","Nbar","alpha","Nstar")   
#
#  If this is just a parameter check, return par.list
#
   if(check)return(par.list)
#
#  For each parameter create an empty list if none specified in input
#   
   if(length(parameters)>0)
   {
      for (i in 1:length(par.list))
      {
         if(is.na(names(parameters[par.list[i]])))
            parameters[[par.list[i]]]=list()
      }
   }
#
#  Next depending on model type, assign non-specified default values
#
   if(model=="CJS" | model=="cjs")
   {
      parameters$Phi$num=-1
      parameters$Phi$begin=0
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      parameters$Phi$type="Triang"
      if(is.null(parameters$Phi$pim.type))parameters$Phi$pim.type="all"
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=-1
      parameters$p$begin=1
      parameters$p$type="Triang"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"
   }
   else
   if(model=="Recovery")
   {
      parameters$S$begin=0
      parameters$S$num=0
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$r$begin=0
      parameters$r$num=0
      parameters$r$type="Triang"
      if(is.null(parameters$r$default))parameters$r$default=0
      if(is.null(parameters$r$pim.type))parameters$r$pim.type="all"
      if(is.null(parameters$r$link))parameters$r$link="logit"
   }
   else
   if(model=="Brownie")
   {
      parameters$S$begin=0
      parameters$S$num=-1
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$f$begin=0
      parameters$f$num=0
      parameters$f$type="Triang"
      if(is.null(parameters$f$default))parameters$f$default=0
      if(is.null(parameters$f$pim.type))parameters$f$pim.type="all"
      if(is.null(parameters$f$link))parameters$f$link="logit"
   }
   else
   if(model=="Burnham")
   {
      parameters$S$begin=0
      parameters$S$num=0
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$p$begin=1
      parameters$p$num=-1
      parameters$p$type="Triang"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$r$begin=0
      parameters$r$num=0
      parameters$r$type="Triang"
      if(is.null(parameters$r$default))parameters$r$default=0
      if(is.null(parameters$r$pim.type))parameters$r$pim.type="all"
      if(is.null(parameters$r$link))parameters$r$link="logit"
      parameters$F$begin=0
      parameters$F$num=-1
      parameters$F$type="Triang"
      if(is.null(parameters$F$default))parameters$F$default=1
      if(is.null(parameters$F$pim.type))parameters$F$pim.type="all"
      if(is.null(parameters$F$link))parameters$F$link="logit"
   }
   else
   if(model=="MSLiveDead")
   {
      parameters$S$begin=0
      parameters$S$num=0
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$p$begin=1
      parameters$p$num=-1
      parameters$p$type="Triang"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$r$begin=0
      parameters$r$num=0
      parameters$r$type="Triang"
      if(is.null(parameters$r$default))parameters$r$default=0
      if(is.null(parameters$r$pim.type))parameters$r$pim.type="all"
      if(is.null(parameters$r$link))parameters$r$link="logit"
      parameters$S$bystratum=TRUE
      parameters$r$bystratum=TRUE
      parameters$p$bystratum=TRUE
      parameters$Psi$num=-1
      parameters$Psi$begin=0
      parameters$Psi$type="Triang"
      if(is.null(parameters$Psi$default))parameters$Psi$default=0
      parameters$Psi$bystratum=TRUE
      parameters$Psi$tostrata=TRUE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~-1+stratum:tostratum
      if(is.null(parameters$Psi$pim.type))parameters$Psi$pim.type="all"
      if(is.null(parameters$Psi$link))parameters$Psi$link="mlogit"
   }
   else
   if(model=="Barker")
   {
      parameters$S$begin=0
      parameters$S$num=0
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$p$begin=1
      parameters$p$num=-1
      parameters$p$type="Triang"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$r$begin=0
      parameters$r$num=0
      parameters$r$type="Triang"
      if(is.null(parameters$r$default))parameters$r$default=0
      if(is.null(parameters$r$pim.type))parameters$r$pim.type="all"
      if(is.null(parameters$r$link))parameters$r$link="logit"
      parameters$R$begin=0
      parameters$R$num=0
      parameters$R$type="Triang"
      if(is.null(parameters$R$default))parameters$R$default=0
      if(is.null(parameters$R$pim.type))parameters$R$pim.type="all"
      if(is.null(parameters$R$link))parameters$R$link="logit"
      parameters$RPrime$num=0
      parameters$RPrime$begin=0
      parameters$RPrime$type="Triang"
      if(is.null(parameters$RPrime$default))parameters$RPrime$default=0
      if(is.null(parameters$RPrime$pim.type))parameters$RPrime$pim.type="all"
      if(is.null(parameters$RPrime$link))parameters$RPrime$link="logit"
      parameters$F$begin=0
      parameters$F$num=-1
      parameters$F$type="Triang"
      if(is.null(parameters$F$default))parameters$F$default=1
      if(is.null(parameters$F$pim.type))parameters$F$pim.type="all"
      if(is.null(parameters$F$link))parameters$F$link="logit"
      parameters$FPrime$begin=0
      parameters$FPrime$num=-1
      parameters$FPrime$type="Triang"
      if(is.null(parameters$FPrime$default))parameters$FPrime$default=0
      if(is.null(parameters$FPrime$pim.type))parameters$FPrime$pim.type="all"
      if(is.null(parameters$FPrime$link))parameters$FPrime$link="logit"
   } else
   if(model=="POPAN" | model=="js")
   {
      parameters$Phi$begin=0
      parameters$Phi$num=-1
      parameters$Phi$type="Square"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$pent$num=-1
      parameters$pent$begin=1
      parameters$pent$type="Square"
      if(is.null(parameters$pent$default))parameters$pent$default=0
      if(number.of.groups>1)
         if(is.null(parameters$pent$formula))parameters$pent$formula=~group
      else
         if(is.null(parameters$pent$formula))parameters$pent$formula=~1
      if(is.null(parameters$pent$link))parameters$pent$link="mlogit"
      parameters$N$num=-(nocc-1)
      parameters$N$begin=0
      parameters$N$type="Square"
      parameters$N$leave.unused=TRUE
      if(number.of.groups>1)
         if(is.null(parameters$N$formula))parameters$N$formula=~group
      else
         if(is.null(parameters$N$formula))parameters$N$formula=~1
      if(is.null(parameters$N$link))parameters$N$link="log"
   } else
   if(model=="Jolly")
   {
      parameters$Phi$begin=0
      parameters$Phi$num=-1
      parameters$Phi$type="Square"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Lambda$num=-1
      parameters$Lambda$begin=0
      parameters$Lambda$type="Square"
      if(is.null(parameters$Lambda$link))parameters$Lambda$link="log"
      parameters$N$num=-(nocc-1)
      parameters$N$begin=0
      parameters$N$type="Square"
      parameters$N$leave.unused=TRUE
      if(number.of.groups>1)
         if(is.null(parameters$N$formula))parameters$N$formula=~group
      else
         if(is.null(parameters$N$formula))parameters$N$formula=~1
      if(is.null(parameters$N$link))parameters$N$link="log"
   } else
   if(model=="Pradel")
   {
      parameters$Gamma$begin=0
      parameters$Gamma$num=-1
      parameters$Gamma$type="Square"
      if(is.null(parameters$Gamma$default))parameters$Gamma$default=1
      if(is.null(parameters$Gamma$link))parameters$Gamma$link="logit"
      parameters$p$num=-1
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
   } else
   if(model=="Pradrec" | model=="LinkBarker")
   {
      parameters$Phi$begin=0
      parameters$Phi$num=-1
      parameters$Phi$type="Square"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$f$num=-1
      parameters$f$begin=0
      parameters$f$type="Square"
      if(is.null(parameters$f$link))parameters$f$link="log"
   } else
   if(model=="Pradsen")
   {
      parameters$Phi$begin=0
      parameters$Phi$num=-1
      parameters$Phi$type="Square"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Gamma$num=-1
      parameters$Gamma$begin=0
      parameters$Gamma$type="Square"
      if(is.null(parameters$Gamma$link))parameters$Gamma$link="logit"
   } else
   if(model=="Pradlambda")
   {
      parameters$Phi$begin=0
      parameters$Phi$num=-1
      parameters$Phi$type="Square"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"
      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Lambda$num=-1
      parameters$Lambda$begin=0
      parameters$Lambda$type="Square"
      if(is.null(parameters$Lambda$link))parameters$Lambda$link="log"
   } else
   if(model=="Closed")
   {
      parameters$p$begin=0
      parameters$p$num=0
      if(is.null(parameters$p$share))parameters$p$share=FALSE
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$c$begin=1
      parameters$c$num=-1
      parameters$c$type="Square"
      if(is.null(parameters$c$default))parameters$c$default=0
      if(is.null(parameters$c$link))parameters$c$link="logit"
      parameters$N$num=-(nocc-1)
      parameters$N$begin=0
      parameters$N$type="Square"
      if(number.of.groups>1)
         if(is.null(parameters$N$formula))parameters$N$formula=~group
      else
         if(is.null(parameters$N$formula))parameters$N$formula=~1
      if(is.null(parameters$N$link))parameters$N$link="log"
   } else
   if(model=="FullHet")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$mix=TRUE
      parameters$p$rows=0
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$share))parameters$p$share=FALSE
      parameters$p$type="Square"
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$c$begin=1
      parameters$c$num=-1
      parameters$c$type="Square"
      parameters$c$mix=TRUE
      parameters$c$rows=0
      if(is.null(parameters$c$default))parameters$c$default=0
      if(is.null(parameters$c$link))parameters$c$link="logit"
      parameters$N$num=-(nocc-1)
      parameters$N$begin=0
      parameters$N$type="Square"
      parameters$N$mix=FALSE
      if(number.of.groups>1)
         if(is.null(parameters$N$formula))parameters$N$formula=~group
      else
         if(is.null(parameters$N$formula))parameters$N$formula=~1
      if(is.null(parameters$N$link))parameters$N$link="log"
      parameters$pi$num=-(nocc-1)
      parameters$pi$begin=0
      parameters$pi$type="Square"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
   } else
   if(model=="HetClosed")
   {
      parameters$p$begin=0
      parameters$p$num=-(nocc-1)
      parameters$p$type="Square"
      parameters$p$mix=TRUE
      parameters$p$rows=0
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$N$num=-(nocc-1)
      parameters$N$begin=0
      parameters$N$type="Square"
      parameters$N$mix=FALSE
      if(number.of.groups>1)
         if(is.null(parameters$N$formula))parameters$N$formula=~group
      else
         if(is.null(parameters$N$formula))parameters$N$formula=~1
      if(is.null(parameters$N$link))parameters$N$link="log"
      parameters$pi$num=-(nocc-1)
      parameters$pi$begin=0
      parameters$pi$type="Square"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
   } else
   if(model=="Huggins")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$share))parameters$p$share=FALSE
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$c$begin=1
      parameters$c$num=-1
      parameters$c$type="Square"
      if(is.null(parameters$c$default))parameters$c$default=0
      if(is.null(parameters$c$link))parameters$c$link="logit"
   } else
   if(model=="HugHet")
   {
      parameters$p$begin=0
      parameters$p$num=-(nocc-1)
      parameters$p$type="Square"
      parameters$p$mix=TRUE
      parameters$p$rows=0
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
   } else
   if(model=="HugFullHet")
   {
      parameters$p$begin=0
      parameters$p$num=0
      if(is.null(parameters$p$share))parameters$p$share=FALSE
      parameters$p$type="Square"
      parameters$p$mix=TRUE
      parameters$p$rows=0
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$c$begin=1
      parameters$c$num=-1
      parameters$c$type="Square"
      if(is.null(parameters$c$default))parameters$c$default=0
      if(is.null(parameters$c$link))parameters$c$link="logit"
      parameters$c$mix=TRUE
      parameters$c$rows=0
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
   }else
   if(model=="Known")
   {
      parameters$S$begin=1
      parameters$S$num=0
      parameters$S$type="Square"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$link))parameters$S$link="logit"
   } else
   if(model=="Nest")
   {
      parameters$S$begin=0
      parameters$S$num=-1
      parameters$S$type="Square"
      if(is.null(parameters$S$default))parameters$S$default=1
      if(is.null(parameters$S$link))parameters$S$link="logit"
   } else
   if(model=="Multistrata")
   {
      parameters$S$num=-1
      parameters$S$begin=0
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      parameters$S$bystratum=TRUE
      if(is.null(parameters$S$formula))parameters$S$formula=~stratum
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$p$num=-1
      parameters$p$begin=1
      parameters$p$type="Triang"
      if(is.null(parameters$p$default))parameters$p$default=0
      parameters$p$bystratum=TRUE
      if(is.null(parameters$p$formula))parameters$p$formula=~stratum
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Psi$num=-1
      parameters$Psi$begin=0
      parameters$Psi$type="Triang"
      if(is.null(parameters$Psi$default))parameters$Psi$default=0
      parameters$Psi$bystratum=TRUE
      parameters$Psi$tostrata=TRUE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~-1+stratum:tostratum
      if(is.null(parameters$Psi$pim.type))parameters$Psi$pim.type="all"
      if(is.null(parameters$Psi$link))parameters$Psi$link="mlogit"
   } else
   if(model=="Occupancy")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Psi$num=-(nocc-1)
      parameters$Psi$begin=0
      parameters$Psi$type="Square"
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~1
      if(is.null(parameters$Psi$link))parameters$Psi$link="logit"
   } else
   if(model=="OccupHet")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      parameters$p$mix=TRUE
      parameters$p$rows=0
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$formula))parameters$pi$formula=~1
      parameters$Psi$num=-(nocc-1)
      parameters$Psi$begin=0
      parameters$Psi$type="Square"
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~1
      if(is.null(parameters$Psi$link))parameters$Psi$link="logit"
   } else
   if(model=="MSOccupancy")
   {
      parameters$p1$begin=0
      parameters$p1$num=0
      parameters$p1$type="Square"
      if(is.null(parameters$p1$share))parameters$p1$share=FALSE
      if(is.null(parameters$p1$default))parameters$p1$default=0
      if(is.null(parameters$p1$link))parameters$p1$link="logit"
      parameters$p2$begin=0
      parameters$p2$num=0
      parameters$p2$type="Square"
      if(is.null(parameters$p2$default))parameters$p2$default=0
      if(is.null(parameters$p2$link))parameters$p2$link="logit"
      parameters$Delta$begin=0
      parameters$Delta$num=0
      parameters$Delta$type="Square"
      if(is.null(parameters$Delta$default))parameters$Delta$default=1
      if(is.null(parameters$Delta$link))parameters$Delta$link="logit"
      parameters$Psi1$num=-(nocc-1)
      parameters$Psi1$begin=0
      parameters$Psi1$type="Square"
      if(is.null(parameters$Psi1$formula))parameters$Psi1$formula=~1
      if(is.null(parameters$Psi1$link))parameters$Psi1$link="logit"
      parameters$Psi2$num=-(nocc-1)
      parameters$Psi2$begin=0
      parameters$Psi2$type="Square"
      if(is.null(parameters$Psi2$formula))parameters$Psi2$formula=~1
      if(is.null(parameters$Psi2$link))parameters$Psi2$link="logit"
   } else
   if(model=="OccupRNPoisson" | model=="OccupRPoisson")
   {
      parameters$r$begin=0
      parameters$r$num=-(nocc-1)
      parameters$r$type="Square"
      if(is.null(parameters$r$link))parameters$r$link="logit"
      parameters$Lambda$begin=0
      parameters$Lambda$num=-(nocc-1)
      parameters$Lambda$type="Square"
      if(is.null(parameters$Lambda$link))parameters$Lambda$link="log"
   } else
   if(model=="OccupRNNegBin" | model=="OccupRNegBin")
   {
      parameters$r$begin=0
      parameters$r$num=-(nocc-1)
      parameters$r$type="Square"
      if(is.null(parameters$r$link))parameters$r$link="logit"
      parameters$Lambda$begin=0
      parameters$Lambda$num=-(nocc-1)
      parameters$Lambda$type="Square"
      if(is.null(parameters$Lambda$link))parameters$Lambda$link="log"
      parameters$VarAdd$begin=0
      parameters$VarAdd$num=-(nocc-1)
      parameters$VarAdd$type="Square"
      if(is.null(parameters$VarAdd$link))parameters$VarAdd$link="log"
   } else
   if(model=="RDOccupEG")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      parameters$p$secondary=TRUE
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Psi$num=-(nocc-1)
      parameters$Psi$begin=0
      parameters$Psi$type="Square"
      parameters$Psi$secondary=FALSE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~1
      if(is.null(parameters$Psi$link))parameters$Psi$link="logit"
      parameters$Epsilon$num=-1
      parameters$Epsilon$begin=0
      parameters$Epsilon$type="Square"
      parameters$Epsilon$secondary=FALSE
      if(is.null(parameters$Epsilon$formula))parameters$Epsilon$formula=~1
      if(is.null(parameters$Epsilon$link))parameters$Epsilon$link="logit"
      parameters$Gamma$num=-1
      parameters$Gamma$begin=0
      parameters$Gamma$type="Square"
      parameters$Gamma$secondary=FALSE
      if(is.null(parameters$Gamma$formula))parameters$Gamma$formula=~1
      if(is.null(parameters$Gamma$link))parameters$Gamma$link="logit"
   } else
   if(model=="RDOccupPE")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      parameters$p$secondary=TRUE
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Psi$num=0
      parameters$Psi$begin=0
      parameters$Psi$type="Square"
      parameters$Psi$secondary=FALSE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~1
      if(is.null(parameters$Psi$link))parameters$Psi$link="logit"
      parameters$Epsilon$num=-1
      parameters$Epsilon$begin=0
      parameters$Epsilon$type="Square"
      parameters$Epsilon$secondary=FALSE
      if(is.null(parameters$Epsilon$formula))parameters$Epsilon$formula=~1
      if(is.null(parameters$Epsilon$link))parameters$Epsilon$link="logit"
   } else
   if(model=="RDOccupPG")
   {
      parameters$p$begin=0
      parameters$p$num=0
      parameters$p$type="Square"
      parameters$p$secondary=TRUE
      if(is.null(parameters$p$default))parameters$p$default=0
      if(is.null(parameters$p$link))parameters$p$link="logit"
      parameters$Psi$num=0
      parameters$Psi$begin=0
      parameters$Psi$type="Square"
      parameters$Psi$secondary=FALSE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~1
      if(is.null(parameters$Psi$link))parameters$Psi$link="logit"
      parameters$Gamma$num=-1
      parameters$Gamma$begin=0
      parameters$Gamma$type="Square"
      parameters$Gamma$secondary=FALSE
      if(is.null(parameters$Gamma$formula))parameters$Gamma$formula=~1
      if(is.null(parameters$Gamma$link))parameters$Gamma$link="logit"
   } else
   if(model=="RDOccupHetPG")
   {
      parameters=setup.parameters("RDOccupPG",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$pi$secondary=TRUE
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$formula))parameters$pi$formula=~1
   } else
   if(model=="RDOccupHetEG")
   {
      parameters=setup.parameters("RDOccupEG",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$pi$secondary=TRUE
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$formula))parameters$pi$formula=~1
   } else
   if(model=="RDOccupHetPE")
   {
      parameters=setup.parameters("RDOccupPE",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$pi$secondary=TRUE
      parameters$pi$begin=0
      parameters$pi$num=-(nocc-1)
      parameters$pi$type="Square"
      if(is.null(parameters$pi$default))parameters$pi$default=0
      if(is.null(parameters$pi$link))parameters$pi$link="logit"
      parameters$pi$mix=TRUE
      parameters$pi$rows=-1
      if(is.null(parameters$pi$formula))parameters$pi$formula=~1
   } else
   if(model=="BaseRobust")
   {
      parameters$S$begin=0
      parameters$S$num=-1
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      parameters$S$secondary=FALSE
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"
      parameters$GammaDoublePrime$num=-1
      parameters$GammaDoublePrime$begin=0
      parameters$GammaDoublePrime$type="Triang"
      if(is.null(parameters$GammaDoublePrime$default))parameters$GammaDoublePrime$default=0
      parameters$GammaDoublePrime$secondary=FALSE
      if(is.null(parameters$GammaDoublePrime$share))parameters$GammaDoublePrime$share=FALSE
      if(is.null(parameters$GammaDoublePrime$pim.type))parameters$GammaDoublePrime$pim.type="all"
      if(is.null(parameters$GammaDoublePrime$link))parameters$GammaDoublePrime$link="logit"
      parameters$GammaPrime$num=-2
      parameters$GammaPrime$begin=2
      parameters$GammaPrime$type="Triang"
      if(is.null(parameters$GammaPrime$default))parameters$GammaPrime$default=0
      parameters$GammaPrime$secondary=FALSE
      if(is.null(parameters$GammaPrime$pim.type))parameters$GammaPrime$pim.type="all"
      if(is.null(parameters$GammaPrime$link))parameters$GammaPrime$link="logit"
   } else
   if(model=="Robust")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("Closed",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$c$secondary=TRUE
      parameters$N$num=NA
      parameters$N$secondary=TRUE
      if(number.of.groups>1)
      {
         if(is.null(parameters$N$formula))
            parameters$N$formula=~group:session
      }else
      {
         if(is.null(parameters$N$formula))parameters$N$formula=~session
      }
   } else
   if(model=="RDHet")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("HetClosed",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$pi$secondary=TRUE
      parameters$pi$num=NA
      parameters$p$num=NA
      parameters$N$num=NA
      parameters$N$secondary=TRUE
      if(number.of.groups>1)
      {
         if(is.null(parameters$N$formula))
            parameters$N$formula=~group:session
      }else
      {
         if(is.null(parameters$N$formula))parameters$N$formula=~session
      }
   } else
   if(model=="RDFullHet")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("FullHet",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$pi$secondary=TRUE
      parameters$pi$num=NA
      parameters$c$secondary=TRUE
      parameters$N$num=NA
      parameters$N$secondary=TRUE
      if(number.of.groups>1)
      {
         if(is.null(parameters$N$formula))
            parameters$N$formula=~group:session
      }else
      {                                    
         if(is.null(parameters$N$formula))parameters$N$formula=~session
      }
   } else
   if(model=="RDHuggins")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("Huggins",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$c$secondary=TRUE
   } else
   if(model=="RDHHet")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("HugHet",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$p$num=NA
      parameters$pi$secondary=TRUE
      parameters$pi$num=NA
   } else
   if(model=="RDHFHet")
   {
      parameters=setup.parameters("BaseRobust",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters=setup.parameters("HugFullHet",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
      parameters$p$secondary=TRUE
      parameters$c$secondary=TRUE
      parameters$pi$secondary=TRUE
      parameters$pi$num=NA
   } else
    if(model=="ORDMS")
   {
      parameters$S$begin=0
      parameters$S$num=-1
      parameters$S$type="Triang"
      if(is.null(parameters$S$default))parameters$S$default=1
      parameters$S$bystratum=TRUE
      parameters$S$secondary=FALSE
      if(is.null(parameters$S$formula))parameters$S$formula=~stratum
      if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
      if(is.null(parameters$S$link))parameters$S$link="logit"

      parameters$Psi$begin=0
      parameters$Psi$num=-1
      parameters$Psi$type="Triang"
      if(is.null(parameters$Psi$default))parameters$Psi$default=1
      parameters$Psi$bystratum=TRUE
      parameters$Psi$tostrata=TRUE
      parameters$Psi$secondary=FALSE
      if(is.null(parameters$Psi$formula))parameters$Psi$formula=~stratum
      if(is.null(parameters$Psi$pim.type))parameters$Psi$pim.type="all"
      if(is.null(parameters$Psi$link))parameters$Psi$link="mlogit"

      parameters$Phi$num=-1
      parameters$Phi$begin=0
      parameters$Phi$type="Triang"
      if(is.null(parameters$Phi$default))parameters$Phi$default=1
      parameters$Phi$bystratum=TRUE
      parameters$Phi$secondary=TRUE
      if(is.null(parameters$Phi$formula))parameters$Phi$formula=~1
      if(is.null(parameters$Phi$pim.type))parameters$Phi$pim.type="all"
      if(is.null(parameters$Phi$link))parameters$Phi$link="logit"

      parameters$p$num=0
      parameters$p$begin=0
      parameters$p$type="Square"
      if(is.null(parameters$p$default))parameters$p$default=0
      parameters$p$bystratum=TRUE
      parameters$p$secondary=TRUE
      if(is.null(parameters$p$formula))parameters$p$formula=~1
      if(is.null(parameters$p$pim.type))parameters$p$pim.type="all"
      if(is.null(parameters$p$link))parameters$p$link="logit"

      parameters$pent$num=-1
      parameters$pent$begin=1
      parameters$pent$type="Square"
      parameters$pent$bystratum=TRUE
      parameters$pent$secondary=TRUE
      parameters$pent$pim.type="all"
      if(is.null(parameters$pent$default))parameters$pent$default=0
      if(number.of.groups>1)
         if(is.null(parameters$pent$formula))parameters$pent$formula=~group
      else
         if(is.null(parameters$pent$formula))parameters$pent$formula=~1
      if(is.null(parameters$pent$link))parameters$pent$link="mlogit"

	  } else
	  if(model=="CRDMS")
	  {
		  parameters$S$begin=0
		  parameters$S$num=-1
		  parameters$S$type="Triang"
		  if(is.null(parameters$S$default))parameters$S$default=1
		  parameters$S$bystratum=TRUE
		  parameters$S$secondary=FALSE
		  if(is.null(parameters$S$formula))parameters$S$formula=~stratum
		  if(is.null(parameters$S$pim.type))parameters$S$pim.type="all"
		  if(is.null(parameters$S$link))parameters$S$link="logit"
		  
		  parameters$Psi$begin=0
		  parameters$Psi$num=-1
		  parameters$Psi$type="Triang"
		  if(is.null(parameters$Psi$default))parameters$Psi$default=1
		  parameters$Psi$bystratum=TRUE
		  parameters$Psi$tostrata=TRUE
		  parameters$Psi$secondary=FALSE
		  if(is.null(parameters$Psi$formula))parameters$Psi$formula=~stratum
		  if(is.null(parameters$Psi$pim.type))parameters$Psi$pim.type="all"
		  if(is.null(parameters$Psi$link))parameters$Psi$link="mlogit"
		   
		  parameters=setup.parameters("Closed",parameters=parameters,nocc=nocc,check=FALSE,number.of.groups=number.of.groups)
		  parameters$p$bystratum=TRUE
		  parameters$c$bystratum=TRUE
		  parameters$p$secondary=TRUE
		  parameters$c$secondary=TRUE
		  parameters$N$num=NA
		  parameters$N$secondary=TRUE
		  parameters$N$bystratum=TRUE
		  if(number.of.groups>1)
		  {
			  if(is.null(parameters$N$formula))
				  parameters$N$formula=~group:session
		  }else
		  {
			  if(is.null(parameters$N$formula))parameters$N$formula=~session
		  }
		  
	  }else
	  if(model=="PoissonMR")
	  {
		  parameters$alpha$begin=0
		  parameters$alpha$num=0
		  parameters$alpha$type="Square"
		  if(is.null(parameters$alpha$default))parameters$alpha$default=0
		  if(is.null(parameters$alpha$pim.type))parameters$alpha$pim.type="all"
		  if(is.null(parameters$alpha$link))parameters$alpha$link="log"
		  parameters$sigma$begin=0
		  parameters$sigma$num=0
		  parameters$sigma$type="Square"
		  if(is.null(parameters$sigma$default))parameters$sigma$default=0
		  if(is.null(parameters$sigma$pim.type))parameters$sigma$pim.type="all"
		  if(is.null(parameters$sigma$link))parameters$sigma$link="log"
		  parameters$U$begin=0
		  parameters$U$num=0
		  parameters$U$type="Square"
		  if(is.null(parameters$U$default))parameters$U$default=0
		  if(is.null(parameters$U$pim.type))parameters$U$pim.type="all"
		  if(is.null(parameters$U$link))parameters$U$link="log"
		  parameters$Phi$num=-1
		  parameters$Phi$begin=0
		  parameters$Phi$type="Triang"
		  if(is.null(parameters$Phi$default))parameters$Phi$default=1
		  if(is.null(parameters$Phi$share))parameters$Phi$share=FALSE
		  if(is.null(parameters$Phi$pim.type))parameters$Phi$pim.type="all"
		  if(is.null(parameters$Phi$link))parameters$Phi$link="logit"		  
		  parameters$GammaDoublePrime$num=-1
		  parameters$GammaDoublePrime$begin=0
		  parameters$GammaDoublePrime$type="Triang"
		  if(is.null(parameters$GammaDoublePrime$default))parameters$GammaDoublePrime$default=0
		  if(is.null(parameters$GammaDoublePrime$share))parameters$GammaDoublePrime$share=FALSE
		  if(is.null(parameters$GammaDoublePrime$pim.type))parameters$GammaDoublePrime$pim.type="all"
		  if(is.null(parameters$GammaDoublePrime$link))parameters$GammaDoublePrime$link="logit"
		  parameters$GammaPrime$num=-2
		  parameters$GammaPrime$begin=2
		  parameters$GammaPrime$type="Triang"
		  if(is.null(parameters$GammaPrime$default))parameters$GammaPrime$default=0
		  if(is.null(parameters$GammaPrime$pim.type))parameters$GammaPrime$pim.type="all"
		  if(is.null(parameters$GammaPrime$link))parameters$GammaPrime$link="logit"
	  }else
	  if(model=="LogitNormalMR")
	  {
		  parameters$p$begin=0
		  parameters$p$num=0
		  parameters$p$type="Square"
		  parameters$p$secondary=TRUE
		  if(is.null(parameters$p$default))parameters$p$default=0
		  if(is.null(parameters$p$link))parameters$p$link="logit"

		  parameters$sigma$num=NA
		  parameters$sigma$type="Square"
		  parameters$sigma$secondary=TRUE
		  if(is.null(parameters$sigma$default))parameters$sigma$default=0
		  if(is.null(parameters$sigma$link))parameters$sigma$link="log"
		  if(number.of.groups>1)
			  if(is.null(parameters$sigma$formula))parameters$sigma$formula=~group:session
			  else
			  if(is.null(parameters$sigma$formula))parameters$sigma$formula=~session

		  parameters$N$num=NA
		  parameters$N$secondary=TRUE
		  parameters$N$type="Square"
		  if(is.null(parameters$N$link))parameters$N$link="log"
		  if(number.of.groups>1)
		  {
			  if(is.null(parameters$N$formula))
				  parameters$N$formula=~group:session
		  }else
		  {
			  if(is.null(parameters$N$formula))parameters$N$formula=~session
		  }
	  } else
	  if(model=="IELogitNormalMR")
	  {
		  parameters$p$begin=0
		  parameters$p$num=0
		  parameters$p$type="Square"
		  parameters$p$secondary=TRUE
		  if(is.null(parameters$p$default))parameters$p$default=0
		  if(is.null(parameters$p$link))parameters$p$link="logit"
		  
		  parameters$sigma$num=NA
		  parameters$sigma$type="Square"
		  parameters$sigma$secondary=TRUE
		  if(is.null(parameters$sigma$default))parameters$sigma$default=0
		  if(is.null(parameters$sigma$link))parameters$sigma$link="log"
		  if(number.of.groups>1)
			  if(is.null(parameters$sigma$formula))parameters$sigma$formula=~group:session
			  else
			  if(is.null(parameters$sigma$formula))parameters$sigma$formula=~session
		  
		  parameters$Nbar$num=NA
		  parameters$Nbar$secondary=TRUE
		  parameters$Nbar$type="Square"
		  if(is.null(parameters$Nbar$link))parameters$Nbar$link="log"
		  if(number.of.groups>1)
		  {
			  if(is.null(parameters$Nbar$formula))
				  parameters$Nbar$formula=~group:session
		  }else
		  {
			  if(is.null(parameters$Nbar$formula))parameters$Nbar$formula=~session
		  }

		  parameters$alpha$begin=0
		  parameters$alpha$num=-1
		  parameters$alpha$type="Square"
		  parameters$alpha$secondary=TRUE
		  if(is.null(parameters$alpha$default))parameters$alpha$default=0
		  if(is.null(parameters$alpha$link))parameters$alpha$link="identity"
		  
		  parameters$Nstar$num=NA
		  parameters$Nstar$secondary=TRUE
		  parameters$Nstar$type="Square"
		  if(is.null(parameters$Nstar$link))parameters$Nstar$link="log"
		  if(number.of.groups>1)
		  {
			  if(is.null(parameters$Nstar$formula))
				  parameters$Nstar$formula=~group:session
		  }else
		  {
			  if(is.null(parameters$Nstar$formula))parameters$Nstar$formula=~session
		  }
	  }
return(parameters)
}
