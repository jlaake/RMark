"make.design.data" <-                                                      
function(data,parameters=list(),remove.unused=FALSE,right=TRUE,common.zero=FALSE)
{
#------------------------------------------------------------------------------------------------------
# make.design.data -  creates a design dataframe that is used to construct the design matrix for mark
#                     in make.mark.model
#
# Arguments:
#
#    data             - data list after using process.data
#    parameters       - list with an element for each parameter
#                       each element is a list with age.bins, time.bins and cohort.bins
#                          age.bins         - bins for grouping ages
#                          time.bins        - bins for grouping times
#                          cohort.bins      - bins for grouping cohorts
#                          pim.type         - type of pim structure "all","time","constant"
#                          subtract.stratum - for each stratum, the one to compute by subtraction (for Psi only)
#                          time.varying     - vector of field names that are time varying for this parameter
#                          fields           - vector of field names to be included in design data that are not time varying
#    remove.unused    - if TRUE, unused design data are removed; for triangular
#                       pims, unused design data are determined based on lack of
#                       ch for a particular row (cohort) of a group;  for square
#                       pims. if there is a cohort field in the design data, then
#                       it excludes any design data in which cohort < time.
#
#    common.zero      - if TRUE, uses a common begin.time to set origin (0) for Time variable
#                      defaults to FALSE for legacy reasons but should be set to TRUE
#                      for models that share formula like p and c with the Time model
#
# Value:
#    full.design.data - list of design data frames for each type of parameter in the model
#
#
# Functions used: setup.parameters, compute.design.data, valid.parameters, setup.model
#
#----------------------------------------------------------------------------------------------------
remove.unused.occasions=function(data,ddl)
{
#
# Check validity of parameter list; stop if not valid
#
		parameter="p"
		if(!valid.parameters(data$model,parameter)) stop()
		parameters=setup.parameters(data$model,parameters=NULL,data$nocc,check=FALSE,
				number.of.groups=dim(data$freq)[2])
		if(parameters[[parameter]]$type !="Triang")stop("\nDoes not work for parameters with non-triangular PIM\n")
		ch=data$data$ch
		if(data$model=="Multistrata")
			ch=gsub("[1-9 a-z A-Z]","1",ch)
#
#    Loop over groups
#
		number.of.groups=dim(data$group.covariates)[1]
		if(is.null(number.of.groups))
		{
			number.of.groups=1
			chsplit=ch
		}
		else
			ch.split=split(ch,data$data$group)
		for(j in 1:number.of.groups)
		{
			chmat=matrix(as.numeric(unlist(strsplit(ch.split[[j]],split=vector(length=0)))),ncol=nchar(ch[1]),byrow=TRUE)
			exclude.occ=(1:dim(chmat)[2])[colSums(chmat)==0]
			if(number.of.groups==1)
				ddl[[parameter]]=ddl[[parameter]][!ddl[[parameter]]$time%in%levels(ddl[[parameter]]$time)[exclude.occ-1],]
			else
			{
				group=levels(ddl[[parameter]]$group)[j]
				ddl[[parameter]]=ddl[[parameter]][(ddl[[parameter]]$group!=group) |
								(ddl[[parameter]]$group==group & !ddl[[parameter]]$time%in%levels(ddl[[parameter]]$time)[exclude.occ-1]),]
			}
		}
		return(ddl)
	}
#
# Check validity of parameter list; stop if not valid
#
  if(!valid.parameters(data$model,parameters)) stop()
#
#  Add following elements based on type of model
#           begin            - index for compute.design.data
#           num              - number of parameters relative to number of occasions
#
  par.list=setup.parameters(data$model,check=TRUE)
  parameters=setup.parameters(data$model,parameters,data$nocc,check=FALSE,
          number.of.groups=dim(data$freq)[2])
  parameters=parameters[par.list]
  model.list=setup.model(data$model,data$nocc,data$mixtures)
# If reverse, set remove.unused=TRUE  
  if(data$reverse)
  {
	  remove.unused=TRUE
	  temp=parameters[["Psi"]]$subtract.stratum
	  if(!is.null(temp) && any(temp!=data$strata.labels)) stop("Cannot set subtract.stratum and use reverse\n")
  }
#
# Create a data matrix for the each parameter in the model with age, year and cohort for each index
# This data matrix (design.data) is used below to create the design matrix from the formulas
# If age,cohort or year bins are given, use those.  Otherwise each is treated as a factor 
# wihtout binning.
#
# 10 Jan 06 ; added pim.type argument in call to compute.design.data
#
full.design.data=vector("list",length=length(parameters))
if(is.null(model.list$stype) | model.list$stype=="mark")
{
   pimtypes=vector("list",length=length(parameters))
   anyTriang=FALSE
   anySquare=FALSE
   for(i in 1:length(parameters))
   {
#
# For mixtures, multistrata and robust designs set up values for input to
# compute.design.data
#
     if(data$mixtures==1)
     {
        parameters[[i]]$mix=FALSE
        parameters[[i]]$rows=1
     }
     if(!is.null(parameters[[i]]$bystratum) && parameters[[i]]$bystratum)
     {
        strata.labels=data$strata.labels
        nstrata=data$nstrata
        if(!is.null(parameters[[i]]$tostrata) && parameters[[i]]$tostrata)
        {
           if(!is.null(parameters[[i]]$subtract.stratum))
              subtract.stratum=parameters[[i]]$subtract.stratum
           else
              subtract.stratum=strata.labels
           tostrata=TRUE
        }
        else
        {
           subtract.stratum=NULL
           tostrata=FALSE
        }
     } 
     else
     {
        subtract.stratum=NULL
        strata.labels=NULL
        nstrata=1
        tostrata=FALSE
     }
     if(!model.list$robust) parameters[[i]]$secondary=FALSE
#
#    Compute design data for this parameter if conditions are valid
#    mod 27 June 2011 -- if data structure (too few occasions) is such that no parameters can be estimated it does not create the design data
     if(is.na(parameters[[i]]$num)||(parameters[[i]]$num+data$nocc)>0)
	 {
         design.data=compute.design.data(data,parameters[[i]]$begin,parameters[[i]]$num,
                      parameters[[i]]$type,parameters[[i]]$mix,parameters[[i]]$rows,
                      parameters[[i]]$pim.type,parameters[[i]]$secondary, nstrata,
                      tostrata,strata.labels,subtract.stratum,common.zero=common.zero)
         if(!is.null(parameters[[i]]$mix) && parameters[[i]]$mix)design.data$mixture=as.factor(design.data$mixture)
         if(parameters[[i]]$secondary)design.data$session=as.factor(design.data$session+data$begin.time-1)
         design.data$group=as.factor(design.data$group)
         if(!is.null(data$group.covariates))
            levels(design.data$group)=apply(data$group.covariates,1,paste,collapse="")
         if(!is.null(design.data$cohort))
            if(is.null(parameters[[i]]$cohort.bins))
               design.data$cohort=factor(design.data$cohort,levels=unique(levels(factor(design.data$cohort))))
            else
               design.data$cohort=cut(design.data$cohort,parameters[[i]]$cohort.bins,include.lowest=TRUE,right=right)
         if(!is.null(design.data$age))
         if(is.null(parameters[[i]]$age.bins))
           design.data$age=factor(design.data$age,levels=unique(levels(factor(design.data$age))))
         else
           design.data$age=cut(design.data$age,parameters[[i]]$age.bins,include.lowest=TRUE,right=right)
         if(!is.null(design.data$time))
# mod 30 Sept 09 to remove unused time factor levels
         if(is.null(parameters[[i]]$time.bins))
            design.data$time=factor(design.data$time,levels=unique(levels(factor(design.data$time))))
         else
            design.data$time=cut(design.data$time,parameters[[i]]$time.bins,include.lowest=TRUE,right=right)
         if(model.list$closed | model.list$robust )
         {
            if(names(parameters)[i]=="p")
            {
               design.data$c=0
               design.data$age=NULL
               design.data$Age=NULL
            }
            if(names(parameters)[i]=="c")
            {
               design.data$c=1
               design.data$age=NULL
               design.data$Age=NULL
            }
            if(names(parameters)[i]=="N" | names(parameters)[i]=="pi")
            {
               design.data$age=NULL
               design.data$Age=NULL
               design.data$time=NULL
               design.data$Time=NULL
            }
         }
         full.design.data[[i]]=design.data
         pimtypes[[i]]=list(pim.type=parameters[[i]]$pim.type)
	     if(!is.null(subtract.stratum))pimtypes[[i]]$subtract.stratum=subtract.stratum
         if(parameters[[i]]$type =="Triang"&&parameters[[i]]$pim.type=="all")anyTriang=TRUE
         if(parameters[[i]]$type =="Square")anySquare=TRUE
	  }
   }
   names(full.design.data)=names(parameters)
   null.design.data=sapply(full.design.data,is.null)
   parameters=parameters[!null.design.data]
   full.design.data=full.design.data[!null.design.data]
#
#  Remove unused design data
#
   if(remove.unused)
   {
      ch=data$data$ch
      if(data$model=="Multistrata")
        ch=gsub("[A-Z a-z 1-9]","1",ch)
      if(anyTriang)
      {
#
#    Loop over groups
#
         number.of.groups=dim(data$group.covariates)[1]
         if(is.null(number.of.groups))number.of.groups=1
         for(j in 1:number.of.groups)
         {
           remove.cohort=NULL
           for(k in 1:data$nocc)
           {
              if(k>1)
                 first.0=paste(rep("0",k-1),collapse="")
              else
                 first.0=""
              if(number.of.groups==1)
              {
                 if(!any(substr(ch,1,k)==paste(first.0,"1",sep="")))
                    remove.cohort=c(remove.cohort,k)
              }
              else
                 if(!any(substr(ch[data$data$group==j],1,k)==paste(first.0,"1",sep="")))
                    remove.cohort=c(remove.cohort,k)
           }
           for(i in 1:length(parameters))
           {
              if(parameters[[i]]$type =="Triang"&&parameters[[i]]$pim.type=="all")
              {
                 if(number.of.groups==1)
					 full.design.data[[i]]=full.design.data[[i]][!(full.design.data[[i]]$occ.cohort%in%remove.cohort),]
#				 full.design.data[[i]]=full.design.data[[i]][!(as.numeric(full.design.data[[i]]$cohort)%in%remove.cohort),]
                 else
                 {
#          modified 7 Apr 08 to handle different begin.times between groups
				full.design.data[[i]]=full.design.data[[i]][!(as.numeric(full.design.data[[i]]$group)==j &
									full.design.data[[i]]$occ.cohort%in%remove.cohort),]
#				full.design.data[[i]]=full.design.data[[i]][!(as.numeric(full.design.data[[i]]$group)==j &
#                             as.numeric(factor(full.design.data[[i]]$cohort,levels=unique(full.design.data[[i]]$cohort[as.numeric(full.design.data[[i]]$group)==j ])))%in%remove.cohort),]
#          modified 10 Aug to remove unused levels created in removing cohorts                    
                    full.design.data[[i]]$cohort=factor(full.design.data[[i]]$cohort)
                    full.design.data[[i]]$age=factor(full.design.data[[i]]$age)
                    full.design.data[[i]]$time=factor(full.design.data[[i]]$time)
                 }
              }
           }                             
        }
     }
#    if reverse Multistrata model, remove design data for S,Psi and p for added occasions/intervals
     if(data$reverse)
	 {
		 full.design.data[["S"]]=full.design.data[["S"]][!full.design.data[["S"]]$occ%in%seq(1,data$nocc-1,2),]
		 full.design.data[["p"]]=full.design.data[["p"]][!full.design.data[["p"]]$occ%in%seq(1,data$nocc-1,2),]
		 full.design.data[["Psi"]]=full.design.data[["Psi"]][!full.design.data[["Psi"]]$occ%in%seq(2,data$nocc-1,2),]
	 }
     if(anySquare)
     {
        for(i in 1:length(parameters))
        {
           if(parameters[[i]]$type =="Square"&is.null(parameters[[i]]$leave.unused))
           {
              time=full.design.data[[i]]$time
              cohort=full.design.data[[i]]$cohort
              full.design.data[[i]]=full.design.data[[i]][as.numeric(levels(time)[as.numeric(time)])
                      >= as.numeric(levels(cohort)[as.numeric(cohort)]),]
           }
        }
     }
#    drop any unused factor levels after removing design data
     for(i in 1:length(parameters))full.design.data[[i]]=droplevels(full.design.data[[i]])
   }
#  Delete occ.cohort which is only used to remove unused cohorts if any
   if(data$reverse)
      for(i in 1:length(parameters))
	      full.design.data[[i]]$occ.cohort=NULL
#  make pim type assignments and return results
   pimtypes=pimtypes[!null.design.data]
   names(pimtypes)=names(parameters)
   full.design.data$pimtypes=pimtypes
   return(full.design.data)
}
}

        
         