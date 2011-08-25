"process.data" <-
function(data,begin.time=1,model="CJS",mixtures=1,groups=NULL,allgroups=FALSE,age.var=NULL,
initial.ages=c(0),age.unit=1,time.intervals=NULL,nocc=NULL,strata.labels=NULL,counts=NULL,reverse=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------
#
# process.data - processes data by adding groups if any, and creates a list that
#                contains the original data and input values that are used in other 
#                routines (e.g., number of occasions)
#
#  Arguments: 
#
#  data			        - data frame with at least one field ch which is the
#                         capture history stored as a character string
#                         data can also have a field freq which is the frequency
#                         of that capture history; it can also have individual covariates
#  begin.time           - time of first capture occasion for all or by group – used for labels
#  model                - type of analysis model
#  mixtures             - number of mixtures in heterogeneity models
#  groups               - vector of factor variable names for creating groups
#  allgroups            - logical variable; if TRUE all groups are created even if there are no data
#  age.var              - index in groups vector for age variable (if any)
#  initial.ages         - vector of initial ages for each level of age factor
#  age.unit             - age increment for each increment of time interval
#  time.intervals       - intervals of time between the capture occasions
#  nocc                 - number of occasions for Nest type; either nocc or time.intervals
#                          must be specified
#  strata.labels        - vector of single character labels for strata in ORDMS or CRDMS
#  counts               - list of numeric vectors (one group) or matrices (>1 group) 
#              	           containing counts for mark-resight models
#  reverse              - only valid for Multistrata model; reverses timing of movement and survival
#
#  Value: result (list with the following elements)
#
#  data 		            - original raw data with group factor variable added if groups were defined
#  model                - type of analysis model (eg, "CJS", "Burnham", "Barker")
#  freq			            - matrix of frequencies (same # of rows as data,
#                         number of columns is the number of groups in the data
#  nocc                 - number of capture occasions 
#  time.intervals       - length of time intervals between capture occasions
#  begin.time           - time of first capture occasion
#  age.unit             - increment of age for each increment of time
#  initial.ages         - an initial age for group 
#  group.covariates     - covariates used to define groups 
#
#  Functions used: setup.model
# -------------------------------------------------------------------------------------
"robust.occasions"<-function(times)
{
   if(times[1] !=0 | times[length(times)]!=0)
      stop("\nIncorrect structure for time intervals with robust design. Time intervals must begin and end with a zero.\n")
   times.vec=match(as.character(times),"0")
   nocc.secondary=nchar(unlist(strsplit(paste(as.character(times.vec),collapse=""),"NA")))
   nocc=length(nocc.secondary)
   nocc.secondary=nocc.secondary+1
   if(any(nocc.secondary==0))
      stop("\nIncorrect setup for time intervals. Must have at least one zero between non-zero intervals.\n")
   return(list(nocc=nocc,nocc.secondary=nocc.secondary))
}
   dataname=substitute(data)
#
#  handle Multistrata reversal if needed
#
   if(reverse)
   {
	   if(model!="Multistrata")
		   stop("reverse can only be set TRUE for the Multistrata model")
	   else
	   {
		   if(is.null(time.intervals))time.intervals=rep(1,nchar(data$ch[1])-1)
		   data$ch=sapply(strsplit(data$ch,""),paste,collapse="0") 
		   time.intervals=as.vector(matrix(c(rep(0,length(time.intervals)),time.intervals),ncol=length(time.intervals),nrow=2,byrow=TRUE))
	   }
   }
#
#   If model="Nest" a completely different structure is used.  No ch is used; fate used instead
#
   if(model=="Nest")
   {
      begin.time=1
      nstrata=1
      strata.labels=""
      if(!all(c("FirstFound","LastPresent","LastChecked","Fate") %in% names(data)))
         stop("data should contain fields: FirstFound, LastPresent, LastChecked, Fate. One or more are missing.")
      if(is.null(time.intervals))
      {
         if(is.null(nocc)) stop("nocc or time.intervals must be set for Nest model")
         time.intervals=rep(1,nocc)
      }
      else
      {
         if(is.null(nocc)) nocc=length(time.intervals)
         if(length(time.intervals)!=nocc)stop("length of time intervals must match nocc for Nest model")
      }
      if(any(data$FirstFound>nocc | data$FirstFound< 1))
         stop("One or more FirstFound values greater than number of occasions or less than 1")
      if(any(data$LastChecked>nocc| data$LastChecked< 1))
         stop("One or more LastChecked values greater than number of occasions or less than 1")
      if(any(data$LastPresent>nocc | data$LastPresent<1))
         stop("One or more LastPresent values greater than number of occasions or less than 1")
   }
   else
   {
#
#  Compute number of occasions and check validity of model
#
      if(is.null(data$ch))
       stop("Field ch is missing in ",substitute(data))
      ch.lengths=nchar(data$ch)
      nocc=median(ch.lengths)
      if(any(ch.lengths!=nocc))
      {
           stop(paste("\nCapture history length is not constant. ch must be a character string",
               "\n row numbers with incorrect ch length",paste(row.names(data[ch.lengths!=nocc,]),collapse=","),"\n"))
      }
  }
#
#  Setup model
#
   model.list=setup.model(model,nocc,mixtures)
#
#  If multistrata design, determine number of strata and their labels
#  Make sure multistrata designs have at least 2 strata
#
   if(model!="Nest")
   {
      ch.values=unique(unlist(strsplit(data$ch,"")))
      if(model=="MSLiveDead")
         inp.strata.labels=sort(ch.values[!(ch.values %in% c("0",".","1"))])
      else
         inp.strata.labels=sort(ch.values[!(ch.values %in% c("0","."))])
      nstrata = length(inp.strata.labels)                  
      if(model=="Multistrata" | model=="ORDMS"| model=="CRDMS" | model=="MSLiveDead")
      {
#        if(nstrata<2)stop("\nMultistrata designs must have at least 2 strata\n")
#        strata.labels=inp.strata.labels
#      } else
#      if(model=="ORDMS")
#      {
        if(is.null(strata.labels)) 
        {
           strata.labels=inp.strata.labels
        }
        else
        {
           nstrata=length(strata.labels)
           if(!all(inp.strata.labels %in% strata.labels))
              stop(paste("Some strata labels in data",paste(inp.strata.labels),"are not in strata.labels"))
           if(sum(as.numeric(strata.labels %in% inp.strata.labels))< (nstrata-1))
              stop("More than one non-observable state has been specified")
        }
        if(nstrata<2)stop("\nCRDMS, ORDMS and Multistrata designs must have at least 2 strata\n")
      } else
      {              
         nstrata=1
         if(!is.null(model.list$occupancy) && model.list$occupancy)
         {
            if(model == "OccupRPoisson" | model=="OccupRNegBin")
            {
               if(any(!ch.values%in%c(0:9)))
                  stop(paste("\nIncorrect count values in data:",paste(ch.values,collapse=""),"\n",sep=""))
            }
            else
            {
               if(any(!ch.values%in%c(".","0","1","2")))
                    stop(paste("\nIncorrect ch values in data:",paste(ch.values,collapse=""),"\n",sep=""))
            }
         }
         else
         {
            if(any(!ch.values%in%c("0","1",".")))
            {
               if(model!="Barker" & model!="MSOccupancy" &model!="PoissonMR")
                  stop(paste("\nIncorrect ch values in data:",paste(ch.values,collapse=""),"\n",sep=""))
               else
				  if(model=="PoissonMR")
				  {			  
					  if(any(!ch.values%in%c(".",as.character(0:9),"+","-")))
						  stop(paste("\nIncorrect ch values in data:",paste(ch.values,collapse=""),"\n",sep=""))
				  } else
				  {
				      if(any(!ch.values%in%c(".","0","1","2")))
                         stop(paste("\nIncorrect ch values in data:",paste(ch.values,collapse=""),"\n",sep=""))
			      }
            }
         }
      }
   }
#
#  If this is a robust design, compute number of primary and secondary occasions
#
   if(model.list$robust)
   {
      if(is.null(time.intervals))
         stop("\nTime intervals must be specified for a robust design\n")
      else
      {
         nocc.list=robust.occasions(time.intervals)
         nocc=nocc.list$nocc
         nocc.secondary=nocc.list$nocc.secondary
         if(any(nchar(data$ch)!=sum(nocc.secondary)))
             stop("Incorrect number of time intervals. One or more capture history lengths do not match time interval structure.")

      }
      num=model.list$num
   }
   else
   {
      nocc=model.list$nocc
      nocc.secondary=NULL
      num=model.list$num
#
#     If time intervals specified make sure there are nocc-1 of them
#     If none specified assume they are 1
#
      if(is.null(time.intervals))
         time.intervals=rep(1,nocc+model.list$num)
      else
         if(length(time.intervals)!=(nocc+num))
             stop("Incorrect number of time intervals")
   }
   mixtures=model.list$mixtures
#
#  Get number of factors to create groups
#
   if(is.null(groups))
     number.of.factors=0
   else
     number.of.factors=length(groups)
#
#  Get number of records in data set
#
number.of.ch=dim(data)[1]
#
#  See if there is already a freq variable in data set
#
if(!is.null(data$Freq)) names(data)[which("Freq"== names(data))]="freq"
has.freq=!is.null(data$freq)
#
#  If there are no factors then
#     if already has freq variable return the input data set as a list
#     otherwise add the freq variable with each value = 1 and return as a list
#  If model=js, then add dummy data for non-captured 
#
if(number.of.factors==0)
{
   if(has.freq)
   {
#       if(model=="js")
#       {
#          data=add.dummy.data(data,nocc=nocc,group.covariates=NULL)     
#          number.of.ch=dim(data)[1]
#       }
       return(list(data=data,model=model,mixtures=mixtures,
                   freq=matrix(data$freq,ncol=1,dimnames=list(1:number.of.ch,"group1")),
                   nocc=nocc, nocc.secondary=nocc.secondary,time.intervals=time.intervals,begin.time=begin.time,
                   age.unit=1,initial.ages=initial.ages[1],group.covariates=NULL,nstrata=nstrata,
                   strata.labels=strata.labels,counts=counts,reverse=reverse))
   }
   else
   {
       data$freq=rep(1,number.of.ch)
#       if(model=="js")
#       {
#          data=add.dummy.data(data,nocc=nocc,group.covariates=NULL)            
#          number.of.ch=dim(data)[1]
#       }
       return(list(data=data,model=model,mixtures=mixtures,
                   freq=matrix(rep(1,number.of.ch),ncol=1,dimnames=list(1:number.of.ch,"group1")),
                   nocc=nocc,  nocc.secondary=nocc.secondary, time.intervals=time.intervals,begin.time=begin.time,
                   age.unit=1,initial.ages=initial.ages[1],group.covariates=NULL,nstrata=nstrata,
                   strata.labels=strata.labels,counts=counts,reverse=reverse))
   }
}
#
#   If there are one or more in the group factor list then
#     make sure each is a factor variable in the data set and compute number
#         of levels for each factor, cumlevels and factor matrix
#     if not a factor variable - stop with error message
# 
else
{
  if(!has.freq)
       data$freq=rep(1,number.of.ch)
  number.of.groups=1
#  var=rep(" ",number.of.factors)
  n.levels=rep(0,number.of.factors)
  facmat=NULL
  faclabs=list()
  for (i in 1:number.of.factors)
  {
    vari=data[,groups[i]]
#    var[i]=paste(dataname,"$",groups[i],sep="")
#    vari=eval.parent(parse(text=var[i]))
    if(!is.factor(vari))
        stop(paste("\n ",groups[i]," is not a factor variable\n"))
     else
     {
        n.levels[i]=length(levels(vari))
        facmat=cbind(facmat,as.numeric(vari)-1)
        faclabs[[i]]=levels(vari)
     }       
  }
  cumlevels=cumprod(n.levels)
  number.of.groups=cumlevels[length(cumlevels)]

#  If age.var is specified, make sure it is valid and that the number of 
#  initial.ages matches number of levels of identified variable
#
   if(is.null(age.var))
      age.var=match("age",groups)
   if(!is.na(age.var))
   {
      if(age.var>length(groups) | age.var<1)
         stop("Invalid age variable. Must be between 1 and ",length(groups))
      if(is.null(initial.ages))
         stop("initial.ages must be specified if age.var is specified")
      else
         if(!is.numeric(initial.ages) | (length(initial.ages)!=n.levels[age.var] &length(initial.ages)>1) )
           stop(paste("intial.ages must be numeric and match length of levels of",groups[age.var]))
      if(age.unit<=0 | !is.numeric(age.unit)) stop("age.unit must be numeric and >0")
   }
#
#  Next compute the group number for each capture history
#
   if(number.of.factors==1)
      data$group=facmat+1
   else
      if(number.of.factors==2)
         data$group=facmat[,2]*cumlevels[1]+facmat[,1]+1
      else
         data$group=facmat[,2:number.of.factors]%*%cumlevels[1:(number.of.factors-1)]+facmat[,1]+1
#
#  Next create frequency matrix for groups   
#
  freqmat=matrix(0,nrow=number.of.ch,ncol=number.of.groups)
  for(i in 1:number.of.ch)
  {
     freqmat[i,data$group[i]]=data$freq[i]
  }
#
#  If allgroups=FALSE, recompute number of groups and group number based on groups with 1 or more capture histories
#
  if(!allgroups)
  {
     test.freq=freqmat
     test.freq[test.freq!=0]=1
     chcounts = apply(test.freq, 2, sum)
     newgroups=rep(0,number.of.groups)
     index=1
     for (i in 1:number.of.groups)
        if(chcounts[i]>0)
        {
           newgroups[i]=index
           index=index+1
        }     
     data$group=as.factor(newgroups[data$group])
     freqmat=freqmat[,chcounts>0]
     number.of.groups=index-1
  }
#
#  Check to make sure length of begin.time is either 1 or equal to the
#  number of groups
#
  if(length(begin.time)!=1 & length(begin.time)!=number.of.groups)
    stop("length of begin.time must either be 1 or match number of groups")
#
#  Create group labels
#  
  labs=expand.grid(faclabs)
  if(!allgroups)labs=as.matrix(labs[chcounts>0,])
#
#  If age.var has not been set, initial ages are set to 0
#
  if(is.na(age.var))
    init.ages=rep(initial.ages[1],number.of.groups)
  else
  {
    if(length(initial.ages)==1)
       initial.ages=rep(initial.ages,length(levels(as.factor(labs[,age.var]))))
    init.ages = initial.ages[as.numeric(factor(labs[,age.var],levels=unique(faclabs[[age.var]])))]
  }
  grouplabs=rep(" ",number.of.groups)
  for (i in 1:number.of.groups)
     grouplabs[i]=paste(groups,labs[i,],sep="",collapse=".") 
  freqmat=as.data.frame(freqmat)
  names(freqmat)=grouplabs
#
#  Store labs as group covariates; set levels to the same as in data
#  
  group.covariates=as.data.frame(labs)
  names(group.covariates)=groups
  for (i in 1:dim(group.covariates)[2])
     group.covariates[,i]=factor(group.covariates[,i],levels=levels(data[,groups[i]]))
#
# Return data as a list with original dataframe and frequency matrix
#
#  if(model=="js")
#     data=add.dummy.data(data,nocc,group.covariates)     
#  else
  if(!has.freq)data$freq=NULL
  return(list(data=data,model=model,mixtures=mixtures,freq=freqmat,
                   nocc=nocc, nocc.secondary=nocc.secondary, time.intervals=time.intervals,begin.time=begin.time,
                   age.unit=age.unit,initial.ages=init.ages,
                   group.covariates=group.covariates,nstrata=nstrata,
                   strata.labels=strata.labels,counts=counts,reverse=reverse))
}
}
