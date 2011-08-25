"compute.design.data" <-
function(data,begin,num,type="Triang",mix=FALSE,rows=0,pim.type="all",
           secondary,nstrata=1,tostrata=FALSE,strata.labels=NULL,
           subtract.stratum=strata.labels,common.zero=FALSE)
{
# -------------------------------------------------------------------------------------------------------------
#
# compute.design.data -  creates a design dataframe that is used to construct the design matrix 
#
# Arguments:
#
#  data             - data list created by process.data
#  begin            - 0 for survival type, 1 for capture type
#  num              - number of parameters relative to number of occasions (0 or -1)
#  type             - type of parameter structure (Triang or Square)
#  mix              - if TRUE this is a mixed parameter
#  rows             - number of rows relative to # of mixtures
#  secondary        - TRUE if a parameter for the secondary periods of robust design
#  pim.type         - type of pim structure; either all different or time
#  nstrata          - number of strata for multistrata
#  tostrata         - set to TRUE for Psi parameters
#  strata.labels    - labels for strata as identified in capture history
#  subtract.stratum - for each stratum, the to.strata that is computed by subtraction
#  common.zero      - if TRUE, uses a common begin.time to set origin (0) for Time variable
#                      defaults to FALSE for legacy reasons but should be set to TRUE
#                      for models that share formula like p and c with the Time model
#
# Value:
#
#  design.data     - design data (cohort, group, age, time) for a particular parameter
#
#
# -------------------------------------------------------------------------------------------------------------
#
# Create a data matrix for the parameter PIM that contains age, year and cohort for each index
# This data matrix (design.data) is used to create the design matrix from the formulas
#
  if(secondary)
     num.sessions=data$nocc
  else
  {
     num.sessions=1
     if(!is.null(pim.type)&&pim.type=="constant")
       num=1
     else
       num=data$nocc+num
  }
  if(type=="Square")
  {
     num.lines=1
     if(is.null(mix) || !mix)
        num.rows=1
     else
        num.rows=data$mixtures+rows 
  }
  else
  {
  #
  #  pim.type field allows either all-different or time pims for Triangular pims
  #
     if(pim.type=="all")
     {
        num.lines=num
        num.rows=1
     }
     else
     {
        num.lines=1
        num.rows=1
     }
  }
  if(setup.model(data$model,data$nocc)$robust)
     time.intervals=data$time.intervals[data$time.intervals>0]
  else
     time.intervals=data$time.intervals
  number.of.groups=dim(data$freq)[2]
  design.data=NULL
  nsubtract.stratum=match(subtract.stratum,strata.labels)
  for(j in 1:number.of.groups)
  for (jj in 1:nstrata)
  for(l in 1:num.sessions)
  {
      if(tostrata)
         other.strata= sequence(nstrata)[sequence(nstrata)!=nsubtract.stratum[jj]]
      else
         other.strata=1
  for(to.strata in other.strata)
  {
     if(secondary)
     {
       if(is.na(num))
          ncol=1
       else
       {
          ncol=data$nocc.secondary[l]+num
          if(type=="Triang")num.lines=ncol
       }
     }
     else
        ncol=num
     for(i in 1:num.lines)
     for(k in 1:num.rows)
     {    
#
#     Define age variable
#
        if(secondary)
           ages=0
        else
           if(begin==0)
              ages=data$initial.ages[j]+data$age.unit*(cumsum(c(0,time.intervals[i:num]))[1:ncol])
           else
              ages=data$initial.ages[j]+data$age.unit*(cumsum(time.intervals[i:num]))
#
#     Define cohort variable
#
        if(secondary)
           if(type!="Triang")
              cohort=0
           else
              cohort=i
        else
          if(i==1)
            if(length(data$begin.time)==1)
               cohort=data$begin.time
            else
               cohort=data$begin.time[j]
         else
            if(length(data$begin.time)==1)
               cohort=data$begin.time+sum(time.intervals[1:(i-1)])
            else
               cohort=data$begin.time[j]+sum(time.intervals[1:(i-1)])
#
#     Define time variable
#
       if(secondary)
          if(is.na(num))
             times=0
          else
             if(type=="Triang")
                times=(begin+i):(data$nocc.secondary[l]+num)
             else
                times=(begin+1):(begin+ncol)
       else
          if(begin==0)
             if(i==num)
                times=cohort
             else
                times=c(cohort,cohort+cumsum(time.intervals[i:(num-1)]))
          else
             times=cohort+cumsum(time.intervals[i:num])
#
#      Create design data as needed for the parameter
#
       if(type=="Triang")
       {
          if(pim.type=="all")
          {
             add.design.data=cbind(rep(j,ncol),rep(cohort,ncol),ages,times,(i-1)+1:ncol,rep(i,ncol))
             dd.names=c("group","cohort","age","time","occ","occ.cohort")
          }
          else
            if(pim.type=="time")
            {
                add.design.data=cbind(rep(j,ncol),times)
                dd.names=c("group","time")
            }
            else
            {
                add.design.data=matrix(rep(j,ncol),nrow=1)
                dd.names=c("group")
            }
       }
       else
       {
          add.design.data=cbind(rep(j,ncol),ages,times)
          dd.names=c("group","age","time")
       }
       if(!is.null(mix) && mix)
       {
          add.design.data=cbind(add.design.data,rep(k,ncol))
          dd.names=c(dd.names,"mixture")
       }
       if(nstrata>1)
           if(tostrata)
           {
              add.design.data=cbind(add.design.data,rep(jj,ncol),rep(to.strata,ncol))
              dd.names=c(dd.names,"stratum","tostratum")
           }
           else
           {
              add.design.data=cbind(add.design.data,rep(jj,ncol))
              dd.names=c(dd.names,"stratum")
           }
      if(secondary)
      {
          add.design.data=cbind(add.design.data,rep(l,ncol))
          dd.names=c(dd.names,"session")
      }
#
#     Add rows to existing design data
#
      design.data=rbind(design.data,add.design.data)
#
#     If trianular pim type, decrement number of cols
#
       if(type=="Triang")
          ncol=ncol-1
     }
  }
  }
   design.data=as.data.frame(design.data,row.names=NULL)
   names(design.data)=dd.names
#
#  Add Cohort, Age and Time variables
#
   if(!is.null(design.data$cohort))design.data$Cohort=design.data$cohort- min(design.data$cohort)
   if(!is.null(design.data$age))design.data$Age=design.data$age
   if(!is.null(design.data$time))
     if(common.zero)
        design.data$Time=design.data$time- data$begin.time     
     else
        design.data$Time=design.data$time- min(design.data$time)
   if(nstrata>1)
      design.data$stratum=as.factor(strata.labels[design.data$stratum])
      if(!is.null(design.data$tostratum))
        design.data$tostratum=as.factor(strata.labels[design.data$tostratum])
#
#  Next add grouping variables
#
   if(!is.null(data$group.covariates))
   {
      ix=grep("age",names(data$group.covariates))
      cnames=names(data$group.covariates)
      if(length(ix)!=0)
         if(names(data$group.covariates)[ix]=="age")
         {
            cnames[ix]="initial.age.class"
            names(data$group.covariates)=cnames
         }
      gc=data.frame(data$group.covariates[design.data$group,]) 
      names(gc)=cnames 
      row.names(gc)=NULL
      design.data=cbind(design.data,gc)
   }
#
#  Finally if there are stratum variables, add dummy variables for each
#
   if(!is.null(design.data$stratum))
      for (label in strata.labels)
      {
         design.data[label]=0
         design.data[design.data$stratum==label,label]=1
      }
   if(!is.null(design.data$tostratum))
      for (label in strata.labels)
      {
         design.data[paste("to",label,sep="")]=0
         design.data[design.data$tostratum==label,paste("to",label,sep="")]=1
      }
#  Remove occ field unless this is a Multistrata model
   if(data$model!="Multistrata")design.data$occ=NULL
   return(design.data)
}
