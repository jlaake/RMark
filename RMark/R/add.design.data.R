"add.design.data" <-
function(data,ddl,parameter,type="age",bins=NULL,name=NULL,replace=FALSE,right=TRUE)
# -------------------------------------------------------------------------------------------------------------
#
# add.design.data  - enables design data fields to be added to the design.data(ddl)
#                    the added fields must be a function of cohort, age or time.
#                    Any other fields (eg effort value for time) can be added with R
#                    commands, but this may be changed later. 
#
# Arguments:
#
# data             - data list resulting from process.data 
# ddl              - current design dataframe 
# parameter        - parameter name
# type             - either "age", "time" or "cohort"
# bins             - bins for grouping 
# name             - variable name
# replace	   - if TRUE, replace field with same name
#
# Value:  
#
#  ddl - modified design data 
#
#
# Functions used: compute.design.data, setup.parameters, valid.parameters
#
# -------------------------------------------------------------------------------------------------------------
{
#
# Check validity of parameter list, if any given
#
  if(!valid.parameters(data$model,parameter))stop()
#
# Make sure that a name has been given for added variable
#
  if(is.null(name)) stop("A name is required for an added design variable")
#
#  Setup variables in parameter list
#
  parameters=setup.parameters(data$model)
  model.list=setup.model(data$model,data$nocc,data$mixtures)
#
#  Assign user defined pim.types
#
  for(pname in names(ddl$pimtypes))
     parameters[[pname]]$pim.type=ddl$pimtypes[[pname]]$pim.type
#
#  Compute design data for the parameter
#
  if(!model.list$robust) parameters[[parameter]]$secondary=FALSE
#
# Compute design data for this parameter
#
  if(data$mixtures==1)
  {
     parameters$mix=FALSE
     parameters[[parameter]]$rows=1
  }
  if(!is.null(parameters[[parameter]]$bystratum) && parameters[[parameter]]$bystratum)
  {
     strata.labels=data$strata.labels
     nstrata=data$nstrata
     if(!is.null(parameters[[parameter]]$tostrata) && parameters[[parameter]]$tostrata)
        tostrata=TRUE
     else
        tostrata=FALSE
  }
  else
  {
     strata.labels=NULL
     nstrata=1
     tostrata=FALSE
  }
#
# Compute design data
#
  design.data=compute.design.data(data,parameters[[parameter]]$begin,parameters[[parameter]]$num,
                   parameters[[parameter]]$type,parameters[[parameter]]$mix,parameters[[parameter]]$rows,
                   parameters[[parameter]]$pim.type,parameters[[parameter]]$secondary, nstrata,
                   tostrata,strata.labels)
#
#  Limit design data to rows in ddl to handle elimination of design data
#
   design.data=design.data[row.names(design.data)%in%row.names(ddl[[parameter]]),]
#
# Add variable depending on type
#

  if(type=="cohort")    
  {
     if(is.null(bins))
        new.data=as.factor(design.data$cohort)
     else
        new.data=cut(design.data$cohort,bins,include.lowest=TRUE,right=right)
  } else
  if(type=="age")
  {
     if(is.null(bins))
        new.data=as.factor(design.data$age)
     else
        new.data=cut(design.data$age,bins,include.lowest=TRUE,right=right)
  } else
  if(type=="time")
  {
     if(is.null(bins)) 
        new.data=as.factor(design.data$time)
     else
        new.data=cut(design.data$time,bins,include.lowest=TRUE,right=right)
  } else stop("invalid type")
  vnames=names(ddl[[parameter]])
  if(name %in% vnames)
  {
    if(replace)
       ddl[[parameter]][,name]=new.data 
    else
       stop(paste("Variable ",name," already in design data. Use replace=TRUE if you want to replace current values"))
  }
  else
  {
     ddl[[parameter]]<-cbind(ddl[[parameter]],new.data)
     names(ddl[[parameter]])<- c(vnames,name)
  }
return(ddl)
}
