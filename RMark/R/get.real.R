"get.real" <-
function(model,parameter,beta=NULL,se=FALSE,design=NULL,data=NULL,vcv=FALSE,show.fixed=TRUE)
{
# ----------------------------------------------------------------------------------------
#
# get.real - extracts real parameters for a particular type of parameter (parameter) and
#            returns in either a table format or in PIM format (se=FALSE)
#
# Arguments:
#
#  model        - MARK model object
#  parameter    - type of parameter in model (character)
#  beta         - estimates of beta parameters for computation of real parameters
#  se           - if TRUE uses table format, if FALSE uses PIM format
#  design       - a numeric design matrix with any covariate values filled in with numerical values
#  data         - covariate data to be averaged for estimates if design=NULL
#  vcv          - if TRUE computes and returns the v-c matrix of the subset of the real parameters
#  show.fixed   - if TRUE fixed values are returned rather than NA in place of fixed values
#
# Value:
#  estimates    - a list of tables of estimates (se=TRUE) or a matrices of estimates (se=FALSE)
#                 if more than one group. If only one group the result is a single data.frame(table) or a
#                 matrix.
#
# Functions used: valid.parameters, Put.in.PIM.format (defined internally below), compute.real,
#                 setup.parameters
# -----------------------------------------------------------------------------------------------
#
# Internal function to create PIM format
#
  Put.in.PIM.format=function(real,pim,design.data)
  {
    wtable=matrix(NA,nrow=dim(pim)[1],ncol=dim(pim)[2])
    for(i in 1:dim(pim)[1])
    {
      wtable[i,(i:(dim(pim)[2]))]=real[pim[i,(i:(dim(pim)[2]))]]
    }
    colnames(wtable)=design.data$time[1:dim(pim)[1]]
    if(!is.null(design.data$cohort))
       rownames(wtable)=design.data$cohort[(diag(pim)-pim[1,1]+1)]
    wtable
  }
#
# First check to make sure model has been run
#
  model=load.model(model)
  if(is.null(model$results)) 
  {
     cat("Model output is not available\n")
     invisible()
  }
#
# Next make sure that requested parameter is appropriate
#
  if(!valid.parameters(model$model,parameter))stop()
#
# Compute real parameters that were estimated rowsum (of design matrix) value >0
#
    rowsums=apply(model$design.matrix,1,
      function(x){if(all(x=="0"))return(0) else return(1)})
    rowsums=as.numeric(rowsums>0)
    if(!is.null(model$simplify))rowsums= rowsums[model$simplify$pim.translation]
#
#  Check to see if there are any covariates used in the model.   If there
#  are covariates used and no design is specified but data is given, then create
#  the design matrix from data
#
  if("covariates" %in% names(model) && !is.null(model$covariates))
    if(is.null(design)& !is.null(data))
       design=fill.covariates(model,find.covariates(model,data=data))
#
#If vcv=TRUE set se=TRUE
#
  if(vcv)se=TRUE
#
#If se=TRUE, warn user if beta.vcv has negative variances
#
  if(se)
    if(any(diag(model$results$beta.vcv)<0))
       warning("\nImproper V-C matrix for beta estimates. Some variances non-positive.\n")
#
# If design is specified, then compute the real estimates with this design matrix
#
  if(!is.null(design))
  {
     if(!se)
     {
       real=compute.real(model,design=design,beta=beta,se=FALSE)$real
       if(!is.null(model$simplify))
           real=real[model$simplify$pim.translation]
     }
     else
     {
       if(!vcv)
          real=compute.real(model,design=design,beta=beta,vcv=FALSE)
       else
       {
          real.list=compute.real(model,design=design,beta=beta,vcv=TRUE)
          real=data.frame(estimate=real.list$real,se=real.list$se.real,
                          lcl=real.list$lcl,ucl=real.list$ucl)
       }
       if(!is.null(model$simplify))
       {
          real=real[model$simplify$pim.translation,]
          rownames(real)=model$simplify$real.labels
       }
     }
  }
#
# If design and data are not specified, extract estimates (se=FALSE) or
# estimate table (includes se and ci); if model$chat is defined and not 1
# then recompute the confidence intervals for the real parameters with the
# se for beta inflated by sqrt(model$chat). Also if vcv=TRUE recompute to get
# vcv for reals.
#
  else
  {
     if(!se & is.null(beta))
       if(!is.null(model$simplify))
           real=model$results$real$estimate[model$simplify$pim.translation]
       else
           real = model$results$real$estimate
     else
     {
        if(vcv || !is.null(beta) || (!is.null(model$chat) && model$chat !=1))
        {
           if(is.null(model$results$covariate.values$Value))
              data=NULL
          else
              data = as.data.frame(t(model$results$covariate.values$Value))
           if (!is.null(data) && dim(data)[1] != 0)
           {
              names(data)=model$covariates
              if(!vcv)
                 real=compute.real(model,data=data,beta=beta,vcv=FALSE)
              else
              {
                  real.list=compute.real(model,data=data,beta=beta,vcv=TRUE)
                  real=data.frame(estimate=real.list$real,se=real.list$se.real,
                          lcl=real.list$lcl,ucl=real.list$ucl)
              }
           }
           else
           {
              if(!vcv)
                 real=compute.real(model,design=model$design.matrix,beta=beta,vcv=FALSE)
              else
              {
                  real.list=compute.real(model,design=model$design.matrix,beta=beta,vcv=TRUE)
                   real=data.frame(estimate=real.list$real,se=real.list$se.real,
                          lcl=real.list$lcl,ucl=real.list$ucl)
              }
           }
        }
        else
           real=model$results$real
        if(!is.null(model$simplify))
        {
           real=real[model$simplify$pim.translation,]
           rownames(real)=model$simplify$real.labels
        }
     }
  }
#
#  Set non-estimated reals to NA or fixed value depending on show.fixed
#
  if(any(rowsums==0))
  if(is.vector(real))
     if(show.fixed)
        real[rowsums==0]=model$fixed$value[!duplicated(model$fixed$index)][unique(model$fixed$index)%in%which(rowsums==0)]
     else
        real[rowsums==0]=NA

  else
     if(is.data.frame(real))
     {
       if(show.fixed)
       {
          real$estimate[rowsums==0]=model$fixed$value[!duplicated(model$fixed$index)][unique(model$fixed$index)%in%which(rowsums==0)]
          real$se[rowsums==0]=0
       }
       else
       {
          real$estimate[rowsums==0]=NA
          real$se[rowsums==0]=NA
       }
       real$lcl[rowsums==0]=real$estimate[rowsums==0]
       real$ucl[rowsums==0]=real$estimate[rowsums==0]
     }
#
# Lookup type of PIM structure
#
  parameters=setup.parameters(model$model)
  parameter.names=names(parameters)
  type=parameters[[match(parameter,parameter.names)]]$type
  ng=length(model$pims[[parameter]])
  if( type =="Triang" | !is.null(model$mixtures)| !is.null(model$nocc.secondary))
      estimates=list()
  else
      estimates=NULL
  parameter.labels=names(model$pims[[parameter]][[1]])
  parameter.labels=parameter.labels[!parameter.labels%in%"pim"]
  if(length(model$strata.labels)==1) parameter.labels=parameter.labels[!parameter.labels%in%"stratum"]
  if(model$number.of.groups==1) parameter.labels=parameter.labels[!parameter.labels%in%"group"]
  output.labels=vector(length=ng)
  for(j in 1:ng)
  {
    if(length(parameter.labels)!=0)
    {
        output.labels[j]=""
        for(k in 1:length(parameter.labels))
        {
           if(parameter.labels[k]=="group")
              output.labels[j]=paste(output.labels[j],"Group:",model$group.labels[model$pims[[parameter]][[j]]$group],sep="")
           else
              if(parameter.labels[k]=="stratum")
                 output.labels[j]=paste(output.labels[j]," Stratum:",model$strata.labels[model$pims[[parameter]][[j]]$stratum],sep="")
              else
                 if(parameter.labels[k]=="tostratum")
                    output.labels[j]=paste(output.labels[j]," To:",model$strata.labels[model$pims[[parameter]][[j]]$tostratum],sep="")
              else
                 if(parameter.labels[k]=="session")
                    output.labels[j]=paste(output.labels[j]," Session:",model$pims[[parameter]][[j]]$session,sep="")

       }
    }
    else
        output.labels[j]=""
    if(!se & is.null(beta))
    {
        if(type=="Triang")
        
        {
           estimates[[j]]=list(pim=Put.in.PIM.format(real,model$pims[[parameter]][[j]]$pim,model$design.data[[parameter]]))
           if(length(parameter.labels)!=0)
              estimates[[j]]=c(estimates[[j]],model$pims[[parameter]][[j]][parameter.labels])
        }
        else
           if(!is.null(model$mixtures) | !is.null(model$nocc.secondary))
           {
              estimates[[j]]=matrix(real[model$pims[[parameter]][[j]]$pim],nrow=dim(model$pims[[parameter]][[j]]$pim)[1])
              if(!is.matrix(estimates[[j]]))estimates[[j]]=matrix(estimates[[j]])
              colnames(estimates[[j]])=model$design.data[[parameter]]$time[model$pims[[parameter]][[j]]$pim[1,]-min(model$pims[[parameter]][[1]]$pim[1,])+1]
              if(is.null(colnames(estimates[[j]])))colnames(estimates[[j]])=""
              if(dim(model$pims[[parameter]][[j]]$pim)[1]>1)
                  rownames(estimates[[j]])=paste("mixture:",1:model$mixtures,sep="")
              else
                  rownames(estimates[[j]])=""

           }
           else
              estimates=rbind(estimates,real[model$pims[[parameter]][[j]]$pim])
    }
    else
    {
    
        if(type=="Triang"&&model$parameters[[parameter]]$pim.type!="all")
           if(model$parameters[[parameter]]$pim.type=="time")
              indices=model$pims[[parameter]][[j]]$pim[1,]
           else
              indices=model$pims[[parameter]][[j]]$pim[1,1]
        else
           indices=as.vector(t(model$pims[[parameter]][[j]]$pim))
        indices=indices[indices>0]
        if(!is.null(model$simplify))
           par.index=model$simplify$pim.translation[indices]
        else
           par.index=indices
        real.names=names(real)
        estimate.df=cbind(all.index=indices,par.index,real[indices,])
        names(estimate.df)=c("all.diff.index","par.index",real.names)
        estimates=rbind(estimates,estimate.df)
    }
  }
  if(!se& is.null(beta))
  {
     if(type =="Triang" | is.list(estimates))
        names(estimates)=output.labels
     else
     {
         if(length(model$begin.time)==1)
            colnames(estimates)=model$design.data[[parameter]]$time[model$pims[[parameter]][[1]]$pim-min(model$pims[[parameter]][[1]]$pim)+1]
         else
            colnames(estimates)=1:dim(estimates)[2]
         rownames(estimates)=output.labels
     }
  } else
     estimates=cbind(estimates,model$design.data[[parameter]])
#
# Return extracted estimates in chosen format
#
if(!vcv)
   return(estimates)
else
{
   if(!is.null(estimates$par.index))
   {
      pindex=sort(unique(estimates$par.index))
      real.list$vcv.real=real.list$vcv.real[pindex,pindex,drop=FALSE]
      row.names(real.list$vcv.real)=pindex
      colnames(real.list$vcv.real)=pindex
   }
   return(list(estimates=estimates,vcv.real=real.list$vcv.real))
}
}


