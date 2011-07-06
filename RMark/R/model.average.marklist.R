"model.average.marklist"<- function(x,parameter=NULL,data=NULL,vcv=FALSE,drop=TRUE,indices=NULL,revised=TRUE,...)
{
# Computes model averaged real parameter estimates and their std errors for the values specified by
# parameter (eg "Psi", "p" etc) using the models contained in model.list.  If parameter=NULL,
# then all real parameters are model averaged. If vcv=TRUE, it also computes the v-c matrix of the
# model averaged real parameters and the confidence intervals for the real parameters.
#
# Arguments:
#
#   x (model.list) - a list of class "marklist" created by collect.models that contains
#                MARK models that have been run and a model.table which summarizes AIC,
#                weight etc for tne models
#
#   parameter  - a character string indicating which set of parameters should be averaged (eg "Psi", "S")
#                if NULL then all real parameters are averaged.
#
#   data       - dataframe with covariate values that are averaged for estimates
#
#   vcv        - a logical indicating whether the v-c matrix and conf intervals for the
#                real parameters should be computed
#
#   drop       - if TRUE, models with any non-positive variance for betas are dropped
#
#   indices    - a vector of parameter indices from the all-different PIM forumlation
#                of the parameter estimates that should be presented.  This argument only
#                works if parameter argument = NULL.  The primary purpose of the argument
#                is to trim the list of parameters in computing a vcv matrix of the real
#                parameters which can get too big to be computed with the available memory.
#    revised - it TRUE uses eq 6.12 in B&A and if FALSE it uses eq 4.9
#
# Value:
#    if vcv=FALSE, it returns a dataframe of the estimates, se and related data
#    if vcv=TRUE, it returns a list containing the dataframe above with confidence intervals added
#               and a v-c matrix
#
# Check validity of arguments and set up some variables
#
model.list=x
model=load.model(model.list[[1]])
if(class(model.list)!="marklist")
  stop("\nArgument for model.average must be a marklist created by collect.models\n")
if(is.null(model.list$model.table))
   stop("\nmarklist created by collect.models must contain a model.table to use model.average\n")
if(!is.null(parameter))
{
   if(!is.null(indices))
      cat("\nNote: indices value has been ignored because parameter was set\n")
   if(!parameter%in%names(model$parameters))
      stop(paste("\n",parameter,"is not a valid parameter for these results\n"))
   parameters=setup.parameters(model$model)
   parameter.names=names(parameters)
   type=parameters[[match(parameter,parameter.names)]]$type
   begin.index=model$pims[[parameter]][[1]]$pim[1,1]
}
else
{
   begin.index=model$pims[[1]][[1]]$pim[1,1]
}
model.table=model.list$model.table
reals=vector("list",length=dim(model.table)[1])
#
# Determine if any of the models should be dropped because beta.var non-positive
#
dropped.models=NULL
for (i in 1:dim(model.table)[1])
{
  model=load.model(model.list[[i]])
  if(is.null(model$output) || is.null(model$results))
  {
     cat("\nModel ",i,"is missing results and was excluded from model averaging\n")
     dropped.models=c(dropped.models,i)
  }
}
if(drop)
{
   for (i in 1:dim(model.table)[1])
   {
      model=load.model(model.list[[i]])
      if(!is.null(parameter))
      {
         indices=NULL
         for (j in 1:length(model$pims[[parameter]]))
           indices=c(indices,as.vector(t(model$pims[[parameter]][[j]]$pim)))
         indices=indices[indices>0]
      }
      model.indices=unique(model$simplify$pim.translation[indices])
      used.beta=which(apply(model$design.matrix[model.indices,,drop=FALSE],2,function(x)!all(x=="0")))
      if(any(diag(model$results$beta.vcv[used.beta,used.beta])<0))
#      if(any(diag(model$results$beta.vcv)<0))
      {
         dropped.models=c(dropped.models,i)
         cat("\nModel ",i,"dropped from the model averaging because one or more beta variances are not positive\n")
      }
   }
#
# If any models have been dropped, recompute the weights for the model table
#
   if(!is.null(dropped.models))
   {
      model.table$weight[as.numeric(row.names(model.table))%in%dropped.models]=0
      model.table$weight=model.table$weight/sum(model.table$weight)
   }
}
#
#  Loop over each model and compute model averaged estimates and
#  model averaged correlation matrix
#
firstmodel=TRUE
for (i in 1:dim(model.table)[1])
{
   if(i %in% dropped.models) next
   model=load.model(model.list[[i]])
   if(is.null(parameter))
   {
      if(is.null(data))
      {
         if(!is.null(model$results$covariate.values$Value))
         {
            Covdata=as.data.frame(matrix(model$results$covariate.values$Value,nrow=1))
         } else
         {
            Covdata=NULL
         }
         if(!is.null(Covdata))
         {
            names(Covdata)=model$covariates
            zlist=compute.real(model,data=Covdata,se=TRUE,vcv=vcv)
         }
         else
            zlist=compute.real(model,design=model$design.matrix,se=TRUE,vcv=vcv)
      }
      else
         zlist=compute.real(model,data=data,se=TRUE,vcv=vcv)
      if(vcv)
      {
         z=list(estimates=data.frame(estimate=zlist$real,se=zlist$se.real,lcl=zlist$lcl,ucl=zlist$ucl,fixed=zlist$fixed),
                     vcv.real=zlist$vcv.real)
         z$estimates$par.index=1:dim(z$vcv.real)[1]
         row.names(z$vcv.real)=z$estimates$par.index
         colnames(z$vcv.real)=z$estimates$par.index
         if(!is.null(model$simplify))
         {
           z$estimates=z$estimates[model$simplify$pim.translation,]
           rownames(z$estimates)=model$simplify$real.labels
         }
      } else
      {
         z=zlist
         if(!is.null(model$simplify))
         {
           z=z[model$simplify$pim.translation,]
           rownames(z)=model$simplify$real.labels
         }
      }
      if(!is.null(indices))
      {
         if(vcv)
            z$estimates=z$estimates[indices,]
         else
            z=z[indices,]
      }
   }
   else
   {
      if(is.null(data))
      {
         if(!is.null(model$results$covariate.values$Value))
         {
            Covdata=as.data.frame(matrix(model$results$covariate.values$Value,nrow=1))
         } else
         {
            Covdata=NULL
         }
         if(!is.null(Covdata))
         {
            names(Covdata)=model$covariates
            z=get.real(model,parameter,data=Covdata,se=TRUE,vcv=vcv)
         }
         else
            z=get.real(model,parameter,design=model$design.matrix,se=TRUE,vcv=vcv)
      }
      else
         z=get.real(model,parameter,data=data,se=TRUE,vcv=vcv)
   }
   if(vcv)
      reals[[i]]=z$estimates
   else
      reals[[i]]=z
   if(firstmodel)
   {
      firstmodel=FALSE
      nreals=dim(reals[[i]])[1]
      estimates.average=rep(0,nreals)
      if(vcv) cor.average=matrix(0,nrow=nreals,ncol=nreals)
   }
   else
      if(dim(reals[[i]])[1]!=length(estimates.average))
         stop("\nCannot model average models with different structures\n")
   estimates.average=estimates.average+reals[[i]]$estimate*model.table$weight[as.numeric(row.names(model.table))==i]
   if(vcv)
   {
      rn=as.numeric(row.names(z$vcv.real))
      expanded.vcv=matrix(NA,nrow=max(rn),ncol=max(rn))
      zz=matrix(1,nrow=length(rn),ncol=length(rn))*rn
      expanded.vcv[cbind(as.vector(zz),as.vector(t(zz)))]=z$vcv.real
      xx=matrix(1,nrow=length(z$estimates$par.index),ncol=length(z$estimates$par.index))*z$estimates$par.index
      full.vcv=matrix(expanded.vcv[cbind(as.vector(xx),as.vector(t(xx)))],nrow=nreals,ncol=nreals)/
             outer(reals[[i]]$se,reals[[i]]$se,"*")
      full.vcv[is.nan(full.vcv)]=0
      full.vcv[is.infinite(full.vcv)]=0
      diag(full.vcv)=1
      cor.average=cor.average+full.vcv*model.table$weight[as.numeric(row.names(model.table))==i]
   }
}
#
#  Next compute model averaged se and create dataframe of estimates
#
se.average=rep(0,nreals)
for (i in 1:dim(model.table)[1])
{
   if(i %in% dropped.models) next
   if(revised)
      se.average=se.average+model.table$weight[as.numeric(row.names(model.table))==i]*
              (reals[[i]]$se^2 + (reals[[i]]$estimate-estimates.average)^2)  
   else
      se.average=se.average+model.table$weight[as.numeric(row.names(model.table))==i]*
              sqrt(reals[[i]]$se^2 + (reals[[i]]$estimate-estimates.average)^2)
}
if(revised) se.average=sqrt(se.average)
first.index=((1:dim(model.table)[1])[!(1:dim(model.table)[1])%in%dropped.models])[1]
if(!is.null(parameter))
   other.values=summary(model.list[[first.index]],se=TRUE)$reals[[parameter]]
else
   other.values=summary(model.list[[first.index]],se=TRUE)$reals

if(!vcv)
{
   if(is.null(parameter))
      if(is.null(indices))
         result=cbind(data.frame(par.index=begin.index:(begin.index+nreals-1),estimate=estimates.average,se=se.average))
      else
         result=cbind(data.frame(par.index=indices,estimate=estimates.average,se=se.average))
   else
      result=cbind(data.frame(par.index=begin.index:(begin.index+nreals-1),estimate=estimates.average,se=se.average),other.values[,(7:dim(other.values)[2])])
   return(result)
}
#
# If vcv requested then compute model averaged vc from model averaged correlations
# and model averaged conf intervals for the parameters
#
else
{
   if(is.null(parameter)&!is.null(indices))
      result=cbind(data.frame(par.index=indices,estimate=estimates.average,se=se.average))
   else
      result=cbind(data.frame(par.index=begin.index:(begin.index+nreals-1),estimate=estimates.average,se=se.average))
   vcv.real=cor.average*outer(se.average,se.average,"*")
   vcv.real[is.nan(vcv.real)]=0
   vcv.real[is.infinite(abs(vcv.real))]=0  
   row.names(vcv.real)=result$par.index
   colnames(vcv.real)=row.names(vcv.real)
   link.list=compute.links.from.reals(result$estimate,model.list[[1]],parm.indices=result$par.index,vcv.real=vcv.real,use.mlogits=FALSE)
   result$lcl=link.list$estimates-1.96*sqrt(diag(link.list$vcv))
   result$ucl=link.list$estimates+1.96*sqrt(diag(link.list$vcv))
   result$lcl=apply(data.frame(x=result$lcl,links=link.list$links),1,function(x){inverse.link(as.numeric(x[1]),x[2])})
   result$ucl=apply(data.frame(x=result$ucl,links=link.list$links),1,function(x){inverse.link(as.numeric(x[1]),x[2])})
   result$lcl[is.na(result$lcl)]=result$estimate[is.na(result$lcl)]
   result$ucl[is.na(result$ucl)]=result$estimate[is.na(result$ucl)]
   if(!is.null(parameter)) result=cbind(result,other.values[,(7:dim(other.values)[2])])
   return(list(estimates=result,vcv.real=vcv.real))
}
}







