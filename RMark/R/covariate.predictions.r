"covariate.predictions" <- function(model,data=NULL,indices=NULL,drop=TRUE)
{
# ------------------------------------------------------------------------------------------------
#
# covariate.predictions
#            -   computes real estimates and var-cov for a set of
#                covariate values
#
# Arguments:
#
#   model          - MARK model object or marklist
#   data           - dataframe with covariate values used for estimates
#                     if it contains a field index the covariates in each row
#                       are only applied to that index and the argument indices is not needed
#   indices        - all-different PIM indices for parameters to be calculated
#   drop           - if TRUE, models with any non-positive variance for betas are dropped
#
#
# Value (list):
#
#   estimates        - data frame containing estimates and se
#   vcv              - variance-covariance matrix of real estimates
#
# ------------------------------------------------------------------------------------------------
if(class(model)[1]!="marklist")
{
  number.of.models=1
  model=load.model(model)
  model.list=list(model)
}
else
{
  number.of.models=length(model)-1
  if(is.null(model$model.table))
     stop("\nmarklist created by collect.models must contain a model.table to use model.average\n")
  model.list=model
  model.table=model$model.table
}
   if(is.null(data))
      stop("\n data argument must be specified\n")
   if(!is.data.frame(data))
      stop("\n data argument must be a dataframe. Do not use processed data list.\n")
#
#   If there is an index field in data, then only use that row of data for that index
#   That filtering is done below after the complete design matrix is created for each model.
#
   if(!is.null(data$index))
   {
      index=data$index
      indices=index
      replicate.values=FALSE
   }
   else
      replicate.values=TRUE
   if(is.null(indices))
     stop("\nValue for indices argument must be given or index field must be included in data argument\n")
#
# Determine if any of the models should be dropped because beta.var non-positive
#
if(number.of.models>1)
{
dropped.models=NULL
if(drop)
{
   for (i in 1:dim(model.table)[1])
   {
      model=load.model(model.list[[i]])
      model.indices=unique(model$simplify$pim.translation[indices])
      used.beta=which(apply(model$design.matrix[model.indices,,drop=FALSE],2,function(x)!all(x=="0")))
      if(any(diag(model$results$beta.vcv[used.beta,used.beta])<0))
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
}
else
  dropped.models=NULL
reals=vector("list",length=number.of.models)
firstmodel=TRUE
for (j in 1:number.of.models)
{
   if(j %in% dropped.models) next
#
#   Assign value of chat - overdispersion constant
#
   model=load.model(model.list[[j]])
   if(is.null(model$chat))
     chat=1
   else
     chat=model$chat
   beta=model$results$beta$estimate
   model.indices=unique(model$simplify$pim.translation[indices])
   links=NULL
   design=NULL
   fixedparms=NULL
   boundaryparms=NULL
#   
#  If there are no data values other than the index, the code below will extract only those
#  rows in the DM.  
#
   if(ncol(data)==1 && names(data)=="index")
   {
     for (i in 1:nrow(data))
     {
        model.index=model$simplify$pim.translation[index[i]]   
        design=rbind(design,model$design.matrix[model.index,,drop=FALSE])
        if(length(model$links)==1)
           links=c(links,model$links)
        else
           links=c(links,model$links[model.index])
        if(!is.null(model$fixed))
        {
           if(is.null(model$simplify))
              xfixedparms=(1:dim(model$design.matrix)[1])%in%model$fixed$index
           else                                                           
              xfixedparms=(1:dim(model$design.matrix)[1])%in%model$simplify$pim.translation[model$fixed$index]
        }
        else
           xfixedparms=rep(FALSE,dim(model$design.matrix)[1])
        fixedparms=c(fixedparms,xfixedparms[model.index])
        boundaryparms=c(boundaryparms,(model$results$real$se==0 & !xfixedparms)[model.index])
     }
   }
   else
# If there are data values for covariates then it fills in a complete DM for each record in the data
# and appends to create a large DM with all individual DMs stacked on each other.  Then in the 
# next loop if an index is specified in data, it selects out only the rows of the large DM for those 
# specific parameter indices.
#
   {
     for (i in 1:nrow(data))
     {
        xdesign=fill.covariates(model,find.covariates(model,data[i,,drop=FALSE]))[model.indices,,drop=FALSE]
        if(length(model$links)==1)
           links=c(links,rep(model$links,dim(xdesign)[1]))
        else
           links=c(links,model$links[model.indices])
        design=rbind(design,xdesign)
        if(!is.null(model$fixed))
        {
           if(is.null(model$simplify))
              xfixedparms=(1:dim(model$design.matrix)[1])%in%model$fixed$index
           else                                                           
              xfixedparms=(1:dim(model$design.matrix)[1])%in%model$simplify$pim.translation[model$fixed$index]
        }
        else
           xfixedparms=rep(FALSE,dim(model$design.matrix)[1])
        fixedparms=c(fixedparms,xfixedparms[model.indices])
        boundaryparms=c(boundaryparms,(model$results$real$se==0 & !xfixedparms)[model.indices])
     }
#
#    Filter rows if each covariate set is not replicated for each parameter
#
     if(!replicate.values)
     {
        row.numbers=NULL
        for(i in 1:length(index))
        {
          model.index=model$simplify$pim.translation[index[i]]
          row.numbers=c(row.numbers,match(model.index,model.indices)+(i-1)*length(model.indices))
        }
        design=design[row.numbers,,drop=FALSE]
        fixedparms=fixedparms[row.numbers]
        boundaryparms=boundaryparms[row.numbers]
        links=links[row.numbers]
     }
   }
#                                                                         
#   The following shouldn't happen unless the model and design matrices are mixed between models
#
   if(dim(design)[2]!=length(beta))
       stop("Mismatch between number of design columns and length of beta")
#
#  Convert to a numeric matrix
#
   design=matrix(as.numeric(design),nrow=dim(design)[1])
# 
#  Create vector of fixed parameters to cope with partial fixing of mlogits
#
   fixedvalues=rep(NA,nrow(design))
   fixedvalues[fixedparms]=model$fixed$value[match(indices[fixedparms],model$fixed$index)]
#
#  Compute real parameters; if neither se or vcv then return vector of real parameters
#
   real=convert.link.to.real(design%*%beta,links=links,fixed=fixedvalues)
#
#  Set fixed real parameters to their fixed values
#
   real[fixedparms]=fixedvalues[fixedparms]
#   Previously read as follows -- fixed in v1.9.5
#   real[fixedparms]=model$fixed$value[model$fixed$index%in%indices[fixedparms]]
#
#  Compute vc matrix for real parameters which needs to be modified for
#  any mlogit parameters
#
   if(length(model$links)==1)
      deriv.real=deriv_inverse.link(real,design,model$links)
   else
      deriv.real=t(apply(data.frame(real=real,x=design,links=links),1,
            function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
   vcv.real=deriv.real%*%model$results$beta.vcv%*%t(deriv.real)
#
#   If vcv=TRUE, compute v-c matrix and std errors of real estimates
#   To handle any mlogit parameters compute pseudo-real estimates using log in place of mlogit
#
   ind=grep("mlogit",links,ignore.case=TRUE)
   templinks=links
   if(length(ind)>0)
   {
      templinks[ind]="log"
      pseudo.real=as.vector(convert.link.to.real(design%*%beta,links=templinks))
      pseudo.real[fixedparms]=fixedvalues[fixedparms]
      pseudo.real[ind][fixedparms[ind]]=exp(pseudo.real[ind][fixedparms[ind]])
#
#     Compute first derivatives of pseudo-real (for any mlogit parameter)
#     estimates with respect to beta parameters
#
      if(length(templinks)==1)
        deriv.pseudo=deriv_inverse.link(pseudo.real,design,templinks)
      else
         deriv.pseudo=t(apply(data.frame(real=pseudo.real,x=design,links=templinks),1,
               function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
      deriv.pseudo[fixedparms,]=0
      vcv.pseudo=chat*deriv.pseudo%*%model$results$beta.vcv%*%t(deriv.pseudo)
#
#     Apply chain rule to get variance of real parameters which has mlogits
#     expressed as zi/(1+z1+...zk) where k is number of mlogit components-1 and
#     non-mlogits are expressed as zi.
#     bottom is either 1 for non-mlogits and the sum for mlogits
#     pbottom is partial with respect to zi
#
      mlogits=outer(links,links,function(x,y)as.numeric(x==y))*as.numeric(substr(links,1,6)=="mlogit" |substr(links,1,6)=="MLogit" )
      pbottom=matrix(0,nrow=dim(vcv.pseudo)[1],ncol=dim(vcv.pseudo)[1]) + mlogits
      bottom=diag(nrow=dim(vcv.pseudo)[1])*(1-as.numeric(substr(links,1,6)=="mlogit" |substr(links,1,6)=="MLogit"))+
          mlogits + pbottom*apply(pbottom*pseudo.real,2,sum)
      deriv.pseudo=(diag(nrow=dim(vcv.pseudo)[1])*bottom-pseudo.real*pbottom)/bottom^2
      deriv.pseudo[is.nan(deriv.pseudo)]=0
      vcv.real=deriv.pseudo%*%vcv.pseudo%*%t(deriv.pseudo)
   }
   else
      vcv.real=chat*vcv.real
#
#  Compute conf interval taking into account use of logit transform for mlogit links
#
   link.se=suppressWarnings(sqrt(chat*diag(design%*%model$results$beta.vcv%*%t(design))))
   link.se[is.na(link.se)]=0
   ind=unique(c(grep("mlogit",links,ignore.case=TRUE),which(links%in%c("sin","Sin","LogLog","loglog","CLogLog","cloglog"))))
   linkse=suppressWarnings(sqrt(diag(vcv.real)[ind])/(real[ind]*(1-real[ind])))
   linkse[is.na(linkse)]=0
   linkse[is.infinite(linkse)]=0
   link.se[ind]=linkse
   link.values=design%*%beta
   link.values[ind]=suppressWarnings(log(real[ind]/(1-real[ind])))
   link.values[ind][abs(real[ind]-1)<1e-7]=100
   link.values[ind][abs(real[ind]-0)<1e-7]=-100
   links[ind]="logit"
   real.lcl=convert.link.to.real(link.values-1.96*link.se,links=links)
   real.ucl=convert.link.to.real(link.values+1.96*link.se,links=links)
   real.lcl[fixedparms]=real[fixedparms]
   real.ucl[fixedparms]=real[fixedparms]   
#
#  Set v-c values of fixed parameters to 0
#
   vcv.real[fixedparms,]=0
   vcv.real[,fixedparms]=0
   se.real=suppressWarnings(sqrt(diag(vcv.real)))
   se.real[is.na(se.real)]=0
   fixed=rep("",dim(design)[1])
   fixed[fixedparms]="Fixed"
   fixed[boundaryparms]="Boundary"
#
#  Now expand unique parameters to a dataframe with indices
#
   if(replicate.values)
   {
      estimates=NULL
      for (i in 1:dim(data)[1])
      {
         model.indices=model$simplify$pim.translation[indices]
         lookup.indices=match(model.indices,c(rep(0,(i-1)*length(unique(model.indices))),unique(model.indices)))
         covdata=data[rep(i,length(lookup.indices)),]
         xdata=data.frame(vcv.index=lookup.indices,model.index=model.indices,par.index=indices,covdata,estimate=real[lookup.indices],se=se.real[lookup.indices],
               lcl=real.lcl[lookup.indices],ucl=real.ucl[lookup.indices],fixed=fixed[lookup.indices])
         estimates=rbind(estimates,xdata)
      }
      row.names(estimates)=1:dim(estimates)[1]
#
#     Expand v-c matrix
#
      nreals=dim(estimates)[1]
      zz=matrix(1,nrow=nreals,ncol=nreals)*estimates$vcv.index
      vcv.real=matrix(vcv.real[cbind(as.vector(zz),as.vector(t(zz)))],nrow=nreals,ncol=nreals)
   }
   else
   {
      model.indices=model$simplify$pim.translation[indices]
      estimates=data.frame(vcv.index=model.indices,model.index=model.indices,par.index=indices,data,estimate=real,se=se.real,
            lcl=real.lcl,ucl=real.ucl,fixed=fixed)
   }
   
   reals[[j]]=subset(estimates,select=c("estimate","se"))
#
#  If this is a single model return results
#
   if(number.of.models==1)
   {
      return(list(estimates=estimates,vcv=vcv.real))
   }
   else
#
#  Otherwise if this is the first model in the list setup dataframe for
#  average estimates and average correlation matrix.
#
   if(firstmodel)
   {
      firstmodel=FALSE
      nreals=dim(estimates)[1]
      estimates.average=estimates
      estimates.average$estimate=0
      cor.average=matrix(0,nrow=nreals,ncol=nreals)
   }
#
#  For each model the estimates are averaged and so is the correlation matrix
#
   estimates.average$estimate=estimates.average$estimate+
         estimates$estimate*model.table$weight[as.numeric(row.names(model.table))==j]
   cor.average=cor.average+vcv.real*model.table$weight[as.numeric(row.names(model.table))==j]/
            outer(estimates$se,estimates$se,"*")
}
#
#  After processing each model, the model averaged se and v-c matrix is
#  computed.
#
se.average=rep(0,nreals)
for (i in 1:dim(model.table)[1])
{
   if(i %in% dropped.models) next
   se.average=se.average+model.table$weight[as.numeric(row.names(model.table))==i]*
              sqrt(reals[[i]]$se^2 + (reals[[i]]$estimate-estimates.average$estimate)^2)
}
estimates.average$se=se.average
vcv.real=cor.average*outer(se.average,se.average,"*")
vcv.real[is.nan(vcv.real)]=0
vcv.real[is.infinite(abs(vcv.real))]=0  
link.list=compute.links.from.reals(estimates.average$estimate,model.list[[1]],parm.indices=estimates.average$par.index,vcv.real=vcv.real,use.mlogits=FALSE)
estimates.average$lcl=link.list$estimates-1.96*sqrt(diag(link.list$vcv))
estimates.average$ucl=link.list$estimates+1.96*sqrt(diag(link.list$vcv))
estimates.average$lcl=apply(data.frame(x=estimates.average$lcl,links=link.list$links),1,function(x){inverse.link(as.numeric(x[1]),x[2])})
estimates.average$ucl=apply(data.frame(x=estimates.average$ucl,links=link.list$links),1,function(x){inverse.link(as.numeric(x[1]),x[2])})
estimates.average$lcl[is.na(estimates.average$lcl)]=estimates.average$estimate[is.na(estimates.average$lcl)]
estimates.average$ucl[is.na(estimates.average$ucl)]=estimates.average$estimate[is.na(estimates.average$ucl)]
return(list(estimates=estimates.average,vcv=vcv.real))
}
