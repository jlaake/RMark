popan.derived=function(x,model,revised=TRUE,normal=TRUE,N=TRUE,NGross=TRUE,drop=FALSE)
{
################################################################################
#  Arguments:
#    x       - processed dataframe for a POPAN model
#    model   - a single mark POPAN model or a marklist of POPAN models
#    revised - if true use eq 6.12 otherwise 4.9 from B&A
#    normal  - if true use normal otherwise log-normal
#
#   Value:  list with following elements:
#   N        - dataframe of estimates by group and occasion and se, lcl,ucl and group/occasion data
#   N.vcv    - var-cov matrix of N
#   Nbyocc   - dataframe of estimates by occasion (summed across groups) and se, lcl,ucl and occasion data
#   Nbyocc.vcv    - var-cov matrix of Nbyocc
#
################################################################################
#  Define function to compute derived parameters for a single POPAN model
#
   conf.int=function(est,se,normal,lower=TRUE)
   {
      if(normal)
      {
         if(lower) 
           return(est-1.96*se)
         else
           return(est+1.96*se)
      }
      else
      {
        Cmult=exp(1.96*sqrt(log(1+(se/est)^2)))
        if(lower)
          return(est/Cmult)
        else
          return(est*Cmult)
      }
   }        
   compute.popan.derived=function(x,model,revised=TRUE,N=TRUE,NGross=TRUE,drop)
   {
#     Check to make sure it is of type POPAN
      if(class(model)[2]!="POPAN") stop("\nNot a POPAN model\n")
#     Set up indices for Phi, pent, N parameters depending on number of groups
      model=load.model(model)
      ng=model$number.of.groups
      k=model$nocc-1
      index=NULL
      phi.index=NULL
	  time.intervals=NULL
      pent.index=NULL
      N.index=NULL
      N.ests=NULL
      for(i in 1:ng)
      {
        index=c(index,(i-1)*k+1:k,ng*k+ng*(k+1)+(i-1)*k+1:k,2*ng*k+ng*(k+1)+i)
        phi.index=c(phi.index,(i-1)*(2*k+1)+1:k)
		time.intervals=c(time.intervals,x$time.intervals)
        pent.index=c(pent.index,(i-1)*(2*k+1)+k+1:k)
        N.index=c(N.index,(i-1)*(2*k+1)+ 2*k + 1)
      }
#     Compute observed number in each group and compute real parameters and their
#     variance-covariance matrix
	  if(ng==1)
		  Mt1=sum(x$freq)
	  else
		  Mt1=colSums(x$freq)
      dd=covariate.predictions(model,data.frame(index=index),drop=drop)
#     Call popan.Nt to compute abundance estimates for each group-occasion and their
#     var-cov matrix.
      if(N) N.list=popan.Nt(dd$estimates$estimate[phi.index],dd$estimates$estimate[pent.index],
          Mt1+dd$estimates$estimate[N.index],dd$vcv,time.intervals)
#     Call popan.NGross to compute gross abundance estimates for each group and their
#     var-cov matrix.
      if(NGross) NGross.list=popan.NGross(dd$estimates$estimate[phi.index],dd$estimates$estimate[pent.index],
          Mt1+dd$estimates$estimate[N.index],dd$vcv, time.intervals)      
#     Construct dataframe with estimates, se and conf limits (95% normal)
      if(N)
      {
        if(ng==1)
           N.ests=cbind(Occasion=rep(1:(k+1),ng),data.frame(N=N.list$N,se=sqrt(diag(N.list$vcv))))
        else    
           N.ests=cbind(x$group.covariates[rep(1:ng,each=k+1),],Occasion=rep(1:(k+1),ng),data.frame(N=N.list$N,se=sqrt(diag(N.list$vcv))))
        N.ests$LCL=conf.int(N.ests$N,N.ests$se,normal)
        N.ests$UCL=conf.int(N.ests$N,N.ests$se,normal,FALSE)
        names(N.ests)=c(names(x$group.covariates),"Occasion","N","se","LCL","UCL")
#       Sum estimates across groups within an occasion and compute the estimates,se, conf int
#       and their v-c matrix
        ncontrast=NULL
        for (i in 1:ng)
          ncontrast=rbind(ncontrast,diag(1,nrow=k+1,ncol=k+1))
        Nbyocc=data.frame(N=as.vector(N.ests$N%*%ncontrast))
        Nbyocc.vcv=t(ncontrast)%*%N.list$vcv%*%ncontrast
        Nbyocc$se=sqrt(diag(Nbyocc.vcv))
        Nbyocc$LCL=conf.int(Nbyocc$N,Nbyocc$se,normal)
        Nbyocc$UCL=conf.int(Nbyocc$N,Nbyocc$se,normal,FALSE)
        Nbyocc=cbind(Occasion=1:(k+1),Nbyocc)
      }
#     Return list of results
      if(N&NGross) return(list(N=N.ests,N.vcv=N.list$vcv, Nbyocc=Nbyocc, Nbyocc.vcv=Nbyocc.vcv, NGross=NGross.list$NGross, NGross.vcv=NGross.list$vcv) )
      if(!N&NGross) return(list(NGross=NGross.list$NGross, NGross.vcv=NGross.list$vcv) )
      if(N&!NGross) return(list(N=N.ests,N.vcv=N.list$vcv, Nbyocc=Nbyocc, Nbyocc.vcv=Nbyocc.vcv) )
   }
#  First check that x is a processed data list of type POPAN
   if(class(x)!="list" | is.null(x$model)) stop(paste(substitute(x)," is not a processed data frame\n"))
   if(x$model!="POPAN") stop(paste(substitute(x)," is not for a POPAN model\n"))
#  If model is a single mark model then call compute.popan.derived and return results list
   if(class(model)[1]=="mark")
      return(compute.popan.derived(x,model,drop=drop))
   else
#     If model is marklist then call compute.popan.derived for each model and model average the
#     results and return tbe model averaged results list.
      if(class(model)[1]=="marklist")
      {
         nmodels=nrow(model$model.table)
         model.nums=as.numeric(row.names(model$model.table))
         popan.list=vector("list",length=nmodels)
         for (i in 1:nmodels)
           popan.list[[i]]=compute.popan.derived(x,model[[model.nums[i]]],drop=drop)
         if(N)
         {
#          Model average estimates by group and occasion
           estimate=matrix(0,nrow=nmodels,ncol=nrow(popan.list[[1]]$N))
           vcv=vector("list",nmodels)
           for (i in 1:nmodels)
           {
              estimate[i,]=popan.list[[i]]$N$N
              vcv[[i]]=popan.list[[i]]$N.vcv
           }
           N.list=model.average(list(estimate=estimate,weight=model$model.table$weight,vcv=vcv),revised=revised)
           N.df=popan.list[[1]]$N
           N.df$N=N.list$estimate
           N.df$se=N.list$se
           N.df$LCL=conf.int(N.df$N,N.df$se,normal)
           N.df$UCL=conf.int(N.df$N,N.df$se,normal,FALSE)
           N.vcv=N.list$vcv
#          Model average estimates by occasion
           estimate=matrix(0,nrow=nmodels,ncol=nrow(popan.list[[1]]$Nbyocc))
           vcv=vector("list",nmodels)
           for (i in 1:nmodels)
           {
              estimate[i,]=popan.list[[i]]$Nbyocc$N
              vcv[[i]]=popan.list[[i]]$Nbyocc.vcv
           }
           Nbyocc.list=model.average(list(estimate=estimate,weight=model$model.table$weight,vcv=vcv))
           Nbyocc=popan.list[[1]]$Nbyocc
           Nbyocc$N=Nbyocc.list$estimate
           Nbyocc$se=Nbyocc.list$se
           Nbyocc$LCL=conf.int(Nbyocc$N,Nbyocc$se,normal)
           Nbyocc$UCL=conf.int(Nbyocc$N,Nbyocc$se,normal,FALSE)
           Nbyocc.vcv=Nbyocc.list$vcv
         }
#        Model average NGross estimates
         if(NGross)
         {
           estimate=matrix(0,nrow=nmodels,ncol=length(popan.list[[1]]$NGross))
           vcv=vector("list",nmodels)
           for (i in 1:nmodels)
           {
              estimate[i,]=popan.list[[i]]$NGross
              vcv[[i]]=popan.list[[i]]$NGross.vcv
           }
           N.list=model.average(list(estimate=estimate,weight=model$model.table$weight,vcv=vcv),revised=revised)
           NGross.df=data.frame(NGross=N.list$estimate,se=N.list$se)
           NGross.df$LCL=conf.int(NGross.df$NGross,NGross.df$se,normal)
           NGross.df$UCL=conf.int(NGross.df$N,NGross.df$se,normal,FALSE)
           NGross.vcv=N.list$vcv
         }
        if(N&NGross)return(list(N=N.df,N.vcv=N.list$vcv, Nbyocc=Nbyocc, Nbyocc.vcv=Nbyocc.vcv, NGross=NGross.df, NGross.vcv=NGross.vcv) )
        if(!N&NGross)return(list(NGross=NGross.df, NGross.vcv=NGross.vcv) )
        if(N&!NGross)return(list(N=N.df,N.vcv=N.list$vcv, Nbyocc=Nbyocc, Nbyocc.vcv=Nbyocc.vcv) )
     }
}
popan.Nt<-function(Phi,pent,Ns,vc,time.intervals)
{
#
# Computes population size for each group/occasion and the var-cov matrix of the estimates
#
# Arguments:
#   Phi  - interval-specific survival estimates for each group
#   pent - occasion-specific prob of entry estimates (first computed by subtraction) for each group
#   Ns   - group specific super-population estimate
#   vc   - variance-covariance matrix of the real parameters
#
# Value: list with following elements:
#   N    - dataframe of estimates by group and occasion and se, lcl,ucl and group/occasion data
#   vcv  - var-cov matrix of N estimates
#
# Define function to return matrix of first partials for N with respect to real parameters
  partial.Nt=function(Phi,pent,Ns,time.intervals)
  {
     k=length(Phi)+1
	 Phi.unit=Phi
	 Phi=Phi^time.intervals
     cumphi=cumprod(c(1,Phi))
     Phimat=sapply(1:k,function(i) c(rep(0,i-1),cumprod(c(1,Phi[i:k])))[1:k])
     Bi=Ns*c(1-sum(pent),pent)
     N=Phimat%*%Bi
     partial=matrix(0,nrow=k,ncol=2*(k-1)+1)
# Partial for phi
     for (i in 1:(k-1))
     {
       pmat=matrix(0,nrow=k,ncol=k)
       pmat[(i+1):k,1:i]=time.intervals[i]*(1/Phi.unit[i])
       partial[,i]=(Phimat*pmat)%*%Bi
     }
# Partial for pent
     for (i in 1:(k-1))
       partial[,i+k-1]=Ns*(Phimat[,i+1]-Phimat[,1])
# Partial for N
     partial[,2*(k-1)+1]=N/Ns
     return(list(N=N,partial=partial))
  }
# For each group call partial.Nt (defined above) and create a partial matrix for
# all groups combined
  ng=length(Ns)
  N=NULL
  k=length(Phi)/ng
  partial=matrix(0,nrow=ng*(k+1),ncol=(2*k+1)*ng)
  for(i in 1:ng)
  {
     index=(i-1)*k+(1:k)
     indexp=(i-1)*(2*k+1)+(1:(2*k+1))
     indexn=(i-1)*(k+1)+(1:(k+1))
     partial.list=partial.Nt(Phi[index],pent[index],Ns[i],time.intervals)
     N=c(N,partial.list$N)
     partial[indexn,indexp]=partial.list$partial
  }
# Construct vc matrix
  vcv=partial%*%vc%*%t(partial)
# Return estimates and v-c matrix of the estimates
  return(list(N=as.vector(N),vcv=vcv))
}

popan.NGross<-function(Phi,pent,Ns,vc,time.intervals)
{
#
# Computes population size for each group/occasion and the var-cov matrix of the estimates
#
# Arguments:
#   Phi  - interval-specific survival estimates for each group
#   pent - occasion-specific prob of entry estimates (first computed by subtraction) for each group
#   Ns   - group specific super-population estimate
#   vc   - variance-covariance matrix of the real parameters
#
# Value: list with following elements:
#   N    - dataframe of estimates by group and occasion and se, lcl,ucl and group/occasion data
#   vcv  - var-cov matrix of N estimates
#
# Define function to return matrix of first partials for N with respect to real parameters
  partial.NGross=function(Phi,pent,Ns)
  {
     k=length(Phi)
	 Phi.unit=Phi
	 time.intervals=time.intervals[1:k]
	 Phi=Phi^time.intervals
     Bi=Ns*c(1-sum(pent),pent)
     BiGross=Bi*c(1,log(Phi)/(Phi-1))
     BiGross[is.nan(BiGross)]=Bi[is.nan(BiGross)]
     NGross=sum(BiGross)
     partial=matrix(0,nrow=1,ncol=2*k+1)
# Partial for phi
     partial[1,1:k]=Bi[-1]*(time.intervals*(Phi-1)-time.intervals*Phi*log(Phi))/(Phi.unit*(Phi-1)^2)
# Partial for pent
     partial[1,(k+1):(2*k)]=Ns*(log(Phi)/(Phi-1)-1)
# Partial for N
     partial[1,2*k+1]=NGross/Ns
     partial[is.nan(partial)]=0
     return(list(NGross=NGross,partial=partial))
  }
# For each group, call partial.NGross (defined above) and create a partial matrix for
# all groups combined
  ng=length(Ns)
  NGross=NULL
  k=length(Phi)/ng
  partial=matrix(0,nrow=ng,ncol=ng*(2*k+1))
  for(i in 1:ng)
  {
     index=(i-1)*k+(1:k)
     indexp=(i-1)*(2*k+1)+(1:(2*k+1))
     partial.list=partial.NGross(Phi[index],pent[index],Ns[i])
     NGross=c(NGross,partial.list$NGross)
     partial[i,indexp]=partial.list$partial
  }
# Construct vc matrix
  vcv=partial%*%vc%*%t(partial)
# Return estimates and v-c matrix of the estimates
  return(list(NGross=as.vector(NGross),vcv=vcv))
}



