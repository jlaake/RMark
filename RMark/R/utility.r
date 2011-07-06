extract.indices=function(model,parameter,df)
{
#  Extracts the parameter indices from a model for a particular parameter
#  as defined by the dataframe df which contains columns group, row, column
#
#  Arguments:
#   model     - mark model object
#   parameter - character string for a type of parameter for that model (eg, "Phi","p")
#   df        - dataframe containing the columns group, row, column which specify
#                the group number, the row number and column number of the PIM
#
#  Value: vector of indices which can be used to specify the set of real parameters
#         to be extracted
#

   if(!valid.parameters(model$model,parameter))stop()
   indices=vector("numeric",length=nrow(df))
   for(i in 1:nrow(df))
   {
      group=df$group[i]
      irow=df$row[i]
      jcol=df$col[i]
      if(group > model$number.of.groups)
        stop(paste("Specified group number", group, " is greater than number of groups", model$number.of.groups))
      if(irow > nrow(model$pims[[parameter]][[group]]$pim))
        stop(paste("Specified row number", jcol, " is greater than number of rows", nrow(model$pims[[parameter]][[group]]$pim)))
      if(jcol > ncol(model$pims[[parameter]][[group]]$pim))
        stop(paste("Specified column number", jcol, " is greater than number of columns", ncol(model$pims[[parameter]][[group]]$pim)))

      indices[i]=model$pims[[parameter]][[group]]$pim[irow,jcol]
   }
   return(indices)
}

nat.surv=function(model,df)
{
#  Computes estimates of natural survival as Sn=S+(1-S)*r See Taylor et al 2005
#
#  Arguments:
#
#   model - mark model object
#   df    - dataframe containing the columns group, row, column which specify
#            the group number, the row number and column number of the PIM
#
#   Value: list with elements Sn and vcv
#           Sn  - a vector of estimates for natural survival; one for each entry in df
#           vcv - a var-cov matrix for the estimates of natural survival
#
   if(class(model)[1]!="mark" | !(class(model)[2]=="Burnham" | class(model)[2]=="Barker"))
     stop("This function only works with Burnham or Barker model for RMark")
   r.indices=extract.indices(model,"r",df)
   S.indices=extract.indices(model,"S",df)
   npar=nrow(df)
   covar=covariate.predictions(model,data.frame(index=c(r.indices,S.indices)))
   Sn=covar$estimates$estimate[(npar+1):(2*npar)]+covar$estimates$estimate[1:npar]*(1-covar$estimates$estimate[(npar+1):(2*npar)])
   partial=matrix(0,nrow=length(Sn),ncol=2*length(Sn))
   partial[cbind(1:length(Sn),1:length(Sn))]=1-covar$estimates$estimate[(npar+1):(2*npar)]
   partial[cbind(1:length(Sn),(length(Sn)+1):(2*length(Sn)))]=1-covar$estimates$estimate[1:npar]
   vcv=partial%*%covar$vcv%*%t(partial)
   return(list(Sn=Sn,vcv=vcv))
}

pop.est=function(ns,ps,design,p.vcv)
{
#  Computes estimates of population size; See Taylor et al 2002
#
#  Arguments:
#
#   ns     - vector of counts of animals captured
#   ps     - vector of capture probabilities which match counts
#   design - design matrix that specifies how counts will be aggregated
#   p.vcv  - variance-covariance matrix for capture probabilities
#
#   Value: list with elements Nhat and vcv
#           Nhat - a vector of estimates for abundance
#           vcv - a var-cov matrix for the estimates of abundance
#
   if(length(ns)!=length(ps))stop("Length of ps must match length of ns")
   if(length(ps)!=nrow(p.vcv) | length(ps)!=ncol(p.vcv))stop("Length of ps must match dimensions of p.vcv")
   if(length(ns)!=nrow(design))stop("Length of ns must match number of rows in design")
#  Compute values of Nhat
   Nhat=t(ns/ps)%*%design
#  Compute values of v-c matrix
   Nhat.vcv= diag(as.vector(t(ns*(1-ps)/ps^2)%*%design),nrow=ncol(design),ncol=ncol(design)) +
                         t(design)%*%(outer(ns/ps^2,ns/ps^2,"*")*p.vcv)%*%design
   return(list(Nhat=Nhat,vcv=Nhat.vcv))
}

compute.Sn=function(x,df,criterion)
{
#  Computes list structure for natural survival using nat.surv to
#  be used for model averaging (model.average(compute.Sn(x,df,criterion))
#
#  Arguments:
#
#   x        - marklist of models
#   df       - dataframe containing the columns group, row, column which specify
#               the group number, the row number and column number of the PIM
#  criterion - vector of model selection criterion values (eg AICc)
#
#   Value: list with elements estimates, vcv, weight
#           estimates - matrix of estimates of natural survival
#           vcv       - list of var-cov matrix for the estimates 
#           weight    - model weights
#
weight=criterion
weight=weight-min(weight)
weight=exp(-.5*weight)/sum(exp(-.5*weight))
modelnums=as.numeric(row.names(x$model.table))
Sn.estimates=matrix(0,nrow=nrow(x$model.table),ncol=nrow(df))
Sn.vcv=vector("list",length=nrow(x$model.table))
model=NULL
for (i in 1:nrow(x$model.table))
{
   if(is.list(x[[1]]))
      Sn.list=nat.surv(x[[modelnums[i]]],df)
   else
   {
      load(x[[modelnums[i]]])
      Sn.list=nat.surv(model,df)
   }
   Sn.estimates[i,]=Sn.list$Sn
   Sn.vcv[[i]]=Sn.list$vcv
}
return(list(estimates=Sn.estimates,vcv=Sn.vcv,weight=weight))
}

logitCI=function(x,se)
{
#
# Computes conf interval of real parameter (bounded bt 0-1) using logit transform
#
#  Arguments:
#
#   x     - vector of real estimates
#   se    - vector of se estimates
#
#  Value:
#
#    A dataframe with x,se and conf interval
#
link.values=log(x/(1-x))
deriv.link.values=1/x+1/(1-x)
if(length(x)==1)
{
  se.links=sqrt(deriv.link.values^2*se^2)
}
else
{
  deriv.link.matrix=matrix(0,nrow=length(deriv.link.values),ncol=length(deriv.link.values))
  diag(deriv.link.matrix)=deriv.link.values
  vcv.real=diag(se^2)
  se.links=sqrt(diag(deriv.link.matrix%*%vcv.real%*%t(deriv.link.matrix)))
}
lcl=plogis(link.values-1.96*se.links)
ucl=plogis(link.values+1.96*se.links)
return(data.frame(estimate=x,se=se,lcl=lcl,ucl=ucl))
}

search.output.files=function(x,string)
{
#
# Searches for occurrence of a specific string in output files associated with models in a marklist.
#
#  Arguments:
#
#   x      - marklist of models
#   string - string to be found in output files
#
#  Value:
#
#   Vector of model numbers in the marklist which have an output file containing the string.
#
	indices=NULL
	for(i in 1:nrow(x$model.table))
	{
		output=readLines(paste(x[[i]]$output,".out",sep=""))
		positions=grep(string,output)
		if(length(positions)!=0)
		{
			indices=c(indices,i)
			cat("\nModel ",i," Messages: ",paste(output[positions],sep="\n"))
		}
	}
	return(indices)
}
