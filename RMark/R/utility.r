#' Various utility functions
#' 
#' Miscellaneous set of functions that can be used with results from the
#' package.
#' 
#' Function \code{extract.indices} extracts the parameter indices from the
#' parameter index matrices (PIMS) for a particular type of \code{parameter}
#' that match a set of group numbers and rows and columns that are defined in
#' the dataframe \code{df}. It returns a vector of indices which can be used to
#' specify the set of real parameters to be extracted by
#' \code{\link{covariate.predictions}} using the index column in \code{data} or
#' the \code{indices} argument. If df is NULL, it returns a dataframe with all of
#' the indices with model.index being the unique index across all parameters and the
#' par.index which is an index to the row in the design data. If parameter is NULL then
#' the the dataframe is given for all of the parameters.
#' 
#' Function \code{nat.surv} produces estimates of natural survival (Sn) from
#' total survival (S) and recovery rate (r) from a joint live-dead model in
#' which all harvest recoveries are reported. In that case, Taylor et al 2005
#' suggest the following estimator of natural survival Sn=S + (1-S)*r.  The
#' arguments for the function are a mark \code{model} object and a dataframe
#' \code{df} that defines the set of groups and times (row,col) for the natural
#' survival computations. It returns a list with elements: 1) \code{Sn} - a
#' vector of estimates for natural survival; one for each entry in \code{df}
#' and 2) \code{vcv} - a variance-covariance matrix for the estimates of
#' natural survival.
#' 
#' Function \code{pop.est} produces estimates of abundance using a vector of
#' counts of animals captured (\code{ns}) and estimates of capture
#' probabilities (\code{ps}).  The estimates can be aggregated or averaged
#' using the \code{design} matrix argument.  If individual estimates are
#' needed, use an nxn identity matrix for design where n is the length of
#' \code{ns}. To get a total of all the estimates use a nx1 column matrix of
#' 1s.  Any other \code{design} matrix can be specified to subset, aggregate
#' and/or average the estimates.  The argument \code{p.vcv} is needed to
#' compute the variance-covariance matrix for the abundance estimates using the
#' formula described in Taylor et al. (2002).  The function returns a list with
#' elements: 1) \code{Nhat} - a vector of abundance estimates and 2) \code{vcv}
#' - variance-covariance matrix for the abundance estimates.
#' 
#' Function \code{Compute.Sn} creates list structure for natural survival using
#' \code{nat.surv} to be used for model averaging natural survival estimates
#' (e.g., \code{model.average(compute.Sn(x,df,criterion))}). It returns a list
#' with elements estimates, vcv, weight: 1) estimates - matrix of estimates of
#' natural survival, 2)vcv - list of var-cov matrix for the estimates, and 3)
#' weight - vector of model weights.
#' 
#' Function \code{search.output.files}searches for occurrence of a specific
#' string in output files associated with models in a marklist x. It returns a
#' vector of model numbers in the marklist which have an output file containing
#' the string.
#' 
#' @usage 	extract.indices(model,parameter,df)
#'  
#'	        nat.surv(model,df)
#' 
#'          pop.est(ns,ps,design,p.vcv)
#'
#'          compute.Sn(x,df,criterion)
#' 
#'          logitCI(x,se)
#' 
#'          search.output.files(x,string)
#'   
#' @aliases extract.indices nat.surv pop.est compute.Sn search.output.files logitCI
#' @param model a mark model object
#' @param parameter character string for a type of parameter for that model
#' (eg, "Phi","p")
#' @param df dataframe containing the columns group, row, column which specify
#' the group number, the row number and column number of the PIM
#' @param ns vector of counts of animals captured
#' @param ps vector of capture probability estimates which match counts
#' @param design design matrix that specifies how counts will be aggregate
#' @param p.vcv variance-covariance matrix for capture probability estimates
#' @param x marklist of models for compute.Sn and a vector of real estimates for logitCI
#' @param se vector of std errors for real estimates
#' @param criterion vector of model selection criterion values (eg AICc)
#' @param string string to be found in output files contained in models in x
#' @author Jeff Laake
#' @export extract.indices nat.surv pop.est compute.Sn logitCI search.output.files
#' @references TAYLOR, M. K., J. LAAKE, H. D. CLUFF, M. RAMSAY and F. MESSIER.
#' 2002. Managing the risk from hunting for the Viscount Melville Sound polar
#' bear population. Ursus 13: 185-202.
#' 
#' TAYLOR, M. K., J. LAAKE, P. D. MCLOUGHLIN, E. W. BORN, H. D. CLUFF, S. H.
#' FERGUSON, A. ROSING-ASVID, R. SCHWEINSBURG and F.  MESSIER. 2005. Demography
#' and viability of a hunted population of polar bears. Arctic 58: 203-214.
#' @examples
#' \donttest{
#' # Example of computing N-hat for occasions 2 to 7 for the p=~time model
#' data(dipper)
#' md=mark(dipper,model.parameters=list(p=list(formula=~time),
#'        Phi=list(formula=~1)))
#' # Create a matrix from the capture history strings 
#' xmat=matrix(as.numeric(unlist(strsplit(dipper$ch,""))),
#'       ncol=nchar(dipper$ch[1]))
#' # sum number of captures in each column but don't use the first 
#' # column because p[1] can't be estimated
#' ns=colSums(xmat)[-1]
#' # extract the indices and then get covariate predictions for p(2),...,p(7)
#' # which are row-colums 1-6 in PIM for p 
#' p.indices=extract.indices(md,"p",df=data.frame(group=rep(1,6),
#'    row=1:6,col=1:6))
#' p.list=covariate.predictions(md,data=data.frame(index=p.indices))
#' # call pop.est using diagonal design matrix to get 
#' # separate estimate for each occasion
#' pop.est(ns,p.list$estimates$estimate,
#'   design=diag(1,ncol=6,nrow=6),p.list$vcv)
#' }
extract.indices=function(model,parameter=NULL,df=NULL)
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
   if(is.null(parameter))
   {
	   ddl=model$design.data
	   link.par=NULL
	   prev=0
	   for(i in 1:(length(ddl)-1))
	   {
		   link.par=rbind(link.par,data.frame(par=names(ddl[i]),model.index=prev+1:nrow(ddl[[i]]),par.index=1:nrow(ddl[[i]])))
		   prev=prev+nrow(ddl[[i]])
	   }
	   return(link.par)
   } else
       if(is.null(df))
	   {
		   ddl=model$design.data
		   link.par=NULL
		   prev=0
		   for(i in 1:(length(ddl)-1))
		   {
			   if(names(ddl)[i]==parameter)
				   link.par=data.frame(par=names(ddl[parameter]),model.index=prev+1:nrow(ddl[[parameter]]),par.index=1:nrow(ddl[[parameter]]))
			   prev=prev+nrow(ddl[[i]])
		   }
		   return(link.par)
	   } else
	   {
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
			message("\nModel ",i," Messages: ",paste(output[positions],sep="\n"))
		}
	}
	return(indices)
}

#' Convert Multistate data for POPAN-style abundance estimation
#' 
#' Converts data and optionally creates and structures design data list
#' such that population size can be derived with multistate data.
#' @param x an RMark dataframe
#' @param augment_num the number to add with a capture history of all 0s; this is the expected number that were in the population and not ever seen
#' @param augment_stratum the single character to represent outside of the population; use a value not used in the data capture history
#' @param enter_stratum the single character to represent inside of the population but not yet entered; use a value not used in the data capture history
#' @param strata vector of single characters for observed and unobserved states
#' @param ps vector of capture probability estimates which match counts
#' @param begin.time beginning time of observed occasions; two occasions are added to the fron of the capture history at times begin.time-1 and begin.time-2
#' @param groups vector of character variable names of factor variables to use for grouping
#' @param ddl if TRUE, will return processed data and a design data list with the appropriate fixed parameters.
#' @param time.intervals intervals of time between observed occasions
#' @author Jeff Laake
#' @export 
#' @examples
#' data(dipper)
#' popan_N=summary(mark(dipper,model="POPAN",model.parameters=list(pent=list(formula=~time))),se=T)$reals$N
#' data.list=MS_popan(dipper,ddl=TRUE,augment_num=30)
#' modMS=mark(data.list$data,data.list$ddl,model.parameters=list(Psi=list(formula=~B:toB:time)),brief=TRUE)
#' Psi_estimates=summary(modMS,se=TRUE)$reals$Psi
#' Nhat_MS=Psi_estimates$estimate[1]*sum(abs(data.list$data$data$freq))
#' se_Nhat_MS=Psi_estimates$se[1]*Nhat_MS
#' cat("Popan N = ",popan_N$estimate," (se = ",popan_N$se,")\n")
#' cat("MS N = ",Nhat_MS," (se = ",se_Nhat_MS,")\n")
MS_popan=function(x,augment_num=100,augment_stratum="A",enter_stratum="B",strata=NULL,begin.time=1,groups=NULL,ddl=FALSE,time.intervals=NULL)
{
	xp=process.data(x,groups=groups)
	x$ch=paste(augment_stratum,"0",x$ch,sep="")
	if(is.null(x$freq)) x$freq=1
	if(is.null(groups))
	{
		x=rbind(x[1,],x)
		if(!length(augment_num)==1)stop("length of augmented numbers does not match number of groups")
		x$freq[1]=augment_num
		x$ch[1]=paste(augment_stratum,"0",paste(rep(0,xp$nocc),collapse=""),sep="")
	}else {
		x=rbind(x[1:nrow(xp$group.covariates),],x)
		if(!length(augment_num)==nrow(xp$group.covariates)&!length(augment_num)==1)stop("length of augmented numbers does not match number of groups")
		x$freq[1:nrow(xp$group.covariates)]=augment_num
		x[1:nrow(xp$group.covariates),colnames(xp$group.covariates)]=xp$group.covariates
		x$ch[1:nrow(xp$group.covariates)]=paste(augment_stratum,"0",paste(rep(0,xp$nocc),collapse=""),sep="")
	}
	rownames(x)=1:nrow(x)
	if(!ddl) return(x)
	
	if(!is.null(time.intervals)) time.intervals=c(0,0,time.intervals)
	if(is.null(strata)) {
		char=unique(unlist(strsplit(dipper$ch,"")))
		strata=char[char!="0"]
	}
	
	dp=process.data(x,model="Multistrata",strata=c(augment_stratum,enter_stratum,strata),groups=groups,begin.time=begin.time-2,time.intervals=time.intervals)
	
	ddl=make.design.data(dp,parameters=list(Psi=list(subtract.stratum=c(augment_stratum,strata[1],strata))))
	
	ddl$Psi$fix=NA
	ddl$Psi$fix[ddl$Psi$stratum==augment_stratum&ddl$Psi$tostratum%in%strata]=0
	ddl$Psi$fix[ddl$Psi$stratum==augment_stratum&ddl$Psi$tostratum==enter_stratum&ddl$Psi$Time>0]=0
	
	ddl$Psi$fix[ddl$Psi$stratum==enter_stratum&ddl$Psi$tostratum==augment_stratum]=0
	ddl$Psi$fix[ddl$Psi$stratum==enter_stratum&ddl$Psi$tostratum==enter_stratum&as.numeric(ddl$Psi$time)==max(as.numeric(ddl$Psi$time))]=0
	ddl$Psi$fix[ddl$Psi$stratum==enter_stratum&ddl$Psi$tostratum==enter_stratum&as.numeric(ddl$Psi$time)==min(as.numeric(ddl$Psi$time))]=1
	
	ddl$Psi$fix[ddl$Psi$stratum=="1"&ddl$Psi$tostratum%in%c(augment_stratum,enter_stratum)]=0
	
	ddl$S$fix=NA
	ddl$S$fix[ddl$S$stratum%in%c(augment_stratum,enter_stratum)]=1
	
	ddl$p$fix=NA
	ddl$p$fix[ddl$S$stratum%in%c(augment_stratum,enter_stratum)]=0
	return(list(data=dp,ddl=ddl))
}



