#' Compute estimates of real parameters with individual and design covariates
#' 
#' Unlike covariate.predictions this function allows modification of design data covariates
#' as well as individual covariates for the computation of the real parameters. It does
#' this by modifying values in the design data, adding individual covariate values to the 
#' design data and then applying the model formula to the modified design data to construct a 
#' design matrix that is used with the beta estimates to construct the real parameter estimates.
#' 
#' There are two restrictions. First,it only works with a single parameter type (eg Phi or p)
#' whereas covariate.predictions allows simultaneous estimation of multiple parameter types.
#' Second, if the row from df is a fixed parameter that will only be identified if it uses the fix
#' parameter in the design data. It will not work with the original approach of using index and fixed as
#' arguments. This should be a minor restrictioin because it doesn't make sense to compute a range of
#' real parameter values for a fixed parameter. 
#' 
#' The primary arguments are model which is a fitted mark model object and df which is one of 
#' the design dataframes from the design data list (ddl) used to fit the model. The value of
#' df can be a subset of the original design dataframe but do NOT create an arbitrary dataframe
#' for use as df because if you don't get the indices correct or you don't construct the factor
#' variables correctly, it will likely fail or the results could be bogus.  
#' 
#' The argument parameter is just the character name for the parameter ("Phi"). It is only used 
#' to extract the correct formula from the fitted model.
#' 
#' The argument beta can provide values for the beta coefficients other than the fitted ones but
#' if you do so, don't expect the variances to make sense as the beta.vcv is computed at the
#' fitted values.  The default for beta is to use the fitted values so it need not be specified.
#' 
#' The argument data is a dataframe containing values for individual covariates or replacement 
#' values for design data covariates.  Do not replace values of factor variables which probably 
#' would not make sense anyhow.  If replicate=FALSE then the number of rows in df must match the
#' number of rows in data and the values in df are replaced (if design covariate) or added if 
#' an individual covariate.  If replicate=TRUE, then design data (df) is replicated for each row 
#' in data and the values in data are computed for each one of the rows in df.  Using replicate=TRUE
#' would make sense in a case where the value can differ for the index. For example, if one of the 
#' design covariates was temperature then you might want to compute the values at a range of temperatures
#' for a row in the design data representing each age class in the data.
#' 
#' The arguments se and vcv control computation of the std errors and v-c matrix.
#' 
#' @param model MARK model object
#' @param df design dataframe subset
#' @param parameter names of the parameter
#' @param replicate if FALSE then number of rows in data must match the number of rows in df
#' @param beta estimates of beta parameters for real parameter computation
#' @param data dataframe with covariate values
#' @param se if TRUE returns std errors and confidence interval of real
#' estimates
#' @param vcv logical; if TRUE, sets se=TRUE and returns v-c matrix of real
#' estimates
#' @return A data frame (\code{real}) is returned if \code{vcv=FALSE};
#' otherwise, a list is returned also containing vcv.real: \item{real}{ data
#' frame containing estimates, and if se=TRUE or vcv=TRUE it also contains
#' standard errors and confidence intervals and notation of whether parameters
#' are fixed or at a boundary} \item{vcv.real}{variance-covariance matrix of
#' real estimates}
#' @author Jeff Laake
#' @export
#' @seealso
#' \code{\link{inverse.link}},\code{\link{deriv_inverse.link}}
#' @keywords utility
#' @examples
#' 
#' data(dipper)
#' dp=process.data(dipper)
#' ddl=make.design.data(dp)
#' model=mark(dp,ddl,model.parameters=list(Phi=list(formula=~Time)))
#' predict_real(model,ddl$Phi[1,,drop=FALSE],"Phi",replicate=TRUE,data=data.frame(Time=-12:12))
#' 
predict_real <-
		function(model,df,parameter,replicate=FALSE,beta=NULL,data=NULL,se=TRUE,vcv=FALSE)
{
	model=load.model(model)
	if(is.null(beta)) 
	{
		which.beta=grep(paste(parameter,":",sep=""),rownames(model$results$beta))
#		if(length(which.beta)!=length(model$results$beta$estimate)) stop("beta vector not the correct length")
		beta=model$results$beta$estimate[which.beta]
	} else{
		if(se | vcv){
			se=FALSE
			vcv=FALSE
			message("\nse and vcv set to FALSE because beta values specified")
		}
	}
#
# Assign value of chat - overdispersion constant
#
	if(is.null(model$chat))
		chat=1
	else
		chat=model$chat
# 
#   Add individual covariate names 
#f
	if(!is.null(model$covariates))
	{
		nc=ncol(df)
		if(is.null(data))
		{
			mc=model$covariates
		} else
		{
			if(any(!model$covariates%in%names(data)))
			{
				mc=model$covariates[!model$covariates%in%names(data)]
				df=cbind(df,matrix(NA,nrow=nrow(df),ncol=length(mc)))
				names(df)[(nc+1):ncol(df)]=mc
			} else
				mc=model$covariates
		}
#       fill in mean values if none provided
		meanval=apply(model$data$data[,mc,drop=FALSE],2,mean)
		df[, mc]=rep(meanval,each=nrow(df))
	}
#
# If indices is null, then there must be an index field in df
#
	indices=df$model.index
	if(!is.null(data))
	{
		if(!replicate)
		{
			if(nrow(df)!=nrow(data)) stop("number of rows in df and data must match if replicate=FALSE")
			if(any(names(df)%in%names(data))) df[,names(df)%in%names(data)]=data[,names(data)%in%names(df),drop=FALSE][,names(df)[ names(df) %in% names(data)]]
			df=cbind(df,data[,!names(data)%in%names(df),drop=FALSE])
		} else
		{
			ndf=nrow(df)
			df=df[rep(1:nrow(df),nrow(data)),,drop=FALSE]
			newdata=NULL
			replacedata=NULL
			if(any(!names(data)%in%names(df)))
			newdata=data[,!names(data)%in%names(df),drop=FALSE]
			if(any(names(data)%in%names(df)))
			   replacedata=data[,names(data)%in%names(df),drop=FALSE]
		   if(!is.null(replacedata))
			   df[,names(df)%in%names(data)]=replacedata[rep(1:nrow(replacedata),each=ndf),,drop=FALSE][,names(df)[ names(df) %in% names(data)]]
		   if(!is.null(newdata))
				df=cbind(df,newdata[rep(1:nrow(newdata),each=ndf),,drop=FALSE])
			
		}
	}
#
# Set indices for real parameters that have been fixed and at any boundaries
#
	fixedparms=rep(FALSE,nrow(df))
	fixedvalues=rep(NA,nrow(df))
	if(!is.null(df$fix))
	{
		fixedparms=which(!is.na(df$fix))
		fixedvalues=df$fix[!is.na(df$fix)]
	}
#
#  create links value
#
    if(length(model$links)==1)
		links=model$links
	else
        links=model$simplify$links[df$model.index]
#
#   create design matrix with formula and design data
#
    design=model.matrix(model$model.parameters[[parameter]]$formula,df)
	design = design[,paste(parameter,":",colnames(design),sep="")%in%colnames(model$design.matrix)]
	
#  Compute real parameters; if neither se or vcv then return vector of real parameters
#
	real=convert.link.to.real(design%*%beta,links=links)
#
#  Set fixed real parameters to their fixed values
#
	real[fixedparms]=fixedvalues
#  If no se or vcv requested, return result
	if(!vcv & !se)
	{
		if(!is.null(df$time.intervals))real=real^df$time.intervals
		return(cbind(df,data.frame(real=real)))
	}
#
# Compute vc matrix for real parameters which needs to be modified for
# any mlogit parameters
#
	if(length(model$links)==1)
		deriv.real=deriv_inverse.link(real,design,model$links)
	else
		deriv.real=t(apply(data.frame(real=real,x=design,links=links),1,
						function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
	if(length(which.beta)>1)
	   vcv.real=deriv.real%*%model$results$beta.vcv[which.beta,which.beta]%*%t(deriv.real)
	else
	   vcv.real=t(deriv.real)%*%model$results$beta.vcv[which.beta,which.beta,drop=FALSE]%*%(deriv.real)
    if(!is.null(df$time.intervals))
	{
		deriv=df$time.intervals*real^(df$time.intervals-1)
		vcv.real=deriv%*%t(deriv)*vcv.real
		real=real^df$time.intervals
	}
#
# If vcv=TRUE, compute v-c matrix and std errors of real estimates
# To handle any mlogit parameters compute pseudo-real estimates using log in place of mlogit
#
	ind=grep("mlogit",links,ignore.case=TRUE)
	templinks=links
	if(length(ind)>0)
	{
		templinks[ind]="log"
		pseudo.real=as.vector(convert.link.to.real(design%*%beta,links=templinks))
#   IF fixed parameters are included in mlogit set, need to recompute the real parameters using 1 in
#   mlogit calculation for every fixed parameter
		if(any(fixedparms[ind]))
		{
			pseudo.real[fixedparms]=0
			pseudo.real[ind][fixedparms[ind]]=exp(pseudo.real[ind][fixedparms[ind]])
			# sums=by(pseudo.real[ind],model$links[ind],sum)
			
			# replacement keeps levels in original order, 
			# otherwise by function creates factor out of character that rearranges
			# the order, only noticeably if >10 levels because factor will order as 
			# 1, 10, 11, 2, 3, 4...
			sums = by(pseudo.real[ind], factor(links[ind], levels = unique(links[ind])), sum)
			
			sums=sums[match(links[ind],names(sums))]
			
			# real[ind]=pseudo.real[ind]/(1+sums[ind])
			# sums is already of correct length, [ind] gives error because it refers
			# to indices outside the size of sums vector
			# I could be wrong, though.
			real[ind]=pseudo.real[ind]/(1+sums)
			
			real[fixedparms]=fixedvalues
		}
#
#   Compute first derivatives of pseudo-real (for any mlogit parameter)
#   estimates with respect to beta parameters
#
		if(length(templinks)==1)
			deriv.pseudo=deriv_inverse.link(pseudo.real,design,templinks)
		else
			deriv.pseudo=t(apply(data.frame(real=pseudo.real,x=design,links=templinks),1,
							function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
		deriv.pseudo[fixedparms,]=0
		vcv.pseudo=chat*deriv.pseudo%*%model$results$beta.vcv[which.beta,which.beta,drop=FALSE]%*%t(deriv.pseudo)
#
#    Apply chain rule to get variance of real parameters which has mlogits
#    expressed as zi/(1+z1+...zk) where k is number of mlogit components-1 and
#    non-mlogits are expressed as zi.
#    bottom is either 1 for non-mlogits and the sum for mlogits
#    pbottom is partial with respect to zi
#
		if(length(model$links)==1)
			links=rep(model$links,length(pseudo.real))
		mlogits=outer(links,links,function(x,y)as.numeric(x==y))*as.numeric(substr(links,1,6)=="mlogit"|substr(links,1,6)=="MLogit")
		pbottom=matrix(0,nrow=dim(vcv.pseudo)[1],ncol=dim(vcv.pseudo)[1]) + mlogits
		bottom=diag(nrow=dim(vcv.pseudo)[1])*(1-as.numeric(substr(links,1,6)=="mlogit"|substr(links,1,6)=="MLogit"))+
				mlogits + pbottom*apply(pbottom*pseudo.real,2,sum)
		deriv.pseudo=(diag(nrow=dim(vcv.pseudo)[1])*bottom-pseudo.real*pbottom)/bottom^2
		deriv.pseudo[is.nan(deriv.pseudo)]=0
		vcv.real=deriv.pseudo%*%vcv.pseudo%*%t(deriv.pseudo)
	}
	else
		vcv.real=chat*vcv.real
#
# Compute conf interval taking into account use of logit transform for mlogit
# and any 0-1 link (loglog,cloglog,sin,logit)
#
	link.se=suppressWarnings(sqrt(chat*diag(design%*%model$results$beta.vcv[which.beta,which.beta,drop=FALSE]%*%t(design))))
	link.se[is.na(link.se)]=0
	if(length(model$links)==1)
		links=rep(model$links,length(real))
	ind=unique(c(grep("mlogit",model$links,ignore.case=TRUE),which(links%in%c("sin","Sin","LogLog","loglog","CLogLog","cloglog"))))
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
	if(!is.null(df$time.intervals))
	{
		real.ucl=real.ucl^df$time.intervals
		real.lcl=real.lcl^df$time.intervals
	}
	real.lcl[fixedparms]=fixedvalues
	real.ucl[fixedparms]=fixedvalues
#
# Set v-c values of fixed parameters to 0
#
	vcv.real[fixedparms,]=0
	vcv.real[,fixedparms]=0
	diag(vcv.real)[diag(vcv.real)<0]=0
	se.real=sqrt(diag(vcv.real))
#se.real[is.na(se.real)]=0
	fixed=rep("",dim(design)[1])
	fixed[fixedparms]="Fixed"
	if(vcv)
		return(list(real=cbind(df,real),se.real=se.real,lcl=real.lcl,ucl=real.ucl,fixed=fixed,vcv.real=vcv.real))
	else
		return(cbind(df,data.frame(real=real,se=se.real,lcl=real.lcl,ucl=real.ucl,fixed=fixed)))
}

