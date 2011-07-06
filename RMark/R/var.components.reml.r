var.components.reml=function(theta,design,vcv=NULL,rdesign=NULL,initial=NULL,interval=c(-25,10),REML=TRUE)
{
#  Arguments:
#     theta - vector of parameter estimates
#     design- design matrix for fixed effects
#     vcv   - estimated variance-covariance matrix for parameters
#     rdesign - design matrix for random effect (do not use intercept form; eg ~-1+year vs ~year)
#     initial - initial values for variance components
#     interval - interval bounds for log(sigma) to help optimization from going awry
#     REML - if TRUE uses reml else ml
#
#  Value: list with following elements
#     neglnl - negative log likelihood
#     AICc   - AICc value for model
#     sigma - process variance estimate
#     beta  - estimate of betas for design
#     vcv.beta - variance covariance matrix for beta
#
   if(is.null(initial))
     if(is.null(rdesign))
       initial=.1 
     else
       initial=c(.1,.1)
   if(nrow(design)!=length(theta)) stop("Number of rows of design matrix must match length of theta vector")
   if(!is.null(vcv))
   {
     if(nrow(vcv)!=length(theta)) stop("Number of rows of vcv matrix must match length of theta vector")
     if(ncol(vcv)!=length(theta)) stop("Number of columns of vcv matrix must match length of theta vector")
   }
   if(length(theta)<= ncol(design))stop("Length theta must exceed number of columns of design")
   if(!is.null(rdesign) && any(rowSums(rdesign)>1))stop("Do not use intercept in formula for rdesign")
#
#  Reduce theta, design and vcv to the theta's that are used in the design
#   
   rn=(1:nrow(design))[apply(design,1,function(x)any(as.numeric(x)!=0))]
   theta=theta[rn]
   design=design[rn,,drop=FALSE]
   rdesign=rdesign[rn,,drop=FALSE]
   if(is.null(vcv))
     vcv=matrix(0,ncol=length(rn),nrow=length(rn))
   else
     vcv=vcv[rn,rn]
   n=length(rn)
   p=ncol(design)
# beta hat computation
   beta.hat = function(H, X, theta) return(solve(crossprod(X,solve(H,X)), crossprod(X,solve(H, theta))))
# Complete v-c matrix
   compute.H=function(alpha,vcv,rdesign)
   {
     sigmasq=exp(alpha)
     K=nrow(vcv)
     sigmamat=diag(sigmasq[1],nrow=K,ncol=K)
     if(!is.null(rdesign))
       sigmamat=sigmamat+rdesign%*%t(rdesign)*sigmasq[2]
     return(sigmamat+vcv)
   }
# reml negative log likelihood
   lnl.reml=function(alpha,vcv,X,theta,rdesign)
   {
      H=compute.H(alpha,vcv,rdesign)
      beta=beta.hat(H,X,theta)
      res=theta-X%*%beta
      lnl=.5*(determinant(H,log=TRUE)$mod+crossprod(res,solve(H,res)))
      if(REML)lnl=lnl+.5*determinant(crossprod(X,solve(H,X)),log=TRUE)$mod
      return(lnl)
   }
#  Find value of log(sigma) that minimizes reml negative log likelihood
   if(length(initial)==1)
   {
     soln=optimize(lnl.reml,interval=interval,vcv=vcv,theta=theta,X=design,rdesign=rdesign)
     estimate=soln$min
     neglnl=soln$objective
   }
   else
   {
     soln=optim(initial,lnl.reml,method="L-BFGS-B",lower=interval[1],upper=interval[2],vcv=vcv,theta=theta,X=design,rdesign=rdesign)
     estimate=soln$par
     neglnl=soln$value
   }
#  Compute beta-hat, std errors and v-c matrix at final values
   if(REML)
     neglnl=neglnl+.5*(n-p)*log(2*pi)
   else
     neglnl=neglnl+.5*n*log(2*pi)
   H=compute.H(estimate,vcv,rdesign)
   beta=beta.hat(H,design,theta)
   rownames(beta)=colnames(design)
   vcv.beta = solve(crossprod(design,solve(H,design)))
   rownames(vcv.beta)=colnames(design)
   colnames(vcv.beta)=colnames(design)
   beta=as.data.frame(beta)
   beta$se=sqrt(diag(vcv.beta))
   names(beta)=c("Estimate","SE")
   K=p+length(estimate)
   return(list(neglnl=as.vector(neglnl),AICc=as.vector(2*neglnl+2*K*(n/(n-K-1))),sigmasq=exp(estimate),beta=beta,vcv.beta=vcv.beta))
}
