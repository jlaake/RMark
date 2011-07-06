var.components=function(theta,design,vcv,LAPACK=TRUE)
{
#  Arguments:
#     theta - vector of parameter estimates
#     design- design matrix for combining parameter estimates (use single column matrix of 1s for mean)
#     vcv   - estimated variance-covariance matrix for parameters
#     LAPACK - controls use of LAPACK with qr solution; TRUE will work the best for most cases
#
#  Value: list with following elements
#     sigma - process variance estimate
#     beta  - estimate of betas for design
#     vcv.beta - variance convariance matrix for beta
#
   if(nrow(design)!=length(theta)) stop("Number of rows of design matrix must match length of theta vector")
   if(nrow(vcv)!=length(theta)) stop("Number of rows of vcv matrix must match length of theta vector")
   if(ncol(vcv)!=length(theta)) stop("Number of columns of vcv matrix must match length of theta vector")
   if(length(theta)<= ncol(design))stop("Length theta must exceed number of columns of design")
#
#  Reduce theta, design and vcv to the theta's that are used in the design
#   
   rn=(1:nrow(design))[apply(design,1,function(x)any(as.numeric(x)!=0))]
   theta=theta[rn]
   design=design[rn,,drop=FALSE]
   vcv=vcv[rn,rn]
   sigma=0
# This is equation (1) in B&W(2002); changed to use qr solution for inverse
    beta.hat = function(Dinv, X, theta) return(solve(qr(t(X) %*% 
        Dinv %*% X,LAPACK=LAPACK)) %*% (t(X) %*% Dinv %*% theta))
# This computes inverse of D; changed to use qr solution for inverse
   compute.Dinv=function(sigma,vcv)
   {
      K=nrow(vcv)
      sigmamat=matrix(0,nrow=K,ncol=K)
      diag(sigmamat)=sigma
      return(solve(qr(vcv + sigmamat,LAPACK=LAPACK)))
   }
# This is equation (2) in B&W(2002)
   mom.sig=function(sigma,vcv,X,theta)
   {
      Dinv=compute.Dinv(sigma,vcv)
      beta=beta.hat(Dinv,X,theta)
      return(t(theta-X%*%beta)%*%Dinv%*%(theta-X%*%beta)-(nrow(X)-length(beta)))
   }
# Uses uniroot to compute value of sigma by finding root of mom.sig  If it results in an error or the root is negative, the
# estimate of sigma is 0
   soln=try(uniroot(mom.sig,lower=-(min(Re(eigen(vcv)$values))+1e-12),upper=max(vcv),vcv=vcv,theta=theta,X=design,tol=1e-15))
   if(class(soln)=="try-error" )
      sigma=0
   else
      if(soln$root<0)
        sigma=0
      else
        sigma=soln$root
# Compute final values of D inverse, beta and its v-c matrix and return those values
   Dinv=compute.Dinv(sigma,vcv)
   beta=beta.hat(Dinv,design,theta)
   rownames(beta)=colnames(design)
   vcv.beta = solve(qr(t(design) %*% Dinv %*% design,LAPACK=LAPACK))
   rownames(vcv.beta)=colnames(design)
   colnames(vcv.beta)=colnames(design)
   beta=as.data.frame(beta)
   beta$se=sqrt(diag(vcv.beta))
   names(beta)=c("Estimate","SE")
   return(list(sigma=sigma,beta=beta,vcv.beta=vcv.beta))
}


