#' Variance components estimation
#' 
#' Computes estimated effects, standard errors and process variance for a set
#' of estimates
#' 
#' Computes estimated effects, standard errors and process variance for a set
#' of estimates using the method of moments estimator described by Burnham and
#' White (2002). The \code{design} matrix specifies the manner in which the
#' estimates (\code{theta}) are combined.  The number of rows of the design
#' matrix must match the length of \code{theta}.  To get a mean estimate use a
#' column matrix of 1's (e.g.,
#' \code{design=matrix(1,ncol=1,nrow=length(theta))}. The function returns a
#' list with the estimates of the coefficients for the design matrix
#' (\code{beta}) with one value per column in the design matrix and the
#' variance-covariance matrix (\code{vcv.beta}) for the \code{beta} estimates.
#' The process variance is returned as \code{sigma}.
#' 
#' @param theta vector of parameter estimates
#' @param design design matrix for combining parameter estimates
#' @param vcv estimated variance-covariance matrix for parameters
#' @param upper upper limit for process variance
#' @param LAPACK argument passed to call to \code{qr} for qr decomposition and
#' inversion
#' @return A list with the following elements \item{sigma}{process variance
#' estimate} \item{beta}{dataframe with estimates and standard errors of betas
#' for design} \item{betarand}{dataframe of shrinkage estimates}
#'  \item{vcv.beta}{variance-covariance matrix for beta} \item{GTrace}{trace of matrix G}
#' @author Jeff Laake; Ben Augustine
#' @export
#' @import matrixcalc
#' @references BURNHAM, K. P. and G. C. WHITE. 2002. Evaluation of some random
#' effects methodology applicable to bird ringing data.  Journal of Applied
#' Statistics 29: 245-264.
#' @examples
#' \donttest{
#' data(dipper)
#' md=mark(dipper,model.parameters=list(Phi=list(formula=~time)))
#' md$results$AICc
#' zz=get.real(md,"Phi",vcv=TRUE)
#' z=zz$estimates$estimate[1:6]
#' vcv=zz$vcv.real
#' varc=var.components(z,design=matrix(rep(1,length(z)),ncol=1),vcv) 
#' df=md$design.data$Phi
#' shrinkest=data.frame(time=1:6,value=varc$betarand$estimate)
#' df=merge(df,shrinkest,by="time")
#' md=mark(dipper,model.parameters=list(Phi=list(formula=~time,fixed=list(index=df$par.index,value=df$value))),adjust=FALSE)
#' npar=md$results$npar+varc$GTrace
#' md$results$lnl+2*(npar + (npar*(npar+1))/(md$results$n-npar-1))
#' }
var.components=function (theta, design, vcv, alpha=0.05, upper=10*max(vcv), LAPACK = TRUE) 
{
	if (nrow(design) != length(theta)) 
		stop("Number of rows of design matrix must match length of theta vector")
	if (nrow(vcv) != length(theta)) 
		stop("Number of rows of vcv matrix must match length of theta vector")
	if (ncol(vcv) != length(theta)) 
		stop("Number of columns of vcv matrix must match length of theta vector")
	if (length(theta) <= ncol(design)) 
		stop("Length theta must exceed number of columns of design")
#
#  Reduce theta, design and vcv to the theta's that are used in the design
#   
	rn = (1:nrow(design))[apply(design, 1, function(x) any(as.numeric(x) != 
										0))]
	theta = theta[rn]
	design = design[rn, , drop = FALSE]
	vcv = vcv[rn, rn]
	sigma = 0
# This is equation (1) in B&W(2002); changed to use qr solution for inverse
	beta.hat = function(Dinv, X, theta) return(solve(qr(t(X) %*% 
												Dinv %*% X, LAPACK = LAPACK)) %*% (t(X) %*% Dinv %*% 
							theta))
# This computes inverse of D; changed to use qr solution for inverse
	compute.Dinv = function(sigma, vcv) {
		K = nrow(vcv)
		sigmamat = matrix(0, nrow = K, ncol = K)
		diag(sigmamat) = sigma
		return(solve(qr(vcv + sigmamat, LAPACK = LAPACK)))
	}
# This is equation (2) in B&W(2002)
	mom.sig = function(sigma, vcv, X, theta) {
		Dinv = compute.Dinv(sigma, vcv)
		beta = beta.hat(Dinv, X, theta)
		return(t(theta - X %*% beta) %*% Dinv %*% (theta - X %*% 
							beta) - (nrow(X) - length(beta)))
	}
# Uses uniroot to compute value of sigma by finding root of mom.sig  If it results in an error or the root is negative, the
# estimate of sigma is 0
	soln = try(uniroot(mom.sig, lower = -(min(Re(eigen(vcv)$values)) + 
								1e-12), upper = upper, vcv = vcv, theta = theta, X = design, 
					tol = 1e-15))
	if (class(soln) == "try-error") 
		sigma = 0
	else if (soln$root < 0) 
		sigma = 0
	else sigma = soln$root
# Compute final values of D inverse, beta and its v-c matrix and return those values
	Dinv = compute.Dinv(sigma, vcv)
	beta = beta.hat(Dinv, design, theta)
#Begin Augustine additions
	eig=eigen(Dinv)
	Dinvsqrt=eig$vectors %*% diag(sqrt(eig$values)) %*% qr.solve(eig$vectors)
	H=sqrt(sigma)*Dinvsqrt
	X=rep(1,length(theta))
	A=X%*%qr.solve(t(X)%*%Dinv%*%X)%*%t(X)
	I=diag(1,length(theta))
	G=H+(I-H)%*%A%*%Dinv
	betarand=G%*%theta
	VCbetarand=G%*%vcv%*%t(G)
	RMSE=sqrt(diag(VCbetarand)+(betarand-theta)^2)
	betarandL=betarand-qnorm(1-alpha/2,0,1)*RMSE
	betarandU=betarand+qnorm(1-alpha/2,0,1)*RMSE
	mom.sigCI = function(sigma, vcv, X, theta,p) {
		Dinv = compute.Dinv(sigma, vcv)
		beta = beta.hat(Dinv, X, theta)
		return(t(theta - X %*% beta) %*% Dinv %*% (theta - X %*% 
							beta) - qchisq(p,nrow(X) - length(beta)))
	}
	solnL = try(uniroot(mom.sigCI, lower = -(min(Re(eigen(vcv)$values)) + 
								1e-12), upper = upper, vcv = vcv, theta = theta, X = design, p=1-alpha/2,
					tol = 1e-15))
	solnU = try(uniroot(mom.sigCI, lower = -(min(Re(eigen(vcv)$values)) + 
								1e-12), upper = upper, vcv = vcv, theta = theta, X = design, p=alpha/2,
					tol = 1e-15))
	betarand=cbind(betarand,RMSE,betarandL,betarandU)
	colnames(betarand)=c("estimate","RMSE","lower","upper")
	sigma = c(sigma,solnL$root,solnU$root)
	names(sigma)=c("est.","lower","upper")
#end Augustine additions
	rownames(beta) = colnames(design)
	vcv.beta = solve(qr(t(design) %*% Dinv %*% design, LAPACK = LAPACK))
	rownames(vcv.beta) = colnames(design)
	colnames(vcv.beta) = colnames(design)
	beta = as.data.frame(beta)
	beta$se = sqrt(diag(vcv.beta))
	names(beta) = c("Estimate", "SE")
	return(list(sigma = sigma, beta = beta,betarand=as.data.frame(betarand),vcv.beta = vcv.beta, GTrace=matrix.trace(G)))
}
