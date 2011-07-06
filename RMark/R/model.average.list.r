model.average.list=function(x,revised=TRUE,...)
{
#  A generic function to compute model averaged estimates and their std errors or variance-covariance matrix
#
#  Arguments:
#     x - a list with the following elements:
#            1) a vector or matrix named estimate which is the estimates to be averaged
#            2) a vector of model selection criterion values named AIC,AICc,QAIC,QAICc or a weight
#            3  a vector or matrix named se with the model-specific std errors or a list of matrices named vcv 
#                 which give the model-specific variance-covariance matrices.
#    revised - it TRUE uses eq 6.12 in B&A and if FALSE it uses eq 4.9
#
#  Value: a list with elements
#      estimate - vector of model averaged estimates
#      se       - vector of std errors for model averaged estimates
#      vcv      - var-cov matrix if vcv was given for each element of x
#
  if(is.null(x$estimate)) stop("The list must contain an element named estimate")
  if(is.null(x$se)&is.null(x$vcv)) stop("The list must contain an element named se or vcv")
  if(is.null(x$AIC)&is.null(x$AICc)&is.null(x$QAIC)&is.null(x$QAICc)&is.null(x$weight))
     stop("The list must contain an element with a value for model selection named AIC,AICc,QAIC,QAICc or weight")
  if(is.vector(x$estimate))
     x$estimate=matrix(x$estimate,ncol=1)     
  else
    if(!is.matrix(x$estimate)) stop("estimate is neither a vector or matrix")
  if(!is.null(x$se))
  {
     if(is.vector(x$se))
        x$se=matrix(x$se,ncol=1)     
     else
        if(!is.matrix(x$se)) stop("se is neither a vector or matrix")
     if(!all(dim(x$se)==dim(x$estimate))) stop("dimensions of estimate and se must match") 
  }   
  if(!is.null(x$vcv))
  {
     if(!is.list(x$vcv))stop("vcv must be a list")
     if(!is.matrix(x$vcv[[1]]))stop("each element of vcv must be a matrix")
     if(length(x$vcv)!=nrow(x$estimate))stop("number of vcv matrices does not match dimension (rows) of estimate")
     for(i in 1:length(x$vcv))
        if(all(dim(x$vcv[[i]])!=ncol(x$estimate)))stop("dimension of one or more vcv matrices does not match dimenstion (columns) of estimate")
  }   
  xnames=names(x)
  estimates=x$estimate
  if("weight" %in% xnames)
  {
     criterion=which("weight" %in% xnames)
     weights=x$weight
     weights=weights/sum(weights)
     if(length(weights)!=nrow(estimates))stop("number of weights does not match dimension of estimate")   
  }
  else
  {
     criterion=which(xnames %in% c("AIC","AICc","QAIC","QAICc"))
     if(length(criterion)>1) stop("More than one model selection criterion value specified")
     weights=x[[criterion]]
     if(length(weights)!=nrow(estimates))stop("number of weights does not match dimension of estimate")
     weights=weights-min(weights)
     weights=exp(-.5*weights)
     weights=weights/sum(weights)
  }
  estimate=colSums(estimates*weights)
  if("vcv" %in% xnames)
  {
     se=t(sapply(x$vcv,function(x) sqrt(diag(x))))
     if(revised)
        se=sqrt(apply((se^2+(t(t(estimates)-estimate))^2)*weights,2,sum,na.rm=TRUE))
     else 
        se=apply(sqrt(se^2+(t(t(estimates)-estimate))^2)*weights,2,sum,na.rm=TRUE)
     cor=matrix(0,nrow=length(estimate),ncol=length(estimate))
     for (i in 1:length(x$vcv))
     {
       xse=sqrt(diag(x$vcv[[i]]))
       cor=cor+weights[i]*x$vcv[[i]]/outer(xse,xse,"*")
     }
     vcv=cor*outer(se,se,"*")
     return(list(estimate=estimate,se=se,vcv=vcv))
   }
   else
   {
     se=apply(sqrt(x$se^2+(t(t(estimates)-estimate))^2)*weights,2,sum,na.rm=TRUE)
     return(list(estimate=estimate,se=se))
   }
}
