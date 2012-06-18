#' Compute model averaged estimates of real parameters from a list structure
#' for estimates
#' 
#' A generic function to compute model averaged estimates and their standard
#' errors or variance-covariance matrix
#' 
#' If a single estimate is being model-averaged then \code{estimate} and
#' \code{se} are vectors with an entry for each model.  However, if there are
#' several estimatee being averaged then both \code{estimate} and \code{se}
#' should be matrices in which the estimates for each model are a row in the
#' matrix.  Regardless, if \code{vcv} is specified it should be a list of
#' matrices and in the case of a single estimate, each matrix is 1x1 containing
#' the estimated sample-variance but that would be rather useless and \code{se}
#' should be used instead.
#' 
#' If the list contains an element named \code{AIC},\code{AICc},\code{QAIC}, or
#' \code{QAICc}, then the minimum value is computed and subtracted to compute
#' delta values relative to the minimum. These are then converted to Akaike
#' weights which are \code{exp(-.5*delta)} and these are normalized to sum to
#' 1.  If the list does not contain one of the above values then it should have
#' a variable named \code{weight}.  It is normalized to 1.  The model-averaged
#' estimates are computed using equation 4.1 of Burnham and Anderson (2002).
#' 
#' If the contains a matrix named \code{vcv}, then a model-averaged
#' variance-covariance matrix is computed using formulae given on page 163 of
#' Burnham and Anderson (2002).  If there is no element named \code{vcv} then
#' there must be an element \code{se} which contains the model-specific
#' estimates of the standard errors.  The unconditional standard error for the
#' model-averaged estimates is computed using equation 4.9 of Burnham and
#' Anderson (2002) if if \code{revised=FALSE}; otherwise it uses eq 6.12.
#' 
#' @usage \method{model.average}{list}(x, revised=TRUE,...)
#' @S3method model.average list
#' @param x a list containing the following elements: 1) \code{estimate} - a
#' vector or matrix of estimates, 2)a vector of model selection criterion value
#' named \code{AIC,AICc,QAIC,QAICc} or a \code{weight} variable that sums to 1
#' across models, and 3) a vector or matrix named \code{se} which give the
#' model-specific standard errors for each estimate or a list of matrices named
#' \code{vcv} which give the model-specific variance-covariance matrices.
#' @param revised if TRUE it uses eq 6.12 from Burnham and Anderson (2002) for
#' model averaged se; otherwise it uses eq 4.9
#' @param ... additional arguments passed to specific functions
#' @return A list containing elements: \item{estimate}{vector of model-averaged
#' estimates} \item{se}{vector of unconditional standard errors (square root of
#' unconditional variance estimator)} \item{vcv}{model-averaged
#' variance-covariance matrix if \code{vcv} was specified input list}
#' @author Jeff Laake
#' @seealso \code{\link{model.average.marklist}}
#' @references BURNHAM, K. P., AND D. R. ANDERSON. 2002. Model selection and
#' multimodel inference. A practical information-theoretic approach. Springer,
#' New York.
#' @keywords utility
#' @examples
#' \donttest{
#' # Create a set of models from dipper data
#' data(dipper)
#' run.dipper=function()
#' {
#' dipper$nsex=as.numeric(dipper$sex)-1
#' mod1=mark(dipper,groups="sex",
#'    model.parameters=list(Phi=list(formula=~sex)))
#' mod2=mark(dipper,groups="sex",
#'    model.parameters=list(Phi=list(formula=~1)))
#' mod3=mark(dipper,groups="sex",
#'    model.parameters=list(p=list(formula=~time),
#'    Phi=list(formula=~1)))
#' dipper.list=collect.models()
#' return(dipper.list)
#' }
#' dipper.results=run.dipper()
#' # Extract indices for first year survival from 
#' #  Females (group 1) and Males (group 2)
#' Phi.indices=extract.indices(dipper.results[[1]],
#'    "Phi",df=data.frame(group=c(1,2),row=c(1,1),col=c(1,1)))
#' # Create a matrix for estimates
#' estimate=matrix(0,ncol=length(Phi.indices),
#'     nrow=nrow(dipper.results$model.table))
#' # Extract weights for models
#' weight=dipper.results$model.table$weight
#' # Create an empty list for vcv matrices
#' vcv=vector("list",length=nrow(dipper.results$model.table))
#' # Loop over each model in model.table for dipper.results
#' for (i in 1:nrow(dipper.results$model.table))
#' {
#' # The actual model number is the row number for the model.table
#'   model.numbers= as.numeric(row.names(dipper.results$model.table))
#' # For each model extract those real parameter values and their
#' # vcv matrix and store them
#'   x=covariate.predictions(dipper.results[[model.numbers[i]]],
#'       data=data.frame(index=Phi.indices))
#'   estimate[i,]=x$estimates$estimate
#'   vcv[[i]]=x$vcv
#' }
#' # Call model.average using the list structure which includes 
#' #  estimate, weight and vcv list in this case
#' model.average(list(estimate=estimate,weight=weight,vcv=vcv))
#' #
#' # Now get same model averaged estimates using model.average.marklist
#' # Obviously this is a much easier approach and what would be used 
#' # if all you are doing is model averaging real parameters in the model.  
#' # The other form is more useful for model averaging
#' # functions of estimates of the real parameters (eg population estimate)
#' #
#' mavg=model.average(dipper.results,"Phi",vcv=TRUE)
#' print(mavg$estimates[Phi.indices,])
#' print(mavg$vcv.real[Phi.indices,Phi.indices])
#' }
#' 
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
