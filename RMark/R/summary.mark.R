"summary.mark" <-function(object,...,se=FALSE,vc=FALSE,showall=TRUE,show.fixed=FALSE,
                    brief=FALSE)
{
# -------------------------------------------------------------------------------------------------------------
#
# summary.mark  - creates a summary of either a MARK model input or model output
#
# Arguments:
#   object    - a MARK model object
#   se        - if FALSE the real parameter estimates are output in PIM (eg. triangular format); if TRUE, they
#                are displayed as a list with se and confidence interval
#   vc        - if TRUE the v-c matrix of the betas is printed
#   showall   - if FALSE only unique real parameters are sumamrized; added 10 Jan 06
#   show.fixed- if TRUE fixed values are returned rather than NA in place of fixed values
#                 if se=TRUE, show.fixed default is TRUE, otherwise FALSE
#   brief     - if TRUE, does not show reals
#
# Value:
#   a list of summary values
#
# Functions used: get.real, setup.parameters
#
# -------------------------------------------------------------------------------------------------------------
#
#
  model=load.model(object)
  if(se & missing(show.fixed))show.fixed=TRUE
#
# Display baseline info about model (type, name, title etc)
#
x=list(model=model$model,title=model$title,model.name=model$model.name,model.call=model$call)
#
# If displaying model input only show call 
#
if(is.null(model$output))
{
  class(x)="summary.mark"
  return(x)
}
#
# If summarizing model output, show num of parameters, deviance, AICc
#
x$npar=model$results$npar
if(!is.null(model$results$npar.unadjusted))
   x$npar.unadjusted=model$results$npar.unadjusted
   x$lnl=model$results$lnl
   x$AICc=model$results$AICc
if(!is.null(model$results$npar.unadjusted))
   x$AICc.unadjusted=model$results$AICc.unadjusted
if(is.null(model$chat))
    chat=1
else
    chat=model$chat
if(chat!=1)
{
   K=model$results$npar
   qaicc= model$results$lnl/chat + 2*K + 2*K*(K+1)/(model$results$n-K-1)
   x$chat=chat
   x$qAICc=qaicc
   if(!is.null(model$results$AICc.unadjusted))
    {
       K=model$results$npar.unadjusted
       qaicc.unadjusted= model$results$lnl/chat + 2*K + 2*K*(K+1)/(model$results$n-K-1)
       x$qAICc.unadjusted=qaicc.unadjusted
    }
}
#
# Display beta coefficients and optionally its v-c matrix
#
  x$beta=coef(model)
if(vc)
{
    vcv=model$results$beta.vcv*chat
    row.names(vcv)=row.names(model$results$beta)
    colnames(vcv)=row.names(vcv)
    x$vcv=vcv
 }
if(!brief)
{
#
# For each parameter type in the model, display the real parameters (by group if any) as either a list
# or in PIM format (se=FALSE)
#
   parameters=model$parameters
   parameter.names=names(parameters)
   x$reals=vector("list",length=length(parameter.names))
   for(i in 1:length(parameter.names))
   {
      x$reals[[i]]=get.real(model,parameter.names[i],se=se,show.fixed=show.fixed)
      if(se)
      {
        if(!showall)
             x$reals[[i]]=x$reals[[i]][!duplicated(x$reals[[i]]$par.index),,drop=FALSE]
        if(!show.fixed)x$reals[[i]]=x$reals[[i]][x$reals[[i]]$fixed!="Fixed",]
      }
      if(parameters[[i]]$type=="Triang" && parameters[[i]]$pim.type=="time"&&!se)
        for (j in 1:length(x$reals[[i]]))
        {
           row.matrix=matrix(x$reals[[i]][[j]]$pim[1,],nrow=1)
           colnames(row.matrix)=colnames(x$reals[[i]][[j]]$pim)
           rownames(row.matrix)=model$begin.time[min(j,length(model$begin.time))]
           x$reals[[i]][[j]]$pim=row.matrix
        }
   }
   names(x$reals)=parameter.names
}
x$brief=brief
class(x)="summary.mark"
return(x)
}
coef.mark=function(object,...)
{
   model=load.model(object)
   if(is.null(model$chat))
       chat=1
   else
       chat=model$chat
   if(chat==1)
       beta=model$results$beta
   else
   {
       beta=model$results$beta
       beta$se=sqrt(chat)*beta$se
       beta$lcl=beta$estimate - 1.96*beta$se
       beta$ucl=beta$estimate + 1.96*beta$se
   }
   return(beta)
}


