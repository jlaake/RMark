"adjust.parameter.count" <-
function(model,npar)
{
#
# adjust.parameter.count - modifies count of estimated parameters and resulting AICc calculation
#
# Arguments:
#
#  model - mark model object
#  npar  - number of estimated parameters 
#
# Value:
#
#  model - mark model object
#
model=load.model(model)
if(model$results$npar!=npar)
{
   if(npar<=dim(model$results$beta)[1])
   {
      cat("\nNumber of parameters adjusted from ",model$results$npar," to ", npar,"\n") 
      if(is.null(model$results$AICc.unadjusted))
      {
         model$results$AICc.unadjusted=model$results$AICc
         model$results$npar.unadjusted=model$results$npar
         AICc.unadjusted=model$results$AICc
      }    
      else
         AICc.unadjusted=model$results$AICc.unadjusted
      model$results$npar=npar
      AICc=model$results$lnl+ 2*npar +2*npar*(npar+1)/(model$results$n - npar -1)
      model$results$AICc=AICc
      cat("Adjusted AICc = ",AICc,"\nUnadjusted AICc = ", AICc.unadjusted,"\n") 
  }
  else
  {
     cat("Specified number of parameters > number of beta parameters. No adjustment made.\n")
  }
}
else
  cat("Specified number of parameters matches current value. No adjustment made.\n")
return(model)}
