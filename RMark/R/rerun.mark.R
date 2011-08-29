"rerun.mark" <-
function(model,data,ddl,initial,output=TRUE,title="",invisible=TRUE,adjust=TRUE,se=FALSE,simplify=TRUE,
 filename=NULL,prefix="mark",default.fixed=TRUE,silent=FALSE,retry=0,realvcv=FALSE,external=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
# rerun.mark -  reruns previous mark model with different initial values
#
# Arguments:
#
#  model                - previously run mark model
#  data                 - processed dataframe
#  ddl                  - design data list which contains an element for each parameter type
#  initial              - vector of initial values for beta parameters
#  output               - if TRUE produces summary of model input and model output
#  invisible            - if TRUE, window for running MARK is hidden
#  adjust               - if TRUE, adjusts npar to # of cols in design matrix, modifies AIC and records both
#  se                   - if TRUE, se and confidence intervals are shown in summary sent to screen
#  simplify             - if TRUE, simplifies PIM structure to match unique number of rows in design matrix
#  filename             - base filename for MARK input and output files
#  prefix               - base filename prefix; default is "mark" for files named marknnn.*
#  default.fixed        - if TRUE, default fixed values are assigned to any parameters missing from the full design data
#  silent               - if TRUE, errors that are encountered are suppressed
#  retry                - number of reanalyses to perform with new starting values when one or more parameters are singular
#  realvcv              - if TRUE the vcv matrix of the real parameters is extracted and stored in the model results
#  external             - if TRUE the mark object is saved externally rather than in the workspace; the filename is kept in its place
#
#  Value: 
#
#  model - a MARK object model containing output and extracted results
#
#  Functions used: make.mark.model, run.mark.model, summary.mark
# 
# -----------------------------------------------------------------------------------------------------------------------
#
#  If the data haven't been processed (data$data is NULL) do it now with specified or default arguments
# 
model=load.model(model)
if(is.null(data$data))
   stop("\nMust specify processed dataframe\n")
#
# If the design data have not been specified, stop
#
if(is.null(ddl))
   stop("\nMust specify design data list\n")
#
#  Assign model.parameters
#
model.parameters=model$model.parameters
#
# Run model as many as times requested if needed
#
i=0
converge=FALSE
while(i<=retry & !converge)
{
#
# Remake the model with new initial values
#

   model<-make.mark.model(data,title=title,parameters=model.parameters,
          ddl=ddl,initial=initial,call=match.call(),simplify=simplify,default.fixed=default.fixed,
          model.name=model$model.name)
   model$model.parameters=model.parameters
#
# Summarize model input if output=TRUE
#
   if(output & i==1)
   {
     cat("\n")
     print(summary(model))
   }
#
# Run model
#
   runmodel<-try(run.mark.model(model,invisible=invisible,adjust=adjust,filename=filename,prefix=prefix,realvcv=realvcv),silent=silent)
   if(class(runmodel)[1]=="try-error")
     stop("\n\n********Following model failed to run :",model$model.name,"**********\n\n")
   else
   {
#
#  Check if any parameters are singular and if retry=TRUE, the refit model with
#  new initial values
#
      if(retry>0 && !is.null(runmodel$results$singular))
      {
         cat("\nRe-running analysis with new starting values\n")
         i=i+1
         converge=FALSE
         initial=runmodel$results$beta$estimate
         initial[runmodel$results$singular]=0
         next
      }
      else
         converge=TRUE
    }
}
#
# Summarize model results if output=TRUE
#
   if(output)
   {
      cat("\n")
      print(summary(runmodel,se=se))
   }
#
# Return fitted MARK model object 
#
   if(external)
   {
     marksave=paste(runmodel$output,".rda",sep="")
     model=runmodel
     save(model,file=marksave)
     class(marksave)=class(runmodel)
     return(marksave)
   } else
     return(runmodel)
}
