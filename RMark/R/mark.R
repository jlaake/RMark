"mark" <-
function(data,ddl=NULL,begin.time=1,model.name=NULL,model="CJS",title="",model.parameters=list(),initial=NULL,
design.parameters=list(), right=TRUE, groups = NULL, age.var = NULL, initial.ages = 0, age.unit = 1, time.intervals = NULL,nocc=NULL,output=TRUE,
invisible=TRUE,adjust=TRUE,mixtures=1,se=FALSE,simplify=TRUE,filename=NULL,prefix="mark",default.fixed=TRUE,silent=FALSE,retry=0,options=NULL,brief=FALSE,
realvcv=FALSE,delete=FALSE,external=FALSE,profile.int=FALSE,chat=NULL,reverse=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
# mark -  a single function that processes data, creates the design data, makes the mark model and runs it.
#
# Arguments:
#
#  data                 - either the raw data which is a dataframe with at least one column named 
#                         ch which is a character field containing the capture history or a processed dataframe
#  ddl                  - design data list which contains an element for each parameter type; if NULL it is created
#  begin.time           - time of first capture(release) occasion
#  model.name           - optional model name
#  model                - type of c-r model (eg CJS, Burnham, Barker) 
#  title                - a title for the analysis 
#  model.parameters     - list of parameter model specifications
#  initial              - vector of initial values for beta parameters
#  design.parameters    - specification of any grouping variables for design data for each parameter
#  right                - if TRUE, any intervals created in design.parameters are closed on the right and open on left and vice-versa if FALSE
#  groups               - list of factors for creating groups
#  age.var              - index in groups of a variable that represents age
#  initial.ages         - an initial age for each age group
#  age.unit             - increment of age for each increment of time
#  time.intervals       - intervals of time between the capture occasions
#  nocc                 - number of occasions specification for Nest type
#  output               - if TRUE produces summary of model input and model output
#  invisible            - if TRUE, window for running MARK is hidden
#  adjust               - if TRUE, adjusts npar to # of cols in design matrix, modifies AIC and records both
#  mixtures             - # of mixtures for heterogeneity model
#  se                   - if TRUE, se and confidence intervals are shown in summary sent to screen
#  simplify             - if TRUE, simplifies PIM structure to match unique number of rows in design matrix
#  filename             - base filename for MARK input and output files (filename.* - no numeric sequence is added)
#  prefix               - base filename prefix for MARK input and output files; eg mark001.* etc
#  default.fixed        - if TRUE, default fixed values are assigned to any parameters missing from the full design data
#  silent               - if TRUE, errors that are encountered are suppressed
#  retry                - number of reanalyses to perform with new starting values when one or more parameters are singular
#  options              - character string of options for Proc Estimate statement in MARK .inp file
#  brief                - if TRUE and output=TRUE only gives a brief summary of model
#  realvcv              - if TRUE the vcv matrix of the real parameters is extracted and stored in the model results
#  delete               - if TRUE the output files are deleted after the results are extracted
#  external             - if TRUE the mark object is saved externally rather than in the workspace; the filename is kept in its place
#  profile.int          - if TRUE will request profile intervals for each real parameter; or you can give a vector of real parameter indices
#  chat                 - value of chat used for profile intervals
#
#  Value: 
#
#  model - a MARK object model containing output and extracted results
#
#  Functions used: process.data, make.design.data, make.mark.model, run.mark.model, summary.mark
# 
# -----------------------------------------------------------------------------------------------------------------------
#
#  If the data haven't been processed (data$data is NULL) do it now with specified or default arguments
# 
if(is.null(data$data))
{
   if(!is.null(ddl))
   {
      cat("Warning: specification of ddl ignored, as data have not been processed\n")
      ddl=NULL
   }
   data.proc=process.data(data,begin.time=begin.time, model=model,mixtures=mixtures, 
                          groups = groups, age.var = age.var, initial.ages = initial.ages, 
                          age.unit = age.unit, time.intervals = time.intervals,nocc=nocc,reverse=reverse)
}   
else
   data.proc=data
#
# If the design data have not been constructed, do so now
#
if(is.null(ddl)) ddl=make.design.data(data.proc,design.parameters,right=right)
#
# Run model as many as times requested if needed
#
i=0
converge=FALSE
while(i<=retry & !converge)
{
#
# Make the model with specified or default parameters
#
   if(is.list(model.parameters))
   {
      model<-make.mark.model(data.proc,title=title,parameters=model.parameters,
             ddl=ddl,initial=initial,call=match.call(),simplify=simplify,default.fixed=default.fixed,
             model.name=model.name,options=options,profile.int=profile.int,chat=chat)
      model$model.parameters=model.parameters
   }
   else
      stop("Model parameters must be specified as a list")
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
   runmodel<-try(run.mark.model(model,invisible=invisible,adjust=adjust,filename=filename,prefix=prefix,realvcv=realvcv,delete=delete),silent=silent)
   if(class(runmodel)[1]=="try-error")
   {
     cat("\n\n********Following model failed to run :",model$model.name,"**********\n\n")
     return(model)
   }
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
      if(!brief)
      {
         cat("\n")
         print(summary(runmodel,se=se))
      }
      else
      {
         sum.res=summary(runmodel)
         cat(paste("\n Model:",sum.res$model.name," npar=",sum.res$npar," lnl = ",sum.res$lnl,"AICc =",sum.res$AICc))
      }
   }
#
# Return fitted MARK model object or if external, return character string with same class and save file
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
