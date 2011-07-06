"run.models" <-
function(model.list=NULL,type=NULL, save=TRUE, ...)
# -----------------------------------------------------------------------------------------------------------------------
#
# run.models   - runs either a list of models as defined in model.list or runs all
#                defined models with no $output defined for all MARK objects (model.list=NULL)
#                of just those of a particular type (eg type="CJS")
#
# The model objects are run and saved in the calling environment. If save=TRUE, a save.image() is
# done between runs in case there is a problem.
#
# Arguments:
#
#   model.list  - either a vector of model names to run or NULL to run all MARK models
#   type        - either a model type (eg "CJS", "Burnham" or "Barker") or NULL for all types
#   save        - if TRUE a save.image() is done between analyses
#   ...         - additonal parameters for call to mark
#
# Value: 
#   None
#
# Functions used: collect.model.names, run.mark.model  
#
# -----------------------------------------------------------------------------------------------------------------------
{
#
# Get a list of objects from calling frame
#  
  lx=ls(envir=parent.frame())
#
# Collect appropriate MARK models from frame list unless a list has already been given
#
  model.list=collect.model.names(lx, type)
  if(is.null(model.list)) stop("No models need to be run")
  run=FALSE
#
#  For each model in the list, run the model unless it already contains output
#
  for(i in 1:length(model.list))
  {
     model=eval(parse(text=model.list[i]),envir=parent.frame())
     if(is.null(model$output))
     {
        run=TRUE
        eval(parse(text=paste(model.list[i],"=run.mark.model(",model.list[i],"...)")),envir=parent.frame())
        if(save)save.image()
     }
  }
  if(!run)cat("All defined models have been run\n")
invisible()
}
