remove.mark=function(marklist,model.numbers)
{
#
# Removes a set of models from a marklist and returns the modified
# marklist
#
# Check validity of arguments
#
  if(class(marklist)[1]!="marklist")
     stop(paste("\n",substitute(marklist), "is not a marklist object\n"))
  if(!is.null(marklist$model.table))
     marklist$model.table=NULL
  if(max(model.numbers)>length(marklist) | min(model.numbers)<1)
     stop("\nInvalid set of model numbers for removal\n")
#
# Remove specified models
#
  marklist=marklist[-model.numbers]
#
# If any models are left, create a model table otherwise return NULL
#
  if(length(marklist)>0)
  {
     marklist$model.table=model.table(marklist)
     class(marklist)="marklist"
     return(marklist)
  }
  else
     return(NULL)
}
