"collect.models" <-
function(lx=NULL,type=NULL,table=TRUE,adjust=TRUE,external=FALSE)
{
#
# collect.models - collects models in list lx of specified type (if any)
#                  and returns models in a list with a table of model
#                  results if table=TRUE
#
# Arguments:
#
#  lx       - if NULL, constructs list from frame of calling function
#             otherwise it uses list of names
#
#  type     - a character string specifying type of mark models (e.g., "CJS");
#             if NULL, all types are used
# 
#  table    - if TRUE, a table of model results is also included in list
#
#  adjust   - if TRUE, adjusts number of parameters to full rank (# of columns of
#              design matrix.
#  external - if TRUE the mark objects are saved externally rather than in the 
#              resulting marklist; the filename for each object is kept in its place
#
#  Value: 
#
#  model.list - a list of mark model objects and a table of model results
#               if table=TRUE
#
# Functions used: collect.model.names, model.table
#
#
# If lx=NULL, collect names of objects in parent frame
#
if(is.null(lx))lx=ls(envir=parent.frame())
#
# Collect names of mark models of specified type (if any) from lx
# 9 Jan 06; pulled stop from collect.model.names and put it here
#
x=collect.model.names(lx,type)
if(is.null(x))stop()
#
# Create list of mark model objects
#
z=eval(parse(text=paste("list(",paste(paste(x,"=",x,sep=""),collapse=","),")")),envir=parent.frame())
#
# Handle external objects
#
if(external)
  for (i in 1:length(z))
    if(is.list(z[[i]]))
    {
       model=z[[i]]
       marksave=paste(z[[i]]$output,".rda",sep="")
       save(model,file=marksave)                                  
       class(marksave)=class(model)
       z[[i]]=marksave
    }
#
# Return list with model table if requested
# 9 Jan 06; reordered model.table in return; also gave class of marklist to
# enable cleanup function
#
if(table)
    z$model.table=model.table(x,type,pf=2,adjust=adjust)
class(z)="marklist"
return(z)
}

