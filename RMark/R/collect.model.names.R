"collect.model.names" <-
function(lx, type=NULL, warning=TRUE)
# ----------------------------------------------------------------------------------------
#
# collect.model.names - this function collects mark models of a particular type (eg "CJS")
#                       or if type is NULL it collects all MARK model names from a list of 
#                       objects (lx) collected from the parent environment
#                       of the function that calls this one (ie parent.frame(2) two back from here).
#                  
# Arguments:        
#
#  lx             - listing of R objects
#  type           - either NULL(all models) or a specific model type (eg "CJS")
#  warning        - if TRUE, gives message that models not found
#
# Value:
#
#  model.list     - a list of MARK model names to use
#
# Functions used: setup.model
#
# ----------------------------------------------------------------------------------------
{
#
# Make sure type is correct if not NULL and assign to model
#
if(!is.null(type)) setup.model(type,0)
#
# Collect models from parent.frame of a particular "type" if any.
#
model.list=NULL
exclude=grep("\\*",lx)
if(length(exclude)>0)lx=lx[-exclude]
for (i in 1:length(lx)) 
{
   classval=class(eval(parse(text=lx[i]),envir=parent.frame(2)))
   if(classval[1]=="mark")
     if(is.null(type))
        model.list=c(model.list,lx[i])
     else
        if(classval[2]==type)
           model.list=c(model.list,lx[i])
}
#
# 09 Jan 06; Changed stops to cat statements and return NULL if none found
#
if(length(model.list)==0)
{
  if(warning)
  {
     if(is.null(type))
        cat(paste("\nNo mark models found\n"))
     else
        cat(paste("\nNo",type,"mark models found\n"))
  }
  return(NULL)
}
else
   return(model.list)
}
