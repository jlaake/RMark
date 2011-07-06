merge.mark=function(...,table=TRUE)
{
#
# Merges an unspecified number of marklist or mark model objects
# together into a marklist object with a model.table if model.table is TRUE.
#
#  Get arguments specified in ... and get their names
#
  args=list(...)
  names(args) = sapply(match.call(expand.dots = FALSE)$..., as.character)

#
# Create an empty list and loop over each value in the arguments
#
  newlist=list()
  for(i in 1:length(args))
  {
#
#    If the object is a single mark object add it to the list
#
     if((class(args[[i]])[1])=="mark")
     {
        newnames=c(names(newlist),names(args)[i])
        newlist=c(newlist,args[i])
        names(newlist)=newnames
     }
#
#    If it is a marklist, add each of the models in the list
#
     else
        if(class(args[[i]])[1]=="marklist")
        {
           if(!is.null(args[[i]]$model.table))
              args[[i]]$model.table=NULL
           newlist=c(newlist,args[[i]])
        }
#
#       If the argument is neither a mark object or marklist object then give
#       an error message and skip over it.
#
        else
           cat(paste("Objects must be of class mark or marklist.",names(args)[i],"ignored"))
  }
#
# Add a model table for the mark models if requested; assign class of marklist and
# return the result
#
  if(table) newlist=c(newlist,model.table=list(model.table(newlist)))
  class(newlist)="marklist"
  return(newlist)
}
