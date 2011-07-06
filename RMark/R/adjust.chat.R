"adjust.value" <- function(field="n",value,model.list)
# ----------------------------------------------------------------------------------------
#
# adjust.value  - adjusts values of field in model list for a collection of models
#
# Arguments:
#
# field         - name of field chat, n etc
# value         - new value of field
# model.list    - a marklist created by collect.models
# 
# Value:  
#
#  model.list with all models given the new chat value and model.table adjusted for chat values
#
# Functions used: collect.model.names
#
# ----------------------------------------------------------------------------------------
{
#
# If no model list specified, collect models from parent.frame; if
# model.list is a list created by collect.models
#
if(!missing(model.list))
{
   if(class(model.list)=="marklist")
   {
      if(names(model.list)[length(model.list)]=="model.table")
         model.list=model.list[1:(length(model.list)-1)]
   }
}
else
{
   stop("A model.list must be given")
}
#
# For each model in the list store the new value of chat in it.
#
for (i in 1:length(model.list))
{
    if(is.character(model.list[[i]]))
    {
       model=load.model(model.list[[i]])
       if(field=="chat")
          model[field]=value
       else
          model[["results"]][field]=value       
       save(model,file=model.list[[i]])
    }
    else
       if(field=="chat")
          model.list[[i]][field]=value
       else
          model.list[[i]][["results"]][field]=value       
}
#
# Next recreate model.table
#
model.list$model.table=model.table(model.list)
class(model.list)="marklist"
return(model.list)
}
"adjust.chat" <- function(chat=1,model.list)
  return(adjust.value("chat",chat,model.list))
  

