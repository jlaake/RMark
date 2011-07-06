"valid.parameters" <-
function(model,parameters)
# ------------------------------------------------------------------------------------------------
# valid.parameters  - checks to make sure specified parameter is valid for a particular model
#
# 
#  Arguments:
#
#   model      -  type of c-r model ("CJS", "Burnham" etc)
#   parameters -  vector of parameter names (for example "Phi" or "p" or "S")
#
#  Value:
#
#   TRUE if all parameters are acceptable
#   FALSE if one or more are not acceptable
#
#  Functions used:  setup.parameters
#
# ------------------------------------------------------------------------------------------------
{
#
# If parameter list is empty return TRUE
#
  if(length(parameters)==0)return(TRUE)
#
#  For this model type, get the default parameter list
#  
  default.parameters=setup.parameters(model,list(),check=TRUE)
#
#  If this is a list, look through each parameter in turn
#
  if(is.list(parameters))
  {
     for(i in 1:length(parameters))
        if(!(names(parameters)[i]%in% default.parameters))
        {
          cat(paste("\n",names(parameters)[i],"parameter not valid in",model,"model.\nThe following parameters are valid:",paste(default.parameters,collapse=" "),"\n"))
          return(FALSE)
        }
     return(TRUE)
  }
#
# If not a list, look at the particular parameter
#
  else
     if(parameters %in% default.parameters )
        return(TRUE)
     else
     {
          cat(paste("\n",parameters,"parameter not valid in",model,"model.\nThe following parameters are valid:",paste(default.parameters,collapse=" "),"\n"))
          return(FALSE)
     }     
}
