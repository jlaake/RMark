"setup.model" <-
function(model,nocc,mixtures=1)
{
#
# setup.model - defines list of acceptable models and creates some global fields for the model
#
# Arguments:
# 
#   model    - name of model (must be in valid.models)
#   nocc     - length of capture history string
#   mixtures - number of mixtures
#
# Value: 
#
#   model.list - a list with following elements
#                  etype - encounter type string; typically same as model name
#                  nocc  - number of capture occasions
#                  num   - number of time intervals relative to number of occasions (0 or -1)
#                  mixtures - number of mixtures if any
#                  derived - TRUE if model produces derived parameters
#
#
# Read in parameter definitions
	fdir=system.file(package="RMark")	
	fdir=file.path(fdir,"models.txt")	
	model_definitions=read.delim(fdir,header=TRUE,
			colClasses=c("character","character",rep("logical",4),rep("numeric",3)))
    model_def=model_definitions[model_definitions$model==model,]	
    if(nrow(model_def)==0)
        stop("Invalid type of model = ",model," Valid types are\n", paste(model_definitions$model,collapse="\n"))
	if(mixtures==1) 
		model_def$mixtures=model_def$default.mixtures
	else
		model_def$mixtures=mixtures
	model_def$default.mixtures=NULL
	model_def$nocc=nocc/model_def$divisor
	model_def$divisor=NULL
    return(as.list(model_def))
}
