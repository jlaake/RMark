"setup.parameters" <-
		function(model,parameters=list(),nocc=NULL,check=FALSE,number.of.groups=1)
# ----------------------------------------------------------------------------------------
#  setup.parameters  - fills in value for begin and num for each parameter type depending
#                      on the type of c-r model. num defines number of parameters relative to
#                      number of occasions.  begin defines the first occasion which is relevant
#                      to the parameter
#
#  Arguments:
#    model      - type of model ("CJS", "Burnham" etc)
#    parameters - list of model parameter specifications
#    nocc       - number of occasions (value only specified if needed)
#    check      - default is FALSE; if TRUE it only returns list of parameter names
#
#  Value:
#    parameters - updated list of model parameter specifications with new fields added
#
#
# ----------------------------------------------------------------------------------------
{
# Read in parameter definitions
	fdir=system.file(package="RMark")	
	fdir=file.path(fdir,"parameters.txt")	
	parameter_definitions=read.delim(fdir,header=TRUE,
			colClasses=c("character","character",rep("numeric",3),rep("character",4),
					"logical","character","logical","numeric",rep("logical",4)))
#
#  Create valid parameter list depending on model.
#
	parameter_definitions=parameter_definitions[parameter_definitions$model==model,]
	par.list=parameter_definitions$parname
#
#  If this is just a parameter check, return par.list
#
	if(check)return(par.list)
#
#  For each parameter create an empty list if none specified in input
#   
	pars=vector("list",length(par.list))
	names(pars)=par.list
	for (i in 1:length(par.list))
	{
		if(par.list[i]%in%names(parameters))
			pars[[i]]=parameters[[par.list[i]]]
	}
#
#  Next depending on model type, assign non-specified default values
#
	for(i in 1:length(par.list))
	{
		for(j in 3:ncol(parameter_definitions))		
			if(!is.na(parameter_definitions[i,j]) & parameter_definitions[i,j]!="" & !names(parameter_definitions)[j]%in%names(parameters[[par.list[i]]]))
				pars[[par.list[i]]][names(parameter_definitions)[j]]=list(parameter_definitions[i,j])
	    if(pars[[par.list[i]]]$formula!=" " && is.character(pars[[par.list[i]]]$formula))
			pars[[par.list[i]]]$formula=as.formula(pars[[par.list[i]]]$formula)
		if(is.null(pars[[par.list[i]]]$num))pars[[par.list[i]]]$num=NA
		if(!is.na(pars[[par.list[i]]]$num)&&pars[[par.list[i]]]$num==1)pars[[par.list[i]]]$num=-(nocc-1)
		if(!is.null(pars[[par.list[i]]]$share) && pars[[par.list[i]]]$share && is.null(pars[[par.list[i]]]$pair)) pars[[par.list[i]]]$share=NULL
	}
	return(pars)
}
