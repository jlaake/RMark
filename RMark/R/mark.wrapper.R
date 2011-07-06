"create.model.list"<-function(model)
{
   parameters=setup.parameters(model,check=TRUE)
   model.list=list()
   for(n in parameters)
   {
      vec=ls(pat=paste("^",n,"\\.",sep=""),envir=parent.frame())
      if(length(vec)>0)
         for (i in 1:length(vec))
         {
            if(eval(parse(text=paste("is.list(",vec[i],")",sep="")),envir=parent.frame()))
            {
               if(eval(parse(text=paste("!is.null(",vec[i],"$formula)",sep="")),envir=parent.frame()) |
                 eval(parse(text=paste("!is.null(",vec[i],"[[1]]$formula)",sep="")),envir=parent.frame()))
                  model.list[[n]]=c(model.list[[n]],vec[i])
            }
         }
	 
   }
   if(length(model.list)==0)
     stop("\nNo model specifications found. Use parameter.description notation (e.g., Phi.time)\n")
   if(length(model.list)>1)
   {
      model.list=expand.grid(model.list)
      for (j in 1:dim(model.list)[2])
         model.list[,j]=as.character(model.list[,j])
   }
   else
      model.list=as.data.frame(model.list)
   return(model.list)
}
"mark.wrapper" <-
function(model.list,silent=FALSE,run=TRUE,use.initial=FALSE,initial=NULL,...)
{
# -----------------------------------------------------------------------------------------------------------------------
# mark.wrapper  -  a wrapper for the mark function; it takes all the arguments and passes them onto mark
#
#  Value:
#
#  returns a list of mark models
#
# -----------------------------------------------------------------------------------------------------------------------
initiallist=NULL
if(class(initial)[1]=="marklist")
	if(nrow(initial$model.table)!=nrow(model.list))
		stop("marklist specified for initial argument does not contain same number of models")
	else
		initiallist=initial
model.names=rep(NA,nrow(model.list))
for (i in 1:nrow(model.list))
{
  model.parameters=list()
  for(j in 1:ncol(model.list))
  {
     if(!is.list(eval(parse(text=model.list[i,j]),envir=parent.frame())[[1]]))
        model.parameters[[names(model.list)[j]]]=eval(parse(text=(as.character(model.list[i,j]))),envir=parent.frame())
  }
  for(j in 1:ncol(model.list))
  {
     if(is.list(eval(parse(text=model.list[i,j]),envir=parent.frame())[[1]]))
        model.parameters=c(model.parameters,eval(parse(text=(as.character(model.list[i,j]))),envir=parent.frame()))
  }
  model.name=paste(model.list[i,],collapse=".")
  cat("\n",model.name,"\n")
  if(use.initial)
  {
	  initial=NULL
	  for(j in 1:ncol(model.list))
	  {
		  mindex=match(model.list[i,j],model.list[,j])
		  if(!is.na(mindex)&& !is.na(model.names[mindex]))
		  {
			  estimates=eval(parse(text=paste(model.names[mindex],"$results$beta",sep="")))
			  estimates=estimates[grep(paste(colnames(model.list)[j],":",sep=""),rownames(estimates)),]
              beta=estimates$estimate
			  names(beta)=rownames(estimates)
		      initial=c(initial,beta)
		  }
	  }
  }
  else
    if(!is.null(initiallist)) 
		if(model.name%in%names(initiallist))initial=initiallist[[model.name]]
	else
		initial=NULL
  if(run)
  {
     mymodel=try(mark(model.parameters=model.parameters,initial=initial,...),silent=silent)
  }
  else
  {
     mymodel=try(make.mark.model(parameters=model.parameters,initial=initial,...),silent=silent)
  }
  if(class(mymodel)[1]!="try-error")
  {
    eval(parse(text=paste(model.name,"=mymodel")))
	model.names[i]=model.name
    if(!run)
    {
       cat("\n Design matrix columns: ", dim(mymodel$design.matrix)[2],"\n")
       print(colnames(mymodel$design.matrix))
    }
  }
}
rm(mymodel)
#
# Return fitted MARK model object
#
rm(initial)
if(run)
   return(collect.models())
else
   return(NULL)
}
load.model=function(model)
{ 
  if(is.character(model))
  {
    if(file.exists(model))
       load(model)
    else
       stop(paste("Cannot find file",model))
  }
return(model)
}
