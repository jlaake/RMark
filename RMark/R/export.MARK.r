export.MARK=function(x,project.name,model=NULL,replace=FALSE,chat=1.0,title="",ind.covariates=NULL)
{
# exports model and data files for import into the MARK interface
#
# Arguments:
# 
# x              - processed dataframe used to build models
# project.name   - character string to be used for prefix of filenames and for MARK project name
# model          - either a single mark model or a marklist
# replace        - if TRUE it will replace any existing files
# chat           - user-specified chat value if desired
# title          - MARK project title string
# ind.covariates - vector of character strings specifying names of individual covariates
#
# Value: no returned value, but it will create a "project.name".inp,"project.name".Rinp, 
#        and copy the output files for the mark models to the names needed to import into MARK
#
  if(is.null(x$model)) stop(paste(substitute(x), "is not a processed dataframe"))
  filename=paste(project.name,".Rinp",sep="")
  if(file.exists(filename)&!replace) 
  {
    stop("Project already exists and replace=FALSE")
  } else
  {
    if(!file.exists(filename))
    { 
       xfilename=paste(project.name,".inp",sep="")
       if(file.exists(xfilename)) stop("Use a different project name because the .inp file already exists")
    }
  }
  write(setup.model(x$model, nchar(x$data$ch[1]), x$mixtures)$etype,file=filename)
  write(x$mixtures,file=filename,append=TRUE)
  write(nchar(x$data$ch[1]),file=filename,append=TRUE)
  if(is.null(ind.covariates))
  {
    write("0",file=filename,append=TRUE)  
  } else
  {
    if(all(ind.covariates%in%names(x$data)))
    {
       write(length(ind.covariates),file=filename,append=TRUE)
       write(matrix(ind.covariates,ncol=1),file=filename,append=TRUE)
    } else
       stop("One or more of the ind.covariates values were not found in the data")
  }  
  write(x$nstrata,file=filename,append=TRUE)
  if(is.null(x$strata.labels))
    x$strata.labels=rep("NA",x$nstrata)
  write(matrix(paste(x$strata.labels,":Stratum",1:length(x$strata.labels),sep=""),ncol=1),file=filename,append=TRUE)
  write(x$time.intervals,ncolumns=length(x$time.intervals),file=filename,append=TRUE)
  if(is.null(x$group.covariates))
    write("1",file=filename,append=TRUE)  
  else    
    write(nrow(x$group.covariates),file=filename,append=TRUE)
  write(matrix(colnames(x$freq),ncol=1),file=filename,append=TRUE)
  write(nrow(x$freq),file=filename,append=TRUE)
  write(chat,file=filename,append=TRUE)
  if(title=="")
    write("NA",file=filename,append=TRUE)
  else
    write(title,file=filename,append=TRUE)
  export.chdata(x,project.name,covariates=ind.covariates,replace=replace)
  if(!is.null(model)) export.model(model,replace=replace)
  return(NULL)
}



