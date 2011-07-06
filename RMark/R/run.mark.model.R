"run.mark.model" <-
function(model,invisible=FALSE,adjust=TRUE,filename=NULL,prefix="mark",realvcv=FALSE,
delete=FALSE,external=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
#
#  run.mark.model     Uses input file from model as input to MARK.  It runs MARK,
#                     gets output and extracts relevant values into results.  Both
#                     output and results list are saved in the original model.
#
#  Arguments:
#
#  model      - MARK model created by make.mark.model
#  invisible  - if TRUE, MARK window is hidden from view; run of vcread.exe after mark.exe is always hidden
#  adjust     - if TRUE, adjusts npar to # of cols in design matrix, modifies AIC and records both
#  filename   - base filename for output files; Only specified to load in a previously run model
#  prefix     - base filename prefix for files created by MARK.EXE; the files are named prefixnnn.*
#  realvcv    - if TRUE the vcv matrix of the real parameters is extracted and stored in the model results
#  delete     - if TRUE the output files are deleted after the results are extracted
#  external   - if TRUE the mark object is saved externally rather than in the workspace; the filename is kept in its place
#
#  Value:
#
#   model    - MARK model with output (file from MARK) and extracted results from output file
#              appended onto list
# 
#  Functions used: extract.mark.output
#
# -----------------------------------------------------------------------------------------------------------------------
  os=R.Version()$os
#
# Check to make sure model has not already been run
#
  modname=substitute(model)
  if(!is.null(model$output))
     stop("Model ",modname," has already been run.")
#
# Simplify model structure
#
#  if(!is.null(model$simplify)) model=simplify.pim.structure(model)
#
# Run mark.exe to do the analysis; assumed to be in normal location in which Mark is installed unless
# the variable MarkPath has been defined
# 24 Aug 05; save all files and give names mark###.*
#  9 Jan 06; use specified name if given
#
    if(!is.null(filename))
		basefile=filename
	else
		basefile=paste(prefix,"001",sep="")
	i = 1
    while (file.exists(paste(basefile, ".out", sep = ""))) 
	{
       i = i + 1
       basefile = paste(prefix, formatC(as.integer(i), flag = "0",width=3),sep = "")
    }
    outfile = paste(basefile, ".out", sep = "")
    inputfile = paste(basefile, ".inp", sep = "")
    vcvfile = paste(basefile, ".vcv", sep = "")
    resfile = paste(basefile, ".res", sep = "")
#
#   If outfile already exists, ask user if mark object should be created with
#   existing file
#
    RunMark=TRUE
    if(file.exists(outfile))
       if(toupper(substr(readline("Create mark model with existing file (Y/N)?"),1,1))=="Y")
         RunMark=FALSE
#
# Write input file to temp file markxxx.inp
#
  writeLines(model$input,inputfile)
  if(os=="mingw32")
  {
    if(!exists("MarkPath"))
	{
		MarkPath=Sys.which("mark.exe")
	    if(MarkPath=="")
			if(file.exists("c:/Program Files/Mark/mark.exe"))
			  MarkPath=shQuote("c:/Program Files/Mark/mark.exe")
            else
			  if(file.exists("c:/Program Files (x86)/Mark/mark.exe"))
				  MarkPath=shQuote("c:/Program Files (x86)/Mark/mark.exe")
		      else	
			      stop("mark.exe cannot be found. Add to system path or specify MarkPath object (e.g., MarkPath='C:/Program Files (x86)/Mark'")
    }else
	{
		if(substr(MarkPath,nchar(MarkPath),nchar(MarkPath))%in%c("\\","/"))
			MarkPath=shQuote(paste(MarkPath,"mark.exe",sep=""))
		else
			MarkPath=shQuote(paste(MarkPath,"mark.exe",sep="/"))
	}		
    if(RunMark)
		system(paste(MarkPath, " BATCH i=",inputfile," o=", outfile,
						" v=markxxx.vcv r=",resfile,sep = ""), invisible = invisible)
	else
      file.rename(vcvfile,"markxxx.vcv")
  } else
  {
    if(!exists("MarkPath"))MarkPath=""
    if(RunMark)
       system(paste("mark i=",inputfile," o=", outfile,
            " v=markxxx.vcv r=",resfile,sep = ""))
    else
      file.rename(vcvfile,"markxxx.vcv")
  }
#
# Read in the output file
#
  out=readLines(outfile)
#
# Extract relevant parts of output
#
  results=extract.mark.output(out,model,adjust,realvcv)
  file.rename("markxxx.vcv",vcvfile)
  model$results=results
#
# 24-Aug-05; output is now the base filename and input is no longer stored in object
# 09 Jan 06; now it only stores base part of filename rather than base.out
#
  model$output=basefile
  model$input=NULL
#
#  23-Aug-05; if simplify replace old design matrix with simplified one
#  10 Jan 06; save labels from full design matrix and store in simplify for output
#
  if(!is.null(model$simplify))
  {
     model$simplify$real.labels=row.names(model$design.matrix)
     model$design.matrix=model$simplify$design.matrix
     model$simplify$design.matrix=NULL
  }
  if(delete)
  {
    unlink(outfile)
    unlink(inputfile)
    unlink(vcvfile)
    unlink(resfile)
  }
#
#  Return the model object
# 
   if(external)
   {
     marksave=paste(model$output,".rda",sep="")
     save(model,file=marksave)
     class(marksave)=class(model)
     return(marksave)
   } else
     return(model)
}
