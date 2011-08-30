#' Runs analysis with MARK model using MARK.EXE
#' 
#' Passes input file from model (\code{model$input}) to MARK, runs MARK, gets
#' \code{output} and extracts relevant values into \code{results} which is
#' appended to the \code{mark} model object.
#' 
#' This is a rather simple function that initiates the analysis with MARK and
#' extracts the output. An analysis was split into two functions
#' \code{\link{make.mark.model}} and \code{run.mark.model} to allow a set of
#' models to be created and then run individually or collectively with
#' \code{\link{run.models}}.  By default, the execution of MARK.EXE will appear
#' in a separate window in which the progress can be monitored. The window can
#' be suppressed by setting the argument \code{invisible=TRUE}. The function
#' returns a \code{mark} object and it should be assigned to the same object to
#' replace the original model (e.g., \code{mymodel=run.mark.model(mymodel)}).
#' The element \code{output} is the base filename that links the objects to the
#' output files stored in the same directory as the R workspace.  To removed
#' unneeded output files after deleting mark objects in the workspace, see
#' \code{\link{cleanup}}. \code{results} is a list of specific output values
#' that are extracted from the output. In extracting the results, the number of
#' parameters can be adjusted (\code{adjust=TRUE}) to match the number of
#' columns in the design matrix, which assumes that it is full rank and that
#' all of the parameters are estimable and not confounded.  This can be useful
#' if that assumption is true, because on occasion MARK.EXE will report an
#' incorrect number of parameters in some cases in which the parameters are at
#' boundaries (e.g., 0 or 1 for probabilities).  If the true parameter count is
#' neither that reported by MARK.EXE nor the number of columns in the design
#' matrix, then it can be adjusted using \code{\link{adjust.parameter.count}}.
#' 
#' If \code{filename} is assigned a value it is used to specify files with
#' those names. This is most useful to capture output from a model that has
#' already been run.  If it finds the files with those names already exists, it
#' will ask if the results should be extracted from the files rather than
#' re-running the models.
#' 
#' @param model MARK model created by \code{\link{make.mark.model}}
#' @param invisible if TRUE, exectution of MARK.EXE is hidden from view
#' @param adjust if TRUE, adjusts number of parameters (npar) to number of
#' columns in design matrix, modifies AIC and records both
#' @param filename base filename for files created by MARK.EXE. Files are named
#' filename.*.
#' @param prefix base filename prefix for files created by MARK.EXE; the files
#' are named prefixnnn.*
#' @param realvcv if TRUE the vcv matrix of the real parameters is extracted
#' and stored in the model results
#' @param delete if TRUE the output files are deleted after the results are
#' extracted
#' @param external if TRUE the mark object is saved externally rather than in
#' the workspace; the filename is kept in its place
#' @return model: MARK model object with the base filename stored in
#' \code{output} and the extracted \code{results} from the output file appended
#' onto list; see \code{\link{mark}} for a detailed description of a
#' \code{mark} object.
#' @author Jeff Laake
#' @export
#' @seealso \code{\link{make.mark.model}}, \code{\link{run.models}},
#' \code{\link{extract.mark.output}}, \code{\link{adjust.parameter.count}},
#' \code{\link{mark}}, \code{\link{cleanup}}
#' @keywords model
#' @examples
#' 
#' data(dipper)
#' for(sex in unique(dipper$sex))
#' {
#' 	x=dipper[dipper$sex==sex,]
#' 	x.proc=process.data(x,model="CJS")
#' 	x.ddl=make.design.data(x.proc)
#' 	Phi.dot=list(formula=~1)
#' 	Phi.time=list(formula=~time)
#' 	p.dot=list(formula=~1)
#' 	p.time=list(formula=~time)
#' 	cml=create.model.list("CJS")
#' 	x.results=mark.wrapper(cml,data=x.proc,ddl=x.ddl,prefix=sex)
#' 	assign(paste(sex,"results",sep="."),x.results)
#' }
#' rm(Male.results,Female.results,x.results)
#' cleanup(ask=FALSE,prefix="Male")
#' cleanup(ask=FALSE,prefix="Female")
#' 
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
