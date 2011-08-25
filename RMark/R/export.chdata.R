"export.chdata" <-
function(data, filename, covariates=NULL, replace=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
#
# export.chdata   -   creates a MARK .inp file from dataframe that can be used to create a MARK .dbf file
#
# Arguments:
#
# data             - processed data list resulting from process.data
# filename         - filename (without .inp extension)
# covariates       - names of covariate variables in data to put in .inp; default is to use none and
#                    all can be passed by setting covariates="all")
# replace          - if file exists and replace=TRUE, file will be over-written
#
# Value:
#
#  none; creates text file 
#
#
#
  outfile=paste(filename,".inp",sep="")
  if(file.exists(outfile))
     if(replace)
     {
        file.create(outfile)
     }
     else
        stop("File already exists and replace=FALSE")
#
# Check to make sure this is a processed data list
#
  if(is.null(data$data))
     stop("\nUse processed data list and not original dataframe for the data argument\n")
#
# Output data portion of MARK input file:
#
  if(data$model!="Nest")
  {
     ch=data$data$ch
     zz=as.data.frame(ch)
     zz=cbind(zz,data$freq)
     if(!is.null(covariates))
     {
        if(covariates[1]=="all")
           zz=data.frame(cbind(zz,data$data[,-1])) 
        else
           zz=data.frame(cbind(zz,data$data[,covariates])) 
     }
#  
#    This outputs capture history, frequency and any covariates
#
	 write.table(zz,file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
 } else
#	 
#    Output nest survival model
#
 {
	 
	 zz=data$data[,1:5]
	 if(!is.null(covariates))
	 {
		 if(covariates[1]=="all")
			 zz=data.frame(cbind(zz,data$data[,-(1:5)])) 
		 else
			 zz=data.frame(cbind(zz,data$data[,covariates])) 
	 }
	 if(is.null(data$group.covariates)) ng=1 else ng=nrow(data$group.covariates)
	 for(i in 1:ng)
	 {
		write(paste("Nest survival group =",i,";"),file=outfile,append=TRUE)
		write.table(zz[zz$group==i,],file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
	 }	 
  }
  invisible()
}
