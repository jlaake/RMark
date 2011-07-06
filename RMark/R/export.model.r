"export.model" <-
function(model,replace=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
#
# export.model   -   creates files for appending into MARK .dbf/.fpt files
#
# Arguments:
#
# model            - a mark model object or marklist of models
# replace          - if file exists and replace=TRUE, file will be over-written
#
# Value:
#
#  none
#
#
  model=load.model(model)
  if(class(model)[1]=="marklist")
     for(i in 1:(length(model)-1))
        export.model(model[[i]],replace=replace)
  else
  {
     file.copy(paste(model$output,".vcv",sep=""),paste(model$output,"V.tmp",sep=""), overwrite = replace)
     file.copy(paste(model$output,".out",sep=""),paste(model$output,"Y.tmp",sep=""), overwrite = replace)
     file.copy(paste(model$output,".res",sep=""),paste(model$output,"X.tmp",sep=""), overwrite = replace)
  }
  invisible()
}
