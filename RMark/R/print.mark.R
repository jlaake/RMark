"print.mark" <- function(x,...,input=FALSE)
{
# -------------------------------------------------------------------------------------------------------------
#
# print.mark  - extracts model output file from MARK and sends to notepad so it can be viewed
#
# Arguments:
#
#  x - a mark model object
#  input  - if TRUE, prints the mark input file rather than the output file
#
# Value:
#  None
#
# -------------------------------------------------------------------------------------------------------------
#
#
#
   os=R.Version()$os
   if(!exists("MarkViewer"))
     if(os=="mingw32")
        MarkViewer="notepad"
     else
        MarkViewer="pico"
#
#  If the model has been run (model$output exists) extract to temp file dummy.xxx and 
#  call notepad to view it; any program could be used in its place. The temp file is immediately deleted
#  9 Jan 06; changed such that model$output is just the baseline value of the filename
#
#  def.options=options()
#  options(useFancyQuotes=FALSE)
  model=load.model(x)
  if(!input)
  {
      if(!is.null(model$output))
      {
         if(file.exists(paste(model$output,".out",sep="")))
         {
            if(os=="mingw32")
               system(paste(shQuote(MarkViewer),paste(model$output,".out",sep="")),invisible=FALSE,wait=FALSE)
            else
               system(paste(MarkViewer,paste(model$output,".out",sep="")),wait=FALSE)
         }
         else
            cat(paste("Cannot locate file ",model$output,".out\n",sep=""))
      }else
        print.default(model)
  }
  else
  {
      if(!is.null(model$output))
      {
         if(file.exists(paste(model$output,".inp",sep="")))
         {
            if(os=="mingw32")
               system(paste(shQuote(MarkViewer),paste(model$output,".inp",sep="")),invisible=FALSE,wait=FALSE)
            else
               system(paste(MarkViewer,paste(model$output,".inp",sep="")),wait=FALSE)
         }
         else
            cat(paste("Cannot locate file ",model$output,".inp\n",sep=""))
      }else
        print.default(model)
  }

#   options(def.options)
   invisible()
}
"print.marklist"<-function(x,...)
{
   ncol=dim(x$model.table)[2]
   if(!is.null(x$model.table))
   {
     if(is.null(x$model.table$chat))
        print(x$model.table[,(ncol-5):ncol])
     else
        print(x$model.table[,(ncol-6):ncol])
   } else cat("No model.table is available")
}

