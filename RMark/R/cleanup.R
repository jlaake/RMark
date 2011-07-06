"cleanup" <- function(lx=NULL,ask=TRUE,prefix="mark")
# ----------------------------------------------------------------------------------------
#
# cleanup   - remove unused MARK* files
#
# Arguments:
#
#  lx             - listing of R objects
#  ask            - if TRUE, ask whether each file should be deleted
#
# Value:
#
#  None
#
#
#  10 Jan 06; added to cleanup files left by deleted mark objects
# ----------------------------------------------------------------------------------------
{
purge=function(type,model.filenames,ask)
{
   file.list=list.files(pattern=paste(prefix,"[0123456789]+[:.:]",type,sep=""))
   if(length(file.list)>0)
   for (i in 1:length(file.list))
   {
     if(!file.list[i] %in% paste(model.filenames,".",type,sep=""))
     {
        if(ask)
           answer=readline(paste("Delete file",file.list[i],"(y/n)[y]?"))
        else
           answer="y"
        if(substr(answer,1,1)!="n")
           unlink(file.list[i])
     }
   }
}
#
# Collect mark model objects
#
xx="\\\\."
if(is.null(lx))lx=ls(envir=parent.frame())
exclude=grep("\\*",lx)
if(length(exclude)>0)lx=lx[-exclude]
model.list=collect.model.names(lx,warning=FALSE)
model.filenames=NULL
if(!is.null(model.list))
{
   for( i in 1:length(model.list))
   {
      if(eval(parse(text=paste("is.list(",model.list[i],")",sep=""))))
         model.filenames=c(model.filenames,eval(parse(text=paste(model.list[i],"$output",sep=""))))
      else
      {
         zz=eval(parse(text=model.list[i]))
         model.filenames=c(model.filenames,eval(parse(text=paste('strsplit("',zz,'",','"',xx,'"',")[[1]][1]",sep=""))))
      }
   }
}        
#  x in the function call did not work in Linux; used xzx and worked fine
#myf=function(xzx)
#{
#   eval(parse(text=paste("ifelse(is.list(xzx),xzx$output,strsplit(xzx,'",xx,"')[[1]][1])",sep="")))
#}
#debug(myf)
#
#model.filenames=sapply(model.list,myf)
#model.filenames=sapply(paste(model.list,"$output",sep=""),function(xzx)eval(parse(text=xzx)))
#
# Collect marklist objects
#
model.list=NULL
for (i in 1:length(lx))
{
   classval=class(eval(parse(text=lx[i]),envir=parent.frame(2)))
   if(classval[1]=="marklist")
   model.list=c(model.list,lx[i])
}
blank=""
if(length(model.list)!=0)
   for(i in 1:length(model.list))
   {
      ml=eval(parse(text=model.list[i]),envir=parent.frame(2))
      num.models=length(ml)-as.numeric(!is.null(ml$model.table))
      for (j in 1:num.models)
         model.filenames=c(model.filenames,
           eval(parse(text=paste("ifelse(is.list(ml[[",j,"]]),ifelse(!is.null(ml[[",j,"]]$output),ml[[",j,"]]$output,blank),strsplit(ml[[",j,"]],'",xx,"')[[1]][1])",sep=""))))
   }
model.filenames=model.filenames[model.filenames!=""]
purge("inp",model.filenames,ask)
purge("out",model.filenames,ask)
purge("res",model.filenames,ask)
purge("vcv",model.filenames,ask)
purge("rda",model.filenames,ask)
#
#  Delete any tmp files created by export.models
#
file.list=list.files(pattern="mark[0123456789]+[:YVX:][:.:]tmp")
unlink(file.list)
invisible()
}
