make.time.factor=function(x,var.name,times,intercept=NULL,delete=TRUE)
#
#  This function takes a time varying factor variable and creates a
#  a set of time varying dummy variables to be used as time dependent
#  individual covariates.
#
#  Arguments:
#      x        - dataframe
#     var.name  - prefix variable name
#     times     - suffix numbers for variable names
#     intercept - value to used for intercept
#     delete    - if TRUE remove the original columns from the data
#
#  Value:
#     dataframe with new time varying dummy variables added
#
{
#  create var.names from prefix and times and check to make sure they exist
   var.names=paste(var.name,times,sep="")
   if(any(!var.names %in%names(x))) stop(paste("Following fields not found: ",
           paste(var.names[!var.names %in%names(x)],collapse=",")))
#  create initial new dataframe and delete orginal columns if requested
   if(delete)
      y=subset(x,select=names(x)[!names(x)%in%var.names])
   else
      y=x
# Compute unique levels across all of the factor variables
  ulevels=NULL
  for(i in 1:length(var.names))ulevels=c(ulevels,levels(x[,var.names[i]]))
  ulevels=unique(ulevels)
# Loop over each time-varying factor variable and create k-1 time varying dummy
# variables for each factor variable where k is the number of levels of the factor
# which is the length of times.  Any "." values are ignored.
#
   for(i in 1:length(var.names))
   {
     xx=x[,var.names[i],drop=FALSE]
     char=unlist(strsplit(var.names[i],""))
     startc=(1:length(char))*as.numeric(char%in%0:9)
     startc=min(startc[startc!=0])
     time.index=substr(var.names[i],start=startc,stop=length(char))
     names(xx)="x"
     xx$x=factor(as.character(xx$x),levels=ulevels)
     mat=data.frame(model.matrix(~-1+x,xx))
     mat=subset(mat,select=names(mat)[names(mat)!="x."])
     xlevels=ulevels
     if(!is.null(intercept))
     {
        mat=subset(mat,select=names(mat)[names(mat)!=paste("x",intercept,sep="")])
        xlevels=ulevels[ulevels!=intercept]
     }
     names(mat)=paste(substr(var.name,start=1,stop=startc-1),xlevels[xlevels!="."],time.index,sep="")
     y=cbind(y,mat)
   }
   return(y)
}
