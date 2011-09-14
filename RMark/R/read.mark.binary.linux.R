read.mark.binary.linux <-
function(filespec)
#
# read.mark.binary 
#
# Arguments:
# 
# Value: list of estimates, se, lcl, ucl and var-cov matrices
#        for beta, real and derived estimates
{
z=file(filespec,"rb")
beta=list()
whatread=readBin(z,raw(),12)
ncovs=readBin(z,integer(),1,size=4)
nlogit=readBin(z,integer(),1,size=4)
ngrps=readBin(z,integer(),1,size=4)
nests=readBin(z,integer(),1,size=4)
#whatread=readBin(z,integer(),2*(max(ncovs,nlogit)-1),size=4)
#whatread=readBin(z,raw(),24)
whatread=""
while(whatread!="B")
{
  whatread=try(readChar(z,1),silent=TRUE)
  if(class(whatread)=="try-error") whatread=""
}
whatread=readBin(z,raw(),7)
beta$estimate=readBin(z,numeric(),ncovs,size=8)
whatread=readBin(z,raw(),16)
beta$se=readBin(z,numeric(),ncovs,size=8)
whatread=readBin(z,raw(),16)
beta$lcl=readBin(z,numeric(),ncovs,size=8)
whatread=readBin(z,raw(),16)
beta$ucl=readBin(z,numeric(),ncovs,size=8)
beta.vcv=matrix(0,nrow=ncovs,ncol=ncovs)
for (i in 1:ncovs)
{
   whatread=readBin(z,raw(),16)
   beta.vcv[i,]=readBin(z,numeric(),ncovs,size=8)
}
whatread=readBin(z,raw(),16)
real=list()
real$estimate=readBin(z,numeric(),nlogit,size=8)
whatread=readBin(z,raw(),16)
real$se=readBin(z,numeric(),nlogit,size=8)
whatread=readBin(z,raw(),16)
real$lcl=readBin(z,numeric(),nlogit,size=8)
whatread=readBin(z,raw(),16)
real$ucl=readBin(z,numeric(),nlogit,size=8)
real.vcv=matrix(0,nrow=nlogit,ncol=nlogit)
cont=TRUE
for (i in 1:nlogit)
{
whatread=readBin(z,raw(),16)
whatread=readBin(z,numeric(),nlogit,size=8)
if(length(whatread)!=nlogit)
{
  cat("\n Incomplete read of the binary file\n")
  real.vcv=NULL
  cont=FALSE
  break
} else
{
  real.vcv[i,]=whatread
}
}
if(cont)
{
  whatread=readBin(z,raw(),16)
  nderiv=readBin(z,integer(),1,size=4)
  if(length(nderiv)>0)
  {
    derived=list()  
    whatread=readBin(z,raw(),16)
    derived$estimate=readBin(z,numeric(),nderiv,size=8)
    whatread=readBin(z,raw(),16)
    derived$se=readBin(z,numeric(),nderiv,size=8)
    whatread=readBin(z,raw(),16)
    derived$lcl=readBin(z,numeric(),nderiv,size=8)
    whatread=readBin(z,raw(),16)
    derived$ucl=readBin(z,numeric(),nderiv,size=8)
    derived.vcv=matrix(0,nrow=nderiv,ncol=nderiv)
    for (i in 1:nderiv)
    {
      whatread=readBin(z,raw(),16)
      value=readBin(z,numeric(),nderiv,size=8)
      if(length(value)!=0) derived.vcv[i,]=value
    }
  }  
  else
  {
   derived=NULL
   derived.vcv=NULL
  }
} else
{
   derived=NULL
   derived.vcv=NULL
}
close(z)
return(list(beta=as.data.frame(beta),beta.vcv=beta.vcv,real=as.data.frame(real),real.vcv=real.vcv,derived=as.data.frame(derived),derived.vcv=derived.vcv))
}
