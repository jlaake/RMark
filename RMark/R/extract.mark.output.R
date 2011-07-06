"extract.mark.output" <-
function(out,model,adjust,realvcv=FALSE)
{
# ----------------------------------------------------------------------------------------
#
#  extract.mark.output     extracts the lnl, AICc, npar, beta and real estimates 
#                          and returns a list of these
#                          beta and real are dataframes with names estimate,se,lcl,ucl
#
#  Arguments:
#     out            - output from MARK analysis
#     model          - mark model object
#     adjust         - if TRUE, adjusts npar to # of cols in design matrix, modifies AIC and records both
#     realvcv        - if TRUE the vcv matrix of the real parameters is extracted and stored in the model results
#
#  Value:
#     result         - list of extracted output elements like npar, n(ESS), deviance etc
#
#
#  Functions used: setup.model, read.mark.binary 
#
# ----------------------------------------------------------------------------------------
  os=R.Version()$os
#
#  Extract basic stats (npar, n, deviance AICc etc)
#
  design.matrix=model$design.matrix
  links=model$links
  derived=setup.model(model$model,model$nocc)$derived
  outfile="markxxx.inp"
  nreal=dim(design.matrix)[1]
  nbeta=dim(design.matrix)[2]
  unlink(paste(outfile,".tmp",sep=""))
  x=grep("Effective sample size ",out,ignore.case=TRUE)
  if(length(x)==0)
    stop("MARK did not run properly.  If error message was not shown, re-run MARK with invisible=FALSE")
  n=type.convert(substr(out[x],regexpr("=",out[x])+1,nchar(out[x])))
  x=grep("-2logL {",out,fixed=TRUE)
#  x=grep("-2logL {",out,ignore.case=TRUE, extended=FALSE)
  lnl=type.convert(substr(out[x],regexpr("=",out[x])+1,nchar(out[x])))
  x=grep("Number of Estimated",out,ignore.case=TRUE)
  npar=type.convert(substr(out[x],regexpr("=",out[x])+1,nchar(out[x])))
  x=grep("DEVIANCE ",out,ignore.case=TRUE)
  deviance=type.convert(substr(out[x],regexpr("=",out[x])+1,nchar(out[x])))[1]
  x=grep("AICc",out,ignore.case=TRUE)
  AICc=type.convert(substr(out[x],regexpr("=",out[x])+1,nchar(out[x])))
  if(length(links)==1)
     x1=grep(paste(links,"link"),out,ignore.case=TRUE)
  else 
     x1=grep("parm-specific link",out,ignore.case=TRUE)
  x2=grep("Real Function Parameters",out,ignore.case=TRUE)
  x3=length(out)
  if(length(grep("proc stop",out,ignore.case=TRUE))==0)
     cat("\nWarning: output from MARK was not complete\n")
  x4=grep("Variable   Value",out,ignore.case=FALSE)+1
  if(length(x4)==0)x4=x2
#
# Extract average covariate values used in real parameter calculation
#
  if(x4>x2)
  {
     ff <- tempfile()
     cat(file=ff, out[(x4+1):(x4+length(model$covariates))],sep="\n")
     covariate.values=read.fwf(file=ff,widths=c(20,15),col.names=c("Variable","Value"))
  }
  else
     covariate.values=NULL
#
# Extract beta parameters ; this could also be done from binary file but from text
# file it is easy to decide which values are fixed.
#
  j=1
  save=NULL
  for(i in x1:(x2-1))
  {
    if(j<=nbeta)
    {
       ind=regexpr(paste(" ",j,":",sep=""),out[i])
       if(ind!=-1 & ind<=20)
       {
         save=c(save,out[i])
         j=j+1
       }
     }
  }
  write(save,file=paste(outfile,".tmp",sep=""))
  x=read.fwf(file=paste(outfile,".tmp",sep=""),widths=c(26,16,16,16,16),col.names=c("","estimate","se","lcl","ucl"))
  dimx=dim(x)[2]
  beta=as.data.frame(x[,((dimx-4+1):dimx)])
  names(beta)=c("estimate","se","lcl","ucl")
  row.names(beta)=names(design.matrix)
  nbeta=length(beta$estimate[beta$estimate!=0.000000])
#
# Extract parameter numbers that were not "estimated"
#
  singular=NULL
  if(nbeta!=npar)
  {
     x=grep("Attempted ordering of parameters",out,ignore.case=TRUE)
     if(length(x)==0)
       warning("\nNot all parameters were estimated but not able to find non-estimable parameters\n")
     else
     {
        nlines=ceiling(nbeta/25)
        ff=tempfile()
        for (i in (x+1):(x+nlines))
           write(out[i],file=ff,append=TRUE)
        par.indices=as.vector(t(read.fwf(file = ff,widths = c(4, rep(3, 24)))))
        singular=par.indices[(npar+1):nbeta]
     }
     
  }
  if(nbeta!=npar & adjust)
  {
    cat("\nNote: only ",npar," parameters counted of ",nbeta," specified parameters\n") 
    cat("AICc and parameter count have been adjusted upward\n")
    AICc.unadjusted=AICc
    npar.unadjusted=npar    
    AICc=lnl+ 2*nbeta +2*nbeta*(nbeta+1)/(n - nbeta -1)
    npar=nbeta
  }
  else
    npar.unadjusted=NULL    
  unlink(paste(outfile,".tmp",sep=""))
#
# Extract real parameters from text file; This could be done from binary file but the text file
# also denotes the fixed parameters
#
  j=1
  if(x4>x2)x2=x4+length(model$covariates)+1
  for(i in x2:(x3-1))
  {
    if(j<=nreal)
    {
       ind=regexpr(paste(" ",j,":",sep=""),out[i])
       if(ind==-1)
		   ind= regexpr("\\*\\*\\*\\*:",out[i])
       if(ind!=-1& ind<=20)
       {
          write(out[i],file=paste(outfile,".tmp",sep=""),append=TRUE)
          j=j+1
       }
    }
  }
  x=read.fwf(file=paste(outfile,".tmp",sep=""),widths=c(26,16,16,16,16,20),col.names=c("","estimate","se","lcl","ucl","fixed"),
                               as.is=TRUE)
  unlink(paste(outfile,".tmp",sep=""))
  x$note=""
  x$fixed[is.na(x$fixed)]=  "       "
  x$note[substr(as.character(x$fixed),3,7)!="Fixed"]=x$fixed[substr(as.character(x$fixed),3,7)!="Fixed"]
  x$fixed[substr(as.character(x$fixed),3,7)!="Fixed"]="     "
  x$fixed[substr(as.character(x$fixed),3,7)=="Fixed"]="Fixed"
  x$fixed[is.na(x$fixed)]=  "       "
  real=data.frame(estimate=as.numeric(x$estimate),se=as.numeric(x$se),lcl=as.numeric(x$lcl),ucl=as.numeric(x$ucl),fixed=x$fixed,note=x$note)
  if(is.null(model$simplify))
     row.names(real) = row.names(design.matrix)
  else
     row.names(real)=row.names(model$simplify$design.matrix)
#  if(!is.factor(real$fixed))real$fixed=""
  if(!is.factor(real$note))real$note=""
  if(file.exists("markxxx.vcv"))
     if(os=="mingw32")
        param=read.mark.binary("markxxx.vcv")
     else
        param=read.mark.binary.linux("markxxx.vcv")
  else
  {
     param=NULL
     cat("\nV-C file is missing. Skipping over it.\n")
  }
  if(realvcv)
    real.vcv=param$real.vcv
  else
    real.vcv=NULL
  if(is.null(npar.unadjusted))
     return(list(lnl=lnl,deviance=deviance,npar=npar,n=n,AICc=AICc,beta=beta,real=real,beta.vcv=param$beta.vcv,derived=param$derived,derived.vcv=param$derived.vcv,
                 covariate.values=covariate.values,singular=singular,real.vcv=real.vcv))

  else
     return(list(lnl=lnl,deviance=deviance,npar=npar,npar.unadjusted=npar.unadjusted,n=n,AICc=AICc,AICc.unadjusted=AICc.unadjusted,
                 beta=beta,real=real,beta.vcv=param$beta.vcv,derived=param$derived,derived.vcv=param$derived.vcv,
                 covariate.values=covariate.values,singular=singular,real.vcv=real.vcv))
}