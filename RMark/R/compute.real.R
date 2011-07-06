"compute.real" <-
function(model,beta=NULL,design=NULL,data=NULL,se=TRUE,vcv=FALSE)
{
# ------------------------------------------------------------------------------------------------
#
#   compute.real  -   computes real estimates and var-cov from design matrix (design) and coefficients
#                     (beta) and inverse link transformation.
#       
# Arguments:  
#
#   model          - MARK model object
#   beta           - estimates of beta parameters for computation of real parameters
#   design         - design matrix from a MARK model
#   data           - dataframe with covariate values that are averaged for estimates
#   se             - if TRUE returns std errors of real estimates
#   vcv            - logical; fct computes and returns v-c matrix of real estimates if TRUE
#
# Value (list):
#
#   real        - data frame containing estimates and se
#   vcv.real    - variance-covariance matrix of real estimates 
#
# ------------------------------------------------------------------------------------------------
model=load.model(model)
if(is.null(beta))beta=model$results$beta$estimate
#
# Assign value of chat - overdispersion constant
#
if(is.null(model$chat))
  chat=1
else
  chat=model$chat
#
# If no design matrix was specified, use the model matrix and fill in any covariate values with default mean value
#
if(is.null(design))
{
   if(is.null(data))
       stop("\n data argument must be specified if design matrix is not specified\n")
   if(!is.data.frame(data))
       stop("\n data argument must be a dataframe. Use data and not processed data list.\n")
   design=fill.covariates(model,find.covariates(model,data))
}
#
# The following shouldn't happen unless the model and design matrices are mixed between models
#
if(dim(design)[2]!=length(beta))
   stop("Mismatch between number of design columns and length of beta")
#
# Change design matrix values to numeric from character and trap to make sure
# the value for design doesn't contain any unfilled covariate entries.
#
if(any(is.na(suppressWarnings(as.numeric(design)))))
   stop("\nInput design matrix must only have numeric values.  Use find.covariates and fill.covariates to fill in covariate values\n")
design=matrix(as.numeric(design),nrow=dim(design)[1])
#
# Set indices for real parameters that have been fixed and at any boundaries
#
if(!is.null(model$fixed))
{
   if(is.null(model$simplify))
      fixedparms=(1:dim(design)[1])%in%model$fixed$index
   else
      fixedparms=(1:dim(design)[1])%in%model$simplify$pim.translation[model$fixed$index]
}
else
   fixedparms=rep(FALSE,dim(design)[1])
boundaryparms=model$results$real$se==0 & !fixedparms
fixedvalues=rep(NA,nrow(design))
fixedvalues[model$simplify$pim.translation[model$fixed$index]]=model$fixed$value
#
#  Compute real parameters; if neither se or vcv then return vector of real parameters
#
   real=convert.link.to.real(design%*%beta,links=model$links,fixed=fixedvalues)
#
#  Set fixed real parameters to their fixed values
#
   real[fixedparms]=fixedvalues[fixedparms]
#  If no se or vcv requested, return result
if(!vcv & !se)return(data.frame(real=real))
#
# Compute vc matrix for real parameters which needs to be modified for
# any mlogit parameters
#
if(length(model$links)==1)
   deriv.real=deriv_inverse.link(real,design,model$links)
else
   deriv.real=t(apply(data.frame(real=real,x=design,links=model$links),1,
         function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
vcv.real=deriv.real%*%model$results$beta.vcv%*%t(deriv.real)
#
# If vcv=TRUE, compute v-c matrix and std errors of real estimates
# To handle any mlogit parameters compute pseudo-real estimates using log in place of mlogit
#
  ind=grep("mlogit",model$links,ignore.case=TRUE)
  templinks=model$links
  if(length(ind)>0)
  {
    templinks[ind]="log"
    pseudo.real=as.vector(convert.link.to.real(design%*%beta,links=templinks))
    pseudo.real[fixedparms]=fixedvalues[fixedparms]
    pseudo.real[ind][fixedparms[ind]]=exp(pseudo.real[ind][fixedparms[ind]])
#
#   Compute first derivatives of pseudo-real (for any mlogit parameter)
#   estimates with respect to beta parameters
#
    if(length(templinks)==1)
      deriv.pseudo=deriv_inverse.link(pseudo.real,design,templinks)
   else
      deriv.pseudo=t(apply(data.frame(real=pseudo.real,x=design,links=templinks),1,
            function(x){deriv_inverse.link(as.numeric(x[1]),as.numeric(x[2:(length(x)-1)]),x[length(x)])}))
    deriv.pseudo[fixedparms,]=0
    vcv.pseudo=chat*deriv.pseudo%*%model$results$beta.vcv%*%t(deriv.pseudo)
#
#    Apply chain rule to get variance of real parameters which has mlogits
#    expressed as zi/(1+z1+...zk) where k is number of mlogit components-1 and
#    non-mlogits are expressed as zi.
#    bottom is either 1 for non-mlogits and the sum for mlogits
#    pbottom is partial with respect to zi
#
    if(length(model$links)==1)
        links=rep(model$links,length(pseudo.real))
    else
        links=model$links
    mlogits=outer(links,links,function(x,y)as.numeric(x==y))*as.numeric(substr(links,1,6)=="mlogit"|substr(links,1,6)=="MLogit")
    pbottom=matrix(0,nrow=dim(vcv.pseudo)[1],ncol=dim(vcv.pseudo)[1]) + mlogits
    bottom=diag(nrow=dim(vcv.pseudo)[1])*(1-as.numeric(substr(links,1,6)=="mlogit"|substr(links,1,6)=="MLogit"))+
       mlogits + pbottom*apply(pbottom*pseudo.real,2,sum)
    deriv.pseudo=(diag(nrow=dim(vcv.pseudo)[1])*bottom-pseudo.real*pbottom)/bottom^2
    deriv.pseudo[is.nan(deriv.pseudo)]=0
    vcv.real=deriv.pseudo%*%vcv.pseudo%*%t(deriv.pseudo)
  }
  else
     vcv.real=chat*vcv.real
#
# Compute conf interval taking into account use of logit transform for mlogit
# and any 0-1 link (loglog,cloglog,sin,logit)
#
link.se=suppressWarnings(sqrt(chat*diag(design%*%model$results$beta.vcv%*%t(design))))
link.se[is.na(link.se)]=0
if(length(model$links)==1)
  links=rep(model$links,length(real))
else
  links=model$links
ind=unique(c(grep("mlogit",model$links,ignore.case=TRUE),which(links%in%c("sin","Sin","LogLog","loglog","CLogLog","cloglog"))))
linkse=suppressWarnings(sqrt(diag(vcv.real)[ind])/(real[ind]*(1-real[ind])))
linkse[is.na(linkse)]=0
linkse[is.infinite(linkse)]=0
link.se[ind]=linkse
link.values=design%*%beta
link.values[ind]=suppressWarnings(log(real[ind]/(1-real[ind])))
link.values[ind][abs(real[ind]-1)<1e-7]=100
link.values[ind][abs(real[ind]-0)<1e-7]=-100
links[ind]="logit"
real.lcl=convert.link.to.real(link.values-1.96*link.se,links=links)
real.ucl=convert.link.to.real(link.values+1.96*link.se,links=links)
#
# Set v-c values of fixed parameters to 0
#
vcv.real[fixedparms,]=0
vcv.real[,fixedparms]=0
diag(vcv.real)[diag(vcv.real)<0]=0
se.real=sqrt(diag(vcv.real))
#se.real[is.na(se.real)]=0
fixed=rep("",dim(design)[1])
fixed[fixedparms]="Fixed"
fixed[boundaryparms]="Boundary"
if(vcv)
   return(list(real=real,se.real=se.real,lcl=real.lcl,ucl=real.ucl,fixed=fixed,vcv.real=vcv.real))
else
   return(data.frame(estimate=real,se=se.real,lcl=real.lcl,ucl=real.ucl,fixed=fixed))
}

