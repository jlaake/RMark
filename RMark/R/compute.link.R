"compute.link" <-
function(model,beta=NULL,design=NULL,data=NULL,parm.indices=NULL,vcv=TRUE)
{
# ------------------------------------------------------------------------------------------------

#   compute.link -   computes link estimates and var-cov from design matrix (design) and coefficients
#                    (beta).
#       
# Arguments:  
#
#   model          - MARK model object
#   beta           - estimates of beta parameters for computation of real parameters
#   design         - design matrix from a MARK model
#   data           - dataframe with covariate values that are averaged for estimates
#   parm.indices   - index numbers from PIMS for rows in design matrix to use
#   vcv             - logical; fct computes se, ci and returns v-c matrix of real estimates if TRUE
#
# Value (list):
#
#   link        - data frame containing estimates and se
#   vcv.link    - variance-covariance matrix of real estimates
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
# If no row #s were given, use all rows; otherwise subset rows
#
if(!is.null(parm.indices))
   design=design[parm.indices,,drop=FALSE]
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
# Set indices for links that have been fixed and compute link values
# if !vcv then return as a dataframe
#
fixedparms=model$results$real$se[parm.indices]==0
link=design%*%beta
link[fixedparms]=NA
if(!vcv)return(data.frame(link=link))
#
# Otherwise compute vc matrix for link values
#
vcv.link=chat*design%*%model$results$beta.vcv%*%t(design)
#
# Compute conf interval based on normal for link
#
link.se=sqrt(diag(vcv.link))
link.se[fixedparms]=NA
link.lcl=link-1.96*link.se
link.ucl=link+1.96*link.se
#
# Set v-c values of fixed parameters to 0
#
vcv.link[fixedparms,]=0
vcv.link[,fixedparms]=0
estimates=data.frame(link=link,se=link.se,lcl=link.lcl,ucl=link.ucl)
if(!is.null(parm.indices))
   row.names(estimates)=row.names(model$design.matrix)[parm.indices]
else
   row.names(estimates)=row.names(model$design.matrix)
return(list(estimates=estimates,vcv=vcv.link))
}
