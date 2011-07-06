"fill.covariates" <-
function(model,values)
{
# ----------------------------------------------------------------------------------------------------
#
#   fill.covariates  -   fills entries in design matrix with the values for covariates. The input argument 
#                        "values" is the output from find.covariates.
# Arguments:  
#
#   model    - MARK model object
#   values   - a dataframe matching structure of output from find.covariates
#
# Value:
#
#   xdesign  - new design matrix with user-defined covariate values entered.
#
# ----------------------------------------------------------------------------------------------------
model=load.model(model)
design=as.matrix(model$design.matrix)
values=values
if(!is.null(values))
for(i in 1:dim(values)[1])
   design[values$row[i],values$col[i]]=values$value[i]
xdesign=matrix(0,dim(design)[1],dim(design)[2])
for(i in 1:dim(design)[2])
   xdesign[,i]=as.numeric(design[,i])
return(xdesign)
}
