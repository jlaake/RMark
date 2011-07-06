deltamethod.special=function(function.name,mean,cov,ses=TRUE)
{
#
# This function computes the delta method std errors or v-c matrix
# for a sum, cumsum (vector of cummulative sums), prod (product),
# or cumprod(vector of cummulative products).  It uses the function deltamethod
# from the msm package.  It will load the msm pacakge but assumes that it has already
# been installed.  See the msm documentation for a complete description on how the
# deltamethod function works.  If ses=TRUE, it returns a vector of std errors for each of
# functions of the estimates contained in mean.  If ses=FALSE, then it returns a v-c matrix for the
# functions of the estimates contained in mean.  cov is the input v-c matrix of the estimates.
#
# This function handles the special cases of sum, cumsum, prod, cumprod.  It simply
# constructs the necessary formula or list of formula and passes them onto deltamethod
# and then returns the values.
  require(msm,quiet=TRUE)
  if(function.name=="prod")
     return(deltamethod(as.formula(paste("~",paste("x",1:length(mean),sep="",collapse="*"),sep="")),mean,cov,ses))
  if(function.name=="cumprod")
  {
     formula.list=vector("list",length=length(mean))
     formula.list[[1]]=~x1
     for (i in 2:length(mean))
        formula.list[[i]]=as.formula(paste("~",paste("x",1:i,sep="",collapse="*"),sep=""))
     return(deltamethod(formula.list,mean,cov,ses))
  }
  if(function.name=="sum")
     return(deltamethod(as.formula(paste("~",paste("x",1:length(mean),sep="",collapse="+"),sep="")),mean,cov,ses))
  if(function.name=="cumsum")
  {
     formula.list=vector("list",length=length(mean))
     formula.list[[1]]=~x1
     for (i in 2:length(mean))
        formula.list[[i]]=as.formula(paste("~",paste("x",1:i,sep="",collapse="+"),sep=""))
     return(deltamethod(formula.list,mean,cov,ses))
  }
}


