#' Reads binary file output from MARK and returns a list of the results
#'
#' Window and linux versions to read binary files created by MARK. This function written
#' by Jim Hines and modified by Jeff Laake to add derived_labels, replaces read.mark.binary
#' and read.mark.binary.linux
#'
#' @param f filename specification for binary output file from MARK;named here as markxxx.vcv
#' @param derived_labels vector of labels for derived parameters; NULL if no derived parameters for model
#' @export
#' @keywords utility
#' @aliases read.mark.binary
#' @return List of estimates, se, lcl, ucl and var-cov matrices for beta, real
#' and derived estimates \item{beta}{Dataframe for beta parameters containing
#' estimates, se, lcl, ucl} \item{beta.vcv}{variance-covariance matrix for beta
#' estimates} \item{real}{Dataframe for real parameters containing estimates,
#' se, lcl, ucl} \item{real.vcv}{variance-covariance matrix for real estimates}
#' \item{derived}{list of Dataframes for derived parameters (if any) containing
#' estimates, se, lcl, ucl} \item{derived.vcv}{variance-covariance matrix for
#' derived estimates (if any)}
#' @author Jim Hines, Jeff Laake
#' @seealso \code{\link{extract.mark.output}}
#' @examples
#' \donttest{
#' #a=readMarkVcv('~/../Downloads/mark005.vcv')
#' #str(a)
#' }

readMarkVcv <- function(f='mark001.vcv',derived_labels) 
  {
  fd      = file(f,'rb'); sz=file.size(f)  # open file and get size
  nrecs   = readBin(fd,"integer",size=4);  # get number of records
  ftype   = readChar(fd,8)                 # 8 bytes, should be "BINARY  ", ignored
  nbeta   = readBin(fd,"integer",size=4);  # number of beta params
  nreal   = readBin(fd,"integer",size=4);  # number of real params
  ndgrps  = readBin(fd,"integer",size=4);  # number of groups
  nderived= readBin(fd,"integer",size=4);  # max number of derived param values
  nderived= length(derived_labels)         # number of types of derived parameters
  itmp    = readBin(fd,"integer",size=4);  # 4 bytes ignored
  
  #  get beta parameters and var-cov matrix
  beta=cnames=NULL          # read 4 cols for est,se,lcl,ucl plus
  for (k in 1:(4+nbeta)) {  #  1 col for each col of var-cov matrix
    itmp=readBin(fd,"integer",size=4);    # ignore
    cnames=c(cnames,readChar(fd,8))       # get column label
    beta=cbind(beta,readBin(fd,"double",n=nbeta)) # get beta's for column
    rcd=readBin(fd,"integer",size=4)      # ignored
  }
  colnames(beta)=gsub(' ','',cnames)
  colnames(beta)[1:4]=c('estimate','se','lcl','ucl')
  
  #   get real parameters and var-cov matrix
  real=cnames=NULL
  for (k in 1:(4+nreal)) {
    a=readBin(fd,"integer",size=4); cnames=c(cnames,readChar(fd,8))
    real=cbind(real,readBin(fd,"double",n=nreal))
    a=readBin(fd,"integer",size=4)
  }
  colnames(real)=gsub(' ','',cnames)
  colnames(real)[1:4]=c('estimate','se','lcl','ucl')
  
  #   get derived parameters... each derived param stored as list element
  derived=list(); derived.vcv=list()
  if (nderived>0) {
    cnames=NULL
    for (k in 1:nderived) {
      zz=NULL
      itmp=readBin(fd,"integer",size=4); # ignored
      if (length(itmp)<1) break          # (At least 1 vcv file had 
                                         # wrong derived parm count)
      dervname=readChar(fd,8)            # derived parameter name
      dervname=derived_labels[k]         # use labels instead
      
      derv_colname=NULL
      zznrow=readBin(fd,"integer",size=4) # number rows in output
      itmp=readBin(fd,"integer",size=4)   # ignored
      
      for (j in 1:(4+zznrow)) {           # number of cols = nrows + 4
        itmp=readBin(fd,"integer",size=4)#  ignored
        derv_colname=c(derv_colname,readChar(fd,8))
        zz=cbind(zz,readBin(fd,"double",n=zznrow))
        itmp=readBin(fd,"integer",1,size=4); # ignored
      }
      
      colnames(zz)=gsub(' ','',derv_colname);
      colnames(zz)[1:4]=c('estimate','se','lcl','ucl')
      derived[[dervname]]=data.frame(zz[,1:4,drop=FALSE])
      derived.vcv[[dervname]]=zz[,5:ncol(zz),drop=FALSE]
    }
  }
  close(fd);
  
  rv=list(beta=data.frame(beta[,1:4]), beta.vcv=beta[,5:ncol(beta)],
          real=data.frame(real[,1:4]), real.vcv=real[,5:ncol(real)],
          derived=derived, derived.vcv=derived.vcv)
}
