process.ch=function(ch,freq=NULL,all=FALSE)
#################################################################################
# process.ch - from a capture history creates vector of first and last times seen,
#              freq vector and indicator matrices used in log-likelihood calculations
#
#  Argument:
#        ch       - vector of character strings of 0/1
#        freq     - frequency of that ch; if null assumed to be 1; if <0 it
#                   signifies a loss on capture at last capture event
#        all      - if TRUE, computes all indicator matrices
#
#  Value: list with following elements
#        nocc        - number of capture occasions
#        freq        - absolute value of frequency for each ch
#        first       - vector of occasion numbers for first 1
#        last        - vector of occasion numbers for last 1
#        loc         - indicator of a loss on capture if set to 1
#        chmat       - capture history matrix
#        The following only returned if all==TRUE
#        FtoL        - 1's from first (1) to last (1) and 0's elsewhere
#        Fplus       - 1's from occasion after first (1) to nocc(last occasion)
#        Lplus       - 1's from occasion after last (1) to nocc
#        L           - 1's from last (1) to nocc
#        First       - 1's from occasion first (1) to nocc(last occasion)
#################################################################################
{
   nch=length(ch)
   if(is.null(freq))freq=rep(1,nch)
   nocc=nchar(ch[1])
#  create a matrix with 1:nocc in each row and one row for each ch
   nums=matrix(1:nocc,nrow=nch,ncol=nocc,byrow=TRUE)
#  create the chmat by splitting the ch string into individual elements
   chmat=matrix(as.numeric(unlist(strsplit(ch,""))),byrow=TRUE,ncol=nocc,nrow=nch)
#  store in a temp matrix and assign any 0 value to NA
   ymat=chmat
   ymat[ymat==0]=NA
#  multiply nums matrix times the chmat
   nums=nums*ymat
#  use apply to get the minimum occasion and max occasion excluding NA values
   first=apply(nums,1,min,na.rm=TRUE)
   last=apply(nums,1,max,na.rm=TRUE)
   loc=rep(0,length(first))
   loc[freq<0]=1
   freq=abs(freq)
#  using the first and last values for each ch, compute the indicator matrices as defined above
   if(all)
   {
      FtoL=t(apply(cbind(first,last),1,function(x,nocc) {return(c(rep(0,x[1]),rep(1,x[2]-x[1]),rep(0,nocc-x[2])))},nocc=nocc))
      First=t(sapply(first,function(x,nocc){return(c(rep(0,x[1]-1),rep(1,nocc-x[1]+1)))},nocc=nocc))
      Fplus=t(sapply(first,function(x,nocc){return(c(rep(0,x[1]),rep(1,nocc-x[1])))},nocc=nocc))
      Lplus=t(sapply(last,function(x,nocc){return(c(rep(0,x[1]),rep(1,nocc-x[1])))},nocc=nocc))
      L=t(sapply(last,function(x,nocc){return(c(rep(0,x[1]-1),rep(1,nocc-x[1]+1)))},nocc=nocc))
#  return a list with each of the values
      return(list(nocc=nocc,freq=freq,first=first,last=last,loc=loc,chmat=chmat,FtoL=FtoL,Fplus=Fplus,Lplus=Lplus,L=L,First=First))
   }
   else
      return(list(nocc=nocc,freq=freq,first=first,last=last,loc=loc,chmat=chmat))      
}

