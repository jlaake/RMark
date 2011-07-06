TransitionMatrix=function(x,vcv.real=NULL)
{
#
#  Make sure certain conditions are met with argument values
#
  if(is.null(x$tostratum))
    stop("\nOnly works for Psi parameter of multistrata models\n")
  if(any(rowSums(table(list(x$stratum,x$tostratum)))!=(length(levels(x$stratum))-1)))
     stop("\nx should only contain one record for each stratum/tostratum pairing\n")
#
# Compute transition values for those estimated
#
  TransitionMat=tapply(x$estimate,list(x$stratum,x$tostratum),sum)
#
# next fill in the values computed by subtraction
#
  cc=col(TransitionMat)[is.na(TransitionMat)]
  rr=row(TransitionMat)[is.na(TransitionMat)]
  missing.index=cbind(rr,cc)[order(rr),]
  subtract.value=1-rowSums(TransitionMat,na.rm=TRUE)
  TransitionMat[missing.index]= subtract.value
#
# if the vcv.real matrix has been specified, then compute se and conf intervals
# for the transition matrix.
#
  if(!is.null(vcv.real))
  {
#
#    This code computes the se for the subtract stratum
#
     pin=tapply(x$par.index,list(x$stratum,x$tostratum),sum)
     se.TransitionMat=matrix(0,nrow=dim(pin)[1],ncol=dim(pin)[1])
     se.TransitionMat[!is.na(pin)]=x$se[match(pin[!is.na(pin)],x$par.index)]
     for (i in 1:dim(se.TransitionMat)[1])
     {
        vcv.indices=match(pin[i,][!is.na(pin[i,])],as.numeric(row.names(vcv.real)))
        se.TransitionMat[i,][is.na(pin[i,])]=sqrt(sum(vcv.real[vcv.indices,vcv.indices]))
     }
#
#    Next compute the conf interval for all the transitions using the
#    logit link individually for each transition.  This may not be
#    exactly correct but it matches what is done in MARK
#
     link.se=se.TransitionMat/(TransitionMat*(1-TransitionMat))
     link=log(TransitionMat/(1-TransitionMat))
     lcl.TransitionMat=inverse.link(link-1.96*link.se,"logit")
     ucl.TransitionMat=inverse.link(link+1.96*link.se,"logit")
#
#    Add strata.labels as row and column names for the matrices
#
     row.names(se.TransitionMat)=row.names(TransitionMat)
     colnames(se.TransitionMat)=row.names(TransitionMat)
     row.names(lcl.TransitionMat)=row.names(TransitionMat)
     colnames(lcl.TransitionMat)=row.names(TransitionMat)
     row.names(ucl.TransitionMat)=row.names(TransitionMat)
     colnames(ucl.TransitionMat)=row.names(TransitionMat)
     return(list(TransitionMat=TransitionMat,se.TransitionMat=se.TransitionMat,
         lcl.TransitionMat=lcl.TransitionMat,ucl.TransitionMat=ucl.TransitionMat))
  }
  else
     return(TransitionMat)
}