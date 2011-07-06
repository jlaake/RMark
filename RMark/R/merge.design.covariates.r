"merge_design.covariates"<-function(ddl,df,bygroup=FALSE,bytime=TRUE)
#                   - enables design data fields to be added to the design.data(ddl)
#                    that match time fields in design data (eg effort), by group (eg site covariates)
#                    or by time and group - site and time dependent covariates.
#
# Arguments:
#
# ddl              - current design dataframe
# df               - dataframe with time and/or group specific data
# bygroup          - if TRUE, values are group specific
# bytime           - if TRUE, values are time-specific
#
# Value:
#
#  ddl - modified design data
#
# -------------------------------------------------------------------------------------------------------------
{
#
# Check to make sure bytime or bygroup
#
  if(!bygroup & !bytime) stop("\n either bygroup or bytime must be TRUE\n")
#
# Check to make sure df is a dataframe, contains the field time (if bytime=TRUE) and
# that a value is given for each time. Likewise do the same for group.
#
  if(!is.data.frame(df))stop(paste("\n",substitute(df),"is not a dataframe"))
  if(bytime)
  {
     if(is.null(df$time))stop(paste("\n",substitute(df),"does not contain field named time"))
     if(any(!ddl$time%in%unique(df$time)))
         stop(paste("\n",substitute(df),"does not contain a time value for each time in design data"))
     if(!bygroup & !is.null(df$group)) stop(paste("\n",substitute(df),"contains a group field but bygroup=FALSE"))
  }
  if(bygroup)
  {
     if(is.null(df$group))stop(paste("\n",substitute(df),"does not contain field named group"))
     if(any(!ddl$group%in%unique(df$group)))
         stop(paste("\n",substitute(df),"does not contain a group value for each group in design data"))
     if(!bytime & !is.null(df$time)) stop(paste("\n",substitute(df),"contains a time field but bytime=FALSE"))
  }
  if(bygroup & bytime)
  {
     all.gt=paste(ddl$group,ddl$time,sep="")
     all.gt.df=paste(df$group,df$time,sep="")
     if(any(!all.gt%in%all.gt.df))
         stop(paste("\n",substitute(df),"does not contain a group/time value for each group/time in design data"))
  }
#
# Check to make sure there is no overlap in field names other than group & time
#
  if(any(names(ddl)%in% names(df)[!names(df)%in%c("time","group")]))
      stop(paste("\n",substitute(df),"uses the same field names as used in design data\n"))
#
# Save row names for the case in which design data have been deleted
#
   save.row.names=row.names(ddl)
#
# Add sequence field to resort after merge
#
  ddl$xxsequence=1:dim(ddl)[1]
#
# Merge data
#
  ddl=merge(ddl,df,sort=FALSE)
#
# Re-sort data and remove field
#
  ddl=ddl[order(ddl$xxsequence),]
  ddl$xxsequence=NULL
#
# reassign row names and return data
#
  row.names(ddl)=save.row.names
  return(ddl)
}
