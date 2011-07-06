"import.chdata" <-
function(filename, header=TRUE, field.names=NULL, field.types=NULL, use.comments=TRUE)
{
#
# import.chdata - reads in capture history and user specified covariates for analysis of 
#                 mark-recapture data. 
#
# Arguments:
#
# filename      - file name and path for file to be imported; fields in file should be space or tab-delimited
# header        - TRUE/FALSE; if TRUE first line is name of variables
# fied.names    - vector of field names if header=FALSE; first field should always be ch - capture history
#                 remaining number of fields and their names are arbitrary 
# field.types   - vector identifying whether fields (beyond ch) are numeric ("n") or factor ("f") or should be skipped ("s")
# use.comments  - logical; if TRUE values within /* and */ on data lines are
#                            used as row.names for the RMark dataframe.  Only use if
#                            they are unique values.
#
#
# Value: dataframe for use in MARK analysis with obligate ch field and optional covariate/grouping variables
#
#
strip.list=strip.comments(filename,use.comments=use.comments,header=header)
rn=strip.list$rn
out.filename=strip.list$out.filename
filename=out.filename
if(!is.null(field.names))header=FALSE
data=read.table(filename,colClasses=c("character"),header=header)
unlink(out.filename)
if(header & names(data)[1]!="ch") stop("First field should be named ch; Either first row doesn't contain field names or first field not named properly")
nvar=dim(data)[2]
#
# Assign field names if they are not the first line of the data file
#
if(!header)
  if(nvar==length(field.names))
     if(field.names[1]=="ch")
        names(data)=field.names
     else
        stop("First field should be the capture-history and named ch in field.names")
  else
     stop("Length of field.names does not match number of columns in data")
#
# If field.types are specified create factor and numeric variables as assigned
# Otherwise presume they are all factors
#
if(nvar>1)
{
   if(is.null(field.types)) field.types=rep("f",nvar-1)
   for( i in 2:dim(data)[2])
   {
       if(field.types[i-1] =="f")
          data[,i]=as.factor(data[,i])
       else
          if(field.types[i-1]=="n")
             data[,i]=as.numeric(data[,i]) 
   }
   data=data[,!c(FALSE,field.types=="s")]
}
row.names(data)=rn
return(data)
}
