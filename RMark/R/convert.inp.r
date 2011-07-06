convert.inp=function(inp.filename,group.df=NULL,covariates=NULL,use.comments=FALSE)
{
#
# Converts MARK encounter history format inp files to RMark dataframe
#
# Arguments:  inp.filename - name of input file (.inp assumed and optional)
#             group.df     - dataframe with grouping variables
#                             contains a row for each group defined in the input file
#                             row1=group1, row2=group2 etc.  Names and number of columns in
#                             the dataframe is set by user to define grouping variables
#                             in RMark dataframe
#             covariates   - names to be assigned to the covariates defined in the inp file
#             use.comments - logical; if TRUE values within /* and */ on data lines are
#                            used as row.names for the RMark dataframe.  Only use if
#                            they are unique values.
#
# Value: returns a dataframe with fields ch(character encounter history), freq (frequency of
#        encounter history), followed by grouping variables (if any) and then covariates (if any)
#
# Call function to strip comments and return row names (rn) and out.filename
#
   strip.list=strip.comments(inp.filename,use.comments=use.comments,header=FALSE)
   rn=strip.list$rn
   out.filename=strip.list$out.filename
#
#  Read in as a table
#
   zz=read.table(out.filename,colClasses=c("character"))
   unlink(out.filename)
#
#  If group.df is null, then there is only one group; otherwise use dim
#
   if(is.null(group.df))
      number.of.groups=1
   else
      number.of.groups=dim(group.df)[1]
   if(is.null(covariates))
      number.of.covariates=0
   else
      number.of.covariates=length(covariates)
   if(dim(zz)[2]!=number.of.covariates+number.of.groups+1)
     stop("\nNumber of columns in data file does not match group/covariate specification\n")
#
#  For each group with non-zero frequency write out a record
#  If number.of.groups > 1, add group covariates from group.df
#
   if(number.of.groups>1)
   {
      if(use.comments)
         rn=rep(rn,number.of.groups)
      else
         rn=paste(rep(1:number.of.groups,each=dim(zz)[1]),rep(rn,number.of.groups),sep=":")
      group.variables=group.df[rep(1:number.of.groups,each=dim(zz)[1]),]
   }
   else
      group.variables=NULL
   ch=as.character(rep(zz[,1],number.of.groups))
   if(number.of.covariates>0)
     for(i in 1:number.of.covariates)
        zz[,i+1+number.of.groups]=as.numeric(zz[,i+1+number.of.groups])
   if(number.of.covariates>0)
   {
      if(is.null(group.variables))
         zz=data.frame(ch=ch,as.numeric(unlist(zz[,2:(2+number.of.groups-1)])),apply(zz[,(2+number.of.groups):dim(zz)[2],drop=FALSE],2,as.numeric))
      else
      {
           xx=apply(zz[, (2 + number.of.groups):dim(zz)[2], drop = FALSE],
                2, as.numeric)
           zz = data.frame(ch = ch, as.numeric(unlist(zz[,
            2:(2 + number.of.groups - 1)])), group.variables,
            t(matrix((rep(t(xx),number.of.groups)),ncol=dim(xx)[1]*number.of.groups)))
      }
      names(zz)=c("ch","freq",names(group.df),covariates)
   }
   else
   {
      if(is.null(group.variables))
         zz=data.frame(ch=ch,as.numeric(unlist(zz[,2:(2+number.of.groups-1)])))
      else
         zz=data.frame(ch=ch,as.numeric(unlist(zz[,2:(2+number.of.groups-1)])),group.variables)
      names(zz)=c("ch","freq",names(group.df))
   }
   zz$ch=as.character(zz$ch)
   rn=rn[!zz$freq==0]
   if(length(unique(rn))!=length(rn))
      stop("\nRow names not unique. Set use.comments to default value FALSE\n")
   zz=zz[!zz$freq==0,]
   row.names(zz)=rn
   return(zz)
}
strip.comments=function(inp.filename,use.comments=TRUE,header=TRUE)
{
#
#  Read in file and strip out comments and blank lines
#
   if(!file.exists(inp.filename))
   {
      inp.filename=paste(inp.filename,".inp",sep="")
      if(!file.exists(inp.filename))
         stop("\nCannot find input file\n")
   }
   x=readLines(inp.filename)
   if(header)
   {
      header.row=x[1]
      x=x[2:length(x)]
   }
   comment.begin=grep("/\\*",x)
   comment.end=grep("\\*/",x)
   if(length(comment.end)!=0& length(comment.begin)!=0)
   {
      commentlines=cbind(comment.begin,comment.end)
      commentlines=commentlines[commentlines[,1]!=commentlines[,2],,drop=FALSE]
      if(dim(commentlines)[1]!=0)
      {
        commentlines=apply(commentlines,1,function(x) seq(x[1],x[2]))
        x=x[-(unlist(commentlines))]
      }
   }
   x=sub(";","",x)
   x=x[x!=""]
   comment.begin=regexpr("/\\*",x)
   comment.end=regexpr("\\*/",x)
   commentstrings=cbind(comment.begin,comment.end)
   fullstring=apply( commentstrings == cbind(1,nchar(x)-1),1,all)
   x=x[!fullstring]
   commentstrings=commentstrings[!fullstring,]
   withcomments=(1:length(x))[commentstrings[,1]!=-1]
   rn=1:length(x)
   for (i in withcomments)
   {
      if(use.comments)rn[i]=substr(x[i],commentstrings[i,1]+2,commentstrings[i,2]-1)
      substr(x[i],commentstrings[i,1],commentstrings[i,2]+1)=paste(rep(" ",commentstrings[i,2]+2-commentstrings[i,1]),collapse="")
   }
#
#  Write out stripped set of lines and then read in as a table
#
   out.filename=tempfile()
   if(header)x=c(header.row,x)
   writeLines(x,out.filename)
#
#  Return row names and output filename
#
   return(list(rn=rn,out.filename=out.filename))
}








