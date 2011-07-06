"release.gof" <-
  function(data,invisible=TRUE,title="Release gof",view=FALSE)
{
# -----------------------------------------------------------------------------------------------------------------------
#
#  release.gof        Runs release to get the goodness of fit results
#
#  Arguments:
#
#  data       - processed RMark data
#  invisible  - if TRUE, RELEASE run window is hidden from view
#  title      - title for output
#  view       - if TRUE, shows release output in a window
#
#  Value:
#
#   results   - a dataframe giving chi-square, degrees of freedom and P value
#               for TEST2, TEST3 and total of tests
#
# -----------------------------------------------------------------------------------------------------------------------
  if(is.null(data$nocc)) stop("\ndata argument must be a processed data list\n")
  nocc=data$nocc
  number.of.groups=dim(data$freq)[2]
# mxxx.tmp is used as input file for release
  outfile="mxxx.tmp"
  unlink(outfile)
  outfile=file(outfile,open="wt")
# create input file for Release
  string=paste("proc title ",title,";\nproc chmatrix occasions=",nocc," groups=",number.of.groups," FPool;")
  write(string,file=outfile)
  if(is.null(names(data$freq)))
     group.labels=paste("Group",1:number.of.groups)
  else
     group.labels=names(data$freq)

  for(j in 1:number.of.groups)
  {
      string=paste("glabel(",j,")=",group.labels[j],";",sep="")
      write(string,file=outfile,append=TRUE)
  }
  for (i in 1:dim(data$data)[1])
    write(paste(data$data$ch[i],paste(data$freq[i,],collapse=" "),";",sep="  "),file=outfile,append=TRUE)
  write("proc stop;",file=outfile,append=TRUE)
# If this is not running on Windows, then stop
# Otherwise, construct a filename that is not used of the form releasennn.tmp
# Then run release
  os=R.Version()$os
  if(os=="mingw32")
  {
	  if(!exists("MarkPath"))
	  {
		  MarkPath=Sys.which("mark.exe")
		  if(MarkPath=="")
			  if(file.exists("c:/Program Files/Mark/rel_32.exe"))
				  MarkPath=shQuote("c:/Program Files/Mark/rel_32.exe")
			  else
			  if(file.exists("c:/Program Files (x86)/Mark/rel_32.exe"))
				  MarkPath=shQuote("c:/Program Files (x86)/Mark/rel_32.exe")
			  else	
				  stop("rel_32.exe cannot be found. Add to system path or specify MarkPath object (e.g., MarkPath='C:/Program Files (x86)/Mark'")
	  }else
	  {
		  if(substr(MarkPath,nchar(MarkPath),nchar(MarkPath))%in%c("\\","/"))
			  MarkPath=shQuote(paste(MarkPath,"rel_32.exe",sep=""))
		  else
			  MarkPath=shQuote(paste(MarkPath,"rel_32.exe",sep="/"))
	  }		
    basefile = "release001"
    i = 1
    while (file.exists(paste(basefile, ".out", sep = ""))) {
        i = i + 1
        basefile = paste("release", formatC(i, flag = "0", width = max(3,ceiling(log10(i+1)))),
                        sep = "")
       }
    release.out=paste(basefile,".out",sep="")
	close(outfile)
    system(paste(MarkPath," i=mxxx.tmp"," o=",release.out,sep="" ),
            invisible = invisible)
    unlink(outfile)
#    options(def.options)
    if(!exists("MarkViewer"))
       MarkViewer="notepad"
# if view=TRUE, show release output in chosen viewer
    if(file.exists(release.out)&view)
    {
       def.options=options()
       options(useFancyQuotes=FALSE)
          system(paste(dQuote(MarkViewer),release.out),invisible=FALSE)
       options(def.options)
    }
# extract results from output file and return then;
#  search depends on whether there are groups or not
    out=readLines(release.out)
    if(number.of.groups==1)
    {
      x1=grep("Group 1 TEST 3",out,ignore.case=TRUE)
      x2=grep("Group 1 TEST 2",out,ignore.case=TRUE)
      x3=grep("TEST 3)",out,ignore.case=TRUE)
      x4=grep("   1   ",out[(x3+4):length(out)],ignore.case=TRUE)[1]
    }
    else
    {
      x1=grep("All Groups TEST 3",out,ignore.case=TRUE)
      x2=grep("All Groups TEST 2",out,ignore.case=TRUE)
      x3=grep("TEST 3)",out,ignore.case=TRUE)
      x4=grep("TOTAL",out[(x3+4):length(out)],ignore.case=TRUE)[1]
    }
    TEST3=data.frame(Chi.square=type.convert(substr(out[x1],28,39))[1],
                     df=type.convert(substr(out[x1],39,43))[1],
                     P=type.convert(substr(out[x1],44,54))[1] )
    TEST2=data.frame(Chi.square=type.convert(substr(out[x2],28,39))[1],
                     df=type.convert(substr(out[x2],39,43))[1],
                     P=type.convert(substr(out[x2],44,54))[1] )
    TEST2Plus3=data.frame(Chi.square=type.convert(substr(out[x3+x4+3],31,41))[1],
                     df=type.convert(substr(out[x3+x4+3],42,47))[1],
                     P=type.convert(substr(out[x3+x4+3],48,57))[1] )
    results=rbind(TEST2,TEST3,TEST2Plus3)
    row.names(results)=c("TEST2","TEST3","Total")
    return(results)
  } else
  {
    stop("\nRelease only works on Windows\n")
  }
}
