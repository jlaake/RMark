print.RMark.version <- function()
{ library(help=RMark)$info[[1]] -> version
	version <- version[pmatch("Version",version)]
	um <- strsplit(version," ")[[1]]
	version <- um[nchar(um)>0][2]
	hello <- paste("This is RMark ",version,"\n",sep="")
	packageStartupMessage(hello)
}

.First.lib<-function(library,pkgname)
{
 print.RMark.version()
 checkForMark()
}


.onAttach <- function(...) { 
	print.RMark.version()
	checkForMark()
}


checkForMark<-function()
{
if(exists("MarkPath"))
{
	isep="/"
	if(substr(MarkPath,nchar(MarkPath),nchar(MarkPath))%in%c("\\","/")) isep=""
	if(R.Version()$os=="mingw32")	
		MarkPath=paste(MarkPath,"mark.exe",sep=isep)
	else
		MarkPath=paste(MarkPath,"mark",sep=isep)
	if(!file.exists(MarkPath)) 
		cat(paste("mark executable cannot be found at specified MarkPath location:",MarkPath,"\n"))		
} else
{
	if(R.Version()$os=="mingw32")
	{
		if(Sys.which("mark.exe")=="" & !file.exists("c:/Program Files/Mark/mark.exe") & !file.exists("c:/Program Files (x86)/Mark/mark.exe"))
		{
			cat("Warning: Software mark.exe not found in path or in c:/Program Files/mark or c:/Program Files (x86)/mark\n. It is available at http://www.cnr.colostate.edu/~gwhite/mark/mark.htm\n")
			cat('         If you have mark.exe, you will need to set MarkPath object to its location (e.g. MarkPath="C:/Users/Jeff Laake/Desktop"')
		}
	}
	else
	{
		if(Sys.which("mark")=="")
		{
			cat("Warning: Software mark not found in path.\n")
			cat('         If you have mark executable, you will need to set MarkPath object to its location (e.g. MarkPath="C:/Users/Jeff Laake/Desktop"')
		}  
	}
}
}
