summary_ch=function(x,bygroup=TRUE,marray=FALSE)
{
compute.marray=function(ch,freq,n)
{
# Assumes that ch is only composed of 0/1; define function to create pairs of 1 entries
		make.pairs=function(x)if(length(x)>1) cbind(x[1:(length(x)-1)],x[2:length(x)]) else NULL
# find all string positions with a 1
		xx=gregexpr("1",ch)
# make pairs of 1 positions
		zz=sapply(xx,make.pairs)
# create a vector of ch frequencies (absolute value) which have at least 1-1 pair
		zz.freq=abs(rep.int(freq,sapply(zz, function(x)if(is.null(x)) return(0) else dim(x)[1])))
# make a matix of all the 1-1 pairs
		zz=do.call("rbind",zz)
# create m-array, assign all NA values to 0 and return it
		marray=matrix(0,nrow=n,ncol=n+1)
		xarray=tapply(zz.freq,list(zz[,1],zz[,2]),sum)
		marray[as.matrix(expand.grid(as.numeric(row.names(xarray)),as.numeric(colnames(xarray))))]=as.vector(xarray)
		marray[is.na(marray)]=0
		marray=marray[,-1]
		return(marray)
	}
#  Only use with models with 0/1 LLL format and non-robust
   if(!x$model%in%   c("CJS","POPAN","Pradel","Pradrec","LinkBarker","Pradsen","Pradlambda",
           "Closed","HetClosed","FullHet","Huggins","HugHet","HugFullHet","Jolly"))
     stop(paste("\n summary.ch will not work for ",x$model))
#  Stop if !marray and there are any negative frequencies
   if(any(x$freq<0)&!marray)
      stop("\n Use marray=TRUE, cannot compute resighting matrix with losses on capture\n")
#  Create times for resighting and releases
   times=x$begin.time+cumsum(c(0,x$time.intervals))
   releases=times[1:(length(times)-1)]
#  If this is done bygroup, split the data and freq by group
   freq.table=apply(x$freq,1,sum)
   if(bygroup&!is.null(x$data$group))
   {
     freqlist=split(freq.table,x$data$group)
     chlist=split(x$data,x$data$group)
   }
   else
   {
     chlist=list(x=x$data)
     freqlist=list(freq.table)
   }
#  create a list with one dataframe per group
   ng=length(chlist)
   table.list=vector("list",length=ng)
#  Loop over groups
   for (i in 1:ng)
   {
      table.list[[i]]=data.frame()
#     Sort capture histories
      xx=sort(chlist[[i]]$ch,decreasing=TRUE)
      freqlist[[i]]=freqlist[[i]][order(chlist[[i]]$ch,decreasing=TRUE)]
#     Turn it into a capture history matrix
      chmat=t(sapply(strsplit(xx,split=""),function(x) rbind(as.numeric(x))))
#     if marray supposed to be calculated, find last entry of each row and zero
#     it out if freq<0; exclude any that were first caught and never released
      if(marray)
      {
         max.column=cbind(1:dim(chmat)[1],apply(t(t(chmat)*1:dim(chmat)[2]),1,max))
         release.mat=chmat
         release.mat[max.column[freqlist[[i]]<0,]]=0
#        Compute the number released on each occasion
         num.released=apply(t(abs(freqlist[[i]])*release.mat),1,sum)
         marray.mat=compute.marray(xx,freqlist[[i]],length(releases))
         table.list[[i]]=cbind(num.released[1:(length(num.released)-1)],marray.mat,apply(marray.mat,1,sum))
         colnames(table.list[[i]])=c("Released",times[2:length(times)],"Total")
         rownames(table.list[[i]])=releases
      }
      else
      {
#        Get the cohort for each entry (cohort = time of first release)
         cohort=apply(times*t(chmat),2,function(x) min(x[x>0]))
#        Compute the number originally released in each cohort
         num.released=tapply(freqlist[[i]],factor(cohort),sum)
#        Compute the number recaptured at least once
         recaught = freqlist[[i]]*apply(chmat, 1, function(x) as.numeric(sum(x)>1))
         num.recaught=tapply(recaught,factor(cohort),sum)
#        For each of the release cohorts, compute the resight table which sums
#        the number resighted in each occasion following the initial release occasion
#        This uses freqlist because each ch can represent more than one individual.
         for(j in releases)
         {
           resight=apply((chmat*freqlist[[i]])[cohort==j,,drop=FALSE],2,sum)
           resight[c(releases,0)==j]=0
           nr=num.released[as.numeric(row.names(num.released))==j]
           if(length(nr)==0)nr=0
           nrc=num.recaught[as.numeric(row.names(num.released))==j]
           if(length(nrc)==0)nrc=0
           table.list[[i]]=rbind(table.list[[i]],
                     c(nr,resight[2:length(times)],nrc))
         }
         colnames(table.list[[i]])=c("Released",times[2:length(times)],"Total")
         rownames(table.list[[i]])=releases
      }
   }
   if(bygroup) names(table.list)=colnames(x$freq)
   return(table.list)
}

    
