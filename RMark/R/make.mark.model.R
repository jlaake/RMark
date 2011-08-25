"make.mark.model" <-
function(data,ddl,parameters=list(),title="",model.name=NULL,initial=NULL,covariates=NULL,call=NULL,
            simplify=TRUE,default.fixed=TRUE,options=NULL,profile.int=FALSE,chat=NULL)
{
# -----------------------------------------------------------------------------------------------------------------------
#
# make.mark.model -   creates a MARK model object that contains an input file, design matrix
#                     and design data for MARK.  It is constructed from a data frame, formulas for design matrix
#                     and optional design components,fixed parameters, and initial values
#
# Arguments:
#
# data             - data list resulting from function process.data
# ddl              - design data list which contains an element for each parameter type
# parameters       - list of parameter model specifications
# title            - a title for the analysis
# model.name       - optional model name
# initial          - vector of initial values for beta parameters or name of a MARK model object with output
# covariates       - list of covariate names used in added components that are not specified in formulae
# call             - can be used to pass function call when this function is called from another function (eg mark)
# simplify         - if TRUE, simplifies PIM structure to match unique number of rows in design matrix
# default.fixed    - if TRUE, default fixed values are assigned to any parameters missing from the full design data
# options          - character string of options for Proc Estimate statement in MARK .inp file
#
# Value:
#
#  model           - a MARK model object
#
# Functions used:  valid.parameters, setup.parameters,
#
# -----------------------------------------------------------------------------------------------------------------------


#  *******************  INTERNAL FUNCTIONS    *********************************
#
#  print.pim: prints pim file to outfile for use in constructing MARK input
#
"print.pim" <-
function(pim,outfile)
{
#
# Arguments:
#
# pim     - pim matrix
# outfile - name of output file to write pim
#
# Value: None
#
# Define internal function "xp" that pastes non-zero values together with
# intervening spaces
#
xp=function(x){paste(x[x>0],collapse=" ")}
#
# For each row in the pim, apply xp to create a vector of concatenated values
# and then paste a ";" to end of each value
#
if(is.matrix(pim))
   strings=paste(apply(pim,1,xp),";")
else
   strings=paste(paste(pim,collapse=" "),";")
#
# Output strings
#
write(strings,outfile,append=TRUE)
return(NULL)
}
#
# spell: changes capitalization on links so it will be acceptable to mark interface
#
"spell" <- function(links)
{
  newlinks=links
  newlinks[newlinks=="logit"]="Logit"
  newlinks[newlinks=="mlogit"]="MLogit"
  newlinks[newlinks=="log"]="Log"
  newlinks[newlinks=="loglog"]="LogLog"
  newlinks[newlinks=="cloglog"]="CLogLog"
  newlinks[newlinks=="identity"]="Identity"
  newlinks[newlinks=="sin"]="Sin"
  return(newlinks)
}
#
#  realign.pims: realigns pim values to represent structure of design matrix
#
"realign.pims" <-
function(model){
#
#  Arguments:
#
#  model - a mark model object that has been created by make.mark.model
#
#  Value:
#
#  new.indices - a vector of new indices for the old PIM values.  The old
#                PIM values are 1:length(new.indices) and the new index is
#                the corresponding value.  For example, new.indices=c(1,1,2,2)
#                renumbers the structure 1,2,3,4 such that 1,2 are now 1
#                and 3,4 are now 2.
#
#  Get all the unique rows in the design matrix and paste all the values
#  in each row together.
#
   uniquevals=apply(unique(model$design.matrix),1,paste,collapse="")
#
#  Get all the rows in the design matrix and paste all the values
#  in each row together.
#
   allvals=apply(model$design.matrix,1,paste,collapse="")
#
#  Find the corresponding sets of indices by matching allvals into uniquevals
#
   new.indices=match(allvals, uniquevals)
#
#  Next cope with fixed real parameters; first determine the unique fixed values
#
   uniquefixedvalues=unique(model$fixed$value)
#
#  Now create a parameter index for each of the unique fixed real parameters
#  assuming that they are all different from parameters defined by design
#  matrix (ie add onto max of uniqueindices),
#
   uniqueindices=match(model$fixed$value,uniquefixedvalues)+max(new.indices)
#
#  Assign these new indices to their position in the original PIM set
#
   new.indices[model$fixed$index]=uniqueindices
#
#  Some may overlap and others are new, so they need to be renumbered once again
#  eliminating extra ones
#
   new.indices=match(new.indices,sort(unique(new.indices)))
#
#  Simplification cannot occur for parameters with an mlogit link, so these
#  must be given new indices
#     Following lines force non-simplification for mlogit parameters
   mlogit.parameters=substr(model$links,1,6)=="mlogit" | substr(model$links,1,6)=="MLogit"
   new.indices[mlogit.parameters]=(1:sum(as.numeric(mlogit.parameters)))+max(new.indices)
   new.indices=match(new.indices,sort(unique(new.indices)))
#
#   This code that was added in v1.4.5 hsa been removed for v1.6.6 once it became
#   clear that you cannot simplify mlogit parameters even in the mlogit set
#   because the summation is only restricted based on the unique real parameters.
#   Some addtional code in make.mark.model that creates the mlogit.structure
#   can also probably be eliminated .
#
#   mlogit.parameters=substr(model$links,1,6)=="mlogit"
#   if(any(mlogit.parameters))
#   {
#      new.indices[mlogit.parameters]=max(new.indices[mlogit.parameters])*(mlogit.list$structure-1)+
#                                     new.indices[mlogit.parameters] + max(new.indices)
 #
 #    The following code was added to adjust for different model structures within
 #    a stratum across tostratum.  Without it the mlogit structure doesn't not work
 #    for models with sum tostratum interactions.
 #
#      for (i in 1:max(mlogit.list$structure))
#      {
#         mm=matrix(new.indices[mlogit.parameters][mlogit.list$structure==i],ncol=mlogit.list$ncol)
#         umm = unique(mm)
#         rownames(umm) = 1:nrow(umm)
#         if(nrow(umm)>1)
#         {
#            unvals=apply(umm,1,paste,collapse="")
#            mmvals=apply(mm,1,paste,collapse="")
#            umm=max(new.indices)+(col(umm)-1)*nrow(umm)+row(umm)
#            mm=umm[as.numeric(names(unvals)[match(mmvals,unvals)]),]
#            new.indices[mlogit.parameters][mlogit.list$structure==i]=as.vector(mm)
#         }
#     }
#      new.indices=match(new.indices,sort(unique(new.indices)))
#  }
   return(new.indices)
}
#
#  renumber.pims: using the vector of new indices that match the old
#                 structure to the new structure, change the values in the
#                 pim argument.  The way this is done depends on whether it is
#                 a square or triangular PIM.
#

"renumber.pims" <-
function(pim,newlist,type){
if(type=="Triang")
{
   pim=t(pim)
   pim[lower.tri(pim,TRUE)]=newlist[pim]
   return(t(pim))
} else
   return(newlist[pim])
}
#check.mlogits=function(model)
#{
#   mlogit.parameters=substr(model$links,1,6)=="mlogit" | substr(model$links,1,6)=="MLogit"
#   if(any(mlogit.parameters))
#   {
#      for (i in 1:max(mlogit.list$structure))
#      {
#         mm=matrix(model$links[mlogit.parameters][mlogit.list$structure==i],ncol=mlogit.list$ncol)
#         if(!all(apply(mm,1,function(x)return(length(unique(x))==1))))
#         {
#            cat("\n Error in mlogit assignment. This should not happen. Contact maintainer.\n")
#            print(mm)
#            stop()
#         }
#      }
#   }
#   invisible()
#}

"pim.header"<- function(group,param.name,parameters,ncol,stratum,tostratum,strata.labels,mixtures,session=NULL)
{
  if(!is.null(stratum)&length(strata.labels)>0)
     if(!is.null(tostratum))
        stratum.designation=paste(strata.labels[stratum],"to",strata.labels[tostratum])
     else
        stratum.designation= paste(strata.labels[stratum],":Stratum",stratum,sep="")
  else
     stratum.designation=""
  if(is.null(session))
     session.designation=""
  else
     session.designation=paste("Session",session)
  if (parameters$type == "Triang")
         string = paste(paste("group=", group,sep=""), param.name, stratum.designation, session.designation, 
                           paste(" rows=",ncol," cols=",ncol,sep=""), parameters$type, ";")
  else
      if(mixtures==1)
          string=paste(paste("group=",group,sep=""),param.name,stratum.designation, session.designation, 
                           paste(" rows=1"," cols=",ncol,sep=""),parameters$type,";")
      else
          if(!is.null(parameters$mix) && parameters$mix)
            string=paste(paste("group=",group,sep=""),param.name,stratum.designation,session.designation, 
              paste(" rows=",mixtures+parameters$rows," cols=",ncol,sep=""),parameters$type,";")
          else
            string=paste(paste("group=",group,sep=""),param.name,stratum.designation,session.designation, 
                          paste(" rows=1"," cols=",ncol,sep=""),parameters$type,";")
return(string)
}
"simplify.pim.structure" <-
function(model)
{
#
# simplify.pim.structure: renumbers PIMS to represent model structure that
#                         was created with the formula. It takes the input
#                         for MARK created in the model by make.mark.model
#                         with the formulas and simplies the PIM structure
#                         represented by the unique rows in the design matrix.
#                         It recreates the new input for MARK to reflect the
#                         change and adds it and some other fields to model
#                         for the pim translation.
#
# Arguments:
#
#  model - a mark model object that has been created by make.mark.model
#
#
# Value:
#
#  model - same mark model object with added list elements simplify
#          and a rewritten input object for MARK
#
#
#
#  Beginning of simplify.pim.structure function; it recreates input for
#  MARK and uses an outfile like make.mark.model
#
outfile="mxxx.tmp"
#
# Use realign.pims to simplify PIM structure
#
new.indices=realign.pims(model)
#
# Copy first portion of the MARK input file because it will be unchanged by
# the simplification
#
input=model$input[1:grep("model=\\{",model$input)]
writeLines(input,outfile)
#
#  If there are fixed real parameters then these need to be included in the
#  design matrix if they are not already done so by the formula.
#
if(!is.null(model$fixed))
{
#
#  Get the unique new indices,values from the original indices for the fixed parameters
#  in model$fixed$index
#
   fixed.parms=unique(data.frame(index=new.indices[model$fixed$index],value=model$fixed$value))
   fixed=NULL
   num.fixed=dim(fixed.parms)[1]
#
#  For each fixed real parameter write out the input strings for MARK to assign
#  the number of fixed values and each fixed index (parm) and its value.
#
   for (i in 1:num.fixed)
      fixed = c(fixed, paste("parm(", fixed.parms$index[i], ")=", fixed.parms$value[i],
                    sep = ""))
   string = paste("fixed=", num.fixed, ";",sep="")
   write(string, file = outfile, append = TRUE)
   write(paste(fixed, ";"), file = outfile, append = TRUE)
}
#
#  Assign some values from the model that are used below
#
parameters=model$parameters
param.names=sub("DoublePrime","''",names(parameters))
param.names=sub("Prime","'",param.names)
number.of.groups=model$number.of.groups
nocc=model$nocc
if(is.null(model$mixtures))
   mixtures=1
else
   mixtures=model$mixtures
#
#  For each type of parameter, output the new PIM structure; This largely
#  follows code in make.mark.model except it uses renumber.pims and print.pim
#  to renumber and print out the pim structure.
#
for (i in 1:length(parameters)) {
     for (j in 1:length(model$pims[[i]]))
     {
         ncol = dim(model$pims[[i]][[j]]$pim)[2]
         string=pim.header(pim[[i]][[j]]$group,param.names[i],parameters[[i]],
                   ncol,model$pims[[i]][[j]]$stratum,model$pims[[i]][[j]]$tostratum,model$strata.labels,mixtures,model$pims[[i]][[j]]$session)
         write(string, file = outfile, append = TRUE)
         if(parameters[[i]]$type == "Triang")
         {
            newpim=renumber.pims(model$pims[[names(model$parameters)[i]]][[j]]$pim,new.indices,parameters[[i]]$type)
            print.pim(newpim,outfile)
         }
         else
         {
             nmix=1
             if(mixtures>1)
               if(!is.null(parameters[[i]]$mix) && parameters[[i]]$mix)
                   nmix=mixtures+parameters[[i]]$rows
             for(k in 1:nmix)
             {
               newpim=renumber.pims(model$pims[[names(model$parameters)[i]]][[j]]$pim[k,],new.indices,parameters[[i]]$type)
               print.pim(newpim,outfile)
             }
         }
     }
}
#
# Look for setting of initial values in the input file; if found write them out
# as is to new input file.  Simplifying the PIM structure does not change the
# column structure of the design matrix and therefore does not change the
# definition of the betas.
#
if(length(grep("initial ",model$input))!=0)
{
  string=model$input[grep("initial ",model$input)]
  write(string, file = outfile, append = TRUE)
}
#
#  If profile intervals requested write out needed statements
#

if(is.null(model$chat))chat=1
if(is.numeric(model$profile.int))
{
   if(any(!model$profile.int%in%new.indices)) 
      stop(paste("Profile interval argument requests values not in beta: 1 to ",
                      ncol(model$design.matrix),"\n"))
      string=paste(paste("Profile Intervals chat=",format(chat,digits=5),sep=""),
                          paste(model$profile.int,collapse=" ")) 
      write(paste(string,";",sep=""),file=outfile,append=TRUE)   
}
else
{
    if(model$profile.int)
    {
      string=paste(paste("Profile Intervals chat=",format(chat,digits=5),sep=""),
                            paste(unique(new.indices),collapse=" "))
      write(paste(string,";",sep=""),file=outfile,append=TRUE)      
    } 
}
#
# Next output the new simplified design matrix.  rownums is the row numbers (indices)
# from the original design matrix but there is only one for each of the new
# parameters with indices 1:length(new.indices).  The new design matrix
# (complete.design.matrix) is obtained by subsetting the rows from the
# original design matrix matching rownums.  This is done using row.names to be
# able to use subset so it will always yield a dataframe.  Using indices for
# row numbers can result in a vector if there is only a single beta.
rownums=match(1:length(unique(new.indices)),new.indices)
# 22-Aug-05; change to use [rownums,] to accomodate fixed parameters
# 1 feb 06; modified to cope with single element selected
if(length(rownums)==1)
   complete.design.matrix=subset(model$design.matrix,1:dim(model$design.matrix)[1]%in%rownums)
else
   complete.design.matrix=model$design.matrix[rownums,]
# 10 Jan 06; change to accomodate S(.) with known fate where design matrix can
# become a single element with simplification
#if(is.vector(complete.design.matrix))
#{
#   complete.design.matrix=as.matrix(complete.design.matrix)
#   row.names(complete.design.matrix)=row.names(model$design.matrix)[1]
#}
#
# Write out the design matrix into the MARK input file
#
string = paste("design matrix constraints=", dim(complete.design.matrix)[1],
        " covariates=", dim(complete.design.matrix)[2], ";",sep="")
write(string, file = outfile, append = TRUE)
write.table(complete.design.matrix, file = outfile, eol = ";\n",
       sep = " ", col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
#
# If there is a link specification for models that use different links for
# each real parameter, write those out now shifting them for translation of
# indices.
#
# 11 Jan 06; modified code for new format of links input
#  1 feb 06; fixed links code to work with simplification - not done previously
#  6 Apr 06; added code to deal with simplification within mlogit parameters
if (!is.null(model$links))
{
   if(length(model$links)>1)
   {
     newlinks=model$links[match(unique(new.indices), new.indices)][order(unique(new.indices))]
     xi=grep("mlogit(",newlinks,fixed=TRUE)
     logit.numbers=as.numeric(gsub(")","",gsub("mlogit(","",newlinks[xi],fixed=TRUE),fixed=TRUE))
     logit.numbers=match(logit.numbers,sort(unique(logit.numbers)))
     newlinks[xi]=paste("mlogit(",logit.numbers,")",sep="")
     write(paste("links=",length(unique(new.indices)),";",sep=""),file=outfile,append=TRUE)
     write(paste(spell(newlinks),";",sep=""), file = outfile, append = TRUE)
   }
   else
     newlinks=NULL
}
else
   newlinks=NULL
#
# Write out labels for betas
#
if(!is.matrix(complete.design.matrix))
   complete.design.matrix=as.matrix(complete.design.matrix)
string = paste("blabel(", 1:dim(complete.design.matrix)[2], ")=", colnames(complete.design.matrix),";",sep="")
write(string, file = outfile, append = TRUE)
#
# Write out labels for real parameters
#
rnames = rep("", dim(complete.design.matrix)[1])
ipos = 0
string = paste("rlabel(", 1:dim(complete.design.matrix)[1], ")=", row.names(complete.design.matrix), ";",sep="")
write(string, file = outfile, append = TRUE)
#
#  Complete with stop statement; then read the outfile into the input vector to
#  store in the model object.  delete the output file and add the fields to the
#  model object and return it.
#
write("proc stop;", file = outfile, append = TRUE)
text = readLines(outfile)
unlink(outfile)
model$input=text
if(!is.null(newlinks))
   model$simplify=list(design.matrix=complete.design.matrix,pim.translation=new.indices,links=newlinks)
else
   model$simplify=list(design.matrix=complete.design.matrix,pim.translation=new.indices)
return(model)
}

#
# Create internal function to create a pim
#
create.pim=function(nocc,parameters,npar,mixtures)
{
    ncol=nocc+parameters$num
    mat=NULL
    if(parameters$type=="Triang")
    {
        for(k in 1:(nocc+parameters$num))
        {
            if(parameters$pim.type=="all")
            {
                mat=rbind(mat,c(rep(0,k-1),npar:(npar+ncol-1)))
                npar=npar+ncol
            }
            else
            {
               if(parameters$pim.type=="time")
                   mat=rbind(mat,c(rep(0,k-1),(npar+k-1):(npar+k-1+ncol-1)))
               else
                   mat=rbind(mat,c(rep(0,k-1),rep(npar,ncol)))
            }
            ncol=ncol-1
        }
#        if(parameters$pim.type!="all")
#            npar=max(mat)+1
   }
   else
   {
        nmix=1
        if(mixtures>1)
            if(!is.null(parameters$mix)&&parameters$mix)
                nmix=mixtures+parameters$rows
            for(k in 1:nmix)
            {
                mat=rbind(mat,npar:(npar+ncol-1))
                npar=npar+ncol
            }
   }
return(mat)
}
#
# Creates time-dependent covariates for age of nest if there
# is a field called AgeDay1 in the data.  Then the field NestAge
# can be used in the formula for S.
#
create.agenest.var=function(data,init.agevar,time.intervals)
{
   nocc=length(time.intervals)
   age.mat=matrix(data[,init.agevar],nrow=dim(data)[1],ncol=nocc-1)
   age.mat=t(t(age.mat)+cumsum(c(0,time.intervals[1:(nocc-2)])))
   age.mat=data.frame(age.mat)
   names(age.mat)=paste("NestAge",1:(nocc-1),sep="")
   return(age.mat)
}


#
#  *******************  END OF INTERNAL FUNCTIONS    *********************************
#
# Outfile is assigned a temporary name
#
  outfile="mxxx.tmp"
#
#  check to make sure all entered as lists
#
  if(length(parameters)!=0)
  for(i in 1:length(parameters))
     if(!is.list(parameters[[i]]))
     {
        cat("\nEach parameter distribution must be specified as a list\n")
        stop()
     }
#
# Check validity of parameter types, if any given
#
  if(!valid.parameters(data$model,parameters))stop()
#
# Next check validity of fields defined in each parameter list.
#
  if(length(parameters)>0)
  for(i in 1:length(parameters))
     for(j in 1:length(names(parameters[[i]])))
        if(!(names(parameters[[i]])[j]%in%c("fixed","formula","component","component.name","link","share","remove.intercept","default")))
        {
           cat("\nInvalid model specification for parameter ",names(parameters)[i],".\nUnrecognized element ",names(parameters[[i]])[j])
           stop()
        }
#
# Initialize some variables
#
  ch=data$data$ch
  mixtures=data$mixtures
  poolpandc=FALSE
  poolgammas=FALSE
  pool.ps=FALSE
  nocc=data$nocc
  nocc.secondary=data$nocc.secondary
  nstrata=data$nstrata
  number.of.groups=dim(data$freq)[2]
  par.list=setup.parameters(data$model,check=TRUE)
  parameters=setup.parameters(data$model,parameters,nocc,number.of.groups=number.of.groups)
  parameters=parameters[par.list]
  temp.rev=data$reverse
  data$reverse=FALSE
  full.ddl=make.design.data(data,parameters=ddl$pimtypes)
  data$reverse=temp.rev
  parameters=parameters[names(parameters)%in%names(full.ddl)]
  for(j in names(parameters))
  {
     parameters[[j]]$pim.type=ddl$pimtypes[[j]]$pim.type
     if(!is.null(ddl$pimtypes[[j]]$subtract.stratum))
        parameters[[j]]$subtract.stratum=ddl$pimtypes[[j]]$subtract.stratum
  }
  for(i in 1:length(parameters))
  {
     if(names(parameters)[i]!="p2" &names(parameters)[i]!="c" & names(parameters)[i]!="GammaPrime")
     {
        if(is.null(parameters[[i]]$formula))parameters[[i]]$formula=~1
        if((parameters[[i]]$formula==~group) & number.of.groups==1)parameters[[i]]$formula=~1
     }
     else
#
#    The following handles sharing of p and c in closed models and gamma in robust models;
#    if p&c not shared it sets default c model to ~1
#
     {
        if(names(parameters)[i]=="c")
           if(!parameters$p$share)
           {
              if(is.null(parameters$c$formula))parameters$c$formula=~1
              poolpandc=FALSE
           }
           else
              poolpandc=TRUE
        if(names(parameters)[i]=="p2")
           if(!parameters$p1$share)
           {
              if(is.null(parameters$p2$formula))parameters$p2$formula=~1
              pool.ps=FALSE
           }
           else
              pool.ps=TRUE
        if(names(parameters)[i]=="GammaPrime")
           if(!parameters$GammaDoublePrime$share)
           {
              if(is.null(parameters$GammaPrime$formula))parameters$GammaPrime$formula=~1
              poolgammas=FALSE
           }
           else
              poolgammas=TRUE
     }
#
#     Test validity of link functions
# 
     if(!(parameters[[i]]$link %in% c("identity","log","logit","mlogit","loglog","cloglog","sin")))
     {
        cat("\nInvalid link value ",parameters[[i]]$link)
        stop()
     }
  }
  param.names=sub("DoublePrime","''",names(parameters))
  param.names=sub("Prime","'",param.names)
  model.list= setup.model(data$model,0)
  etype=model.list$etype
#	
# Output data portion of MARK input file:
#   proc title
#   proc chmatrix
#
  if(etype=="Nest")
  {
     zz=subset(data$data,select=c("FirstFound","LastPresent","LastChecked","Fate"))
     zz=cbind(zz,rowSums(data$freq))
     if(!is.null(data$data$AgeDay1))
        data$data=cbind(data$data,create.agenest.var(data$data,"AgeDay1",data$time.intervals))
  }
  else
  {
     zz=as.data.frame(ch)
     zz=cbind(zz,data$freq)
  }
#
# p&c in closed models, gammas in robust models, and p1-p2 in MS Occupancy are handled differently
# to allow shared parameters.  If there is no c$formula, gammaDoublePrime$formula, or p2$formula then the design data
# are appended to the shared parameters (p for c and gammaPrime for gammaDoublePrime)
# In the design data for p a covariate "c" is added to the recapture parameters and for
# a covariate emigrate for the gammaDoublePrime parameters.
#
  if(!is.null(parameters$p))
     if(!is.null(parameters$c))
        if(poolpandc)
        {
           if(dim(ddl$p)[2]==dim(ddl$c)[2])
           {
              ddl$p=rbind(ddl$p,ddl$c)
              ddl$p$c=c(rep(0,(dim(ddl$p)[1]-dim(ddl$c)[1])),rep(1,dim(ddl$c)[1]))
              row.names(ddl$p)=1:dim(ddl$p)[1]
           }
           else
           {
              cat("Error: for a shared p&c model, their design data columns must match\n. If you add design data to p it must also be added to c.\n")
              cat("Columns of p: ",names(ddl$p),"\n") 
              cat("Columns of c: ",names(ddl$c),"\n")
              stop("Function terminated\n") 
           }
        }
  if(!is.null(parameters$p1))
     if(!is.null(parameters$p2))
        if(pool.ps)
        {
           if(dim(ddl$p1)[2]==dim(ddl$p2)[2])
           {
              ddl$p1=rbind(ddl$p1,ddl$p2)
              ddl$p1$p2=c(rep(0,(dim(ddl$p1)[1]-dim(ddl$p2)[1])),rep(1,dim(ddl$p2)[1]))
              row.names(ddl$p1)=1:dim(ddl$p1)[1]
           }
           else
           {
              cat("Error: for a shared p1&p2 model, their design data columns must match\n. If you add design data to p1 it must also be added to p2.\n")
              cat("Columns of p1: ",names(ddl$p1),"\n")
              cat("Columns of p2: ",names(ddl$p2),"\n")
              stop("Function terminated\n")
           }
        }
  if(!is.null(parameters$GammaDoublePrime))
     if(!is.null(parameters$GammaPrime))
        if(poolgammas)
        {
           if(dim(ddl$GammaDoublePrime)[2]==dim(ddl$GammaPrime)[2])
           {
              ddl$GammaDoublePrime=rbind(ddl$GammaDoublePrime,ddl$GammaPrime)
              ddl$GammaDoublePrime$emigrate=c(rep(1,(dim(ddl$GammaDoublePrime)[1]-dim(ddl$GammaPrime)[1])),rep(0,dim(ddl$GammaPrime)[1]))
              row.names(ddl$GammaDoublePrime)=1:dim(ddl$GammaDoublePrime)[1]
           }
           else
           {
              cat("Error: for a shared Gamma model, their design data columns must match\n. If you add design data to GammaPrime it must also be added to GammaDoublePrime.\n")
              cat("Columns of GammaPrime: ",names(ddl$GammaPrime),"\n")
              cat("Columns of GammaDoublePrime: ",names(ddl$GammaDoublePrime),"\n")
              stop("Function terminated\n")
           }
        }
#
# For each parameter type determine which values in the formula are covariates that need to be
# added to design data and put in data portion of input file.
#
  xcov=list()
  time.dependent=list()
  session.dependent=list()
  for(i in 1:length(parameters))
  {
     if(!is.null(parameters[[i]]$formula))
     {
#
#    First get the variables in the formula.  Identify those not in the design data and make sure
#    that they are in the data. If there are any, add the covariate to the covariate list and add a column 
#    to the design data.
#
#    Note parx is the name of the ith parameter.  ddl is always constructed in the same order for each model
#    but the order of the parameters in the model specification may be different, therefore the indexing into
#    ddl must be done by name rather than position from the parameters specification.
#
      parx=names(parameters)[i]
      vars=all.vars(parameters[[i]]$formula)
      termslist=colnames(attr(terms(parameters[[i]]$formula),"factors"))
      xcov[[parx]]=vars[!(vars%in%names(ddl[[parx]]))]
      time.dependent[[parx]]=rep(FALSE,length(xcov[[parx]]))
      session.dependent[[parx]]=rep(FALSE,length(xcov[[parx]]))
      if(any(!(vars%in%names(ddl[[parx]]))))
         for(j in 1:length(xcov[[parx]]))
         {
            if(!(xcov[[parx]][j]%in%names(data$data)))
            {
              if(!is.null(full.ddl[[parx]]$session))
              {
                 cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$session),as.character(ddl[[parx]]$time),sep=""))
                 if(any(!cov.bytime%in%names(data$data)))
                 {
                    session.dependent[[parx]][j]=TRUE                 
                    cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$session),sep=""))
                 }
              }                 
              else
                 cov.bytime=unique(paste(xcov[[parx]][j],as.character(ddl[[parx]]$time),sep=""))
              if(all(!cov.bytime%in%names(data$data)))
                 stop("\nError: Variable ",xcov[[parx]][j]," used in formula is not defined in data\n")
              else
              {
                 time.dependent[[parx]][j]=TRUE
                 if(any(!cov.bytime%in%names(data$data)))
                 {
                     cat(paste("\nThe following covariates are missing:",cov.bytime[!cov.bytime%in%names(data$data)],collapse=""))
                     cat("\nIf any are used in the resulting model it will fail\n")
                     cov.bytime=cov.bytime[cov.bytime%in%names(data$data)]
                 }
              }
            }
            savenames=names(ddl[[parx]])
            ddl[[parx]]=cbind(ddl[[parx]],rep(1,dim(ddl[[parx]])[1]))
            names(ddl[[parx]])=c(savenames,xcov[[parx]][j])
            if(!time.dependent[[parx]][j])
               covariates=c(covariates,xcov[[parx]][j])
            else
               covariates=c(covariates,cov.bytime)
         }
     }
  }
#
# Output title and list of covariates
# 11 Jan 06; Added code for multistratum - nstrata and strata labels
#
  if(is.null(nocc.secondary))
     string=paste("proc title ",title,";\nproc chmatrix occasions=",nocc," groups=",number.of.groups," etype=",etype)
  else
     string=paste("proc title ",title,";\nproc chmatrix occasions=",sum(nocc.secondary)," groups=",number.of.groups," etype=",etype)
  if(etype=="Multistrata"|etype=="ORDMS"|etype=="CRDMS"|etype=="MSLiveDead")string=paste(string," strata=",data$nstrata,sep="")
  if(!is.null(covariates))
  {
     covariates=unique(covariates)
     string=paste(string," icovar = ",length(covariates))
  }
  if(mixtures!=1)
      string=paste(string," mixtures =",mixtures)
  time.int=data$time.intervals
  if(data$reverse)time.int[time.int==0]=1
  string=paste(string," ICMeans NoHist hist=",dim(zz)[1],
           ";\n time interval ",paste(time.int,collapse=" "),";")
  if(etype=="Multistrata"|etype=="ORDMS"|etype=="MSLiveDead"|etype=="CRDMS")string=paste(string,"\n strata=",paste(data$strata.labels[1:data$nstrata],collapse=" "),";",sep="")
  if(!is.null(covariates))
  {
     string=paste(string,"\nicovariates ",paste(covariates,collapse=" "),";")
     any.factors=sapply(data$data[,covariates,drop=FALSE],is.factor)
     if(any(any.factors))
        stop(paste("The following individual covariates are not allowed because they are factor variables: ",paste(names(data$data[,covariates,drop=FALSE])[any.factors],collapse=",")))
     else
     {
        any.na=apply(data$data[,covariates,drop=FALSE],2,function(x) any(is.na(x)))
        if(any(any.na))
           stop(paste("The following individual covariates are not allowed because they contain NA: ",paste(names(data$data[,covariates,drop=FALSE])[any.na],collapse=",")))
        else
           zz=data.frame(cbind(zz,data$data[,covariates]))
     }
  }
  write(string,file=outfile)
#
#  Output group labels
#
  if(is.null(names(data$freq)))
     group.labels=paste("Group",1:number.of.groups)
  else
     group.labels=names(data$freq) 
  for(j in 1:number.of.groups)
  {
      string=paste("glabel(",j,")=",group.labels[j],";",sep="")
      write(string,file=outfile,append=TRUE)
  }
#
# This outputs capture history, frequency and any covariates
#
  if(etype=="Nest")
  {
     for(i in 1:number.of.groups)
     {
        string=paste("Nest Survival Group =", i, ";")
        write(string,file=outfile,append=TRUE)
        if(number.of.groups>1)
            write.table(zz[data$data$group==i,],file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
        else
            write.table(zz,file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
     }
  }
  else
     write.table(zz,file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
#
# Output counts section of Mark-resight models if appropriate
#
  if(!is.null(data$counts))
	  for(i in 1:number.of.groups)
	  {
		  for(j in names(data$counts))
		  {
			  write("",file=outfile,append=TRUE)
			  string=paste(j,"Group =", i, ";")
		      write(string,file=outfile,append=TRUE)
		      if(number.of.groups==1)
		          string=paste(paste(data$counts[[j]],collapse=" "),";",sep="")
		      else
			      string=paste(paste(data$counts[[j]][i,],collapse=" "),";",sep="")
 		      write(string,file=outfile,append=TRUE)
		  }
	  }  
#
# First create model name using each parameter unless a model name was given as an argument;
#
  if(is.null(model.name))
  {
    model.name=""
    for(i in 1:length(parameters))
    {
       model.name=paste(model.name,param.names[i],"(",paste(parameters[[i]]$formula,collapse=""),sep="")
       if(!is.null(parameters[[i]]$component))
          if(is.null(parameters[[i]]$component.name))
             model.name=paste(model.name,"+component",sep="")
          else
             model.name=paste(model.name,"+",parameters[[i]]$component.name,sep="")
       model.name=paste(model.name,")",sep="")
     }
  }
#
# Next determine if a single link was used or differing links
#
  link=parameters[[1]]$link
  for(i in 1:length(parameters))
     if(link!=parameters[[i]]$link)link="Parm-Specific"
#
# Next output proc Estimate 
#
  string=paste(paste("proc estimate link=",spell(link),sep=""),"NOLOOP varest=2ndPart ",options," ;\nmodel={",model.name,"};")
  write(string,file=outfile,append=TRUE)
#
# Next compute PIMS for each parameter in the model - these are the all different PIMS
# 11 Jan 06 code added for multistrata models
#
  pim=list()
  npar=1
  for(i in 1:length(parameters))
  {
     pim[[i]]=list()
     k=0
     for(j in 1:number.of.groups)
     {
       for (jj in 1:nstrata)
       {
          other.strata=1
          if(!is.null(parameters[[i]]$tostrata))
          {
             nsubtract.stratum=match(parameters[[i]]$subtract.stratum,data$strata.labels)
             other.strata= sequence(nstrata)[sequence(nstrata)!=nsubtract.stratum[jj]]
          }
          for(to.stratum in other.strata)
          {
               if(model.list$robust && parameters[[i]]$secondary)
                  num.sessions=nocc
               else
                  num.sessions=1
               for (l in 1:num.sessions)
               {
                  k=k+1
                  pim[[i]][[k]]=list()
                  if(num.sessions==1)
                     pim[[i]][[k]]$pim=create.pim(nocc,parameters[[i]],npar,mixtures)
                  else
                  {
                     if(is.na(parameters[[i]]$num))
                     {
                         parameterx=parameters[[i]]
                         parameterx$num=0
                         pim[[i]][[k]]$pim=create.pim(1,parameterx,npar,mixtures)
                     }
                     else
                         pim[[i]][[k]]$pim=create.pim(nocc.secondary[l],parameters[[i]],npar,mixtures)
                     pim[[i]][[k]]$session=l
                  }
                  pim[[i]][[k]]$group=j
                  if(length(data$strata.labels)>0) pim[[i]][[k]]$stratum=jj
                  if(!is.null(parameters[[i]]$tostrata)) pim[[i]][[k]]$tostratum=to.stratum
                  npar=max(pim[[i]][[k]]$pim)+1
               }
           }
       }
     }
  }
  npar=npar-1
  names(pim)=names(parameters)
#
# If there are fixed parameters output the text here
#
  num.fixed=0
  fixed=NULL
  fixedvalues=NULL
  for(i in 1:length(parameters))
  {
     parx=names(parameters)[i]
#
#    Add any default fixed values
#
     fixlist=NULL
     fixvalues=NULL
     fix.indices=NULL
     if(default.fixed)
     {
        rn=row.names(full.ddl[[parx]][!row.names(full.ddl[[parx]])%in%row.names(ddl[[parx]]),])
        if(length(rn)>0)
        {
           fixvalues=rep(parameters[[i]]$default,length(rn))
           fixlist=as.numeric(rn)
        }
     }
     if(!is.null(parameters[[i]]$fixed)|!is.null(fixlist))
     {
#
#      All values of this parameter type are fixed at one value
#
       if(length(parameters[[i]]$fixed)==1) 
       {
          for(j in 1:length(pim[[i]]))
          {
             fixlist=unique(as.vector(pim[[i]][[j]]$pim))
             fixlist=fixlist[fixlist>0]
             if(is.null(fixedvalues))
                 fixedvalues=data.frame(index=fixlist,value=rep(parameters[[i]]$fixed,length(fixlist)))
             else
                 fixedvalues=rbind(fixedvalues,data.frame(index=fixlist,value=rep(parameters[[i]]$fixed,length(fixlist))))
             for(k in 1:length(fixlist))
             {
                num.fixed=num.fixed+1
                fixed=c(fixed,paste("parm(",fixlist[k],")=",parameters[[i]]$fixed,sep=""))
             }
          }
        }else
#
#       The parameters with indices in the first list element are given specified value(s)
#
        if(is.list(parameters[[i]]$fixed)|!is.null(fixlist))
        {
             if("index"%in%names(parameters[[i]]$fixed))
                fix.indices=parameters[[i]]$fixed$index
             else
                if(is.list(parameters[[i]]$fixed)&!any(names(parameters[[i]]$fixed)%in%c("time","age","cohort","group")))
                {
                   cat("\nUnrecognized structure for fixed parameters =",parameters[[i]]$fixed)
                   stop()
                }
                else
                    if(!is.null(parameters[[i]]$fixed[["time"]]))
                    {
                        times=parameters[[i]]$fixed[["time"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$time%in%times,]))
                    }
                    else
                    if(!is.null(parameters[[i]]$fixed[["age"]]))
                    {
                        ages=parameters[[i]]$fixed[["age"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$age%in%ages,]))
                    }
		                else
                    if(!is.null(parameters[[i]]$fixed[["cohort"]]))
                    {
                        cohorts=parameters[[i]]$fixed[["cohort"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$cohort%in%cohorts,]))
                    }
                    else
                    if(!is.null(parameters[[i]]$fixed[["group"]]))
                    {
                        groups=parameters[[i]]$fixed[["group"]]
                        fix.indices=as.numeric(row.names(full.ddl[[parx]][full.ddl[[parx]]$group%in%groups,]))
                    }
             if(!is.null(fix.indices))fixlist=c(fixlist,fix.indices)
             if(length(parameters[[i]]$fixed$value)==1)
                 fixvalues=c(fixvalues,rep(parameters[[i]]$fixed$value,length(fix.indices)))
             else
             {
                 fixvalues=c(fixvalues,parameters[[i]]$fixed$value)
                 if(length(fixlist)!=length(fixvalues))
                 {
                    cat(paste("\nLengths of indices and values do not match for fixed parameters for",names(parameters)[i],"\n"))
                    stop()
                 }
             }
             fixlist=fixlist+ pim[[i]][[1]]$pim[1,1]-1
             for(k in 1:length(fixlist))
             {
                num.fixed=num.fixed+1
                fixed=c(fixed,paste("parm(",fixlist[k],")=",fixvalues[k],sep=""))
             }
             if(is.null(fixedvalues))
                fixedvalues=data.frame(index=fixlist,value=fixvalues)
             else
                fixedvalues=rbind(fixedvalues,data.frame(index=fixlist,value=fixvalues))
        }
     }
  }
  if(num.fixed>0)
  {
     string=paste("fixed =",num.fixed,";",sep="")
     write(string,file=outfile,append=TRUE)
     write(paste(fixed,";"),file=outfile,append=TRUE)
  }
#
# Unless model will be simplified, output PIMS for each parameter in the model
#
  if(!simplify)
  {
     for(i in 1:length(parameters))
     {
        for(j in 1:length(pim[[i]]))
        {
           ncol=dim(pim[[i]][[j]]$pim)[2]
           string=pim.header(pim[[i]][[j]]$group,param.names[i],parameters[[i]],
                   ncol,pim[[i]][[j]]$stratum,pim[[i]][[j]]$tostratum,
                   data$strata.labels,mixtures,pim[[i]][[j]]$session)
           write(string,outfile,append=TRUE)
           print.pim( pim[[i]][[j]]$pim,outfile)
        }
     }
  }
#
# Create design matrix for each parameter type
#
  design.matrix=list()
  for(i in 1:length(parameters))
  {
  if(is.null(parameters[[i]]$formula))
     design.matrix[[i]]=list()
  else
  {    
#
#    Calculate number of parameters for this type
#
     parx=names(parameters)[i]
     npar=dim(ddl[[parx]])[1]
#
#       Compute design matrix with model.matrix
#         31 Jan 06; made change to allow for NA or missing design data
#
        dm=model.matrix(parameters[[parx]]$formula,ddl[[parx]])
#
#       In cases with nested interactions it is necessary to remove the intercept
#       to avoid over-parameterizing the model; this is user-specified
#
        if(!is.null(parameters[[parx]]$remove.intercept)&&parameters[[parx]]$remove.intercept)
        {
            intercept.column=(1:dim(dm)[2])[colSums(dm)==dim(dm)[1]]
            if(length(intercept.column)==0)
            {
               cat("\nIntercept column not found.  Do not use ~-1 with remove.intercept\n")
               stop()
            }
            else
            {
               if(length(intercept.column)==1)
                  dm=dm[,-intercept.column]
            }
        }
        maxpar=dim(full.ddl[[parx]])[1]
        if(poolpandc&names(parameters)[i]=="p")
            maxpar=maxpar+dim(full.ddl[["c"]])[1]
        if(pool.ps&names(parameters)[i]=="p1")
            maxpar=maxpar+dim(full.ddl[["p2"]])[1]
        if(poolgammas&names(parameters)[i]=="GammaDoublePrime")
            maxpar=maxpar+dim(full.ddl[["GammaPrime"]])[1]
        design.matrix[[i]]=matrix(0,ncol=dim(dm)[2],nrow=maxpar)
        if(dim(design.matrix[[i]][as.numeric(row.names(ddl[[parx]])),,drop=FALSE])[1]==dim(dm)[1])
           design.matrix[[i]][as.numeric(row.names(ddl[[parx]])),]=dm
        else
           stop(paste("\nProblem with design data. It appears that there are NA values in one or more variables in design data for ",parx,"\nMake sure any binned factor completely spans range of data\n",sep=""))
        colnames(design.matrix[[i]])=colnames(dm)
#
#       It appears that model.matrix can add unneeded columns to the design matrices
#       It can add interactions that are not relevant.  The results are columns in the design
#       matrix that are all zero.  These are stripped out here.
#
        col.sums=apply(design.matrix[[i]],2,sum)
        design.matrix[[i]]=design.matrix[[i]][,col.sums!=0,drop=FALSE]
#
#       Next substitute variable names for covariates into design matrix 
#
        if(length(xcov[[parx]])!=0)
          for(j in 1:length(xcov[[parx]]))
          {
             which.cols=NULL
             cnames=colnames(design.matrix[[i]])
#
#            fix for v1.6.2 needed to delete [ or ] or ( or ) or , from the names
#            to use the all.vars command.  These are created in using the cut command
#            on a numeric variable.
#
             cnames=sub("\\(","",cnames)
             cnames=sub("\\)","",cnames)
             cnames=sub("\\[","",cnames)
             cnames=sub("\\]","",cnames)
             cnames=sub(",","",cnames)
             for(k in 1:dim(design.matrix[[i]])[2])
                if(xcov[[parx]][j]%in%all.vars(formula(paste("~",cnames[k],sep=""))))
                     which.cols=c(which.cols,k)
             if(length(which.cols)>0)
             for(k in which.cols)
               if(all(design.matrix[[i]][,k]==1 | design.matrix[[i]][,k]==0))
                  if(time.dependent[[parx]][j])
                     if(!is.null(full.ddl[[parx]]$session))
                        if(session.dependent[[parx]][j])
                            design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=
                                paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$session[design.matrix[[i]][,k]==1]),sep="")
                        else
                           design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=
                                paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$session[design.matrix[[i]][,k]==1]),
                                  as.character(full.ddl[[parx]]$time[design.matrix[[i]][,k]==1]),sep="")
                     else   
                        design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$time[design.matrix[[i]][,k]==1]),sep="")
                  else
                     design.matrix[[i]][,k][design.matrix[[i]][,k]==1]=xcov[[parx]][j]
               else
                  if(time.dependent[[parx]][j])
                  {
                     if(!is.null(full.ddl[[parx]]$session))
                     {
                        if(session.dependent[[parx]][j])
                           design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                              paste("product(",paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$session[design.matrix[[i]][,k]!=0]),sep=""),
                                       ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
                        else
                           design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                              paste("product(",paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$session[design.matrix[[i]][,k]!=0]),
                                      as.character(full.ddl[[parx]]$time[design.matrix[[i]][,k]!=0]),sep=""),
                                       ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")                     
                     }
                     else
                        design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                           paste("product(",paste(xcov[[parx]][j],as.character(full.ddl[[parx]]$time[design.matrix[[i]][,k]!=0]),sep=""),
                                    ",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
                  }
                  else
                     design.matrix[[i]][,k][design.matrix[[i]][,k]!=0]=
                        paste("product(",xcov[[parx]][j],",",design.matrix[[i]][,k][design.matrix[[i]][,k]!=0],")",sep="")
          }

        row.names(design.matrix[[i]])=NULL
#     }
#
#    If there any additional design components add them on here
#
     if(is.null(parameters[[i]]$component))
        design.matrix[[i]]=as.data.frame(design.matrix[[i]])
     else
     {
        savenames=colnames(design.matrix[[i]])
        if(is.null(savenames))savenames="(Intercept)"
        design.matrix[[i]]=cbind(design.matrix[[i]],parameters[[i]]$component)
        if(is.data.frame(parameters[[i]]$component))
           savenames=c(savenames,names(parameters[[i]]$component))
        else
        {
            if(is.null(parameters[[i]]$component.name)) 
                assigned.name="cov"
            else
                assigned.name=parameters[[i]]$component.name
            if(is.matrix(parameters[[i]]$component))
               savenames=c(savenames,paste(assigned.name,1:dim(parameters[[i]]$component)[2],sep=""))
            else
               savenames=c(savenames,assigned.name)
        }
        if(!is.data.frame(design.matrix[[i]]))design.matrix[[i]]=as.data.frame(design.matrix[[i]])
        names(design.matrix[[i]])=savenames
     }
     if(parameters[[i]]$formula=="~1")
        names(design.matrix[[i]])[1]="(Intercept)"
     names(design.matrix[[i]])=paste(names(parameters)[i],names(design.matrix[[i]]),sep=":")
  } 
  }
  names(design.matrix)=names(parameters)
#
# Merge to create a single design matrix
#
  complete.design.matrix=NULL
  nrows=0
  for(i in 1:length(parameters))
  {
     if(!is.null(parameters[[i]]$formula))
     {    
        mat=NULL
        lastpim=length( pim[[length(parameters)]])
        lastindex=max(pim[[length(parameters)]][[lastpim]]$pim)
        if(poolpandc & names(parameters)[i]=="c")
        {
           minrow=pim[["c"]][[1]]$pim[1,1]
           maxrow=max(pim[["c"]][[length(pim[["c"]])]]$pim)
           if(minrow>1)
              mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=minrow-1)
           mat=rbind(mat,as.matrix(design.matrix[[i]]))
           if(i<length(parameters))
               mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-maxrow ))
        } else
        if(pool.ps & names(parameters)[i]=="p2")
        {
           minrow=pim[["p2"]][[1]]$pim[1,1]
           maxrow=max(pim[["p2"]][[length(pim[["p2"]])]]$pim)
           if(minrow>1)
              mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=minrow-1)
           mat=rbind(mat,as.matrix(design.matrix[[i]]))
           if(i<length(parameters))
               mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-maxrow ))
        } else
        if(poolgammas & names(parameters)[i]=="GammaPrime")
        {
           minrow=pim[["GammaPrime"]][[1]]$pim[1,1]
           maxrow=max(pim[["GammaPrime"]][[length(pim[["GammaPrime"]])]]$pim)
           if(minrow>1)
              mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=minrow-1)
           mat=rbind(mat,as.matrix(design.matrix[[i]]))
           if(i<length(parameters))
               mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-maxrow ))
        } else
        {    
           if(i>1)
              mat=matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=nrows)
           mat=rbind(mat,as.matrix(design.matrix[[i]]))
           nrows=dim(mat)[1]
           if(i<length(parameters))
              mat=rbind(mat,matrix("0",ncol=dim(design.matrix[[i]])[2],nrow=lastindex-nrows ))
        }
        names(mat)=names(design.matrix[[i]])
        complete.design.matrix=cbind(complete.design.matrix,mat)
     }
  }
  row.names(complete.design.matrix)=1:dim(complete.design.matrix)[1]
  complete.design.matrix=as.data.frame(complete.design.matrix)
#
#  If there any initial values, output them to the MARK input file
#  after making sure that the vector length matches the number of parameters  
#
   if(!is.null(initial))
   {
#
#     If a vector of values was given check to make sure it is of the correct length and then output
#
      if(is.vector(initial))
      {
         if(length(initial)==dim(complete.design.matrix)[2])
            initial.values=initial
         else
         {
            if(length(initial)==1)
               initial.values=rep(initial,dim(complete.design.matrix)[2])
            else
            {
                if(length(names(initial))==0)
				{
				  cat("\nLength of initial vector doesn't match design matrix\n")
                  stop()
			    }else
				{
					beta.index=match(names(complete.design.matrix),names(initial))
					initial.values=rep(0,dim(complete.design.matrix)[2])
					initial.values[!is.na(beta.index)]=initial[beta.index[!is.na(beta.index)]]				
				}
            }
         }
      } 
      else
#
#     If it was a MARK object; check to make sure it has output and then use the betas from the other object
#     2 May 06 jll; use names instead of values in design matrix to match initial values
#
      {
         if(class(initial)[1]=="mark")
         {
            initial=load.model(initial)
            if(!is.null(initial$output))
            {
               beta.index=match(names(complete.design.matrix),colnames(initial$design.matrix))
               initial.values=rep(0,dim(complete.design.matrix)[2])
               initial.values[!is.na(beta.index)]=initial$results$beta$estimate[beta.index[!is.na(beta.index)]]
            }
         }
      }
      string=paste("initial ",paste(initial.values,collapse=" "),";")
      write(string,file=outfile,append=TRUE)
   }
#
#  If model will not be simplified, output design matrix to the MARK input file
#
  if(!simplify)
  {
    string=paste("design matrix constraints=",dim(complete.design.matrix)[1], " covariates=",dim(complete.design.matrix)[2],";",sep="")
    write(string,file=outfile,append=TRUE)
    write.table(complete.design.matrix,file=outfile,eol=";\n",sep=" ",col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
  }
#
#  If parm-specific links, output them here
#
  mlogit.list=list(structure=NULL,ncol=1)
  max.logit.number=0
  if(link=="Parm-Specific")
  {
     string=NULL
     for(i in 1:length(parameters))
     {
        parx=names(parameters)[i]
        if(parameters[[i]]$link=="mlogit"|parameters[[i]]$link=="MLogit")
        {
          if(parx=="Psi")
          {
              logit.numbers = max.logit.number+1:(nrow(full.ddl[[parx]])/(nstrata*(nstrata-1)*number.of.groups))
              logits.per.group=nstrata*length(logit.numbers)
              for (k in 1:number.of.groups)
              {
                 if(k>1)logit.numbers=logit.numbers+logits.per.group
                 for (j in 1:nstrata)	 
                   string=c(string,paste("mlogit(",rep(logit.numbers+(j-1)*length(logit.numbers),(nstrata-1)),")",sep="")) 
			  }
			  max.logit.number=max.logit.number+logits.per.group*number.of.groups
           }
           else
           {
             if(parx=="pent")
             {
                 nsets=length(pim[[parx]])
                 for (kk in 1:nsets)
                 {
                    x.indices=as.vector(t(pim[[parx]][[kk]]$pim))
                    x.indices=x.indices[x.indices!=0]
                    max.logit.number=max.logit.number+1
                   string=c(string,paste("mlogit(",rep(max.logit.number,length(x.indices)),")",sep=""))
                 }
             }else
             {
                 stop(paste("Mlogit link not allowed with parameter",parx))
             }
           }
        } else
        {
          xstring=rep(spell(parameters[[i]]$link),dim(full.ddl[[parx]])[1])
          string=c(string,xstring)
        }
     }
     write(paste("links=",length(string),";",sep=""),file=outfile,append=TRUE)
     links=string
     string=paste(string,";")
     write(string,file=outfile,append=TRUE)
  } else
  links=link
#
# write out labels for design matrix columns
#
  for(i in 1:dim(complete.design.matrix)[2])
  {
     string=paste("blabel(",i,")=",colnames(complete.design.matrix)[i],";",sep="")
     write(string,file=outfile,append=TRUE)
  }
#
# write out labels for real parameters
#

  labstring=NULL
  rnames=NULL
  ipos=0
  for(i in 1:length(parameters))
  {
    parx=names(parameters)[i]
    plimit=dim(full.ddl[[parx]])[1]
    stratum.strings=rep("",plimit)
    if(!is.null(full.ddl[[parx]]$stratum)) stratum.strings=paste(" s",full.ddl[[parx]]$stratum,sep="")
    if(!is.null(full.ddl[[parx]]$tostratum)) stratum.strings=paste(stratum.strings," to",full.ddl[[parx]]$tostratum,sep="")
    strings=paste(param.names[i],stratum.strings," g",full.ddl[[parx]]$group,sep="")
    if(!is.null(full.ddl[[parx]]$cohort))strings=paste(strings," c",full.ddl[[parx]]$cohort,sep="")
	if(!is.null(full.ddl[[parx]]$occ.cohort))strings=paste(strings," c",full.ddl[[parx]]$occ.cohort,sep="")
	if(!is.null(full.ddl[[parx]]$age))strings=paste(strings," a",full.ddl[[parx]]$age,sep="")
	if(!is.null(full.ddl[[parx]]$occ))strings=paste(strings," o",full.ddl[[parx]]$occ,sep="")
	if(model.list$robust && parameters[[parx]]$secondary)
       strings=paste(strings," s",full.ddl[[parx]]$session,sep="")
    if(!is.null(full.ddl[[parx]]$time))strings=paste(strings," t",full.ddl[[parx]]$time,sep="")
    if(mixtures >1 && !is.null(parameters[[i]]$mix) &&parameters[[i]]$mix)
       strings=paste(strings," m",full.ddl[[parx]]$mixture,sep="")
    rnames=c(rnames,strings)
    if(!simplify)
    {
       strings=paste("rlabel(",ipos+1:plimit,")=",strings,";",sep="")
       labstring=c(labstring,strings)
       ipos=ipos+plimit
    }
  }
  if(!simplify) write(labstring,file=outfile,append=TRUE)
  row.names(complete.design.matrix)=rnames
#
# Write out Proc stop statement
#
  write("proc stop;",file=outfile,append=TRUE)
  text=readLines(outfile)
  unlink(outfile)
  if(mixtures==1)
     mixtures=NULL
  if(is.null(call))call=match.call()
    model = list(data = substitute(data), model = data$model,
        title = title, model.name = model.name, links = links, mixtures=mixtures,
        call = call, parameters=parameters,time.intervals=data$time.intervals, input = text, number.of.groups = number.of.groups,
        group.labels = group.labels, nocc = nocc, begin.time = data$begin.time, covariates=covariates,
        fixed=fixedvalues,design.matrix = complete.design.matrix, pims = pim,
        design.data = full.ddl,strata.labels=data$strata.labels,mlogit.list=mlogit.list)
  if(model.list$robust)model$nocc.secondary=nocc.secondary
#
#  If requested, simplify model which reconstructs PIMS, design matrix and rewrites the
#  MARK input file for the simplified model.
#
  model$profile.int=profile.int
  model$chat=chat
  if(simplify) model=simplify.pim.structure(model)
#
# Check to make sure that the only rows in the design matrix that are all zeros are
# ones that correspond to fixed parameters.
#
  if(simplify)
  {                               
      dm=model$simplify$design.matrix
      fixed.rows=unique(model$simplify$pim.translation[model$fixed$index])
      zero.rows=(1:dim(dm)[1])[apply(dm,1,function(x) return(all(x=="0")))]
      if(length(fixed.rows)==0)
      {
         if(length(zero.rows)!=0)
            stop("One or more formulae are invalid because the design matrix has all zero rows for the following non-fixed parameters\n",
                  paste(row.names(dm)[zero.rows],collapse=","))
      }
      else
      {
         if(any(! (zero.rows%in%fixed.rows)))
            stop("One or more formulae are invalid because the design matrix has all zero rows for the following non-fixed parameters\n",
               paste(row.names(dm)[! (zero.rows%in%fixed.rows)],collapse=","))
      }
  }
#
#  Check to make sure that any parameter that used a sin link has an identity design matrix
#
  if(simplify)
  {
     for(i in 1:length(parameters))
     {
        parx=names(parameters)[i]
        if(model$parameters[[parx]]$link=="sin")
        {
           dm=model$simplify$design.matrix
           rows=unique(model$simplify$pim.translation[sort(unique(as.vector(sapply(model$pims[[parx]],function(x)x$pim[x$pim>0]))))])
           if(length(grep('[[:alpha:]]',as.vector(dm[rows,,drop=FALSE])))>0)
              stop("\nCannot use sin link with covariates")
           dm=suppressWarnings(matrix(as.numeric(dm),nrow=dim(dm)[1],ncol=dim(dm)[2]))
           if(any(rowSums(dm[rows,,drop=FALSE])>1) | any(colSums(dm[rows,,drop=FALSE])>1))
              stop("\nCannot use sin link with non-identity design matrix")
        }
     }
  }
#
#  check.mlogits(model)
#  model$mlogit.structure=NULL
  if(!is.null(model$simplify$links))
  {
     newlinks=model$simplify$links
     model$simplify$links=model$links
     model$links=newlinks
  }
  class(model)=c("mark",data$model)
  return(model)
}
