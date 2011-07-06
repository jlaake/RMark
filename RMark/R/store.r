store=function(x)
{
  if(class(x)!="marklist") stop("\nThis function only works on a marklist\n")
  for (i in 1:(length(x)-1))
  {
    model=x[[i]]
    save.mark=paste(model$output,".rda",sep="")
    save(model,file=save.mark)
    saveclass=class(x[[i]])
    x[[i]]=save.mark
    class(x[[i]])=saveclass
  }
return(x)
}
restore=function(x)
{
  model=NULL
  if(class(x)!="marklist") stop("\nThis function only works on a marklist\n")
  for (i in 1:(length(x)-1))
  {
    load(file=x[[i]])
    unlink(x[[i]])
    x[[i]]=model
  }
return(x)
}
