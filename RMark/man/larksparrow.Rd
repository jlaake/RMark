\docType{data}
\name{larksparrow}
\alias{LASP}
\alias{larksparrow}
\title{Lark Sparrow}
\format{A data frame with 52 observations on the following 20 variables.
\describe{\item{ch}{a character vector containing the encounter history}
\item{ceap}{a factor with two levels "Deferred" and "Grazed" corresponding to a rest rotation grazing system with pastures either rested (Deferred) or grazed (Grazed) during the spring bird breeding season.}
\item{cwx}{a continuous covariate for primary occasion x, representing an ocular estimate of the proportion of area  covered by crested wheatgrass in a 50-m radius around the point count location.}
\item{tdx}{ a continuous covariate for primary occasion x, representing the starting time (h) of each 6-min point count survey measured on the ratio scale (1.5 h = 1 h 30 min).}
}}
\description{
An example of Multiple Scale Occupancy model for some lark
sparrow data that was contributed by Davi Pavalacky at
Rocky Mountain bird observatory. The study design was a
GRTS selection of paired "Deferred" and "Grazed" pastures.
The point count locations within each pasture were a random
selection of systematic point count locations separated by
125 m. A removal design was used to the set the data to
missing after the first detection.  The point count data
were set to missing when fewer than nine points were
surveyed.
}
\examples{
\donttest{

# Create dataframe
data(LASP)
mscale=LASP

# Process data with MultScalOcc model and use group variables

mscale.proc=process.data(mscale,model="MultScalOcc",groups=c("ceap"),begin.time=1,mixtures=3)

# Create design data

ddl=make.design.data(mscale.proc)

# Create function to build models

do.Species=function()
{
	p.1=list(formula=~1)
	p.2=list(formula=~ceap)
	p.3=list(formula=~td)

	Theta.1=list(formula=~1)
	Theta.2=list(formula=~ceap)
	Theta.3=list(formula=~cw)

	Psi.1=list(formula=~1)
	Psi.2=list(formula=~ceap)

	cml=create.model.list("MultScalOcc")
	return(mark.wrapper(cml,data=mscale.proc,ddl=ddl,adjust=F,realvcv=T))
}

# Run function to get results

Species.results=do.Species()

# Output model table and estimates

Species.results$model.table

Species.results[[as.numeric(rownames(Species.results$model.table[1,]))]]$results$real
Species.results[[as.numeric(rownames(Species.results$model.table[1,]))]]$results$beta

write.csv(Species.results$model.table,file="lasp_model_selection.csv",row.names=F)

write.csv(Species.results[[as.numeric(rownames(Species.results$model.table[1,]))]]$results$real,
 file="lasp_m1_real.csv")
write.csv(Species.results[[as.numeric(rownames(Species.results$model.table[1,]))]]$results$beta,
 file="lasp_m1_beta.csv")

# Covariate prediction and model averaging

# p(time of day)

mintd <- min(mscale[,12:20])
maxtd <- max(mscale[,12:20])
td.values <- mintd+(0:100)*(maxtd-mintd)/100

PIMS(Species.results[[1]],"p",simplified=F)

td <- covariate.predictions(Species.results,data=data.frame(td1=td.values),indices=c(21))

write.table(td$estimates,file="lasp_cov_pred_p_td.csv",sep=",",col.names=T,row.names=F)

# Theta(crested wheatgrass cover)

mincw <- min(mscale[,3:11])
maxcw <- max(mscale[,3:11])
cw.values <- mincw+(0:100)*(maxcw-mincw)/100

PIMS(Species.results[[1]],"Theta",simplified=F)

cw <- covariate.predictions(Species.results,data=data.frame(cw1=cw.values),indices=c(3))

write.table(cw$estimates,file="lasp_cov_pred_theta_cw.csv",sep=",",col.names=T,row.names=F)

# Psi(ceap grazing for wildlife practice)

ceap.values <- as.data.frame(matrix(c(1,2),ncol=1))
names(ceap.values) <- c("index")

PIMS(Species.results[[1]],"Psi",simplified=F)

ceap <- covariate.predictions(Species.results,data=data.frame(ceap=ceap.values))

write.table(ceap$estimates,file="lasp_cov_pred_psi_ceap.csv",sep=",",col.names=T,row.names=F)

}
}
\keyword{datasets}

