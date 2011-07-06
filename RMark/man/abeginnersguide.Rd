\name{ABeginnersGuide}
\alias{ABeginnersGuide }
\title{A beginners introduction and guide to RMark}
\description{
The RMark package is a collection of R functions that can be used as an interface to MARK for analysis of
capture-recapture data.
}
\details{
The library contains various functions that import/export capture data, build capture-recapture models, 
run the FORTRAN program MARK.EXE, and extract and display output.  Program MARK has its own user interface; 
however, model development can be rather tedious and error-prone because the parameter structure and design matrix 
are created by hand. This interface in R was created to use the formula and design matrix functions in R to ease 
model development and reduce errors. This R interface has the following advantages:
1) Uses model notation to create design matrices rather than designing them by hand in MARK or in EXCEL, which makes model development faster and more reliable. All-different PIMS are automatically created for each group (if any).
2) Allows models based on group (factor variables) and individual covariates with groups created on the fly. Age, cohort, group and time variables are pre-defined for use in formulas.
3) Both real and beta labels are automatically added for easy output interpretation. 
4) Input, output and specific results (eg parameter estimates, AICc etc) are stored in an R object where they can be manipulated as deemed useful (eg plotting, further calculations, simulation etc).
5) Parameter estimates can be displayed in triangular PIM format (if appropriate) for ease of interpretation.
6) Easy setup of batch jobs and the calls to the R functions document the model specifications and allow models to be easily reproduced or re-run if data are changed.
7) Covariate-specific estimates of real parameters can be computed within R without re-running the analysis.

The following are the MARK capture-recapture models that are currently supported:
\tabular{ll}{
\code{model} \tab Selection in MARK \cr
\code{CJS} \tab Recaptures only\cr
\code{Recovery} \tab Recoveries only\cr
\code{Burnham} \tab Both(Burnham)\cr
\code{Barker} \tab Both(Barker)\cr
\code{Pradel} \tab Pradel recruitment only\cr
\code{Pradsen} \tab Pradel survival and seniority\cr
\code{Pradlambda} \tab Pradel survival and lambda\cr
\code{Pradrec} \tab Pradel survival and recruitment\cr
\code{LinkBarker} \tab Available only in change data type as Link-Barker\cr
\code{Closed} \tab Closed - no heterogeneity\cr
\code{HetClosed} \tab Closed with heterogeneity\cr
\code{FullHet} \tab Closed with full heterogeneity\cr
\code{Huggins} \tab Huggins with no heterogeneity\cr
\code{HugHet} \tab Huggins with heterogeneity\cr
\code{HugFullHet} \tab Huggins with full heterogeneity\cr
\code{POPAN} \tab POPAN\cr
\code{Jolly} \tab Burnham formulation for original Jolly-Seber model\cr
\code{Known} \tab Known - known fate data (e.g, radio-tracking)\cr
\code{Multistrata} \tab Multistrata - CJS model with strata\cr
\code{Robust} \tab Robust design with Closed models for secondary periods with no heterogeneity\cr
\code{RDHet} \tab Robust design with Closed models for secondary periods with heterogeneity\cr
\code{RDFHet} \tab Robust design with Closed models for secondary periods with full heterogeneity\cr
\code{RDHuggins} \tab Robust design with Huggins models for secondary periods with no heterogeneity\cr
\code{RDHHet} \tab Robust design with Huggins models for secondary periods with heterogeneity\cr
\code{RDHFHet} \tab Robust design with Huggins models for secondary periods with full heterogeneity\cr
\code{Nest} \tab Nest survival\cr
\code{Occupancy} \tab Site occupancy modelling\cr
\code{OccupHet} \tab Site occupancy modelling with mixture model for heterogeneity\cr
\code{RDOccupEG} \tab Robust design site occupancy modelling; single Psi, espsilon, and gamma\cr
\code{RDOccupPE} \tab Robust design site occupancy modelling; mutliple Psi and espsilon\cr
\code{RDOccupPG} \tab Robust design site occupancy modelling; mutliple Psi and gamma\cr
\code{RDOccupHetEG} \tab Robust design site occupancy modelling with heterogeneity; single Psi, 
                      espsilon, and gamma\cr
\code{RDOccupHetPE} \tab Robust design site occupancy modelling with heterogeneity; mutliple Psi
                      and espsilon\cr
\code{RDOccupHetPG} \tab Robust design site occupancy modelling with heterogeneity; mutliple Psi
                       and gamma\cr
\code{OccupRNPoisson} \tab Royle-Nichols Poisson site occupancy modelling\cr
\code{OccupRNNegBin} \tab Royle-Nichols Negative Binomial site occupancy modelling\cr
\code{OccupRPoisson} \tab Royle count Poisson site occupancy modelling\cr
\code{OccupRNegBin} \tab Royle count Negative Binomial site occupancy modelling\cr
\code{MSOccupancy} \tab Multi-state site occupancy modelling\cr
\code{ORDMS} \tab Open robust design multi-state\cr
\code{CRDMS} \tab Closed robust design multi-state\cr
}

There is one limitation of this interface beyond the obvious that it does not currently handle all of the models
and capabilities in MARK. All models in this interface are developed via a design matrix approach rather 
than coding the model structure via parameter index matrices (PIMS). In most cases, a logit or other link is
used by default which has implications for ability of MARK to count the number of identifiable parameters
(see \code{\link{dipper}} for an example).  However, beginning with v1.7.6 the sin link is now
supported if the formula specifies an identity design matrix for the parameter.

Before you begin, you must have installed MARK (\url{http://www.cnr.colostate.edu/~gwhite/mark/mark.htm})
on your computer or at least have a current copy of MARK.EXE. As long as you selected the default location for
the MARK install (c:/Program Files/Mark), the \code{RMark} library will be able to find it.  If for some reason, you
chose to install it in a different location, see the note section in \code{\link{mark}} for instructions on setting the variable
MarkPath to specify the path.  In addition to installing MARK, you must have installed the \code{RMark} library into
the R library directory.  Once done with those tasks, run R and enter library(RMark) (or put it in your .First function)
to attach the library of functions.

The following is a categorical listing of the functions in the library with a link to the help for each function.
To start, read the help for functions \code{\link{import.chdata}} and \code{\link{mark}} to learn how to import
your data and fit a simple model.  The text files for the examples shown in \code{import.chdata} are in the
subdirectory data within the R Library directory in RMark. Next look at the example data sets and analyses
 \code{\link{dipper}}, \code{\link{edwards.eberhardt}}, and \code{\link{example.data}}. After you
see the structure of the examples and the use of functions to fit a series of analyses, explore
the remaining functions under Model Fitting, Batch Analyses, Model Selection and Summary and Display. 
If your data and models contain individual covariates, read the section on Real Parameter Computation to learn
how to compute estimates of real parameters at various covariate values. 


Input/Output data & results

\code{\link{import.chdata}},\code{\link{read.mark.binary}}, \code{\link{extract.mark.output}}

Exporting Models to MARK interface

\code{\link{export.chdata}}, \code{\link{export.model}}
 
Model Fitting

\code{\link{mark}}, \code{\link{process.data}}, \code{\link{make.design.data}}, \code{\link{add.design.data}}, \code{\link{make.mark.model}}, \code{\link{run.mark.model}}
\code{\link{merge_design.covariates}}

Batch analyses with functions

\code{\link{run.models}}, \code{\link{collect.models}}, \code{\link{create.model.list}}, \code{\link{mark.wrapper}}

Summary and display

\code{\link{summary.mark}}, \code{\link{print.mark}},  
\code{\link{print.marklist}},
 \code{\link{get.real}}, \code{\link{compute.real}},  
\code{\link{print.summary.mark}}

Model Selection/Goodness of fit

\code{\link{adjust.chat}}, \code{\link{adjust.parameter.count}}, 
\code{\link{model.table}} , \code{\link{release.gof}}, 
\code{\link{model.average}}

Real Parameter computation

\code{\link{find.covariates}}, \code{\link{fill.covariates}}, 
\code{\link{compute.real}} , \code{\link{covariate.predictions}}

Utility and internal functions

\code{\link{collect.model.names}}, \code{\link{compute.design.data}}, 
\code{\link{extract.mark.output}}, \code{\link{inverse.link}}, \code{\link{deriv.inverse.link}}, 
\code{\link{setup.model}}, \code{\link{setup.parameters}},  \code{\link{valid.parameters}}, 
\code{\link{cleanup}}


For examples, see \code{\link{dipper}} for CJS and POPAN, see \code{\link{example.data}} for CJS with multiple
grouping variables, see \code{\link{edwards.eberhardt}} for various closed-capture models, see \code{\link{mstrata}} for Multistrata,
and see \code{\link{Blackduck}} for known fate. The latter two are examples of the use of \code{\link{mark.wrapper}} for
a shortcut approach to creating a series of models. Other examples have been added for the various other models.
}

\references{
MARK: Dr. Gary White, Department of Fishery and Wildlife Biology, Colorado State University, Fort Collins,
Colorado, USA \url{http://www.cnr.colostate.edu/~gwhite/mark/mark.htm}}
\author{Jeff Laake}
\keyword{utility}