% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RMark-package.R
\name{HidMarkov}
\alias{HidMarkov}
\title{Hidden Markov Model}
\description{
Hidden Markov Model
}
\examples{
\donttest{
# posted by Chris on phidot 15 Sept 2021
# ----------------------------------------------------------------------------------
# Fit HMM model with 2 unobservable states
# 5 states:
# 1 = successful breeder
# 2 = failed breeder
# 3 = post-successful breeder (unobservable)
# 4 = post-failed breeder (unobservable)
# 5 = dead

# 4 field observations:
# 0 = not seen
# 1 = seen as successful breeder
# 2 = seen as failed breeder
# 5 = seen with uncertain breeding state (** this is an event in MARK terms)

#-----------------------------------------------------------------------------------

# Add a few uncertainty events (event = '5') to the above input data
df = c('1010001',
      '1050000', '1010000', '1010000', '1010100', '1000000', '1000001', '1000001',
      '1000001', '1010105', '1050101', '1010101', '1050101', '1010101', '1010101',
      '1010101', '1010101', '1010001', '1000001', '1000001', '1000001', '1010101',
      '1010101', '1010101', '1050101', '1010101', '1010101', '1010201', '1010201',
      '1010201', '1010201', '1010201', '1010201', '1010201', '1010201', '1010201',
      '1010201', '1010201', '1010201', '1010201', '1010201', '1050201', '1010205',
      '2010201', '2010000', '2000000', '2000000', '2000000', '2000000', '2000000',
      '2050220', '2010221', '2010221', '2050221', '2010221', '2010221', '2010221',
      '2010221', '2010521', '2010251', '2012221', '2012222', '2012222', '2012222',
      '2012251', '5012221', '2012020', '2022010', '2025010', '2022010', '2022010',
      '2052010', '2022010', '2022010', '2022010', '2022010', '2021010', '2021010',
      '2021010', '2051010', '2051010', '5021010', '2001000', '2025010', '2021010',
      '2021010')

df=as.data.frame(df); names(df) = "ch"; df$freq = 1; df$ch = as.character(df$ch)
head(df)

# 'event' is only for the uncertain observation - not the 'state' observations!
# Error in process.data: unused argument (events) if package marked is loaded in session
# states 3 & 4 = unobservable

# Bill Kendall: For pi, MARK defaults to deriving the probability of the last state listed by subtraction.
# Since we like to fix pi for states 3 and 4 to 0, the solution is to list states 1 or 2
# last when you specify the states.

dp = process.data(df, model = "HidMarkov", strata.labels = c("3", "4","1", "2"),
                 events = c("5")) 

## create design data
ddl = make.design.data(dp)

# Set movement parameters (PSI) to 0
ddl$Psi$fix[ddl$Psi$stratum=="1" & ddl$Psi$tostratum=="4"]=0
ddl$Psi$fix[ddl$Psi$stratum=="2" & ddl$Psi$tostratum=="3"]=0

ddl$Psi$fix[ddl$Psi$stratum=="3" & ddl$Psi$tostratum=="4"]=0
ddl$Psi$fix[ddl$Psi$stratum=="4" & ddl$Psi$tostratum=="3"]=0

# Set unobservable p to 0
ddl$p$fix[ddl$p$stratum=="3"]=0
ddl$p$fix[ddl$p$stratum=="4"]=0

# animals can only enter as breeders, not as post-breeders (unobservable)
# In HMMs, the pi parameter is the probability of the event on the first capture.
# Note: P(event)! and not as in ESurge, where it is P(state on first capture)!

# Set unobservable pi to 0
ddl$pi$fix[ddl$pi$stratum=="3"]=0
ddl$pi$fix[ddl$pi$stratum=="4"]=0

# The Delta parameters are the probability of determining the state given detection
# with probability p (MARK defaults to obtaining the probability of correctly assigning
# the state by subtraction)
ddl$Delta$fix[ddl$Delta$stratum=="3"]=0
ddl$Delta$fix[ddl$Delta$stratum=="4"]=0


# define a model to fit
# make a survival class (successful = post-successful != failed = post-failed)
ddl$S$class = ifelse(ddl$S$`3`=="1" |ddl$S$`1`=="1" , 1, 0)

Phi.ct = list(formula=~class)
p.ct = list(formula=~stratum)
Psi.class =list(formula =~ -1+stratum:tostratum)   # see page 918 C.17. Multi-strata example in Mark book
pi.ct = list(formula=~stratum) 
Delta.class  = list(formula=~stratum)

table(ddl$Psi[,c("stratum","tostratum")])
table(ddl$pi[,c("stratum")])
table(ddl$Delta[,c("event")])


# Aim: run a HMM where uncertain events (=5) could be successful breeders or unsuccesful breeders

Model.4 = mark(dp, ddl, model.parameters=list(S = Phi.ct,
                                             p = p.ct,
                                             Psi = Psi.class,
                                             pi = pi.ct,
                                             Delta = Delta.class),
              output = FALSE,delete=T, model ="HidMarkov", mlogit0 = T) 

summary(Model.4)


Psilist=get.real(Model.4,"Psi",vcv=T)
Psivalues=Psilist$estimates
TransitionMatrix(Psivalues[Psivalues$time==1,],vcv.real=Psilist$vcv.real)
}
}
\author{
Chris Oosthuizen
}
