\name{process.ch}
\alias{process.ch}
\title{ Process release-recapture history data }
\description{
  Creates needed constructs from the release-recapture history. }
\usage{
process.ch(ch, freq=NULL, all=FALSE)
}
\arguments{
  \item{ch}{Vector of character strings containing 0/1 capture-history}
  \item{freq}{Optional vector of frequencies for ch; if missing assumed to be a; if <0 indicates a loss on capture}
  \item{all}{Not necessary unless R code used to compute lnl instead of FORTRAN}
}
\value{
  \item{nocc}{number of capture occasions}
  \item{freq}{absolute value of frequency for each ch}
  \item{first}{vector of occasion numbers for first 1}
  \item{last}{vector of occasion numbers for last 1}
  \item{loc}{vector of indicators of a loss on capture if set to 1}
  \item{chmat}{capture history matrix}
  \item{FtoL}{1's from first (1) to last (1) and 0's elsewhere; only if all==TRUE}
  \item{Fplus}{1's from occasion after first (1) to nocc(last occasion); only if all==TRUE}
  \item{Lplus}{1's from occasion after last (1) to nocc; only if all==TRUE}
  \item{L}{1's from last (1) to nocc; only if all==TRUE}
}
\author{ Jeff Laake}
