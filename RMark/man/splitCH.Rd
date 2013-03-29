\name{splitCH}
\alias{collapseCH}
\alias{splitCH}
\title{Split/collapse capture histories}
\usage{
  splitCH(x="ch", data=NULL, prefix="Time")

  collapseCH(chmat=NULL, data=NULL, prefix="Time")
}
\arguments{
  \item{x}{A vector containing the character strings of
  capture histories or the column number or name in the
  data set \code{data}}

  \item{data}{A data frame containing columnwith value in x
  if x indicates a column in a data frame}

  \item{prefix}{first portion of field names for split ch}

  \item{chmat}{capture history matrix}
}
\value{
  A data frame if data specified and a matrix if vector ch
  is specified
}
\description{
  splitCH will split a character string vector of capture
  histories into a matrix. The matrix is appended to the
  original data set (data) if one is specified. Will handle
  character and numeric values in ch. Results will differ
  depending on content of ch. collapseCH will collapse a
  capture history matrix back into a character vector.
  Argument can either be a capture history matrix (chmat)
  or a dataframe (data) that contains fields with a
  specified prefix.
}
\examples{
\donttest{
data(dipper)
# following returns a matrix
chmat=splitCH(dipper$ch)
# following returns the original dataframe with the ch split into columns
newdipper=splitCH(data=dipper)
# following collapses chmat
ch=collapseCH(chmat)
# following finds fields in newdipper and creates ch
newdipper$ch=NULL
newdipper=collapseCH(data=newdipper)
}
}
\author{
  Devin Johnson; Jeff Laake
}

