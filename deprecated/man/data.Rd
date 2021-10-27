\name{lca.data}
\alias{data}
\docType{data}
\title{empirical datasets}
\description{lca.data currently contains 81 empirical LCA data sets.  These datasets come from both plants and animals and include crosses within and between species.}
\usage{data(lca.data)}
\value{A list with 82 elements
  \item{1...81}{  objects of class data.frames that each contain 3 columns with cohort IDs, mean phenotype measure and the standard error of the cohort.  These data frames are formatted to be used in the r-package SAGA}
  \item{82 }{  table of citations that relate each dataset to the original publication}
  }
\source{}
\examples{
data(lca.data)
# select the first dataset
data <- lca.data[[1]]
# extract the citation table for the data
citations <- lca.data[[82]]
}
\keyword{datasets}
