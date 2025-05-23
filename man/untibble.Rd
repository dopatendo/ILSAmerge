% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/untibble.R
\name{untibble}
\alias{untibble}
\title{Untibble}
\usage{
untibble(tibble, mistoNAs = FALSE)
}
\arguments{
\item{tibble}{a tibble object or a list of tibbles.}

\item{mistoNAs}{a logical value indicating if missing values should be 
converted into NAs. Default is \code{FALSE}.}
}
\value{
A tibble.
}
\description{
Converts a tibble into a plain data frame with no column attributes.
}
\examples{
# Path where raw 'SPSS' files are
input <- system.file("extdata/reds", package = "ILSAmerge")

# Load complete data
fulllist <- justload(inputdir = input, population = "BCGV1", justattributes = FALSE)

# Untibble first element
unt1 <- untibble(fulllist[[1]])

# Untibble all list
unt2 <- untibble(fulllist)


}
\keyword{tibblemanagement}
