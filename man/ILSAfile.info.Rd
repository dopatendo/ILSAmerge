% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ILSAfile.info.R
\name{ILSAfile.info}
\alias{ILSAfile.info}
\title{ILSA data files information}
\usage{
ILSAfile.info(inputdir)
}
\arguments{
\item{inputdir}{a string indicating the path were ILSA SPSS files are stored.}
}
\value{
A data frame with the number of files and MBs per population.
}
\description{
Aggregates ILSA data files information by population.
}
\examples{
\dontrun{
# For example, after downloading TIMSS 1995 G4 data:

# Path were original SPSS files are stored
input <- 'C:/TIMSS1995_IDB_SPSS_G4/Data/'

# Get file information
ILSAfile.info(inputdir = input)

}

}
