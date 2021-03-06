% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RPModel.R
\name{tvt}
\alias{tvt}
\title{Train/Validation/Test Split}
\usage{
tvt(train, val, test, n = NULL, prop = FALSE)
}
\arguments{
\item{train}{The number of training samples to generate IDs for.}

\item{val}{The number of validation samples to generate IDs for.}

\item{test}{The number of testing samples to generate IDs for.}

\item{n}{If prop = TRUE, the total number of samples.}

\item{prop}{If TRUE, the variables train, val and test act as proportions (and n is required).}
}
\value{
A list with three elements: train, val and test, containing a vector of IDs for each.
}
\description{
Produces sample IDs from a train/val/test split.
}
\examples{
ids <- tvt(70, 20, 10)
}
