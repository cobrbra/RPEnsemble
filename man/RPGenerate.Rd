% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RPGenerate.R
\name{RPGenerate}
\alias{RPGenerate}
\title{Generates random matrices}
\usage{
RPGenerate(p = 100, d = 10, method = "Haar", B2 = 10)
}
\arguments{
\item{p}{The original data dimension.}

\item{d}{The lower dimension.}

\item{method}{Projection distribution, either "Haar" for Haar distributed projections, "Gaussian" for Gaussian distributed projections with i.i.d. N(0,1/p) entries, "axis" for uniformly distributed axis aligned projections, or "other" for user defined method.}

\item{B2}{The number of projections}
}
\value{
Returns B2 random matrices, each of size p by d, as a single p by d*B2 matrix.
}
\description{
Generates B2 random p by d matrices according to Haar measure, Gaussian or axis-aligned projections.
}
\examples{
R1 <- RPGenerate(p = 20, d = 2, "Haar", B2 = 3)
t(R1)\%*\%R1
R2 <- RPGenerate(p = 20, d = 2, "Gaussian", B2 = 3)
t(R2)\%*\%R2
R3 <- RPGenerate(p = 20, d = 2, "axis", B2 = 3)
colSums(R3)
rowSums(R3)
}
