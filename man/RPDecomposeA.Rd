% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RPReduce.R
\name{RPDecomposeA}
\alias{RPDecomposeA}
\title{Produce singular value decomposition for average choice of projection}
\usage{
RPDecomposeA(
  XTrain,
  YTrain,
  XVal = NULL,
  YVal = NULL,
  d,
  B1 = 100,
  B2 = 10,
  base = "LDA",
  projmethod = "Haar",
  estmethod = "training"
)
}
\arguments{
\item{XTrain}{An n by p matrix containing the training data feature vectors.}

\item{YTrain}{A vector of length n of the classes (either 1 or 2) of the training data.}

\item{XVal}{An n.test by p matrix containing the validation data feature vectors.}

\item{YVal}{A vector of length n.val of the classes (either 1 or 2) of the validation data.}

\item{d}{The lower dimension of the image space of the projections.}

\item{B1}{The number of blocks.}

\item{B2}{The block size.}

\item{base}{The base classifier one of "knn","LDA","QDA" or "other".}

\item{projmethod}{Either "Haar", "Gaussian" or "axis".}

\item{estmethod}{Method for estimating the test errors to choose the projection: either training error "training", leave-one-out "loo", or sample split "samplesplit".}
}
\value{
A list of three entries: eigv, u and v. These correspond to the eigenvalues and the two factor matrices of the singular value decomposition.
}
\description{
For the concatenated matrix containing B1 selections of p by B2 sized projection matrices, 
outputs the singular value decomposition.
}
\examples{
set.seed(100)
Train <- RPModel(1, 50, 100, 0.5)
Choose <- RPDecomposeA(XTrain = Train$x, YTrain = Train$y, d = 3, B1 = 10, B2 = 5, base = "QDA", 
projmethod = "Haar", estmethod = "loo")
}
