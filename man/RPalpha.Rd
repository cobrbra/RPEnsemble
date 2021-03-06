% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RPalpha.R
\name{RPalpha}
\alias{RPalpha}
\title{Choose alpha}
\usage{
RPalpha(RP.out, Y, p1)
}
\arguments{
\item{RP.out}{The result of a call to RPParallel.}

\item{Y}{Vector of length n or n.val containing the training or validation dataset classes.}

\item{p1}{(Empirical) prior probability.}
}
\value{
The value of alpha that minimises the empirical error
}
\description{
Chooses the best empirical value of the cutoff alpha, based on the leave-one-out, resubstitution or sample-split estimates of the class labels.
}
\details{
See precise details in Cannings and Samworth (2015, Section 5.1).
}
\examples{
Train <- RPModel(1, 50, 100, 0.5)
Test <- RPModel(1, 100, 100, 0.5)
Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, d = 2, B1 = 10, 
                  B2 = 10, base = "LDA", projmethod = "Haar", estmethod = "training", cores = 1)
alpha <- RPalpha(RP.out = Out, Y = Train$y, p1 = sum(Train$y == 1)/length(Train$y))
}
\seealso{
RPParallel
}
