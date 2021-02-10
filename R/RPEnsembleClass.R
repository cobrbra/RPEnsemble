#' Classifies the test set using the random projection ensemble classifier
#'
#' @description Performs a biased majority vote over B1 base classifications to assign the test set.
#'
#' @param RP.out The result of a call to RPParallel.
#' @param n Training set sample size.
#' @param n.val Validation set sample size.
#' @param n.test Test set sample size.
#' @param p1 Prior probability estimate.
#' @param samplesplit TRUE if using sample-splitting method.
#' @param alpha The voting threshold.
#' @param ... Optional further arguments if base = "other".
#'
#' @details An observation in the test set is assigned to class 1 if B1*alpha or more of the base classifications are class 1 (otherwise class 2).
#' 
#' @return A vector of length n.test containing the class predictions of the test set (either 1 or 2).
#' @export
#'
#' @seealso RPParallel, RPalpha, RPChoose
#' @examples
#' Train <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 100, 100, 0.5)
#' Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
#' d = 2, B1 = 50, B2 = 10, base = "LDA", projmethod = "Haar", 
#' estmethod = "training", clustertype = "Default")
#' Class <- RPEnsembleClass(RP.out = Out, n = length(Train$y), 
#' n.test = nrow(Test$x), p1 = sum(Train$y == 1)/length(Train$y),  
#' splitsample = FALSE,  alpha = RPalpha(Out, Y = Train$y, 
#' p1 = sum(Train$y == 1)/length(Train$y)))
#' mean(Class != Test$y)

RPEnsembleClass <- function(RP.out, # the result of a call to RPParallel
                            n,            # training sample size
                            n.val, #validation set size if samplesplit = TRUE
                            n.test, #test sample size
                            p1, #(estimate of) prior probability
                            samplesplit = FALSE, #split sample TRUE/FALSE
                            alpha,  #voting cutoff alpha
                            ... ) {
  
  if (samplesplit == TRUE) {
    Test.Class <- RP.out[n.val + 1:n.test, ]
  }
  
  else {
    Test.Class <- RP.out[n + 1:n.test, ]
  }
  
  if (n.test == 1) {
    vote <- mean(Test.Class, na.rm = TRUE)
  }
  
  if (n.test > 1) {
    vote <- rowMeans(Test.Class, na.rm = TRUE)
  }
  
  Class  <- 1 + as.numeric(vote > alpha)
  
  return(Class)
}
