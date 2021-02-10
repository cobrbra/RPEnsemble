#' Choose alpha
#' 
#' @description Chooses the best empirical value of the cutoff alpha, based on the leave-one-out, resubstitution or sample-split estimates of the class labels.
#'
#' @param RP.out The result of a call to RPParallel.
#' @param Y Vector of length n or n.val containing the training or validation dataset classes.
#' @param p1 (Empirical) prior probability.
#'
#' @details See precise details in Cannings and Samworth (2015, Section 5.1).
#'
#' @return The value of alpha that minimises the empirical error
#' @export
#'
#' @seealso RPParallel
#' @examples 
#' Train <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 100, 100, 0.5)
#' Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, d = 2, B1 = 10, 
#'                   B2 = 10, base = "LDA", projmethod = "Haar", estmethod = "training", cores = 1)
#' alpha <- RPalpha(RP.out = Out, Y = Train$y, p1 = sum(Train$y == 1)/length(Train$y))
#' alpha

RPalpha <- function(RP.out, 
                    Y, 
                    p1) {
    
    n <- length(Y)
    Train.Class <- RP.out[1:n, ]
    vote1 <- rowMeans(Train.Class[Y == 1, ], na.rm = TRUE)
    vote2 <- rowMeans(Train.Class[Y == 2, ], na.rm = TRUE)
    
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
    errecdfm <- function(x) {
        p1 * ecdf(vote1)(x) + (1 - p1) * (1 - ecdf(vote2)(x))
    }
    
    errecdfM <- function(x) {
        p1 * ecdf(vote1)(-x) + (1 - p1) * (1 - ecdf(vote2)(-x))
    }
    
    alpham <- optimise(errecdfm, c(1, 2), maximum = TRUE)$maximum
    alphaM <- optimise(errecdfM, c(-2, -1), maximum = TRUE)$maximum
    alpha <- (alpham - alphaM)/2
    
    return(alpha)
}
