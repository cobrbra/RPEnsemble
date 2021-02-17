#' Generate pairs x,y from joint distribution
#'
#' @description Generates data from the models described in Cannings and Samworth (2016).
#'
#' @param Model.No Model Number
#' @param n Sample size
#' @param p Data dimension
#' @param Pi Class one prior probability
#'
#' @return 
#' \describe{
#'   \item{x}{An n by p data matrix – n observations of the p-dimensional features.}
#'   \item{y}{A vector of length n containing the classes (either 1 or 2).}
#'   } 
#' @export
#'
#' @note Models 1 and 2 require p = 100 or 1000.
#' @examples
#' Data <- RPModel(Model.No = 1, 100, 100, Pi = 1/2)
#' table(Data$y)
#' colMeans(Data$x[Data$y==1,])
#' colMeans(Data$x[Data$y==2,])

RPModel <- function(Model.No,
                    n,
                    p,
                    Pi = 0.5) {
  
  if (Model.No == 1) {
    
    Y1 <- stats::rmultinom(1, n, c(Pi, 1 - Pi))
    Y <- c(rep(1, Y1[1, 1]), rep(2, Y1[2, 1]))
    Y11 <- stats::rmultinom(1, Y1[1, 1], c(0.5, 0.5))
    Y22 <- stats::rmultinom(1, Y1[2, 1], c(0.5, 0.5))
    
    mu1 <- c(2, 2, rep(0, p - 2))
    mu2 <- c(2, -2, rep(0, p - 2))
      
    if (p == 100) {
      Signal1 <- rbind(t(matrix(mu1, p, Y11[1, 1])), t(matrix(-mu1, p, Y11[2, 1])))
      Noise1 <- matrix(stats::rnorm(Y1[1, 1]*p), Y1[1, 1], p)
      X1 <- Signal1 + Noise1
    
      Signal2 <- rbind(t(matrix(mu2, p, Y22[1, 1])), t(matrix(-mu2, p, Y22[2, 1])))
      Noise2 <- matrix(stats::rnorm(Y1[2, 1]*p), Y1[2, 1], p)
      X2 <- Signal2 + Noise2
    }
      
    if (p == 1000) {
      Signal1 <- rbind(t(matrix(mu1, p, Y11[1, 1])), t(matrix(-mu1, p, Y11[2, 1])))
      Noise1 <- cbind(matrix(stats::rnorm(Y1[1, 1]*2), Y1[1, 1], 2), matrix(stats::rnorm(Y1[1, 1]*(p - 2))/4, Y1[1, 1], p - 2))
      X1 <- Signal1 + Noise1
      
      Signal2 <- rbind(t(matrix(mu2, p, Y22[1, 1])), t(matrix(-mu2, p, Y22[2, 1])))
      Noise2 <- cbind(matrix(stats::rnorm(Y1[2, 1]*2), Y1[2, 1], 2), matrix(stats::rnorm(Y1[2, 1]*(p - 2))/4, Y1[2, 1], p - 2))
      X2 <- Signal2 + Noise2
    }
      
    X <- rbind(X1, X2)
  }
    
  if (Model.No == 2) {
    if (p == 100) {
      R <- RPEnsemble::R # I feel like this shouldn't be needed with proper lazy-loading?
    }
    
    Y1 <- stats::rmultinom(1, n, c(Pi, 1 - Pi))
    Y <- c(rep(1, Y1[1, 1]), rep(2, Y1[2, 1]))
    
    mu <- c(rep(3, 3), rep(0, p - 3))
    Sigma1 <- 0.5 * diag(c(rep(1, 3), rep(0, p - 3))) + 0.5 * c(rep(1, 3), rep(0, p - 3)) %*% t(c(rep(1, 3), rep(0, p - 3))) + 0.5 * diag(c(rep(0, 3), rep(1, p - 3))) + 0.5 * c(rep(0, 3), rep(1, p - 3)) %*% t(c(rep(0, 3), rep(1, p - 3)))
    Sigma2 <- 1.5 * diag(c(rep(1, 3), rep(0, p - 3))) + 0.5 * c(rep(1, 3), rep(0, p - 3)) %*% t(c(rep(1, 3), rep(0, p - 3))) + 0.5 * diag(c(rep(0, 3), rep(1, p - 3))) + 0.5 * c(rep(0, 3), rep(1, p - 3)) %*% t(c(rep(0, 3), rep(1, p - 3)))
     
    X1 <- MASS::mvrnorm(Y1[1, 1], R %*% rep(0, p), R %*% Sigma1 %*% t(R))
    X2 <- MASS::mvrnorm(Y1[2, 1], R %*% mu, R %*% Sigma2 %*% t(R))
    X <- rbind(X1, X2)
  }
  
  if (Model.No == 3) {
    Y1 <- stats::rmultinom(1, n, c(Pi, 1 - Pi))
    Y <- c(rep(1, Y1[1, 1]), rep(2, Y1[2, 1]))
   
    mu <- c(rep(1/sqrt(p), p/2), rep(0, p/2))
    
    X1 <- cbind(matrix(sample(c(-1, 1), Y1[1, 1] * p, replace = TRUE) * stats::rexp(Y1[1, 1] * p, 1), Y1[1, 1], p))
    X2 <- MASS::mvrnorm(Y1[2, 1], mu, diag(p))
    X  <- rbind(X1, X2)
  }
    
  if (Model.No == 4) {
    
      Y1 <- stats::rmultinom(1, n, c(Pi, 1 - Pi))
      Y <- c(rep(1, Y1[1, 1]), rep(2, Y1[2, 1]))
      
      mu <- c(rep(1, 10), rep(0, p - 10))
      U1 <- stats::rchisq(Y1[1, 1], 1)
      U2 <- stats::rchisq(Y1[2, 1], 2)
      Sigma1 <- diag(p)
      Sigma2 <- 0.5 * diag(p) + 0.5 * c(rep(1, 10), rep(0, p - 10)) %*% t(c(rep(1, 10), rep(0, p - 10))) + 0.5 * diag(c(rep(0, 10), rep(1, p - 10)))
      
      X1 <- MASS::mvrnorm(Y1[1, 1], rep(0, p), Sigma1) / sqrt(U1/1)
      X2 <- t(mu + t(MASS::mvrnorm(Y1[2, 1], rep(0, p), Sigma2) / sqrt(U2/2)))
      X  <- rbind(X1, X2)
  }
  
  return(list(x = X, y = Y))
}

#' Train/Validation/Test Split
#' 
#' @description Produces sample IDs from a train/val/test split.
#'
#' @param train The number of training samples to generate IDs for.
#' @param val The number of validation samples to generate IDs for.
#' @param test The number of testing samples to generate IDs for.
#' @param n If prop = TRUE, the total number of samples.
#' @param prop If TRUE, the variables train, val and test act as proportions (and n is required).
#'
#' @return A list with three elements: train, val and test, containing a vector of IDs for each.
#' @export
#'
#' @examples
#' ids <- tvt(70, 20, 10)

tvt <- function(train,
                val,
                test, 
                n = NULL,
                prop = FALSE) {
  
  if (prop) {
    if (is.null(n)) {
      stop("Need a total value for n")
    }
    
    else {
      
      if (abs(sum(prop) - 1) > 10^(-6)) {
        stop(paste("Probabilities in prop must sum to 1, not", sum(prop)))
      }
      
      else {
        train <- floor(train * n)
        val <- floor(val * n)
        test <- n - train - val
      }
    }
  }
    
  train_ids <- sample(1:(train + val + test), train)
  val_ids <- sample(setdiff(1:(train + val + test), train_ids), val)
  test_ids <- setdiff(1:(train + test), c(train_ids, val_ids))
  
  
  return(list(train = train_ids, val = val_ids, test = test_ids))
}
