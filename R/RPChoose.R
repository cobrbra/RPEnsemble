#' Chooses projection and produces predictions
#'
#' @description Chooses a the best projection from a set of size B2 based on a test error estimate, then classifies the training and test sets using the chosen projection.
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data.
#' @param XTest An n.test by p matrix of the test data.
#' @param d The lower dimension of the image space of the projections.
#' @param B2 The block size.
#' @param base The base classifier: one of "knn","LDA","QDA" or "other".
#' @param k The options for k if base is "knn".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training" or leave-one-out "loo".
#' @param ... Optional further arguments if base = "other".
#'
#' @details Randomly projects the the data B2 times. Chooses the projection yielding the smallest estimate of the test error. Classifies the training set (via the same method as estmethod) and test set using the chosen projection.
#'
#' @note Resubstitution method unsuitable for the k-nearest neighbour classifier.
#' 
#' @return Returns a vector of length n + n.test: the first n entries are the estimated classes of the training set, the last n.test are the estimated classes of the test set.
#' @export 
#'
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 100, 100, 0.5)
#' Choose.out5 <- RPChoose(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
#'                         d = 2, B2 = 5, base = "QDA", projmethod = "Haar", estmethod = "loo")
#' Choose.out10 <- RPChoose(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, 
#'                          d = 2, B2 = 10, base = "QDA", projmethod = "Haar", estmethod = "loo")
#' sum(Choose.out5[1:50] != Train$y)
#' sum(Choose.out10[1:50] != Train$y)
#' sum(Choose.out5[51:150] != Test$y)
#' sum(Choose.out10[51:150] != Test$y)

RPChoose <- function(XTrain, 
                     YTrain, 
                     XTest,  
                     d,      
                     B2 = 10,
                     base = "LDA", 
                     k = c(3,5), 
                     projmethod = "Haar",  
                     estmethod = "training",
                     ...) {
  
      n <- length(YTrain)
      ntest <- nrow(XTest)
      p <- ncol(XTrain)
      k1 <- 1
      RP <-  RPGenerate(p, d, method = projmethod, B2)
      XRP <- as.matrix(crossprod(t(XTrain), RP),n,d*B2)

      if (base == "knn") {
        if(estmethod == "training") {
          stop("training error estimate unsuitable for knn classifier")
        }
        
        if (estmethod == "loo"){
          weight.test <- sapply(1:B2, function(j) {min(sapply(k,function(x){mean(class::knn.cv(as.matrix(XRP[,d*(j-1) + 1:d ],n,d), YTrain, x) != YTrain, na.rm = TRUE)}))})
        }
        
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        kcv.voteRP <- sapply(k,function(x){mean(class::knn.cv(as.matrix(XRP[, cols1],n,d), YTrain, x) != YTrain, na.rm = TRUE)})
        k1 <- k[which.min(kcv.voteRP)]
        Train.Class <- as.numeric(class::knn.cv(as.matrix(XRP[, cols1],n,d), YTrain, k1))
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(class::knn(as.matrix(XRP[, cols1],n,1), XRPTest, YTrain, k1))
      }
      
      if (base == "LDA") {
        if(estmethod == "training") {
          weight.test <- sapply(1:B2, function(j){mean(stats::predict(MASS::lda(x = as.matrix(XRP[,d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRP[1:n, d*(j-1) + 1:d], n,d))$class != YTrain, na.rm = TRUE)})
          cols1 <- d*(which.min(weight.test) - 1) + 1:d
          Train.Class <- as.numeric(stats::predict(MASS::lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), as.matrix(XRP[, cols1],n,d))$class)
          XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
          Test.Class <- as.numeric(stats::predict(MASS::lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
        
        if (estmethod == "loo") {
          weight.test <- sapply(1:B2, function(j){mean(MASS::lda(x = as.matrix(XRP[1:n, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
          cols1 <-  d*(which.min(weight.test) - 1) + 1:d
          Train.Class <- as.numeric(MASS::lda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
          XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
          Test.Class <- as.numeric(stats::predict(MASS::lda(x = as.matrix(XRP[1:n, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
      }
      
      if (base == "QDA") {      
        if(estmethod == "training") {
          weight.test <- sapply(1:B2, function(j){mean(stats::predict(MASS::qda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRP[,d*(j-1) + 1:d],n,d))$class != YTrain, na.rm = TRUE)})
          cols1 <-  d*(which.min(weight.test) - 1) + 1:d
          Train.Class <- as.numeric(stats::predict(MASS::qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), as.matrix(XRP[, cols1],n,d))$class)
          XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
          Test.Class <- as.numeric(stats::predict(MASS::qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
        
        if (estmethod == "loo"){      
          weight.test <- sapply(1:B2, function(j){mean(MASS::qda(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
          cols1 <-  d*(which.min(weight.test) - 1) + 1:d
          Train.Class <- as.numeric(MASS::qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
          XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]), ntest, d)
          Test.Class <- as.numeric(stats::predict(MASS::qda(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain), XRPTest)$class)
        }
      }
      
      if (base == "Other") {
        weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
        cols1 <- d*(which.min(weight.test) - 1) + 1:d
        Train.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain, CV = TRUE)$class)
        XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),ntest,d)
        Test.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[, cols1],n,d), grouping = YTrain,  XRPTest)$class)
      }
      
      return(c(Train.Class, Test.Class)) 
    }


#' Chooses projection
#'
#' @description Chooses a the best projection from a set of size B2 based on a test error estimate.
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data.
#' @param d The lower dimension of the image space of the projections.
#' @param B2 The block size.
#' @param base The base classifier: one of "knn","LDA","QDA" or "other".
#' @param k The options for k if base is "knn".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training" or leave-one-out "loo".
#' @param ... Optional further arguments if base = "other".
#'
#' @details Randomly projects the the data B2 times. Chooses the projection yielding the smallest estimate of the test error. 
#' @note Resubstitution method unsuitable for the k-nearest neighbour classifier.
#'
#' @return A matrix of size p by d giving the selected projection.
#' @export
#'
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Choose <- RPChooseA(XTrain = Train$x, YTrain = Train$y, d = 2, B2 = 5, base = "QDA", 
#' projmethod = "Haar", estmethod = "loo")

RPChooseA <- function(XTrain, 
                      YTrain, 
                      d, 
                      B2 = 10, 
                      base = "LDA", 
                      k = c(3, 5), 
                      projmethod = "Haar", 
                      estmethod = "training", 
                      ...) {
  
  n <- length(YTrain)
  p <- ncol(XTrain)
  k1 <- 1
  RP <- RPGenerate(p, d, method = projmethod, B2)
  XRP <- as.matrix(crossprod(t(XTrain), RP), n, d * B2)
  
  if (base == "knn") {
    if (estmethod == "training") {
      stop("training error estimate unsuitable for knn classifier")
    }
    
    if (estmethod == "loo") {
      weight.test <- sapply(1:B2, function(j) {min(sapply(k, function(x) {mean(class::knn.cv(as.matrix(XRP[, d * (j - 1) + 1:d], n, d), YTrain, x) != YTrain, na.rm = TRUE)}))})
      cols1 <- d * (which.min(weight.test) - 1) + 1:d
    }
  }
  
  if (base == "LDA") {
    if (estmethod == "training") {
      weight.test <- sapply(1:B2, function(j) {mean(stats::predict(MASS::lda(x = as.matrix(XRP[, d * (j - 1) + 1:d], n, d), grouping = YTrain), as.matrix(XRP[1:n, d * (j - 1) + 1:d], n, d))$class != YTrain, na.rm = TRUE)})
      cols1 <- d * (which.min(weight.test) - 1) + 1:d
    }
    
    if (estmethod == "loo") {
      weight.test <- sapply(1:B2, function(j) {mean(MASS::lda(x = as.matrix(XRP[1:n, d * (j - 1) + 1:d], n, d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
      cols1 <- d * (which.min(weight.test) - 1) + 1:d
    }
  }
  if (base == "QDA") {
    if (estmethod == "training") {
      weight.test <- sapply(1:B2, function(j) {mean(stats::predict(MASS::qda(x = as.matrix(XRP[, d * (j - 1) + 1:d], n, d), grouping = YTrain), as.matrix(XRP[, d * (j - 1) + 1:d], n, d))$class != YTrain, na.rm = TRUE)})
      cols1 <- d * (which.min(weight.test) - 1) + 1:d
    }
    if (estmethod == "loo") {
      weight.test <- sapply(1:B2, function(j) {mean(MASS::qda(x = as.matrix(XRP[, d * (j - 1) + 1:d], n, d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
      cols1 <- d * (which.min(weight.test) - 1) + 1:d
    }
  }
  
  if (base == "Other") {
    weight.test <- sapply(1:B2, function(j) {mean(Other.classifier(x = as.matrix(XRP[, d * (j - 1) + 1:d], n, d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
    cols1 <- d * (which.min(weight.test) - 1) + 1:d
  }
  
  return(RP[,cols1])
}
   
