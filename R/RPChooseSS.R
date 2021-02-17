#' A sample splitting version of RPChoose
#'
#' @description Chooses the best projection based on an estimate of the test error of the classifier with training data (XTrain, YTrain), the estimation method counts the number of errors made on the validation set (XVal, YVal).
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data.
#' @param XVal An n.val by p matrix containing the validation data feature vectors.
#' @param YVal A vector of length n.val of the classes (either 1 or 2) of the validation data.
#' @param XTest An n.test by p matrix of the test data feature vectors.
#' @param d The lower dimension of the image space of the projections.
#' @param B2 The block size.
#' @param base The base classifier one of "knn","LDA","QDA" or "other".
#' @param k The options for k if base = "knn".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param ... Optional further arguments if base = "other".
#' 
#' @details Maps the the data using B2 random projections. For each projection the validation set is classified using the the training set and the projection yielding the smallest number of errors over the validation set is retained. The validation set and test set are then classified using the chosen projection.
#'
#' @return Returns a vector of length n.val + n.test: the first n.val entries are the estimated classes of the validation set, the last n.test are the estimated classes of the test set.
#' @export
#'
#' @seealso RPParallel, RPChoose, lda, qda, knn 
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Validate <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 100, 100, 0.5)
#' Choose.out5 <- RPChooseSS(XTrain = Train$x, YTrain = Train$y, XVal = Validate$x, 
#'   YVal = Validate$y, XTest = Test$x, d = 2, B2 = 5, base = "QDA", projmethod = "Haar")
#' Choose.out10 <- RPChooseSS(XTrain = Train$x, YTrain = Train$y, XVal = Validate$x, 
#'   YVal = Validate$y, XTest = Test$x, d = 2, B2 = 10, base = "QDA", projmethod = "Haar")
#' sum(Choose.out5[1:50] != Validate$y)
#' sum(Choose.out10[1:50] != Validate$y)
#' sum(Choose.out5[51:150] != Test$y)
#' sum(Choose.out10[51:150] != Test$y)

RPChooseSS <- function(XTrain, 
                       YTrain, 
                       XVal, 
                       YVal, 
                       XTest,  
                       d,      
                       B2 = 100, 
                       base = "LDA", 
                       k = c(3,5), 
                       projmethod = "Haar", 
                       ... ) {
  
  n <- length(YTrain)
  p <- ncol(XTrain)
  n.val <- length(YVal)
  n.test <- nrow(XTest)
  w <- n.val
  RP <-  RPGenerate(p, d, method = projmethod, B2)
  XRP <- as.matrix(crossprod(t(XTrain), RP),n,B2*d)
  XRPVal <- as.matrix(crossprod(t(XVal), RP),n.val,B2*d)
  
  if (base == "knn") {
    weight.test <- sapply(1:B2, function(j){min(sapply(k,function(x){mean(class::knn(as.matrix(XRP[, d*(j-1) + 1:d],n,d), as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d), YTrain, x) != YVal, na.rm = TRUE)}))})
    cols1 <- d*(which.min(weight.test) - 1) + 1:d
    kcv.voteRP <- sapply(k,function(x){mean(class::knn(as.matrix(XRP[,cols1],n,d), as.matrix(XRPVal[,cols1],n.val,d), YTrain, x) != YVal, na.rm = TRUE)})
    k1 <- k[which.min(kcv.voteRP)]
    Val.Class <- as.numeric(class::knn(as.matrix(XRP[,cols1],n,d), as.matrix(XRPVal[,cols1],n.val,d), YTrain, k1))
    XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
    Test.Class <- as.numeric(class::knn(as.matrix(XRP[,cols1],n,d), XRPTest, YTrain, k1))
  }
    
  if (base == "LDA") {
    weight.test <- sapply(1:B2, function(j){mean(stats::predict(MASS::lda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d))$class != YVal, na.rm = TRUE)})
    cols1 <- d*(which.min(weight.test) - 1) + 1:d
    Val.Class <- as.numeric(stats::predict(MASS::lda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), as.matrix(XRPVal[,cols1],n.val,d))$class)
    XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
    Test.Class <- as.numeric(stats::predict(MASS::lda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), XRPTest)$class)
  }
  
  if (base == "QDA") {      
    weight.test <- sapply(1:B2, function(j){mean(stats::predict(MASS::qda(x =  as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain),  as.matrix(XRPVal[, d*(j-1) + 1:d],n.val,d))$class != YVal, na.rm = TRUE)})
    cols1 <-  d*(which.min(weight.test) - 1) + 1:d
    Val.Class <- as.numeric(stats::predict(MASS::qda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), as.matrix(XRPVal[,cols1],n.val,d))$class)
    XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
    Test.Class <- as.numeric(stats::predict(MASS::qda(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain), XRPTest)$class)
  }
    
  if (base == "Other") {
    weight.test <- sapply(1:B2, function(j){mean(Other.classifier(x = as.matrix(XRP[, d*(j-1) + 1:d],n,d), grouping = YTrain, CV = TRUE)$class != YTrain, na.rm = TRUE)})
    cols1 <- d*(which.min(weight.test) - 1) + 1:d
    Val.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain, as.matrix(XRPVal[,cols1],n.val,d))$class)
    XRPTest <- as.matrix(crossprod(t(XTest),RP[,cols1]),n.test,d)
    Test.Class <- as.numeric(Other.classifier(x = as.matrix(XRP[,cols1],n,d), grouping = YTrain, XRPTest)$class)
  }
    
  return(c(Val.Class, Test.Class)) 
}
