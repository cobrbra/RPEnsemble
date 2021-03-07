#' Choose a set of B1 projections
#'
#' @description Creates B1*B2 projections, and for each block of B2 selects the best projection.
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data.
#' @param XVal An n.test by p matrix containing the validation data feature vectors.
#' @param YVal A vector of length n.val of the classes (either 1 or 2) of the validation data. 
#' @param d The lower dimension of the image space of the projections.
#' @param B1 The number of blocks.
#' @param B2 The block size.
#' @param base The base classifier one of "knn","LDA","QDA" or "other".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training", leave-one-out "loo", or sample split "samplesplit".
#'
#' @return A p by d*B1 matrix, where each row block of size p by d is one of the B1 selections.
#' @export
#'
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Choose <- RPConcatenateA(XTrain = Train$x, YTrain = Train$y, d = 3, B1 = 10, B2 = 5, base = "QDA", 
#' projmethod = "Haar", estmethod = "loo")

RPConcatenateA <- function(XTrain, 
                           YTrain,
                           XVal = NULL,
                           YVal = NULL, 
                           d,
                           B1 = 100,
                           B2 = 10,
                           base = "LDA",
                           projmethod = "Haar",
                           estmethod = "training") {
  
  p <- ncol(XTrain)
  A_concat <- matrix(0, p, d * B1)
  
  cat("\nChoosing projections")
  pb = utils::txtProgressBar(min = 0, max = B1, initial = 0, style = 3) 
  
  
  for (b1 in 1:B1) {
    if (estmethod == "samplesplit") {
      A_concat[, (b1-1)*d + 1:d] <- RPChooseSSA(XTrain = XTrain, YTrain = YTrain, XVal = XVal, YVal = YVal, d = d, B1 = B1, 
                                                B2 = B2, base = base, projmethod = projmethod, 
                                                estmethod = estmethod)
    }
    
    else {
      A_concat[, (b1-1)*d + 1:d] <- RPChooseA(XTrain = XTrain, YTrain = YTrain, d = d, B1 = B1, 
                                              B2 = B2, base = base, projmethod = projmethod, 
                                              estmethod = estmethod)
    }
    
    utils::setTxtProgressBar(pb, b1)
  }
  
  return(A_concat)
  
}

#' Produce singular value decomposition for average choice of projection
#'
#' @description For the concatenated matrix containing B1 selections of p by B2 sized projection matrices, 
#' outputs the singular value decomposition.
#' 
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data.
#' @param XVal An n.test by p matrix containing the validation data feature vectors.
#' @param YVal A vector of length n.val of the classes (either 1 or 2) of the validation data.  
#' @param d The lower dimension of the image space of the projections.
#' @param B1 The number of blocks.
#' @param B2 The block size.
#' @param base The base classifier one of "knn","LDA","QDA" or "other".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training", leave-one-out "loo", or sample split "samplesplit".
#'
#'
#' @return A list of three entries: eigv, u and v. These correspond to the eigenvalues and the two factor matrices of the singular value decomposition.
#' @export
#'
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Choose <- RPDecomposeA(XTrain = Train$x, YTrain = Train$y, d = 3, B1 = 10, B2 = 5, base = "QDA", 
#' projmethod = "Haar", estmethod = "loo")

RPDecomposeA <- function(XTrain,
                         YTrain,
                         XVal = NULL,
                         YVal = NULL, 
                         d,
                         B1 = 100,
                         B2 = 10,
                         base = "LDA",
                         projmethod = "Haar",
                         estmethod = "training") {
  
  A_concat <- RPConcatenateA(XTrain = XTrain, YTrain = YTrain, XVal = XVal, YVal = YVal, d = d, B1 = B1, B2 = B2, 
                             base = base, projmethod = projmethod, estmethod = estmethod)
  
  cat("\nDecomposing matrix")
  average_projection <- (1/B1) * A_concat %*% t(A_concat)
  decomposition <- svd(average_projection)
  names(decomposition) <- c("eigv", "u", "v")
  
  return(decomposition)
  
}

#' Produce low-dimensional representations of a dataset
#' 
#' @description Implements the full supervised dimensionality reduction workflow, by generating then selecting matrices, decomposing the resultant concatenated matrix and then projecting onto a given sized space.
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n of the classes (either 1 or 2) of the training data. 
#' @param XVal An n.test by p matrix containing the validation data feature vectors.
#' @param YVal A vector of length n.val of the classes (either 1 or 2) of the validation data. 
#' @param reduced_dim The dimensionality of reduced data to produce.
#' @param d The lower dimension of the image space of the projections.
#' @param B1 The number of blocks.
#' @param B2 The size of each block.
#' @param base The base classifier one of "knn","LDA","QDA" or "other".
#' @param projmethod Either "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training", leave-one-out "loo", or sample split "samplesplit".
#' @param decomposition (optional) the result of a call to RPDecomposeA().
#'
#' @return A list with two elements, XTest_r and YTest. XTest_r is an n.test by reduced_dim sized matrix containing reduced data representations. YTest is whatever has been provided.
#' @export
#'
#' @examples
#' set.seed(100)
#' Train <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 50, 100, 0.5)
#' Rep <- RPReduce(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, reduced_dim = 2, d = 3, 
#' B1 = 10, B2 = 5, base = "QDA", projmethod = "Haar", estmethod = "loo")

RPReduce <- function(XTrain,
                     YTrain,
                     XTest,
                     YTest,
                     reduced_dim = 2,
                     d,
                     B1 = 100,
                     B2 = 10,
                     base = "LDA",
                     projmethod = "Haar",
                     estmethod = "training",
                     decomposition = NULL) {

  if (is.null(decomposition)) {
    decomposition <- RPDecomposeA(XTrain = XTrain, YTrain = YTrain, d = d, B1 = B1, B2 = B2, 
                                 base = base, projmethod = projmethod, estmethod = estmethod)
  }
  
  cat("\nProducing reductions")
  reductions <- as.data.frame(XTest %*% decomposition$v[, 1:reduced_dim, drop = FALSE])
  colnames(reductions) <- paste0("Dim_", 1:reduced_dim)
  
  if (!is.null(YTest)) {
    reductions$YTest <- factor(YTest)
  }
  
  return(reductions)
}


