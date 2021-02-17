#' Chooses a projection from each block in parallel
#' 
#' @description Makes B1 calls to RPChoose or RPChooseSS in parallel and returns the results as a matrix.
#'
#' @param XTrain An n by p matrix containing the training data feature vectors.
#' @param YTrain A vector of length n containing the classes (either 1 or 2) of the training data.
#' @param XVal An n.val by p matrix containing the validation data feature vectors.
#' @param YVal A vector of length n.val of the classes (either 1 or 2) of the validation data.
#' @param XTest An n.test by p matrix containing the test data feature vectors.
#' @param d The lower dimension of the image space of the projections.
#' @param B1 The number of blocks.
#' @param B2 The size of each block.
#' @param base The base classifier one of "knn","LDA","QDA" or "other".
#' @param projmethod "Haar", "Gaussian" or "axis".
#' @param estmethod Method for estimating the test errors to choose the projection: either training error "training", leave-one-out "loo", or sample split "samplesplit".
#' @param k The options for k if base is "knn".
#' @param clustertype The type of cluster: "Default" uses just one core, "Fork" uses a single machine, "Socket" uses many machines. Note "Fork" and "Socket" are not supported on windows.
#' @param cores Required only if clustertype==Fork: the number of computer cores to use (note: cores > 1 not supported on Windows).
#' @param machines Required only if clustertype==Socket: the names of the machines to use e.g. c("Computer1", "Computer2") (not supported on Windows).
#' @param seed If not NULL, sets random seed for reproducible results.
#' @param ... Optional further arguments if base = "other".
#' 
#' @importFrom MASS lda qda
#' @importFrom class knn knn.cv
#' 
#' @details Makes B1 calls to RPChoose or RPChooseSS in parallel..
#'
#' @return If estmethod == "training" or "loo" , then returns an n+n.test by B1 matrix, each row containing the result of a call to RPChoose. If estmethod == "samplesplit", then returns an n.val+n.test by B1 matrix, each row containing the result of a call to RPChooseSS.
#' @export
#'
#' @seealso RPChoose, RPChooseSS.
#' @examples
#' Train <- RPModel(1, 50, 100, 0.5)
#' Test <- RPModel(1, 100, 100, 0.5)
#' Out <- RPParallel(XTrain = Train$x, YTrain = Train$y, XTest = Test$x, d = 2, B1 = 10, 
#'   B2 = 10, base = "LDA", projmethod = "Haar", estmethod = "training")
#' colMeans(Out)

RPParallel <- function(XTrain, 
                       YTrain, 
                       XVal = NULL,
                       YVal = NULL, 
                       XTest,  
                       d,    
                       B1 = 500, 
                       B2 = 50, 
                       base = "LDA", 
                       projmethod = "Gaussian",
                       estmethod = "training",
                       k = c(3,5,9), 
                       clustertype = "Default", 
                       cores = 1,
                       machines = NULL,  
                       seed = 1,
                       ... ) {
  
  if (clustertype == "Default") {
    cluster = parallel::makePSOCKcluster(1)
  }
  
  if (clustertype == "Fork") {
    cluster = parallel::makeForkCluster(cores)
  }
  
  if (clustertype == "Socket") {
    cluster = parallel::makePSOCKcluster(names = machines)
  }
  
  parallel::clusterExport(cluster, c(ls(), "knn.cv", "knn", "lda", "qda"), envir = environment())

  if (is.null(seed) == FALSE) {
    parallel::clusterSetRNGStream(cluster, seed)
  }

  if (estmethod == "samplesplit") {
      n.val <- length(YVal)
      RP.out <- simplify2array(parallel::parLapply(cl = cluster, 1:B1, function(x){return(RPChooseSS(XTrain, YTrain, XVal, YVal, XTest, d, B2, base, k, projmethod))}))
  }
  
  else {
    RP.out <- simplify2array(parallel::parLapply(cl = cluster, 1:B1, function(x){return(RPChoose(XTrain, YTrain, XTest, d, B2, base, k, projmethod, estmethod))}))
  }
  
  parallel::stopCluster(cluster) 
  
 return (RP.out)
}
