RPConcatenateA <- function(XTrain, 
                           YTrain,
                           d,
                           B1 = 100,
                           B2 = 10,
                           base = "LDA",
                           estmethod = "training") {
  
  p <- ncol(XTrain)
  
  A_concat <- matrix(0, p, d * B1)
  
  pb = utils::txtProgressBar(min = 0, max = B1, initial = 0, style = 3) 
  
  
  for (b1 in 1:B1) {
    A_concat[, (b1-1)*d + 1:d] <- RPChooseA(XTrain = XTrain, YTrain = YTrain, d = d, B1 = B1, 
                                            B2 = B2, base = base, estmethod = estmethod)
  
    utils::setTxtProgressBar(pb, b1)
    
    }
  
  return(A_concat)
  
}

RPDecompose <- function(XTrain,
                     YTrain,
                     d,
                     B1 = 100,
                     B2 = 10,
                     base = "LDA",
                     estmethod = "training") {
  
  A_concat <- RPConcatenateA(XTrain = XTrain, YTrain = YTrain, d = d, B1 = B1, B2 = B2, 
                             base = base, estmethod = estmethod)
  
  average_projection <- (1/B1) * A_concat %*% t(A_concat)
  decomposition <- svd(average_projection)
  
  return(decomposition)
  
}

RPReduce <- function(XTrain,
         YTrain,
         XTest,
         YTest = NULL,
         reduced_dim = 2,
         d,
         B1 = 100,
         B2 = 10,
         base = "LDA",
         estmethod = "training",
         decomposition = NULL) {

  if (is.null(decomposition)) {
    decomposition <- RPDecompose(XTrain = XTrain, YTrain = YTrain, d = d, B1 = B1, B2 = B2, 
                                 base = base, estmethod = estmethod)
  }
  
  
  
}


