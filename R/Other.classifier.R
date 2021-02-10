#' The user's favourite classifier
#'
#' @description User defined code to convert existing R code for classification to the correct format.
#'
#' @param x An n by p matrix containing the training dataset.
#' @param grouping A vector of length n containing the training data classes.
#' @param xTest An n.test by p test dataset.
#' @param CV If TRUE perform cross-validation (or otherwise) to classify training set. If FALSE, classify test set.
#' @param ... Optional arguments e.g. tuning parameters
#'
#' @details User editable code for your choice of base classifier.
#'
#' @return A vector of classes of the training or test set.
#' @export
#'
#' @examples
Other.classifier <- function(x, #n by p trining data matrix
                             grouping, #n vector of the classes of the trining samples
                             xTest,  #n.test by p test data matrix
                             CV = FALSE, #performs leave-one-out cross-validation if CV is TRUE 
                             ... ) {
  # Write code for choice of base classifier that returns vector of predicted classes of the training or test set as variable class. 
}
