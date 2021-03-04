#' Tim's pre-loaded data
#'
#' @description Tim's pre-loaded dataset. Need to ask him where it comes from.
#'
#' @format a matrix
"R"

#' EEG Eye State Data Set
#'
#' @description All data is from one continuous EEG measurement with the Emotiv EEG Neuroheadset. The duration of the measurement was 117 seconds. The eye state was detected via a camera during the EEG measurement and added later manually to the file after analysing the video frames. '1' indicates the eye-closed and '0' the eye-open state. All values are in chronological order with the first measured value at the top of the data.
#' 
#' @source \url{http://archive.ics.uci.edu/ml/datasets/EEG+Eye+State#}
#' 
#' @format 14,980 x 15 matrix. First fourteen columns are V1 - V14, final is eyeDetection. 
"EEG_Eye_State"

