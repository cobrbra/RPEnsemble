col_names <- c(paste0("V", 1:14), "eyeDetection")
EEG_Eye_State <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/00264/EEG%20Eye%20State.arff", col_names = col_names, comment = "@")

usethis::use_data(EEG_Eye_State, overwrite = TRUE)
