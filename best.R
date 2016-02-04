best <- function(state, outcome) {
    data <- read.csv('outcome-of-care-measures.csv')
    if (!(state %in% data[,7])) {
      stop("invalid state")
    }
    if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
      stop("invalid outcome")
    }
    if (outcome == "heart attack"){
      sub_data <- subset(data, data[,7]==state,select=c(2,13))
    }
    else {
        if (outcome == "heart failure") {
          sub_data <- subset(data, data[,7]==state,select=c(2,19))
        }
        else {
            sub_data <- subset(data, data[,7]==state,select=c(2,25))
        }
    }
    lowest <- min(as.numeric(as.character(sub_data[,2])),na.rm = TRUE)
    low_hos <- subset(sub_data,as.numeric(as.character(sub_data[,2]))==lowest,select=1)
    as.character(low_hos[[1]][1])
}
    
