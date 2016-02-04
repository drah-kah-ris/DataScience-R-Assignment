rankhospital <- function(state, outcome, num = "best") {
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
    sub_data <- data.frame(as.character(sub_data[,1]),as.numeric(as.character(sub_data[,2])))
    names(sub_data) <- c("Name","Rate")
    sortd <- sub_data[order(sub_data[,2],sub_data[,1]),]
    i <- 0
    if (num == "best") {
      i <- 1
    }
    else {
      if (num == "worst") {
        i <- nrow(sortd)
      }
      else {
        i <- num
      }
    }
    as.character(sortd[[1]][i])
}
    
