rankall <- function(outcome, num = "best") {
    data <- read.csv('outcome-of-care-measures.csv')
    if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
      stop("invalid outcome")
    }
    if (outcome == "heart attack"){
      sub_data <- subset(data, select=c(2,7,13))
    }
    else {
        if (outcome == "heart failure") {
          sub_data <- subset(data, select=c(2,7,19))
        }
        else {
            sub_data <- subset(data, select=c(2,7,25))
        }
    }
    sub_data <- data.frame(as.character(sub_data[,1]), as.character(sub_data[,2]), as.numeric(as.character(sub_data[,3])))
    names(sub_data) <- c("Name","State","Rate")
    sortd <- na.omit(sub_data[order(sub_data[,2],sub_data[,3],sub_data[,1]),])
    split_data <- split(sortd,sortd[,2])
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
    f_data <- data.frame(NULL)
    for (j in split_data) {
      x <- data.frame(j)
      s <- x[[1]][2]
      if (i<=nrow(j)){
        h <- x[[i]][1]  
      }
      else {
        h <- NULL
      }
      rbind(f_data,h,s)
    }
    f_data
}
    
