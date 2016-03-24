rankall <- function(outcome,num=1) {
 if (num=="best"){
   num <- 1
 }

  splitData <- extractState()
  states <- names(splitData)
  n <- length(states)
  
  output <- data.frame()
  for (i in 1:n) {
  #stateNumber <- 2
  
  list1 <- sapply(splitData,function(x) x[2])
  list2 <- sapply(splitData,function(x) x[11])
  list3 <- sapply(splitData,function(x) x[17])
  list4 <- sapply(splitData,function(x) x[23])
  x<- as.data.frame(c(list1[i],list2[i],list3[i],list4[i]))
  if (outcome=="heart attack") {
  y <- order(x[ ,2],x[ ,1],na.last=NA)
  
  } else if (outcome=="heart failure") {
    y <- order(x[ ,3],x[ ,1],na.last=NA)
  } else if (outcome=="pneumonia"){
    y<- order(x[ ,4],x[ ,1],na.last=NA)
  } else (stop("Bad Outcome"))
  
  if (num=="worst"){
    rank <- y[length(y)]
  } else {
  rank <- y[num]
  }
  
  if (is.na(rank)) {
    z <- (cbind("NA",states[i]))
  }
  z <- (cbind(as.character(x[rank,1]),states[i]))
  z <- as.data.frame(z)
  output <- rbind(output,z)
  }
  
  output <- as.data.frame(output,row.names=states)
  colnames(output) <- c("hospital","state")
  return(output)
}

extractState <- function(datafile="outcome-of-care-measures.csv"){
  data <- read.csv(datafile,colClasses = "character")
  data[,11]<- as.numeric(data[,11])
  data[,17]<- as.numeric(data[,17])
  data[,23]<- as.numeric(data[,23])
  splitData <- split(data,f=data$State)
}
