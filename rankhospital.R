rankhospital<- function(state,outcome,num) {
stateData <- extractState(state=state)
if (num == "best"){
  return(best(state,outcome))
} 

else if (num > 0) {
   if (outcome =="heart attack"){
  rankOrder <- order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,stateData$Hospital.Name,na.last = NA)
   } else if (outcome =="heart failure") {
  rankOrder <- order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,stateData$Hospital.Name,na.last = NA)
   } else if (outcome =="pneumonia") {
  rankOrder <- order(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,stateData$Hospital.Name,na.last = NA)
   } else {
     stop("Bad outcome")
   }
  ranktoExtract <- rankOrder[num]
}

   if (num == "worst"){
     ranktoExtract <- rankOrder[length(rankOrder)]
  } else if (num > length(rankOrder)){
    return("NA")
  }
  return(stateData[ranktoExtract, 2])
}




best <- function(state,outcome) {
  
  stateData <- extractState(state=state)
  if (outcome =="heart attack"){
    minMortality <- min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm="TRUE")
    splitMort <- split(stateData,f=stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)}
  else if (outcome =="heart failure"){
    minMortality <- min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm="TRUE")
    splitMort <- split(stateData,f=stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }
  else if (outcome =="pneumonia") {
    minMortality <- min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm="TRUE")
    splitMort <- split(stateData,f=stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }
  else stop("invalid outcome")
  #print(minMortality)
  bestHosp <- splitMort[[as.character(minMortality)]]
  if(length(bestHosp$Hospital.Name) > 1){
    order <- sort(bestHosp$Hospital.Name)
    return(order[1])
  }
  else {
    return(bestHosp$Hospital.Name)
  }
  
}

extractState <- function(datafile="outcome-of-care-measures.csv",state){
  data <- read.csv(datafile,colClasses = "character")
  data[,11]<- as.numeric(data[,11])
  data[,17]<- as.numeric(data[,17])
  data[,23]<- as.numeric(data[,23])
  splitData <- split(data,f=data$State)
  stateData <- splitData[[state]]
  if (is.null(stateData)){
    stop("invalid state")
  }
  
  return(stateData)
}

