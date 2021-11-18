best <- function(state, outcome) {
  ## Read outcome data
  baza <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that state and outcome are valid / chanege this 
  #negation of %in% define by myslef 
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  state_names <- unique(baza[,7])
  outcome_names <- c("heart attack", "heart failure", "pneumonia")
  
  if (state %!in% state_names) {
   
    print(noquote(paste("Error in best(","\"",state,"\"",",","\"",outcome,"\"",") : invalid state", sep="")))
  } else if (outcome %!in% outcome_names) {
    
    print(noquote(paste("Error in best(","\"",state,"\"",",","\"",outcome,"\"",") : invalid outcome", sep="")))
  } else {
  # converting multiple columns from character to numeric format in r
  
  cols.num <- c(11,17,23)
  baza[cols.num] <- suppressWarnings(sapply(baza[cols.num], as.numeric))
  # subsetting data base to only variables needed 
  baza2 <- baza[,c(2,7,11,17,23)]
  # changing column names 
  colnames(baza2)[c(3:5)] <- c("heart attack", "heart failure", "pneumonia")
  #spliting dataframe by state 
  baza3 <- split(baza2, baza$State)
  ## Return hospital name in that state with lowest 30-day death
  hosp<- baza3[[state]][which(baza3[[state]][[outcome]] == min(baza3[[state]][[outcome]], na.rm = TRUE)), "Hospital.Name"]
  sort(hosp, decreasing = FALSE)[1]
  
}
}
  
  
             