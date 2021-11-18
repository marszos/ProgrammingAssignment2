rankhospital <- function(state, outcome, num) {
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
    # sorting by outcome and hospital_name 
    baza4 <- baza3[[state]][order(baza3[[state]][[outcome]], baza3[[state]][['Hospital.Name']]),]
    # subsetting dataframe / only cols needed 
    col <- c("Hospital.Name",outcome)
    baza4 <- baza4[,col]
   # removing rows with NA
    baza4 <- baza4[complete.cases(baza4), ]
   # creating new variable Rank 
    baza4$Rank <- 1:nrow(baza4) 
    
    if (num == 'best') {
      num <- 1
      baza4[which(baza4$Rank == num), 'Hospital.Name']
    } else if (num == 'worst') {
      num <- nrow(baza4)
      baza4[which(baza4$Rank == num), 'Hospital.Name']
      
    } else if (num > nrow(baza4)) {
      print(NA)
    } else {
    
    baza4[which(baza4$Rank == num), 'Hospital.Name']
    
    }
  }
}

