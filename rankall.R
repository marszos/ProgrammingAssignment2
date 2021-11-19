rankall <- function(outcome, num = "best") {
  ## Read outcome data
  baza <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


  # Check that state and outcome are valid / exception handling
  #negation of %in% define by myself
  '%!in%' <- function(x,y)!('%in%'(x,y))
  state_names <- unique(baza[,7])
  num_names <- c("best", "worst")
  outcome_names <- c("heart attack", "heart failure", "pneumonia")
  
  if(outcome %!in% outcome_names) {stop("invalid outcome", call. = FALSE)}
  if(!is.numeric(num) && num %!in% num_names) {stop("invalid outcome", call. = FALSE)} 
  
  
  
  #casting columns as numeric type
  cols.num <- c(11,17,23)
  baza[cols.num] <- suppressWarnings(sapply(baza[cols.num], as.numeric))

  # subsetting db to only variables needed and spliting data based on state
  baza2 <- baza[,c(2,7,11,17,23)]
  colnames(baza2)[c(3:5)] <- c("heart attack", "heart failure", "pneumonia")
  baza3 <- split(baza2, baza$State)

  #creating empty dataframe which will be populated later in a loop
  df <- data.frame(hospital = character(length(state_names)), state = character(length(state_names)))
  row.names(df) <- sort(state_names)

  # assigning char input argumnet "best" to value 1
  if(num == "best") {num <- 1}

  # condition (if arg == "worst" and the rest), main function retrieving the data
  if(num == "worst") {
    for (state in sort(state_names)) {

      # in each iteration / state sort the table on value of outcome and hospital name alphabetically
      baza3[[state]] <- baza3[[state]][order(baza3[[state]][[outcome]], baza3[[state]][['Hospital.Name']]),]
      #subsetting tables in each state only to columns hosp name and outcome
      col <- c("Hospital.Name",outcome)
      baza3[[state]] <- baza3[[state]][,col]
      #ignore NA
      baza3[[state]] <- baza3[[state]][complete.cases(baza3[[state]]), ]
      #create new column/variable "rank" with values ranging from 1 to len of particular table/state
      baza3[[state]][["Rank"]] <- 1:nrow(baza3[[state]]) 
      # num of rows in particular table / important for "worst" argument
      num <- nrow(baza3[[state]])

      
      # loop which populates empty df dataframe created earlier
      # if there is no hospital in the state with no specific rank output NA
      # otherwise populate the df dataframe with a hospital related to specific rank
      if (length(baza3[[state]][which(baza3[[state]][['Rank']] == num), 'Hospital.Name']) == 0) {
        df[state, "state"] <- state
        df[state, "hospital"] <- NA
      } else {
        df[state, "state"] <- state
        df[state, "hospital"] <-  baza3[[state]][which(baza3[[state]][['Rank']] == num), 'Hospital.Name']
      }
    }
    
    } else {
  
    for (state in sort(state_names)) {
    baza3[[state]] <- baza3[[state]][order(baza3[[state]][[outcome]], baza3[[state]][['Hospital.Name']]),]
    col <- c("Hospital.Name",outcome)
    baza3[[state]] <- baza3[[state]][,col]
    baza3[[state]] <- baza3[[state]][complete.cases(baza3[[state]]), ]
    baza3[[state]][["Rank"]] <- 1:nrow(baza3[[state]]) 
    
    if (length(baza3[[state]][which(baza3[[state]][['Rank']] == num), 'Hospital.Name']) == 0) {
      df[state, "state"] <- state
      df[state, "hospital"] <- NA
    } else {
      df[state, "state"] <- state
      df[state, "hospital"] <-  baza3[[state]][which(baza3[[state]][['Rank']] == num), 'Hospital.Name']
      }
    }
    }
  df
  
}