rankall<- function(outcome, num = "best") { 
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  # sample output
  # rankall("heart attack", 20) returns the 20th ranked hospital for heart attacks
#     hospital                      state
#      <NA>                          AK
#  D W MCMILLAN MEMORIAL HOSPITAL    AL
  
  # I can validate the outcome as before
  allOutcomes<-c('heart attack', 'heart failure','pneumonia');
  checkOutcome<-function(){
    validOutcome <- (outcome %in% allOutcomes);
    if(!validOutcome){
      stop('invalid outcome');
    }
    else{
      return(TRUE);
    }
  }
  checkOutcome();
  #---------------
  ## and read outcome data as before
  outcome<-tolower(outcome);
  if(outcome=='heart attack') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack';
  if(outcome=='heart failure') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure';
  if(outcome=='pneumonia') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia';
  
  ocm1<-read.csv('outcome-of-care-measures.csv', stringsAsFactors=FALSE);
  
  # I want to check over all states
  hd<-read.csv('hospital-data.csv',stringsAsFactors=F);
  allStates<-unique(hd$State);
  # TODO this is where I am up to. state does not exist, & needs to go into a loop
 singleState = "AL";
  ocm2<-subset(ocm1, State==singleState,select=c('Hospital.Name',chooseCol));
 # remove "not available"
 ocm2[,3] <- suppressWarnings(as.numeric(ocm2[,3]));# introduces NA's for "not available"
 completeCasesBoolean<-complete.cases(ocm2);
 ocm2 <- ocm2[completeCasesBoolean,]; 
 
 # order data by increasing mortality rate (smallest first)
 ocm2=ocm2[order(ocm2[,3]),];

  return(ocm2);
}