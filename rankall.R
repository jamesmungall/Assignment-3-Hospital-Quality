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
    
  # Open outcome of care measures file
# Use small sample data for now
ocm1<-read.csv('sampleOutcomeOfMeasures.csv', stringsAsFactors=FALSE);
  #  ocm1<-read.csv('outcome-of-care-measures.csv', stringsAsFactors=FALSE);
  getResultForSingleState<-function(singleState){
    ocmSingleState<-subset(ocm1, State==singleState,select=c('Hospital.Name','State',chooseCol));
    # remove "not available"
    ocmSingleState[,chooseCol] <- suppressWarnings(as.numeric(ocmSingleState[,chooseCol]));# introduces NA's for "not available"
    completeCasesBoolean<-complete.cases(ocmSingleState);
    ocmSingleStateCompleteCases <- ocmSingleState[completeCasesBoolean,]; 
    
    # order data by increasing mortality rate (smallest first)
    ordering <-order(ocmSingleStateCompleteCases[,chooseCol]);
    ocmSingleStateCompleteCasesOrdered=ocmSingleStateCompleteCases[ordering,];
    
    return(ocmSingleStateCompleteCasesOrdered);
  }
  
  # We now select just the one hospital with the ranking as given in the argument, 'num', of the rankall function
  getSingleHospital<-function(resultForSingleState){
    # validate num argument, given as second argument to rankall function
    if(num=='best') num=1;
    if(num=='worst') num=length(resultForSingleState[,1]);
    if(!(is.numeric(num))) stop('Please use best, worst or an integer for ranking');
     return(resultForSingleState[num,]);
    
  }
 
  # I want to check over all states
  hd<-read.csv('hospital-data.csv',stringsAsFactors=F);
  allStates<-unique(hd$State);
  # TODO loop over all states
allHospitals=data.frame();
  for(i in 1:length(allStates)){
    resultForSingleState<-getResultForSingleState(singleState=allStates[i]);
    singleHospital<-getSingleHospital(resultForSingleState);
    allHospitals<-rbind(allHospitals,singleHospital);
  }
 
  return(allHospitals);

}