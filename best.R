best <- function(state, outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death rate
  allOutcomes<-c('heart attack', 'heart failure','pneumonia');
  checkOutcome<-function(){
    outcome <- tolower(outcome)
    validOutcome <- (outcome %in% allOutcomes);
    if(!validOutcome){
      stop('invalid outcome');
    }
    else{
      return(TRUE);
    }
  }
  checkState<-function(){
    hd<-read.csv('hospital-data.csv',stringsAsFactors=F);
    allStates<-unique(hd$State);
    state<-toupper(state);
    validState <- (state %in% allStates);
    if(!validState){
      stop('invalid state');
    }
    else{
      return(TRUE);
    }
  }
  turnOutcomeIntoMortalityRateColumnNames<-function(){
    outcome<-tolower(outcome);
    if(outcome=='heart attack') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack';
    if(outcome=='heart failure') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure';
    if(outcome=='pneumonia') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia';
    return(chooseCol)
  }
  checkOutcome();
  checkState();
  outcome<-turnOutcomeIntoMortalityRateColumnNames();
    
  tbl_df(read.csv('outcome-of-care-measures.csv', stringsAsFactors=F)) %>%
    select(Hospital.Name,State,mortalityRate=one_of(outcome)) %>%
    filter(State==state) %>%
    arrange(Hospital.Name) %>%
    filter(rank(mortalityRate, ties.method="first")==1) %>%
    select(Hospital.Name) %>%
    as.character()

}