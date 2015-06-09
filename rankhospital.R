rankhospital <- function(state, outcome, num = 'best'){
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
  turnNumIntoAnAppropriateValue<-function(){}
  checkOutcome();
  checkState();
  outcome<-turnOutcomeIntoMortalityRateColumnNames();
suppressWarnings(    # NAs introduced during mutate due to 'not available' data
  r1 <- tbl_df(read.csv('outcome-of-care-measures.csv', stringsAsFactors=F)) %>%
    select(Hospital.Name,State,mortalityRate=one_of(outcome)) %>%
    filter(State==state) %>%
    mutate(mortalityRateNumeric=as.numeric(mortalityRate)) %>%
    arrange(mortalityRateNumeric,Hospital.Name) %>%
    na.omit()
) 
# Work with num
#----------------
# Now that we know the mortality rate vector we can do logical work with num.
# Firstly, turn 'best' and 'worst' into numbers.
# Secondly, test that num is not too big.
  if(num=='best') 1 -> num;
lengthMortalityRateVector <- r1 %>% count %>% as.numeric
  if(num=='worst') lengthMortalityRateVector -> num
  if(!(is.numeric(num))) stop('Please use best, worst or an integer for ranking');
if(num > lengthMortalityRateVector){
  return(NA) # Cannot request a higher ranking than the length of mortality rate vector
}

#-------
#Continue to filter now we have verified num.  
  r1 %>%
    filter(rank(mortalityRateNumeric, ties.method="first")==num) %>%
    select(Hospital.Name) %>%
    as.character()


}