best<-function(state, outcome){
#---------
# Check that state and outcome are valid
hd<-read.csv('hospital-data.csv',stringsAsFactors=F);
allStates<-unique(hd$State);
allOutcomes<-c('heart attack', 'heart failure','pneumonia');


checkState<-function(){
  state<-toupper(state);
  validState <- (state %in% allStates);
  if(!validState){
    stop('invalid state');
  }
  else{
    return(TRUE);
  }
}
checkState();

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
## Read outcome data
# columns 11, 17, 24 are heart attack, heart failure and pneumonia respectively
outcome<-tolower(outcome);

## Return hospital name in that state with lowest 30-day death ## rate)
if(outcome=='heart attack') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack';
if(outcome=='heart failure') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure';
if(outcome=='pneumonia') chooseCol <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia';

ocm1<-read.csv('outcome-of-care-measures.csv', stringsAsFactors=FALSE);

ocm2<-subset(ocm1, State==state, select=c('Hospital.Name',chooseCol));

indexOfLowest <-suppressWarnings(which.min(ocm2[,chooseCol])); # throws a warning for NA's introduced
# To ensure that I get both hospitals if there is a tie, I need to find the 
# minimum value, and then find index of hospitals with this value.
#minMortality <- min(ocm2[,chooseCol]);

#booleanIndicesForMinMortality <- ocm2[,chooseCol] %in% minMortality;
#bestHospWithMortalityRate <- ocm[booleanIndicesForMinMortality,];
#  TODO - above code not working yet.

bestHosp <- subset(ocm2, indexOfLowest==rownames(ocm1), select='Hospital.Name');

return(bestHosp);  
}