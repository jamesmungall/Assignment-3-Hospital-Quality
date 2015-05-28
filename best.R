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
if(outcome=='heart attack') chooseCol <- 11;
if(outcome=='heart failure') chooseCol <- 17;
if(outcome=='pneumonia') chooseCol <- 24;
ocm1<-read.csv('outcome-of-care-measures.csv', stringsAsFactors=FALSE);
ocm2<-subset(ocm1, State==state, select=chooseCol);
ocm3<-as.numeric(ocm2[,1]); # Convert characters to numeric. May throw a warning for NA's produced
ocm4<-ocm3[complete.cases(ocm3)]; # Remove NA's
lowestRate<-min(ocm4);
indexOfLowest <- match(lowestRate, ocm3);
bestHosp <- subset(ocm1, indexOfLowest==rownames(ocm1), select='Hospital.Name');
# TODO: THIS FUNCTION IS RETURNING INCORRECT RESULTS
return(bestHosp);  
}