rankall<- function(outcome='heart attack', num = "best") { 
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  # sample output
  # rankall("heart attack", 20) returns the 20th ranked hospital for heart attacks
#     hospital                      state
#      <NA>                          AK
#  D W MCMILLAN MEMORIAL HOSPITAL    AL

allStates = sort(state.abb)
  result <- data_frame();
  for(state in state.abb){
    hospital <- rankhospital(state, outcome, num);
    result <- result %>% bind_rows(data_frame(hospital=hospital,state=state))
  }
return(result)
}