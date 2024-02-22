##############################################################################
#This script checks min and max to compare two RW model runs

#DEVELOPMENT IS ON GOING ON THIS

#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
##############################################################################

generic_minmax_check <- function(scen_dir,scens,timestep) { 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  
  #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. max/min value
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
 
  message(paste("Creating max/min value",variable))
  
  for (i in 1:length(scens)){
    min <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Month, TraceNumber) %>%
      summarise(min = min(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Month, min) %>%
      arrange(TraceNumber) %>%
      summarise_at(
        month.name, 
        .funs = funs(min_val = min, min_trace = which.min)
      ) %>%
      gather(Variable, Value, -Scenario) %>% 
      # the rest just gets a little nicer looking table
      mutate(
        Month = stringr::str_split_fixed(Variable, "_", 2)[,1],
        Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      ) %>%
      spread(Variable, Value) %>%
      slice(match(month.name, Month))
    
    max <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Month, TraceNumber) %>%
      summarise(max = max(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Month, max) %>%
      arrange(TraceNumber) %>%
      summarise_at(
        month.name, 
        .funs = funs(max_val = max, max_trace = which.max)
      ) %>%
      gather(Variable, Value, -Scenario) %>% 
      # the rest just gets a little nicer looking table
      mutate(
        Month = stringr::str_split_fixed(Variable, "_", 2)[,1],
        Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      ) %>%
      spread(Variable, Value) %>%
      slice(match(month.name, Month))
    
    if (i == 1){
      minmaxchk <- cbind.data.frame(min,max[,c("max_trace","max_val")])
    } else if (i > 1 | is.data.frame(minmaxchk)){
      minmaxchk <- rbind.data.frame(minmaxchk,
                                    cbind.data.frame(min,max[,c("max_trace","max_val")]))
    }
    
    
  } #closes scen loop
  
  var_nospacenocol <- gsub(":", ".", gsub("[[:space:]]", "", variable)) 
  #remove var name spaces and any colons  
  
  write.csv(minmaxchk,file = paste0(ofigs,'/minmaxchk_',var_nospacenocol,'.csv'))
  
  return(minmaxchk)  
  

} #end function