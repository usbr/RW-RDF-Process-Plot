##############################################################################
#This script checks for mins <= 0 to compare two RW model runs
#This is different than what generic_minmax_check returns since it can return 
#mutliple values occuring in the same month

#DEVELOPMENT IS ON GOING ON THIS

#   Created by C. Felletter 4/2019
##############################################################################

generic_min0_check <- function(scen_dir,scens,timestep) { 
  
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 1. Set Up ##
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #Additional plotting functions and libraries 
  library('tidyverse') #ggplot2,dplyr,tidyr
  
  message(paste("Finding <= 0 values of",variable))
  
  for (i in 1:length(scens)){
    min <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Month, TraceNumber) %>%
      summarise(min = min(Value)) %>%
      filter(min <= 0)  
    
    
    if (i == 1){
      min0chk <- cbind.data.frame(min)
    } else if (i > 1 | is.data.frame(min0chk)){
      min0chk <- rbind.data.frame(min0chk,
                                 cbind.data.frame(min))
    }
    
  } #closes scen loop
  
  var_nospacenocol <- gsub(":", ".", gsub("[[:space:]]", "", variable)) 
  #remove var name spaces and any colons  
  
  if(dim(min0chk)[1] != 0){
    write.csv(min0chk,file = paste0(ofigs,'/min0chk_',var_nospacenocol,'.csv'))
  } else {
    message(paste(variable,"contains no <= 0 values"))
  }



} #end function