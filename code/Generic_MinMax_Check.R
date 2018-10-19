##############################################################################
#This script checks min and max to compare two RW model runs

#DEVELOPMENT IS ON GOING ON THIS

#   Created by C. Felletter 8/2018
#   Updated by CF on 10/2018 to be a function
##############################################################################

generic_minmax_check <- function(scen_res) { 
  
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
  
  #get everything on a date 
  scen_res$MonthNum = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
  #get a numeric month number
  scen_res$MonthNum = as.numeric(format.Date(scen_res$MonthNum, format = "%m"))
  
  
  # minvals <- cbind.data.frame(
  #   #check 
    
    
    
    
    # #My tring to make max##### friday final me 
    # 
    # scen_res %>%
    #   dplyr::filter(Variable == variable) %>%
    #   dplyr::filter(Scenario == names(scens[1]))  %>%
    #   dplyr::group_by(MonthNum) %>%
    #   dplyr::filter(MonthNum == 5) %>%
    #   # summarise(max = max(Value))
    # 
    # scen_res %>%
    #   dplyr::filter(Variable == variable) %>%
    #   dplyr::filter(Scenario == names(scens[2]))  %>%
    #   dplyr::group_by(MonthNum) %>%
    #   summarise(max = max(Value),max = max(Value)) %>%
    #   dplyr::filter(MonthNum == 5) 
      
    
  #alans suggestion 
    
    scen_res %>% 
      filter(ObjectSlot == variable) %>% # may need to replace this row wit Variable == "your variable"
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
    
    
   
  # #returns 357 for this, are they duplicates? 
  # 
  #   
  #   scen_res %>%
  #     dplyr::filter(Scenario == names(scens[1]))  %>%
  #     dplyr::group_by(MonthNum) %>%
  #     dplyr::filter(Value %in% min(Value)) %>%
  #     dplyr::arrange(MonthNum) #%>%
  #     #remove duplicates with ALAN MAGIC
  #     select(Month, Value, TraceNumber)
  #   
    #returns 13 for this, and Aug is a duplicate 
    
 
  # )
  #  
  
  print(minvals[,c(1,2,4)])
  
  write.csv(minvals[,c(1,2,4,6,8,10,12)],file = paste0(ofigs,'/BMFix_mins.csv'))

} #end function