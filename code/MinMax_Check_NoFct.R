##############################################################################
#This script checks min and max to compare two RW model runs
#this scripts works on basis of annual values only 
#different than 

library('tidyverse') #ggplot2,dplyr,tidyr


#INPUTS: scens. startyr, endyr 
#and proceed using rw_scen_aggregate() to create scen_dir as ANNUAL values  
  
variables <- unique(scen_res$Variable)
years <- as.character(startyr:endyr)

  #
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 4. max/min value
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#this could be used to loop through multiple plots 

for(i in 1:length(variables)){
  ## Process Variables ##
  variable <- variables[i]  
 
  message(paste("Creating max/min value",variable))
  
  for (i in 1:length(scens)){
    min <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Year, TraceNumber) %>% #THIS STEP TAKES FOREVER!
      summarise(min = min(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Year, min) %>%
      arrange(TraceNumber)  %>% #table is now Scenario, TraceNumber, Years as Cols
      ungroup() %>% #use to get summarise_at which.min working 
      summarise_at( #error
        years, 
        .funs = funs(min_val = min, #min_val works #funs() provides a flexible way to generate a named list of functions for input to other functions like summarise_at().
                     min_trace = which.min) #error:  Column `2020_min_trace` must be length 1 (a summary value), not 0
                    #which.min(min$`2020`) works
      ) #%>%
    
    min_df <- cbind.data.frame(
      t(min[1:length(years)]),
      t(min[(length(years)+1):length(min)]))
    
    colnames(min_df) <- c("Min Val","Min Trace")
    row.names(min_df) <- years
    
    # ?pivot_wider -- should be able to use this somehow with contains()
            #Alans old Code 
      # gather(Variable, Value, -Scenario) %>% 
      # # the rest just gets a little nicer looking table
      # mutate(
      #   Year = stringr::str_split_fixed(Variable, "_", 2)[,1],
      #   Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      # ) %>%
      # spread(Variable, Value) %>%
      # slice(match(years, Year))
    
    mean <- scen_res %>%
      filter(Scenario == names(scens[i]))  %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
      dplyr::group_by(Scenario, Year) %>%
      dplyr::summarise(Value = mean(Value)) 
    colnames(mean) <- c("Scenario","Year","Mean Val")
    
    max <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Year, TraceNumber) %>%
      summarise(max = max(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Year, max) %>%
      arrange(TraceNumber)  %>%
      ungroup() %>% #use to get summarise_at which.min working 
      summarise_at(
        years, 
        .funs = funs(max_val = max, max_trace = which.max)
      ) 
    
    max_df <- cbind.data.frame(
      t(max[1:length(years)]),
      t(max[(length(years)+1):length(max)]))
    
    colnames(max_df) <- c("Max Val","Max Trace")
    row.names(max_df) <- years
    
      # gather(Variable, Value, -Scenario) #%>% 
      # # the rest just gets a little nicer looking table
      # mutate(
      #   Year = stringr::str_split_fixed(Variable, "_", 2)[,1],
      #   Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      # ) %>%
      # spread(Variable, Value) %>%
      # slice(match(years, Year))  
    
    df <- cbind.data.frame(mean,
                           min_df,
                           max_df)
    
    
    if (i == 1){
      minmaxchk <- df
    } else if (i > 1 | is.data.frame(df)){
      minmaxchk <- rbind.data.frame(minmaxchk,
                                    df)
    }
    
    
  } #closes scen loop
  
  var_nospacenocol <- gsub(":", ".", gsub("[[:space:]]", "", variable)) 
  #remove var name spaces and any colons  
  
  write.csv(minmaxchk,file = paste0(oFigs,'/minmaxchk_',var_nospacenocol,'.csv'))
  
} #closes variable loop 
