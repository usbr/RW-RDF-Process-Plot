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

if (variables[1] == "AnnlSlntyLsFrry_FWAAC"){
for(i in 1:4){ #length(variables)){
  ## Process Variables ##
  variable <- variables[i]  
 
  message(paste("Creating max/min value",variable))
  
  for (i in 1:length(scens)){
    min <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Year, TraceNumber) %>%
      summarise(min = min(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Year, min) %>%
      arrange(TraceNumber)  %>%
      summarise_at(
        years, 
        .funs = funs(min_val = min, min_trace = which.min)
      ) %>%
      gather(Variable, Value, -Scenario) %>% 
      # the rest just gets a little nicer looking table
      mutate(
        Year = stringr::str_split_fixed(Variable, "_", 2)[,1],
        Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      ) %>%
      spread(Variable, Value) %>%
      slice(match(years, Year))
    
    mean <- scen_res %>%
      filter(Scenario == names(scens[i]))  %>%
      dplyr::filter(Variable == variable) %>%
      dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
      dplyr::group_by(Scenario, Year) %>%
      dplyr::summarise(Value = mean(Value)) 
    mean$ensemble_mean_val <- mean$Value  
    
    max <- scen_res %>% 
      filter(Scenario == names(scens[i]))  %>%
      filter(Variable == variable) %>% # may need to replace this row wit Variable == "your variable"
      group_by(Scenario, Year, TraceNumber) %>%
      summarise(max = max(Value)) %>% # can probably also add max = max(Value) to this row, and then to the summarise_at below
      spread(Year, max) %>%
      arrange(TraceNumber)  %>%
      summarise_at(
        years, 
        .funs = funs(max_val = max, max_trace = which.max)
      ) %>%
      gather(Variable, Value, -Scenario) %>% 
      # the rest just gets a little nicer looking table
      mutate(
        Year = stringr::str_split_fixed(Variable, "_", 2)[,1],
        Variable = stringr::str_split_fixed(Variable, "_", 2)[,2]
      ) %>%
      spread(Variable, Value) %>%
      slice(match(years, Year))  
    
    df <- cbind.data.frame(min,
                           mean[,"ensemble_mean_val"],
                           max[,c("max_trace","max_val")])
    
    
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
} else {
  warning("Run MinMax Script manually if want something other than FWAC")
}
