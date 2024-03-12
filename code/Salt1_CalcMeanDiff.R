#### after running Salt1_WQAnn.R use this code to compare scenario 2-1 mean over entire run period and report out diff 


data_averages <- scen_res %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Variable) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

m1 <- data_averages %>%
  dplyr::filter(Scenario == names(scens[1]))

m2 <- data_averages %>%
  dplyr::filter(Scenario == names(scens[2]))

data_diff <- m2
data_diff$Mean = m2$Mean - m1$Mean
data_diff <- data_diff %>%
  select(names(data_diff)[1:3])
data_diff$MinusScenario = m1$Scenario
data_diff

#save scen_res 1 data 
data_averages1 <- data_averages
data_diff1 <- data_diff

## repeat this for scen_res2 data 

data_averages <- scen_res2 %>%
  dplyr::filter(startyr <= Year) %>% dplyr::filter(Year <= endyr) %>% #filter year
  dplyr::group_by(Scenario, Variable) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

m1 <- data_averages %>%
  dplyr::filter(Scenario == names(scens[1]))

m2 <- data_averages %>%
  dplyr::filter(Scenario == names(scens[2]))

data_diff <- m2
data_diff$Mean = m2$Mean - m1$Mean
data_diff <- data_diff %>%
  select(names(data_diff)[1:3])
data_diff$MinusScenario = m1$Scenario
data_diff

#combine scen_res and scen_res2 data 
data_averages <- rbind(data_averages1,data_averages)
data_diff <- rbind(data_diff1,data_diff)


write.csv(data_averages,file = paste0(data_dir,'/','Average_FlowMassConc_',startyr,"-",endyr,'.csv'))
write.csv(data_diff,file = paste0(data_dir,'/','AverageDifference_FlowMassConc_',startyr,"-",endyr,'.csv'))

