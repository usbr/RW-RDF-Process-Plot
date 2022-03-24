
AFMonCFS <- c(61.48760331,55.53719008,61.48760331,59.50413223,61.48760331,59.50413223,61.48760331,
              61.48760331,59.50413223,61.48760331,59.50413223,61.48760331)

CFSAFMon <- 1/AFMonCFS  

#convert for only one var and scen
convert <- scen_res %>%
  dplyr::filter(Variable == "FlamingGorge.Outflow") %>%
  dplyr::filter(Scenario == "Offc CRSS") #%>%
# convert <- scen_res[which(scen_res$Variable == "FlamingGorge.Outflow" && scen_res$Scenario == "Offc CRSS"),] # doesn't work for some reason 
#gotta be a better way than the below but it works... 
changeslots <- which(scen_res$Variable == "FlamingGorge.Outflow")[which(which(scen_res$Variable == "FlamingGorge.Outflow") %in% which(scen_res$Scenario == "Offc CRSS"))]
# convert == scen_res[changeslots,] #just a check 
convert$Value = convert$Value*rep(CFSAFMon,times = length(convert$Value)/12)
scen_res[changeslots,] = convert
head(convert)


#convert cfs to af/mo for all scens and one var 

#annual sum of outflows (should be in ac-ft to do this) vs year
y_lab = "Annual Flow (ac-ft/yr)"

#convert cfs to af/mo 
convert <- scen_res %>%
  dplyr::filter(Variable == variable) 
convert$Value = convert$Value*rep(AFMonCFS,times = length(convert$Value)/12)
p <- convert %>%
  dplyr::filter(Year <= last(yrs2show)) %>% #one run has 2023 so filter that out so axis work
  dplyr::group_by(Scenario, Year) %>%
  dplyr::summarise(Value = sum(Value)) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario, group = Scenario)) +
  geom_line() +
  # geom_boxplot() +
  # scale_x_discrete("Month",labels = month.abb) + #display abb. month names
  labs(title = paste("Average Annual",title), y = y_lab)
print(p)


########
#change a individual scenario name afte the fact 
changeslots <- which(scen_res$Scenario == "Max Constrained")
scen_res[changeslots,"Scenario"] = rep("800<=Max(Min(+25))",times = length(changeslots))
unique(scen_res$Scenario)

##########

# # Adding factors so ggplot does not alphebetize legend
xx$Slot = factor(xx$Slot, levels=c("Depletion Requested","Depletion","CUL"))



