
#filter out LB nodes 
allWU <- allWU %>%
  dplyr::filter(Node %in% nodes)

unique(allWU$Node)



#filter out all but one node

CUL <- allCUL %>%
  dplyr::filter(Sector != "ResReg") #remove from total demands  

which(!is.numeric(CUL$Value))
sum(CUL$Value)

#plot total demand and total CUL 
zz <- rbind(allWU[,c("Date","Value","Year","Slot")], 
            CUL[,c("Date","Value","Year","Slot")])
head(CUL)
head(zz)
summary(CUL)

#create a depleted slot   
requested <- zz %>%
  dplyr::filter(Slot == "Depletion Requested")
depleted <- zz %>%
  dplyr::filter(Slot == "Depletion Shortage")

depleted$Value = requested$Value - depleted$Value   
depleted$Slot = rep("Depletion",times = length(depleted$Slot))

xx <- rbind.data.frame(zz,depleted)

p <- xx %>% 
  dplyr::filter(Slot %in% c("Depletion Requested","Depletion","CUL"))  %>% 
  group_by(Slot,Year) %>%
  summarise(Value = sum(Value))  %>%
  ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() + 
  geom_line() +
  scale_color_manual(values = c(mycolors[3],mycolors[1])) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(title = paste(node_title,"Total Annual Demand"), y = "Depletions (AF/yr)")
print(p)
if(printfigs==T){ ggsave(filename = file.path(ofigs,nodes[i],paste0(nodes[i]," Ann Total Demand",scens,".png")), width = gage_widths[5],height = gage_heights[5])}


p <- xx %>% 
  dplyr::filter(Slot %in% c("Depletion Requested","Depletion","CUL"))  %>% 
  group_by(Slot,Year) %>%
  summarise(Value = sum(Value))  %>%
  ggplot(aes(x = Year, y = Value, color = Slot)) + theme_light() + 
  geom_line() +
  scale_color_manual(values = c(mycolors[3],mycolors[2],mycolors[1])) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(title = paste("UB Total Annual Demand"), y = "Depletions (AF/yr)")
print(p)

