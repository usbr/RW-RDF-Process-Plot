
names(zz_inout)
df <- zz_inout %>% pivot_wider(names_from = "Variable",values_from = "Value")

# df1 <- df %>% dplyr::filter(Scenario == "June 2021 - ST IG") #unique(zz_inout$ScenarioGroup)[1])
df1 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_inout$ScenarioGroup)[1])
df2 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_inout$ScenarioGroup)[2])
df <- left_join(df1,df2,by=c("TraceNumber","Year"))
# View(df) #.x slots are IG, .y slots are noDRO
names(df)
df <- df %>% mutate(FlamingGorge.delFlow = FlamingGorge.Outflow.x - FlamingGorge.Outflow.y,
                    BlueMesa.delFlow = BlueMesa.Outflow.x - BlueMesa.Outflow.y,
                    Navajo.delFlow = Navajo.Outflow.x - Navajo.Outflow.y,
                    Powell.delFlow = Powell.Inflow.x - Powell.Inflow.y) #i didn't process this!

# df <- df %>% select(-ScenarioGroup.x, -Scenario.x, -ScenarioGroup.y, -Scenario.y) # if want to keep 2 sceonarios x & y Outflows
df <- df %>% select(TraceNumber,Year,FlamingGorge.delFlow,BlueMesa.delFlow,Navajo.delFlow,Powell.delFlow) #only keep delFlow

#gather up to use ggplot 
df<-df %>% pivot_longer(cols=FlamingGorge.delFlow:Powell.delFlow,names_to = "Variable",values_to = "Value")
head(df)
# df<- df %>% mutate(Value = -1*Value) #make loss of Outflow positive 

#mean plot
df %>% group_by(Variable,Year) %>%
  # filter(Variable == "FlamingGorge.delFlow") %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(x=Year,y=Value,color = Variable)) +
  geom_line()

#need zz_all for Cloud_plot_woHistv2.R
zz_all <- df %>% mutate(Scenario = "June 2021") %>%
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Variable, Year) %>% #don't use scenario here 
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

write.csv(zz_all,file.path(ofigs,"Stats",paste0(Hydro,"_delRelease_stats.csv")))


y_lab = "Flow (kaf/yr)"
title = "Additional Release for DRO - June 2021 CRSS" 


zz_all %>% group_by(Variable,Year) %>%
  # filter(Variable == "FlamingGorge.delFlow") %>%
  ggplot(aes(x=Year,y=Mean,color = Variable)) +
  geom_line() +
  labs(y = y_lab, x = '',title = title) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) #+ # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  # facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.addDRORelease","Mean",".png")), width = 11,height = 7) #maxes out pptx slide height 

zz_all %>% group_by(Variable,Year) %>%
  # filter(Variable == "FlamingGorge.delFlow") %>%
  ggplot(aes(x=Year,y=Max,color = Variable)) +
  geom_line() +
  labs(y = y_lab, x = '',title = title) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) #+ # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
# facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.addDRORelease","90th",".png")), width = 11,height = 7) #maxes out pptx slide height 


zz_all %>% group_by(Variable,Year) %>%
  # filter(Variable == "FlamingGorge.delFlow") %>%
  ggplot(aes(x=Year,y=Mean,color = Variable)) +
  geom_line() +
  labs(y = y_lab, x = '',title = title) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) #+ # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
# facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.addDRORelease","10th",".png")), width = 11,height = 7) #maxes out pptx slide height 


View(zz_all)
NumCrit <- NA #3490
powtiers <- F
variable = "FlamingGorge.delFlow"
y_lab = "Additional Release for DRO (kaf/yr)"
title = "Flaming Gorge" 
source("code/Cloud_plot_woHistv2.R")

variable = "BlueMesa.delFlow"
title = "Blue Mesa" 
source("code/Cloud_plot_woHistv2.R")

variable = "Navajo.delFlow"
title = "Navajo" 
source("code/Cloud_plot_woHistv2.R")

variable = "Powell.delFlow"
title = "Powell" 
y_lab = "Additional Inflow from DRO (kaf/yr)"
source("code/Cloud_plot_woHistv2.R")


df$Scenario = df$ScenarioGroup


