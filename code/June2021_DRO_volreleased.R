
zz_inout <- scen_res %>%
  dplyr::filter(Variable %in% c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow','Powell.Inflow')) %>%

names(zz_inout)
df <- zz_inout %>% pivot_wider(names_from = "Variable",values_from = "Value")

# df1 <- df %>% dplyr::filter(Scenario == "June 2021 - ST IG") #unique(zz_stor$ScenarioGroup)[1])
df1 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_stor$ScenarioGroup)[1])
df2 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_stor$ScenarioGroup)[2])
df <- left_join(df1,df2,by=c("TraceNumber","Year"))
# View(df) #.x slots are IG, .y slots are noDRO
names(df)
df <- df %>% mutate(FlamingGorge.delStor = FlamingGorge.Storage.x - FlamingGorge.Storage.y,
                    BlueMesa.delStor = BlueMesa.Storage.x - BlueMesa.Storage.y,
                    Navajo.delStor = Navajo.Storage.x - Navajo.Storage.y)#,
# Powell.delStor = Powell.Storage.x - Powell.Storage.y) #i didn't process this!

# df <- df %>% select(-ScenarioGroup.x, -Scenario.x, -ScenarioGroup.y, -Scenario.y) # if want to keep 2 sceonarios x & y storages
df <- df %>% select(TraceNumber,Year,FlamingGorge.delStor,BlueMesa.delStor,Navajo.delStor,) #only keep delStor

#gather up to use ggplot 
df<-df %>% pivot_longer(cols=FlamingGorge.delStor:Navajo.delStor,names_to = "Variable",values_to = "Value")

df<- df %>% mutate(Value = -1*Value) #make loss of storage positive 

df %>% group_by(Year) %>%
  filter(Variable == "FlamingGorge.delStor") %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(x=Year,y=Value)) +
  geom_line()

df$Scenario = df$ScenarioGroup



names(zz_stor)
df <- zz_stor %>% pivot_wider(names_from = "Variable",values_from = "Value")

# df1 <- df %>% dplyr::filter(Scenario == "June 2021 - ST IG") #unique(zz_stor$ScenarioGroup)[1])
df1 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_stor$ScenarioGroup)[1])
df2 <- df %>% dplyr::filter(ScenarioGroup == unique(zz_stor$ScenarioGroup)[2])
df <- left_join(df1,df2,by=c("TraceNumber","Year"))
# View(df) #.x slots are IG, .y slots are noDRO
names(df)
df <- df %>% mutate(FlamingGorge.delStor = FlamingGorge.Storage.x - FlamingGorge.Storage.y,
              BlueMesa.delStor = BlueMesa.Storage.x - BlueMesa.Storage.y,
              Navajo.delStor = Navajo.Storage.x - Navajo.Storage.y)#,
              # Powell.delStor = Powell.Storage.x - Powell.Storage.y) #i didn't process this!

# df <- df %>% select(-ScenarioGroup.x, -Scenario.x, -ScenarioGroup.y, -Scenario.y) # if want to keep 2 sceonarios x & y storages
df <- df %>% select(TraceNumber,Year,FlamingGorge.delStor,BlueMesa.delStor,Navajo.delStor,) #only keep delStor

#gather up to use ggplot 
df<-df %>% pivot_longer(cols=FlamingGorge.delStor:Navajo.delStor,names_to = "Variable",values_to = "Value")

df<- df %>% mutate(Value = -1*Value) #make loss of storage positive 

df %>% group_by(Year) %>%
  filter(Variable == "FlamingGorge.delStor") %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(x=Year,y=Value)) +
  geom_line()

df$Scenario = df$ScenarioGroup
