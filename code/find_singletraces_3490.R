#explore_powell_power.R has a few different sections, but those "vulnerability" figures are included pretty early in that file
#get_data.R creates data frames that are used in explore_powell_power.R

library(feather)
library(tidyverse)
library(RWDataPlyr)
library(patchwork)
library(crssplot)

feather_path <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/"
feather_path <- "D:/2021/June2021_2021DRO/" #BA D drive
list.files(feather_path)
# crsp_ops_data <- feather::read_feather(file.path(feather_path,"crsp_ops_data.feather")) #both hydro
crsp_ops_data <- feather::read_feather(file.path(feather_path,"crsp_ops_data_ST.feather")) #ST only 
MdPwll_PE_data <- read_feather(file.path(feather_path,"MeadPowellPE.feather"))
MdPwll_PE_data_offc <- read_feather(file.path(feather_path,"MeadPowellPE_offc.feather"))

# sys_data <- read_feather(file.path(feather_path,"SysCond.feather"))
# xtra <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/jun_extra_powell.feather")
names(MdPwll_PE_data)
names(MdPwll_PE_data_offc)
names(MdPwll_PE_data_offc) <- c(names(MdPwll_PE_data_offc)[1:6],"ScenarioGroup")

unique(MdPwll_PE_data$Variable)
unique(MdPwll_PE_data$ScenarioGroup)
unique(MdPwll_PE_data_offc$ScenarioGroup)

zz <- rbind.data.frame(MdPwll_PE_data,MdPwll_PE_data_offc)
unique(zz$ScenarioGroup)

zz <- zz %>%
  mutate(ScenarioGroup = case_when(
    ScenarioGroup %in% "ST; with future DRO" ~ "ST - no 2107 DROA delivery - future DRO",
    ScenarioGroup %in% "Jun 2021 - ST IG noUBDRO" ~ "ST - no 2107 DROA delivery - no future DRO",
    ScenarioGroup %in% "ST; 2021 DRO + no future DRO" ~ "ST - 2107 DROA delivery - no future DRO", 
    ScenarioGroup %in% "ST; 2021 DRO + future DRO" ~ "ST - 2107 DROA delivery - future DRO",
    TRUE ~ "BAD"
  ))
unique(zz$ScenarioGroup)

zz <- zz %>%
  filter(ScenarioGroup != "BAD")
unique(zz$ScenarioGroup)
scen_nms <- unique(zz$ScenarioGroup)


# zz <- zz %>% #### DOESN'T WORK FOR SOME REASON
#   mutate(Value = case_when(
#     Value %in% "NA" ~ -999
#   ))
# unique(zz$Value)

### show % risk by year acrossed scenarios
zz %>%
  group_by(Year, ScenarioGroup, Variable) %>%
  filter(
    Variable == "powell_wy_min_lt_3490", Year <= 2026, 
    !(ScenarioGroup %in% scen_nms)) %>%
  summarise(Value = mean(Value) * 100) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") %>%
  as.data.frame()

xx <- zz %>%
  filter( Variable %in% "powell_wy_min_lt_3490", Year == 2023) #%>%
unique(xx$Variable)
unique(xx$Year)
unique(xx$Value)
xx <- xx[order(xx$TraceNumber),]

xx1 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[1]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx2 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[2]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx3 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[3]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx4 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[4]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

names(xx1)
xx <- cbind.data.frame(xx1,xx2$`ST - 2107 DROA delivery - future DRO`,xx3$`ST - 2107 DROA delivery - no future DRO`,xx4$`ST - no 2107 DROA delivery - no future DRO`)
rm(xx1,xx2,xx3,xx4)

names(xx)
names(xx) <- c(names(xx)[1:5],scen_nms)
names(xx)

View(xx)

n_traces <- dim(xx)[1]
n_traces/32 #should equal 35 traces for the CRMMS ICs

#First comparison - One where 2021 DROA helps and one where it doesn't help - 
#"no 2021 DROA; no future DROA" < 3490 in WY22 vs. "2021 DRO; no future DROA" > 3490 in WY22 for same hydrology trace
# which(xx$`ST - 2107 DROA delivery - no future DRO` == 0 && xx$`ST - no 2107 DROA delivery - no future DRO` == 1)
case1 <- which(xx$`ST - 2107 DROA delivery - no future DRO` == 0 )
# case1
case2 <- which(xx$`ST - no 2107 DROA delivery - no future DRO` == 1)
# case2
helps <- which(case1 %in% case2)
helps <- case1[helps]
helps
length(which(case1 %in% case2)) #48 

# Second comparison - "no 2021 DROA; no future DROA" < 3490 in WY22 vs. "2021 DRO; no future DROA" < 3490 in WY22 for same hydrology trace
# One where 2021 DROA doesn't help
# which(xx$`ST - 2107 DROA delivery - no future DRO` == 1 && xx$`ST - no 2107 DROA delivery - no future DRO` == 1)
case1 <- which(xx$`ST - 2107 DROA delivery - no future DRO` == 0 )
case1
case2 <- which(xx$`ST - no 2107 DROA delivery - no future DRO` == 0)
case2
nohelp <- which(case1 %in% case2)
nohelp <- case1[nohelp]
length(which(case1 %in% case2)) #1353 

helps
nohelp

xx[7,] #helps
xx[8,] #nohelp


#### Single Trace Plots 
unique(crsp_ops_data$Scenario)

df <- feather::read_feather(file.path(feather_path,"June.feather")) #ST only 
unique(df$Variable)


zz <- zz %>%
  filter( Variable %in% "powell_wy_min_lt_3490", Year == 2023) #%>%
scen_res_ST$Scenario

scen_res_ST <- cbind.data.frame(scen_res_ST, 
                                CRMMSTraceNumber = stringr::str_remove(stringr::str_split_fixed(scen_res_ST$Scenario, ",", 5)[,5],"Trace"))


unique(df$TraceNumber)
unique(df$Scenario)




unique(df$)
xx <- xx[order(xx$TraceNumber),]

xx1 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[1]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx2 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[2]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx3 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[3]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

xx4 <- xx %>%
  filter( ScenarioGroup %in% scen_nms[4]) %>%
  pivot_wider(names_from = "ScenarioGroup", values_from = "Value") 

names(xx1)
xx <- cbind.data.frame(xx1,xx2$`ST - 2107 DROA delivery - future DRO`,xx3$`ST - 2107 DROA delivery - no future DRO`,xx4$`ST - no 2107 DROA delivery - no future DRO`)
rm(xx1,xx2,xx3,xx4)

# create a pdf #######
# pdf(file.path(ofigs,paste0("SingleTrace_June2021_",Hydro,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

### 4 Panel Plots ###
# slotNames_plot = c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow',)
slotNames_plot = c('FlamingGorge.PE', 'BlueMesa.PE','Navajo.PE','Powell.PE')
CRSStrace <- 15 #ST 15 = 2002, worst case! #ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 24 # 24 = 2001
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
title
# ytitle <- "EOCY Water Surface Elevation (ft)"
ytitle <- "Water Surface Elevation (ft)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%
#if I wanted EOCY I'd do below 
# dplyr::filter(MonthNum %in% exc_month) %>%
# dplyr::group_by(Variable,ScenarioGroup, Year) %>%
# dplyr::summarise(Value = mean(Value)) #inflow and outflow this needs to be sum(Value)
g <- df_plot  %>% 
  # ggplot(aes(Year, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  # geom_hline(yintercept=0, col="grey20") +
  geom_line() +
  # scale_y_continuous() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

slotNames_plot = c('FlamingGorge.PE', 'BlueMesa.PE','Navajo.PE','Powell.PE')
CRSStrace <- 98#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 26 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
title
ytitle <- "Water Surface Elevation (ft)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  geom_line() +
  # scale_x_continuous(minor_breaks = 1990:3000, breaks = 1990:3000,
  #                    labels = 1990:3000, expand = c(0,0)) +  
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

dev.off()
dev.off()



### 3 Storage + Powell PE Plots ###
# slotNames_plot = c('FlamingGorge.Outflow', 'BlueMesa.Outflow','Navajo.Outflow',)
slotNames_plot = c('FlamingGorge.Storage', 'BlueMesa.Storage','Navajo.Storage','Powell.PE')
CRSStrace <- 14 #ST 15 = 2002, worst case! #ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 23 # 24 = 2001
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 
ytitle <- "Storage (kaf)"

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%
g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

CRSStrace <- 15#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 24 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

CRSStrace <- 26#ST 14 = 2001, DNF 96 = 2001
CRMMStrace <- 17 # 25 = 2003
title = paste0("CRMMS trace ",CRMMStrace," (",CRMMStrace+1977,'), CRSS trace ',CRSStrace," (",CRSStrace+1987,")") 

df_plot = scen_res %>% filter(Variable %in% slotNames_plot) %>%
  mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
  dplyr::filter(TraceNumber %in% CRSStrace) %>%
  dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%

g <- df_plot  %>% 
  ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
  # bor_style() +
  geom_line() +
  labs(y = ytitle, x = '',title = title) +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 12), 
        legend.spacing.x  = unit(0.5, units = 'cm'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(size=rel(1.2)),
        plot.margin=unit(c(1,1.5,3,1),"lines"),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(label = comma) + # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
print(g)
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11,height = 7)} #maxes out pptx slide height 

dev.off()
dev.off()






View(whichblw)
write.csv(whichblw,"powell_wy_min_lt_3490.csv")
getwd()




# ### hurts 
# case1 <- which(xx$`ST - 2107 DROA delivery - no future DRO` == 1 )
# case2 <- which(xx$`ST - no 2107 DROA delivery - no future DRO` == 0)
# hurts <- which(case1 %in% case2)
# length(which(case1 %in% case2)) #8 
# hurts #[1]   52   92  257  558  572  708  923 1155
