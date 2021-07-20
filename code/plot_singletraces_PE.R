
##########################################################################
############################## Process Feather ###########################
##########################################################################

# feather_path <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot/"
feather_path <- "D:/2021/June2021_2021DRO/" #BA D drive
list.files(feather_path)

scen_res <- feather::read_feather(file.path(feather_path,"June.feather")) #ST only 
names(scen_res)
unique(scen_res$Variable) #[1] "mead_pe"       "powell_pe"     "powell_wy_rel" "lf_nf"        
unique(scen_res$Year) #2022-2060


# create a pdf #######
# pdf(file.path(ofigs,paste0("SingleTrace_June2021_",Hydro,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)


mtom <- paste0("Trace", sprintf('%02d', 4:38))
mtom2 <- paste0("Trace", 4:38)   

jun_dnf <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCP", mtom)
jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_dnf_nodo <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO", mtom2)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom2)

dnf_2021dro <- rw_scen_gen_names('Jun2021_2022,DNF,2016Dems,IG_DCP_2021DRO', paste0('Trace', sprintf('%02d', 4:38)))
st_2021dro <- rw_scen_gen_names('Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP_2021DRO', paste0('Trace', sprintf('%02d', 4:38)))
dnf_2021dro_nodo <- rw_scen_gen_names('Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO_2021DRO', paste0('Trace', sprintf('%02d',4:38)))
st_2021dro_nodo <- rw_scen_gen_names('Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO_2021DRO', paste0('Trace', sprintf('%02d',4:38)))


scen_res <- mutate(scen_res, ScenarioGroup = case_when(
  Scenario %in% jun_dnf ~ "DNF - no 2107 DROA delivery - w/future DRO",
  Scenario %in% jun_st ~ "ST - no 2107 DROA delivery - w/future DRO",
  Scenario %in% jun_dnf_nodo ~ "DNF - no 2107 DROA delivery - no future DRO",
  Scenario %in% jun_st_nodo ~ "ST - no 2107 DROA delivery - no future DRO",
  Scenario %in% dnf_2021dro ~ "DNF - 2107 DROA delivery - w/future DRO",
  Scenario %in% st_2021dro ~ "ST - 2107 DROA delivery - w/future DRO",
  Scenario %in% dnf_2021dro_nodo ~ "DNF - 2107 DROA delivery - no future DRO",
  Scenario %in% st_2021dro_nodo ~ "ST - 2107 DROA delivery - no future DRO",
  TRUE ~ "BAD"
))

head(scen_res)
unique(scen_res$ScenarioGroup)
scens_ST <- unique(scen_res$ScenarioGroup)[c(2,4,6,8)]
scens_ST 

scen_res <- scen_res %>%
  filter(ScenarioGroup %in% scens_ST)
unique(scen_res$ScenarioGroup)

#add a CRMMSTraceNumber
scen_res <- cbind.data.frame(scen_res, 
                                CRMMSTraceNumber = stringr::str_remove(stringr::str_split_fixed(scen_res$Scenario, ",", 5)[,5],"Trace"))
#that's a pretty expensive calc if you have a big df so suggest not doing it more than once if you don't have to. 
# or you could do something like select_scens <- paste0("Apr2021_2022.v5.1,DNF,2016Dems,", c("IG_DCP.v5.1", "IG_DCPNoUBDRO"), "Trace", mtom_i) and then just filter to those two scenarios. 
unique(scen_res$CRMMSTraceNumber)

# convert a double digit number, e.g. 04, to single e.g. 4 ----- FIX THIS LATER 
scen_res_x <- mutate(scen_res, CRMMSTraceNumber = case_when(
  # CRMMSTraceNumber %in% sprintf("%02d", 4:9) ~ 4:9,
  CRMMSTraceNumber %in% "04" ~ "4"
))
unique(scen_res$CRMMSTraceNumber)

#get everything on a date
scen_res$Date = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")
unique(scen_res$Date)
# # RW Jan 31 = Jan 1  

# scen_res_x$Timestep <- as.POSIXct(strptime(scen_res_x$Timestep,"%Y-%m-%d %H:%M"))
# scen_res_x$Timestep <- as.Date(scen_res_x$Timestep) - 1 # fix first entry -  first entry is 2019-1-31 24:00 which gets converted to 2019-02-01

##########################################################################
############################## 1 Panel Plot ##############################
##########################################################################

unique(scen_res$Variable) #[1] "mead_pe"       "powell_pe"     "powell_wy_rel" "lf_nf"        
ytitle <- c("Pool Elevation (ft)","Pool Elevation (ft)","WY Release (af)","Lees Ferry Natural Flow (af)")
printfigs_singletrace=T
ofigs <- "M:/felletter/June2021_2021DROA"

# slotNames_plot = unique(scen_res$Variable) 
slotNames_plot = "powell_pe"
CRSStrace = unique(scen_res$TraceNumber)
CRSStrace <- c(31,15) #ST 15 = 2002, doesn't help. #ST 31 = 2018, works well 
CRSStrace <- 25
# unique(scen_res$CRMMSTraceNumber)
CRMMStrace <- c("4","04") # 24 = 2001
plot_yrs <- 2022:2023

i = j = 1 #debug
for (i in 1:length(slotNames_plot)) {
  df_plot = scen_res %>% filter(Variable %in% slotNames_plot[i]) %>%
    # mutate(Variable = factor(Variable, levels = slotNames_plot)) %>%
    dplyr::filter(Year %in% plot_yrs) %>%
    dplyr::filter(TraceNumber %in% CRSStrace) %>%
    dplyr::filter(CRMMSTraceNumber %in% CRMMStrace) # %>%
  # #checks 
  # head(df_plot)
  # unique(df_plot$Variable)
  # unique(df_plot$CRMMSTraceNumber) 
  # unique(df_plot$TraceNumber) 
  # unique(df_plot$Year) 
  
  for (j in 1:length(CRSStrace)) {
    #single CRSS trace per plot 
    df_plot_j = df_plot %>% dplyr::filter(TraceNumber %in% CRSStrace[j])
    unique(df_plot$TraceNumber)
    unique(df_plot$Date)
    
    
    g <- df_plot_j  %>% 
      # ggplot(aes(Year, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
      ggplot(aes(Date, Value, fill = ScenarioGroup,color = ScenarioGroup)) + 
      # bor_style() +
      geom_hline(yintercept=3490, col="red") +
      geom_vline(xintercept = as.Date("2022-10-01"), col="black") +
      
      geom_line() +
      # scale_y_continuous() +
      labs(y = ytitle[i], x = '') +#,title = paste0(slotNames_plot[i],"_CRMMStr",CRMMStrace[1],"_CRSStr",CRSStrace[j])) +
      theme(legend.title = element_blank(),
            legend.position = 'right',
            legend.text = element_text(size = 12), 
            legend.spacing.x  = unit(0.5, units = 'cm'),
            axis.text.x = element_text(angle = 90, hjust = 1),
            # plot.title = element_text(size=rel(1.2)),
            plot.margin=unit(c(1,1.5,3,1),"lines"),
            strip.text = element_text(size = 12)) #+
      # scale_y_continuous(label = comma) + #Error in check_breaks_labels(breaks, labels) : object 'comma' not found
      # guides(fill = guide_legend(nrow=legend_rows,byrow=T)) +
      # facet_wrap(~ Variable, scales = 'free_y',ncol = 1)
    print(g)
    if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(slotNames_plot[i],"_CRMMStr",CRMMStrace[1],"_CRSStr",CRSStrace[j],".png")), width = 11.55,height = 5.73)} #room for a title, maxes out pptx slide width 
  } #end j CRSS Trace loop
} # end i plot df loop






###########################













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
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11.55,height = 5.73)} #leave room for a title but maxes out pptx slide width  

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
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11.55,height = 5.73)} #leave room for a title but maxes out pptx slide width  

dev.off()
dev.off()




#####################################################################################################


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
if(printfigs_singletrace==T){ ggsave(filename = file.path(ofigs,paste0(Hydro,".CRSP.PE.",title,".png")), width = 11.55,height = 5.73)} #leave room for a title but maxes out pptx slide width  

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
