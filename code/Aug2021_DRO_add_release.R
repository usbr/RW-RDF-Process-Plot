# script makes single traces plots given a CRMMS IC trace # and a CRSS trace #
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Set Up ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls()) #clear the enviornment

results_nm <- "Aug2021" #results dir folder 

#load libraries and set directories 
if (TRUE) {
  onBA <- TRUE # which computer BA or my PC? find RW-RDF-Process-Plot dir 
  if (onBA == TRUE) {
    rwprocess_dir <- "C:/Users/fellette/Documents/GIT/RW-RDF-Process-Plot"
  } else {
    rwprocess_dir <- "C:/Users/cfelletter/Documents/RW-RDF-Process-Plot"
  }
  
  #libraries and setup directories 
  source(file.path(rwprocess_dir,"code","libs_n_dirs.R")) 
  # scen_dir = #OVERWRITE if not manoa/Shared/CRSS/2021/Scenario
  
  #set up folders for stats 
  figstats <- file.path(results_dir,"Stats") 
  if (!file.exists(figstats)) {
    message(paste('Creating folder:', figstats))
    dir.create(figstats)
  }
  message('Stats will be saved to: ', figstats)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Get data ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#do I need to aggregate it from scen_dir?
if (FALSE) {

mtom <- paste0("Trace", 4:38)

jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom)
aug_st <- "Aug2021_2022,ISM1988_2019,2016Dems,IG_DCP,CRMMS_Most"
aug_st_nodo <- "Aug2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO,CRMMS_Most"

scens <- c(aug_st_nodo, aug_st, jun_st, jun_st_nodo)
scens <- c(aug_st_nodo, aug_st)


rw_agg_nm <- "rw_agg_DRO_augjun.csv"
feather_file_nm <- "augjun2021_drodata.feather"

rw_agg_nm <- "rw_agg_add_DRO_release.csv"
feather_file_nm <- "add_DRO_release_aug2021.feather"

print(paste("reading from",scen_dir))

rwd <- read_rwd_agg(file.path(rwprocess_dir,"rw_agg",rw_agg_nm)) 
rw_scen_aggregate(scens,agg = rwd, scen_dir = scen_dir,file = file.path(feather_data_dir,feather_file_nm))

#add scenario group 
zz <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 

zz <- zz %>%
  mutate(Scenario = case_when(
    Scenario %in% aug_st ~ "July 2021 w DRO",
    Scenario %in% aug_st_nodo ~ "July 2021 no DRO", 
    TRUE ~ "BAD"))

feather::write_feather(zz,file.path(feather_data_dir,feather_file_nm)) 

}
#or do I already have a feather? 
zz <- feather::read_feather(file.path(feather_data_dir,feather_file_nm)) 
unique(zz$Scenario)
unique(zz$Variable)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Process ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
zz_inout <- zz %>%
  dplyr::filter(Year <= 2026) %>%
  dplyr::filter(Scenario %in% c('July 2021 w DRO')) %>%
  dplyr::group_by(Variable,Scenario,TraceNumber,Year) %>%
  dplyr::summarise(Value = sum(Value)) %>% #inflow and outflow this needs to be sum(Value)
  mutate(Value = Value/1000) #convert to MAF after we convert to AF
head(zz_inout)
unique(zz_inout$Variable)

df <- zz_inout %>% pivot_wider(names_from = "Variable",values_from = "Value")
names(df)
df_all <- df %>% mutate(tot_add_dro_rel = add_dro_rel_bm + add_dro_rel_fg + add_dro_rel_nav)
names(df_all)

write.csv(zz_all,file.path(results_dir,"Stats",paste0("AddDRORelease_alldata_",results_nm,".csv")))

#gather up to use ggplot 
df<-df_all %>% pivot_longer(cols=add_dro_rel_bm:tot_add_dro_rel,names_to = "Variable",values_to = "Value")
View(df)

length(which(df$Value < 0))/length(df$Value) #how many are less than 0
dim(df)

df<-df %>% filter(Value >= 0) # drop < 0 traces 
dim(df)

#better names for plot
df <- df %>%
  mutate(Variable = case_when(
    Variable %in% "add_dro_rel_bm" ~ "Blue Mesa",
    Variable %in% "add_dro_rel_fg" ~ "Flaming Gorge",
    Variable %in% "add_dro_rel_nav" ~ "Navajo",
    Variable %in% "tot_add_dro_rel" ~ "CRSP Total",
    TRUE ~ "BAD"))
unique(df$Variable)

#need for Cloud_plot_woHistv2.R
zz_cloud <- df %>% 
  # compute the 10/50/90 and aggregate by start month
  dplyr::group_by(Scenario, Variable, Year) %>% #don't use scenario here 
  dplyr::summarise('Mean' = mean(Value), 'Med' = median(Value),
                   'Min' = quantile(Value,.1),'Max' = quantile(Value,.9),
                   'MinOut' = min(Value),'MaxOut' = max(Value)) #add in outliers for plot 

write.csv(zz_cloud,file.path(results_dir,"Stats",paste0("AddDRORelease_stats_",results_nm,".csv")))

# #need zz_all for Cloud_plot_woHistv2.R
# zz_all <- zz_cloud 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot ##
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subtitle =   "August CRSS"

pdf(file = file.path(figures_dir,paste0("addDRORelease_",results_nm,".pdf")), width = 11,height = 7) #maxes out pptx slide height 

# #mean
# y_lab = "Flow (kaf/yr)"
# title = "Additional Release for DRO - August 2021 CRSS" 
# zz_cloud %>% group_by(Variable,Year) %>%
#   filter(Variable != "CRSP Total") %>%
#   ggplot(aes(x=Year,y=Mean,color = Variable)) +
#   geom_line() +
#   labs(y = y_lab, x = '',title = paste("Mean",title)) +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(label = comma) #+ 
# # 
# ggsave(filename = file.path(figures_dir,paste0("Mean_","CRSPaddDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

# #lumped 22-26 total boxplot
# df %>% group_by(Variable) %>%
#   filter(Variable == "CRSP Total") %>%
#   ggplot(aes(y=Value,color = Variable,fill=Variable)) +
#   geom_boxplot() +
#   labs(y = y_lab, x = "2022 - 2026",title = paste(title)) +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(label = comma) + 
#   theme(axis.line=element_blank(),axis.text.x=element_blank())#,
# # axis.title.x=element_blank())
# # 
# ggsave(filename = file.path(figures_dir,paste0("Bxplt_total_","addDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

#2226 boxplot total 
y_lab = "[kaf]"
title = "Total UB DRO release"
df %>% group_by(Variable,TraceNumber,Year) %>%
  summarise(Value=sum(Value)) %>%
  filter(Variable == "CRSP Total") %>%
  ggplot(aes(x=as.factor(Year),y=Value,color = Variable,fill=Variable)) +
  geom_boxplot() +
  labs(y = y_lab, x = "Trace",title = paste(title),subtitle = paste(subtitle)) +
  theme(legend.position="none") +
  scale_y_continuous(label = comma) #+ 

ggsave(filename = file.path(figures_dir,paste0("Bxplt_totalbyyr_","addDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

#combined boxplots by reservoir by year
title = "UB DRO release by reservoir"
df %>% group_by(Variable,Year) %>%
  filter(Variable != "CRSP Total") %>%
  ggplot(aes(x=as.factor(Year),y=Value,color = Variable,fill=Variable)) +
  geom_boxplot() +
  labs(y = y_lab, x = "Trace",title = paste(title),subtitle = paste(subtitle)) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) #+ 

ggsave(filename = file.path(figures_dir,paste0("Bxplt_","CRSPaddDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

#seperate boxplots by reservoir by year 
df %>% group_by(Variable,Year) %>%
  filter(Variable != "CRSP Total") %>%
  ggplot(aes(x=as.factor(Year),y=Value,color = Variable,fill=Variable)) +
  geom_boxplot() +
  labs(y = y_lab, x = "Trace",title = paste(title),subtitle = paste(subtitle)) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) + 
  facet_wrap(~ Variable, scales = 'free_y',ncol = 1)

ggsave(filename = file.path(figures_dir,paste0("Bxplt_combined_","CRSPaddDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

#lumped 22-26 boxplots by reservoir
df %>% group_by(Variable,TraceNumber) %>%
  summarise(Value=sum(Value)) %>%  
  filter(Variable != "CRSP Total") %>%
  ggplot(aes(y=Value,color = Variable,fill=Variable)) +
  geom_boxplot() +
  labs(y = y_lab, x = "2022 - 2026",title = paste(title)) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(label = comma) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank())#,
# axis.title.x=element_blank())
# 
ggsave(filename = file.path(figures_dir,paste0("Bxplot_2226_","CRSPaddDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

# #by trace lumped 22-26 boxplots by reservoir
# y_lab = "Total contribution through 2026 [kaf]"
# title = "Total UB DRO release by trace"
# x_breaks <- c("5","10","15","20","25","30")
# x_labs <- c("1992","1997","2002","2007","2012","2017")
# df %>% group_by(Variable,TraceNumber) %>%
#   summarise(Value=sum(Value)) %>%  
#   filter(Variable == "CRSP Total") %>%
#   ggplot(aes(y=Value,x=as.factor(TraceNumber),color = Variable,fill=Variable)) +
#   # geom_boxplot() + #### its wrong to boxplot these I could do stacked bars but I'm not sure how to do that
#   geom_bar() + #how do I get the plot I want? 
#   labs(y = y_lab, x = "Trace",title = paste(title),subtitle = paste(subtitle,"(2022-2026)")) +
#   theme(legend.position = "none") +
#   scale_y_continuous(label = comma) + 
#   scale_x_discrete(breaks=x_breaks,
#                    labels=x_labs) 
# # 
# ggsave(filename = file.path(figures_dir,paste0("bytracetotCRSP_addDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 
# 
# #by trace lumped 22-26 boxplots by reservoir
# y_lab = "Contribution through 2026 [kaf]"
# title = "UB DRO release by trace by reservoir"
# df %>% group_by(Variable,TraceNumber) %>%
#   summarise(Value=sum(Value)) %>%  
#   filter(Variable != "CRSP Total") %>%
#   ggplot(aes(y=Value,x=as.factor(TraceNumber),color = Variable,fill=Variable)) +
#   # geom_boxplot() + #### its wrong to boxplot these I could do stacked bars but I'm not sure how to do that
#   geom_bar() + #how do I get the plot I want? 
#   labs(y = y_lab, x = "Trace",title = paste(title),subtitle = "August CRSS (2022-2026)") +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(label = comma) + 
#   scale_x_discrete(breaks=x_breaks,
#                    labels=x_labs) 
# # 
# ggsave(filename = file.path(figures_dir,paste0("bytracetotbyres_addDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

#histogram  total
y_lab = "Total contribution through 2026 [kaf]"
title = "Total UB DRO release histogram"
df %>% group_by(Variable,TraceNumber) %>%
  summarise(Value=sum(Value)) %>%
  filter(Variable == "CRSP Total") %>%
  # filter(Variable == "FlamingGorge.delFlow") %>%
  ggplot(aes(x = Value,color = Variable,fill=Variable)) +
  geom_histogram(binwidth = 250) +
  labs(y = "Count", x = y_lab,title = paste(title),subtitle = "August CRSS (2022-2026)") +
  theme(legend.position = "none") #+
  # scale_y_continuous(label = comma) #+ #leave out for no decimals 
# 
ggsave(filename = file.path(figures_dir,paste0("Hist_","CRSPaddDRORelease_",results_nm,".png")), width = 11,height = 7) #maxes out pptx slide height 

dev.off()