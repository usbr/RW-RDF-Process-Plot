##############################################################################
##############################################################################

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process UB 3 Gage
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Model.Step.Name <- Figs <- "Jan2022_vs_TriRvwOffset2023" #plot title and results/folder name #[plot type] identifying name .pdf
customcolorltpt <- F

scens <- list(
  "2020TriRvw_Scen3" = "2007Dems,MTOM_Most,DNF with Salinity 19312018,Jan2020_RW8,IG_DCP_4.4.0_RW8,WQIP_Scenario3_2020_20200409",
  "Jan2022_2020Scen3" = "CRMMS_Most,DNF with Salinity 19312018,CRSS.V5.3.0.203.Jan2022.2023TriRvw.0,CRSS.Baseline.2027IGDCPnoUBDRO.v5.3.0"
)

# check folders
if(!file.exists(file.path(scen_dir, scens[1])) 
   | !file.exists(file.path(scen_dir, scens[2])))
  stop('Scenarios folder(s) do not exist or scen_dir is set up incorrectly. 
       Please ensure Scenarios is set correctly.')

oFigs <- file.path(getwd(),'results') 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', oFigs))
  dir.create(oFigs)
}

oFigs <- file.path(oFigs,Model.Step.Name) 
if (!file.exists(oFigs)) {
  message(paste('Creating folder:', Model.Step.Name))
  dir.create(oFigs)
}

message('Figures and tables will be saved to: ', oFigs)

if(customcolorltpt == F && length(scens) == 2){ #1 color, first scen dashed, second solid 
  lt_scale <- rep(c(2, 1), 1)
  pt_scale <- rep(c(1, 19), 1)
  mycolors <- c("#ba9e66","#407ec9") #reclamation sand (old/baseline rslts), rec blue )
  
} else if (customcolorltpt == F && length(scens) <= 4){ #4 colors, solid
  lt_scale <- rep(1, 4)
  pt_scale <- rep(19, 4)
  # mycolors <- c("#407ec9" , "#6b8f00", "#9a3324" , "#FECB00") #Reclamation blue, green, red, yellow
  # mycolors <- c("#9a3324" , "#FECB00", "#6b8f00" , "#407ec9") #TRY 1 red, yellow, red (stop light), blue
  mycolors <- c("#D55E00" , "#F0E442", "#009E73" , "#407ec9") #TRY 2 - color blind red, yellow, red (stop light), blue
  
} else if (customcolorltpt == F && length(scens) > 4) {
  stop("customcolorltpt not setup or too many Scens")
} 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Powell/Mead 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rw_agg_file <- "PowellMeadInOut.csv" 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

if(names(scens[1])=="2020TriRvw_Scen3" && 2020 %in% unique(scen_res$Year)){
  #Shift TriRvw forward 3 years
  trirvwrows <- which(scen_res$Scenario == names(scens[1]))
  scen_res$Year[trirvwrows] = scen_res$Year[trirvwrows] + 3
  unique(scen_res$Year)
}

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))

rw_agg_file <- "UB3GageFlowSaltMass.csv" 
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
UB_res_raw <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

if(names(scens[1])=="2020TriRvw_Scen3" && 2020 %in% unique(scen_res$Year)){
  #Shift TriRvw forward 3 years
  trirvwrows <- which(UB_res_raw$Scenario == names(scens[1]))
  UB_res_raw$Year[trirvwrows] = UB_res_raw$Year[trirvwrows] + 3
  unique(UB_res_raw$Year)
}


UB_res$Scenario = factor(UB_res$Scenario, levels=names(scens))

yrs <- startyr:endyr #simplify 

#ylims_pow_inoutflow <- c(5,13)
#ylims_pow_inoutmass <- c(3,9)
#ylims_mead_inoutflow <- c(7,11)
#ylims_mead_inoutmass <- c(7,9)

######
tracenm = 64 #64 
######

# unique(scen_res$Variable)
divide_1M <- unique(scen_res$Variable)[1:12]
zz_1M <- scen_res %>%
  dplyr::filter(Variable %in% divide_1M) %>%
  dplyr::filter(Year %in% yrs) %>%
  dplyr::filter(TraceNumber == tracenm) %>%
  dplyr::mutate(Value = Value/1000000) 

zz_conc <- scen_res %>%
  dplyr::filter(!(Variable %in% divide_1M)) %>%
  dplyr::filter(Year %in% yrs) %>%
  dplyr::filter(TraceNumber == tracenm) 

# unique(zz_conc$Year)

UB_res <- UB_res_raw %>%
  dplyr::filter(Year %in% yrs) %>%
  dplyr::filter(TraceNumber == tracenm) 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dev.off()

if(T){

## create a pdf  
pdf(file.path(oFigs,paste0("SingleTrace",tracenm,"_Salt_",Figs,".pdf")), width= width, height= height)


variable = "TotVal.Powell"
y_lab = "Inflow (MAF/yr)"
title = "Total Inflow Above Powell" 
title = paste("Trace",tracenm,title)

p1 <- UB_res %>%
  dplyr::filter(Variable == variable) %>%
  dplyr::mutate(Value = Value/1000000) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
p1

variable = "8_conc_cisco"
y_lab = "Salt Concentration (mg/l)"
title = paste(variable,"Trace",tracenm)

p1 <- UB_res %>%
  dplyr::filter(Variable == variable) #%>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))


variable = "16_conc_grut"
y_lab = "Salt Concentration (mg/l)"
title = paste(variable,"Trace",tracenm)

p2 <- UB_res %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "19_conc_bluf"
y_lab = "Salt Concentration (mg/l)"
title = paste(variable,"Trace",tracenm)

p3 <- UB_res %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

grid.arrange(p1,p2,p3,ncol=1)



#-------------------------------------------------------------------------------------
# Powell Inflow 
#-------------------------------------------------------------------------------------

variable = "Powell.Inflow"
y_lab = "Inflow (MAF/yr)"
title = "Lake Powell Annual Inflow" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=3.9)

p1 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p1 = p1 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 


variable = "Powell.Storage"
y_lab = "Storage (MAF)"
title = "Lake Powell EOCY Storage" 
title = paste("Trace",tracenm,title)

p2 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Powell.Outflow"
y_lab = "Outflow (MAF/yr)"
title = "Lake PowellAnnual Outflow" 
title = paste("Trace",tracenm,title)
p3 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

grid.arrange(p1,p2,p3,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Powell_Inflow_3Panel.png")), width= width, height= height)

variable = "Powell.Inflow Salt Mass"
y_lab = "Inflow Salt Mass (MTons/yr)"
title = "Lake Powell Annual Inflow Salt Mass" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=2.6)

p1 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p1 = p1 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

variable = "Powell.Reservoir Salt Mass"
y_lab = "Reservoir Salt Mass (MTons)"
title = "Lake Powell Reservoir Salt Mass" 
title = paste("Trace",tracenm,title)

p2 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Powell.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (MTons/yr)"
title = "Lake Powell Annual Outflow Salt Mass" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=4.7)

p3 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p3 = p3 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

grid.arrange(p1,p2,p3,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Powell_Inflow_3Panel.png")), width= width, height= height)

variable = "AnnlSlnty_In_Powell_FWAAC"
y_lab = "Inflow Salt Concentration (mg/l)"
title = "Lake Powell Annual Inflow Salt Concentration" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=342)

p1 <- zz_conc %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  # #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p1 = p1 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

variable = "AnnlSlntyLsFrry_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Lees Ferry Average Annual Salt Concentration" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=405)

p2 <- zz_conc %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  # #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p2 = p2 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

grid.arrange(p1,p2,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Powell_Inflow_3Panel.png")), width= width, height= height)

################################################################################
#############################  Mead  ##########################################
################################################################################


variable = "Mead.Inflow"
y_lab = "Inflow (MAF/yr)"
title = "Lake Mead Annual Inflow" 
title = paste("Trace",tracenm,title)

p1 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Mead.Storage"
y_lab = "Storage (MAF)"
title = "Lake Mead EOCY Storage" 
title = paste("Trace",tracenm,title)

p2 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Mead.Outflow"
y_lab = "Outflow (MAF/yr)"
title = "Lake MeadAnnual Outflow" 
title = paste("Trace",tracenm,title)
p3 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

grid.arrange(p1,p2,p3,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Mead_Inflow_3Panel.png")), width= width, height= height)

variable = "Mead.Inflow Salt Mass"
y_lab = "Inflow Salt Mass (MTons/yr)"
title = "Lake Mead Annual Inflow Salt Mass" 
title = paste("Trace",tracenm,title)

p1 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Mead.Reservoir Salt Mass"
y_lab = "Reservoir Salt Mass (MTons)"
title = "Lake Mead Reservoir Salt Mass" 
title = paste("Trace",tracenm,title)

p2 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

variable = "Mead.Outflow Salt Mass"
y_lab = "Outflow Salt Mass (MTons/yr)"
title = "Lake Mead Annual Outflow Salt Mass" 
title = paste("Trace",tracenm,title)

p3 <- zz_1M %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

grid.arrange(p1,p2,p3,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Mead_Inflow_3Panel.png")), width= width, height= height)

variable = "AnnlSlnty_In_Hvr_FWAAC"
y_lab = "Inflow Salt Concentration (mg/l)"
title = "Lake Mead Annual Inflow Salt Concentration" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=471)

p1 <- zz_conc %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  # #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p1 = p1 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

variable = "AnnlSlntyMead_FWAAC"
y_lab = "Salt Concentration (mg/l)"
title = "Below Hoover Average Annual Salt Concentration" 
title = paste("Trace",tracenm,title)
HistMin <- data.frame(yintercept=542)

p2 <- zz_conc %>%
  dplyr::filter(Variable == variable) %>%
  group_by(Scenario) %>%
  ggplot(aes(x = factor(Year), y = Value, group = Scenario, color = Scenario)) + #, group = Scenario, linetype = Scenario, shape = Scenario)) +  
  theme_light() +
  geom_line() +
  # #ylim(#ylims_pow_inoutflow) +
  scale_linetype_manual(values = lt_scale) +
  scale_shape_manual(values = pt_scale) +
  scale_color_manual(values = mycolors) +
  labs(title = title, y = y_lab, x = "Year")+ #remove model step name from title
  theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

p2 = p2 + geom_hline(aes(yintercept=yintercept), data=HistMin, color = "red", lty = 3) 

grid.arrange(p1,p2,ncol=1)

# ggsave(filename = file.path(oFigs,paste0("Mead_Inflow_3Panel.png")), width= width, height= height)


dev.off()

}
