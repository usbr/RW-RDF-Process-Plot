##############################################################################
#This script creates SaltMassBal figures 
##############################################################################

# # Disable Scientifc Notation 
# options(scipen=999)

if (file.exists(oFigs)){
  file_dir <- oFigs ### old TriRvw code relies on oFigs, replace by file_dir
} else if (file.exists(results_dir)){
  file_dir <- file.path(results_dir,scens[1])
} else {
  stop('need either oFigs or results_dir defined')
}

#### File Checks #####
if (!file.exists(file_dir)) {
  message(paste('Creating folder:', file_dir,'aka file_dir'))
  dir.create(file_dir)
  stop('if created folder need to move results rdfs into this dir and then proceed with code') #if created folder need to move results rdfs into this dir and then proceed with code
}

fig_dir <-  file.path(file_dir,"png_figures")
data_dir <-  file.path(file_dir,"csv_data")

if (!file.exists(fig_dir) | !file.exists(data_dir)) {
  dir.create(fig_dir)
  dir.create(data_dir)
}


#agg file specifying which slots
rw_agg_file <- "SaltMassGains_rwagg_annual.csv"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[1],"SaltMassBalance.rdf")))

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE)) 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names 

## Divide Values by KAF
scen_res$Value=(scen_res$Value)/1000
y_lab = "Salt Mass (thousand tons/yr)"

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

#you don't get a scenario name assigned if you just have one, need it later for plotting 
if(length(scens) == 1){
  scen_res$Scenario = rep(scens,times=length(scen_res$Value))
} else {
  # # Adding factors so ggplot does not alphebetize legend
  scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
  #this comes back as <NA> when only have 1 scen
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

vars <- unique(scen_res$Variable)

## create a pdf  
pdf(file.path(file_dir,paste0("Gain_SaltMass_",Figs,".pdf")), width=9, height=6)


for (i in 1:length(vars)) {
  variable = vars[i]
  title = variable
  # ylims <- c(NA,NA) #c(-1,1)
  
  
  df_ub <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value),                    
                     'Min' = quantile(Value,.1),'Max' = quantile(Value,.9)) 
  p <- df_ub %>%
    ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
    geom_line() +
    geom_point() +
    # ylim(ylims) +
    scale_linetype_manual(values = lt_scale) +   scale_shape_manual(values = pt_scale) +   scale_color_manual(values = mycolors) +
    labs(title = paste(title,Model.Step.Name) , y = y_lab, x = "Year")+
    geom_line(aes(x=factor(Year), y=MinOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1) +  #bound with min max
    geom_line(aes(x=factor(Year), y=MaxOut, color=Scenario, group=Scenario),linetype = "dotted",size = 1)  +
    theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
  
  print(p)
  
  # ggsave(filename = file.path(fig_dir,paste0(variable,".png")), width= width, height= height)
  
  write.csv(df_ub,file = paste0(data_dir,'/','Stats_',variable,'.csv'))
}


#-------------------------------------------------------------------------------------


dev.off()

