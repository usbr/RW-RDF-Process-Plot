##############################################################################
#This script creates monthly boxplots of Outflow and PE to compare two MTOM runs
##############################################################################

#agg file specifying which slots

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))

# set up 

# res <- c("Powell","Mead")

slots = c(
  #don't need to look at inflow for Mohave and Havasu because resevoir storage is the same, in = out 
  "Powell.Outflow", 
  "Powell.Outflow Salt Mass", 
  "Powell.Outflow Salt Concentration",
  "Mead.Outflow", 
  "Mead.Outflow Salt Mass", 
  "Mead.Outflow Salt Concentration",
  "Mohave.Outflow", #All Res.rdf #"Mohave.Inflow","Mohave.Storage",      
  "Mohave.Outflow Salt Mass", # "Mohave.Reservoir Salt Mass"#salt.rdf #don't have inflow salt mass
  "Mohave.Outflow Salt Concentration", 
  "Havasu.Outflow", 
  "Havasu.Outflow Salt Mass", 
  "Havasu.Outflow Salt Concentration"
)

# variables <-gsub(" ","",slots) #remove blanks 

rdfs = c(rep(c('AllRes.rdf','Salt.rdf','Salt.rdf'),4))

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rdfs,
  slot = slots, 
  period = rep("asis", length(slots)),
  summary = rep(NA, length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  # variable = variables, #remove blanks 
  stringsAsFactors = FALSE
))


#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))


#get everything on a date 
scen_res$Date = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d")


limit_date = as.Date("2024-12-01")

df <- scen_res %>%
  dplyr::filter(Date <= limit_date) %>% #filter year
  # dplyr::filter(TraceNumber == 1) %>%
  dplyr::group_by(Scenario, Date,Variable) %>%
  dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Plot 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# if (T) {

pdf(file.path(oFigs,paste0("Monthly_LBOut_",Figs,".pdf")), width= width, height= height)

# i=1  
for (i in 1:length(slots)) {
  variable = slots[i]
  
  p<-df %>%
    dplyr::filter(Variable == variable) %>%
    ggplot(aes(x = Date, y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + 
    theme_light() +
    scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
    geom_line() +
    geom_point() + 
    # ylim(ylims) +
    scale_linetype_manual(values = lt_scale) +
    scale_shape_manual(values = pt_scale) +
    scale_color_manual(values = mycolors) +  # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette  labs(title = title, y = y_lab, x = "")+ #remove model step name from title
    labs(title = variable)+ #, y = y_lab, x = "")+ #remove model step name from title
    theme(text = element_text(size=8),axis.text.x = element_text(angle=90,size=8,vjust=0.5))
  
  print(p)
}



dev.off()


# }





