#agg file specifying which slots

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. Process Results 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# list.files(file.path(scen_dir,scens[2]))
# 
# rdf_slot_names(read_rdf(iFile = file.path(scen_dir,scens[2],"WQANN.rdf")))

rw_agg_file <- "ReachSaltConc.csv" #doesn't include outflow

#read agg file specifying which slots
# # NEW files are annual slots so use AsIs
rwa1 <- rwd_agg(read.csv(file.path(getwd(),"rw_agg", rw_agg_file), stringsAsFactors = FALSE))
# # # Old files from 2017 review are monthly so use EOCY 

#rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
scen_res <- rw_scen_aggregate(
  scens,
  agg = rwa1,
  scen_dir = scen_dir
)

# unique(scen_res$Variable) #check variable names

#add scenario names to line, point and color scales
names(lt_scale) <- unique(scen_res$Scenario)
names(pt_scale) <- unique(scen_res$Scenario)
names(mycolors) <- unique(scen_res$Scenario)

# # Adding factors so ggplot does not alphebetize legend
scen_res$Scenario = factor(scen_res$Scenario, levels=names(scens))
scen_res2$Scenario = factor(scen_res2$Scenario, levels=names(scens))


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. Plot Custom UB Figures 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

variables <- unique(scen_res$Variable)


## create a pdf  
pdf(file.path(oFigs,paste0("RchSaltConc_",Figs,".pdf")), width= width, height= height)

# ++++++++++++++++++++++++++Below Powell+++++++++++++++++++++++++++++++++++++



### Means ###
for (i in 1:length(unique(scen_res$Variable))) {
  
  variable <- variables[i]  
  
  y_lab = "Salt Concentration (mg/l)"
  title = paste("Mean",variable) 
  # ylims <- c(400,600)
  
  
  df <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(startyr <= Year && Year <= endyr) %>% #filter year
    dplyr::group_by(Scenario, Year) %>%
    dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
  
  p1 <- df %>%
    ggplot(aes(x = factor(Year), y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
    
    scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
    geom_line() +
    geom_point() + 
    # ylim(ylims) +
    scale_linetype_manual(values = lt_scale) +
    scale_shape_manual(values = pt_scale) +
    scale_color_manual(values = mycolors) +
    # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
    labs(title = title, y = y_lab, x = "") + # ,subtitle = subtitle)+ #remove model step name from title
    theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))
  # print(p)
  
  # ggsave(filename = file.path(oFigs,paste0(variable,".png")), width= width, height= height)
  
  write.csv(df,file = paste0(oFigs,'/','Stats_',variable,'.csv'))
  
  title = paste("Min",variable) 
  # ylims <- c(400,600)
  
  
   p2 <- df %>%
    ggplot(aes(x = factor(Year), y = MinOut, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
    
    scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
    geom_line() +
    geom_point() + 
    # ylim(ylims) +
    scale_linetype_manual(values = lt_scale) +
    scale_shape_manual(values = pt_scale) +
    scale_color_manual(values = mycolors) +
    # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
    labs(title = title, y = y_lab, x = "") + # ,subtitle = subtitle)+ #remove model step name from title
    theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

  
  title = paste("Max",variable) 
  
  
  p3 <- df %>%
    ggplot(aes(x = factor(Year), y = MaxOut, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
    
    scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
    geom_line() +
    geom_point() + 
    # ylim(ylims) +
    scale_linetype_manual(values = lt_scale) +
    scale_shape_manual(values = pt_scale) +
    scale_color_manual(values = mycolors) +
    # # annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + # make custom axis shading, don't use for now doesn't look good with plotte pallette
    labs(title = title, y = y_lab, x = "") + # ,subtitle = subtitle)+ #remove model step name from title
    theme(axis.text.x = element_text(angle=90,size=8,vjust=0.5))

  grid.arrange(p1,p2,p3,ncol=1)
  
  
}

dev.off()
