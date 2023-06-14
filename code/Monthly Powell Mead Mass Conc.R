


variables = c("Powell.Reservoir Salt Mass","Mead.Reservoir Salt Mass",
              "Powell.Inflow Salt Mass","Mead.Inflow Salt Mass",
              "Powell.Outflow Salt Mass","Mead.Outflow Salt Mass",
             "Powell.Reservoir Salt Concentration","Mead.Reservoir Salt Concentration")

  rdf <- "Salt.rdf"
  noslots<-length(variables)
  rwa1 <- rwd_agg(data.frame(
    file = c(rep(rdf, noslots)),
    slot = c("Powell.Reservoir Salt Mass","Mead.Reservoir Salt Mass",
             "Powell.Reservoir Salt Concentration","Mead.Reservoir Salt Concentration"
    ),
    period = rep("asis", noslots), #c("cy", "eocy", "djf", "July", "wy", "asis"),
    summary = rep(NA, noslots),#c("min", NA, "sum", NA, "sum", NA),
    eval = rep(NA, noslots),#c("<=", rep(NA, 5)),
    t_s = rep(NA, noslots),#c(1060, NA, 0.001, NA, 0.000001, NA),
    variable = variables,
    stringsAsFactors = FALSE
  ))

  #rw_scen_aggregate() will aggregate and summarize multiple scenarios, essentially calling rdf_aggregate() for each scenario. Similar to rdf_aggregate() it relies on a user specified rwd_agg object to know how to summarize and process the scenarios.
  scen_res <- rw_scen_aggregate(
    scens,
    agg = rwa1,
    scen_dir = scen_dir,
  )
  
  #get everything on a date 
  scen_res$Date = as.Date(paste0(scen_res$Year,scen_res$Month,"01"), format = "%Y%B%d") #this takes so long! 

for (i in 1:length(variables)) {
  variable = variables[i] # "16_flow_grut" "19_flow_bluf"
  y_lab = variable
  title = variable
  df <- scen_res %>%
    dplyr::filter(Variable == variable) %>%
    dplyr::filter(Year <= 2030) %>% #filter year
    dplyr::group_by(Scenario, Date) %>%
    dplyr::summarise('Mean' = mean(Value),'Med' = median(Value),'MinOut' = min(Value),'MaxOut' = max(Value)) 
  
  pf <- df %>%
    ggplot(aes(x = Date, y = Mean, color = Scenario, group = Scenario, linetype = Scenario, shape = Scenario)) + theme_light() +
    scale_shape_identity() + #tells it to use the numeric codes directly for point shapes
    geom_line() +
    geom_point() + 
    scale_linetype_manual(values = lt_scale) +
    scale_shape_manual(values = pt_scale) +
    scale_color_manual(values = mycolors) +
    labs(title = title, y = y_lab, x = "")+ #remove model step name from title
    theme(text = element_text(size=8),axis.text.x = element_text(angle=90,size=8,vjust=0.5))
  print(pf)
}  
  
  