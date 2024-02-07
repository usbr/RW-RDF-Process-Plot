rm(list=ls()) #clear the enviornment 

library(RWDataPlyr)
library(tidyverse)
library(patchwork)
library(scales)

CRSSDIR <- Sys.getenv("CRSS_DIR")

# scen_dir <- file.path(CRSSDIR,"Scenario")
scen_dir <- "//manoa.colorado.edu/BOR/Shared/CRSS/2023/WYdataRequest"
# scen_dir <- "M:/Shared/CRSS/2021/Scenario_dev/"

results_dir <- file.path(CRSSDIR,"results") 

scens <-  c(
  # "CMIP3_IG" = "Mar2023_2024,CMIP3,2016Dems,IG_DCPnoUBDRO,CRMMS_Most",
  "CMIP3_NA" = "Mar2023_2024,CMIP3,2016Dems,NA_noUBDRO,CRMMS_Most",
  # "DNF_IG" ="Mar2023_2024,DNF,2016Dems,IG_DCPnoUBDRO,CRMMS_Most",
  "DNF_NA" = "Mar2023_2024,DNF,2016Dems,NA_noUBDRO,CRMMS_Most",
  # "1931_2020_IG" = "Mar2023_2024,ISM1931_2020,2016Dems,IG_DCPnoUBDRO,CRMMS_Most",
  "1931_2020_NA" = "Mar2023_2024,ISM1931_2020,2016Dems,NA_noUBDRO,CRMMS_Most",
  # "1988_2020_IG" = "Mar2023_2024,ISM1988_2020,2016Dems,IG_DCPnoUBDRO,CRMMS_Most",
  "1988_2020_NA" = "Mar2023_2024,ISM1988_2020,2016Dems,NA_noUBDRO,CRMMS_Most",
  # "2000_2020_IG" ="Mar2023_2024,ISM2000_2020,2016Dems,IG_DCPnoUBDRO,CRMMS_Most"#,
  "2000_2020_NA" = "Mar2023_2024,ISM2000_2020,2016Dems,NA_noUBDRO,CRMMS_Most"#,
)

# library(scales)
mainScenGroup <- "IG_allHydro"
mainScenGroup <- "NA_allHydro"
mainScenGroup <- "NA_allHydro_10"
mycolors <- hue_pal()(5)

# mainScenGroup <- "CIMP3_IG_NA"
# mainScenGroup <- "ST_IG_NA"
# mycolors <- hue_pal()(2)

# mainScenGroup <- "all"
# mycolors <- hue_pal()(10)

ofigs <- file.path(results_dir,mainScenGroup) 
if (!file.exists(ofigs)) {
  message(paste('Creating folder:', ofigs))
  dir.create(ofigs)
}

message('Figures will be saved to: ', ofigs)

# list.files(scen_dir)

# mycolors <- c("#407ec9" , "#6b8f00", "#9a3324" , "#FECB00") #Reclamation blue, green, red, yellow
#output image parameters 
width <- widths <- 9 #inches
height <- heights <- 6

endyr <- 2060
startyr <-2024

if (!any(list.files(ofigs) == "lf_dnf.RDS")) {

  rwa <- read_rwd_agg("rw_agg/lf_flow_rwa.csv")
  
  dnf <- rw_scen_aggregate(
    scens,
    agg = rwa, 
    scen_dir = scen_dir,
    file = "data/lf_dnf.feather" #Optionally save the tbl_df of aggregated scenario data as a .txt, .csv, or .feather file. If file is specified, then the data are saved in the specified output format.
    #### should really be saving feathers for each of my scens rather than RDS
  )
  
  # st <- rw_scen_aggregate(
  #   st_scens,
  #   agg = rwa,
  #   scen_dir = scen_dir,
  #   file = "data/lf_st.feather"
  # )
} else {
  dnf <- readRDS(file.path(ofigs,paste0("lf_dnf.RDS")))
  # dnf <- feather::read_feather("data/lf_st.feather")
}

saveRDS(dnf,file=file.path(ofigs,paste0("lf_dnf.RDS"))) #prevent neeed to reprocess

zz <- dnf 

if(F){
  #filter out scenarios from a larger set of all scenarios which you have a RDS of 
  zz <- readRDS(file.path(results_dir,"all",paste0("lf_dnf.RDS")))
  zz <- zz %>% #filter out scens you don't want to keep for plots
    dplyr::filter(Scenario %in% names(scens))
  unique(zz$Scenario)
}

# dnf$hydrology <- "Full Hydrology"
# st$hydrology <- "Stress Test Hydrology"

# zz <- bind_rows(dnf, st)

zz <- zz %>% mutate(Scenario = factor(Scenario, 
                                levels = names(scens))) 

# Plot ---------------------
cap_text <- NULL
var_name <- c(
  "lf_10yr_lt75" = "Risk of 10-year Volume at Lee Ferry < 75 maf", 
  "lf_10yr_lt825" = "Risk of 10-year Volume at Lee Ferry < 82.5 maf"
)

gg <- list()

# colors <- mycolors

tomaf <- function(x) {
  x/1000000
}

zz <- zz %>%
  filter(Year <= endyr) #last year is bad
  
# lf_10yr is raw data 
# lf_10yr_lt75 is lf_10yr <= 75000000  
# lf_10yr_lt825 is  lf_10yr <= 82500000 

pdf(file.path(ofigs,paste0("LF10yr_",mainScenGroup,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

# for (s in keepscens) {
#   
#   tmp <- filter(zz, hydrology == s)
  # tmp_colors <- mycolors #colors[[s]]
  
  # plot 75 maf over 10

Variable = "lf_10yr_lt75"
  d1 <- zz %>%
    # filter(Variable != "lf_10yr") %>%
    filter(Variable == "lf_10yr_lt75") %>%
    mutate(Variable = var_name[Variable]) %>%
    # mutate(Value = 1 - Value) %>%
      
    group_by(Year, Scenario, Variable) %>%
    #average by year - variable over each of the traces 
    summarise(Value = mean(Value)) #%>%
  
  export <- pivot_wider(d1,names_from=Scenario,values_from=Value)
  write.csv(file = file.path(ofigs,paste0("Risk10yrblw75.csv")),x = export)
  
    p1 <- d1 %>% 
    ungroup() %>%
    ggplot(aes(Year, Value, color = Scenario)) %+%
    geom_line(size = 1) %+%
    facet_wrap(~Variable, nrow = 2) %+%
    labs(
      x = NULL,
      y = NULL,
      color = "Scenario:"#,
      # caption = s
    ) %+%
    coord_cartesian(ylim = c(0, 1)) %+%
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) %+%
    scale_x_continuous(breaks = seq(2020, 2060, 5), minor_breaks = 2020:2060) +
    theme(legend.position = "bottom") + 
    scale_color_manual(values = mycolors)
  
  p1
  ggsave(filename = file.path(ofigs,paste0("Risk10yrblw75.png")), width = widths[1],height = heights[1])#width= width, height= height)
  
  # plot 825 maf over 10

    Variable = "lf_10yr_lt825"
    d2 <- zz %>%
      # filter(Variable != "lf_10yr") %>%
      filter(Variable == "lf_10yr_lt825") %>%
      mutate(Variable = var_name[Variable]) %>%
      # mutate(Value = 1 - Value) %>%
      
      group_by(Year, Scenario, Variable) %>%
      #average by year - variable over each of the traces 
      summarise(Value = mean(Value)) #%>%
    
    export <- pivot_wider(d2,names_from=Scenario,values_from=Value)
    write.csv(file = file.path(ofigs,paste0("Risk10yrblw825.csv")),x = export)
    
    p2 <- d2 %>% 
      ungroup() %>%
      ggplot(aes(Year, Value, color = Scenario)) %+%
      geom_line(size = 1) %+%
      facet_wrap(~Variable, nrow = 2) %+%
      labs(
        x = NULL,
        y = NULL,
        color = "Scenario:"#,
        # caption = s
      ) %+%
      coord_cartesian(ylim = c(0, 1)) %+%
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .2)) %+%
      scale_x_continuous(breaks = seq(2020, 2060, 5), minor_breaks = 2020:2060) +
      theme(legend.position = "bottom") + 
      scale_color_manual(values = mycolors)
    
    p2
    ggsave(filename = file.path(ofigs,paste0("Risk10yrblw825.png")), width = widths[1],height = heights[1])#width= width, height= height)
  
  
  # gg[[paste(s, "p2")]] <- p2
  
  # cdf of 10-year flow
  p3 <- zz %>%
    filter(Variable == "lf_10yr") %>%
    mutate(Value = Value/1000000) %>% #convert to MAF after we convert to AF  
    ggplot(aes(Value, color = Scenario)) + 
    stat_ecdf(size = 1) +
    geom_vline(xintercept = c(75, 82.5), linetype = 2) +
    # geom_vline(xintercept = c(75000000, 82500000), linetype = 2) +
      
    labs(
      x = "10-year Lee Ferry Volume (maf)",
      y = "Cumulative Probability",
      color = "Scenario",
      title = "CDF of Lee Ferry 10-year Volume",
      subtitle = paste0(startyr,"-",endyr)#,
      # caption = s
    ) +
    scale_x_continuous(breaks = seq(70, 100, 10),limits = (c(70,100))) +
    # scale_x_continuous(labels=tomaf) +
    scale_y_continuous(breaks = seq(0, 1, .2)) +
    scale_color_manual(values = mycolors)
  
  p3
  # gg[[paste(s, "p3")]] <- p3
  ggsave(filename = file.path(ofigs,paste0("CDF10yrLFvol.png")), width = widths[1],height = heights[1])#width= width, height= height)
  
# }

dev.off()







# # save in one pdf
# pdf("results/lf_deficit.pdf", width = 9, height = 6)
# for (i in seq_along(gg)) {
#   print(gg[[i]])
# }
# dev.off()
# 
# # save as two png dashboards
# (gg[[1]] + labs(caption = NULL)) + 
#   (gg[[2]] + theme(legend.position = "none") + labs(caption = NULL)) + 
#   plot_layout(widths = c(6,4))
# ggsave("results/lf_deficit_dnf.png", width = 11, height = 6)
# 
# (gg[[3]] + labs(caption = NULL)) + 
#   (gg[[4]] + theme(legend.position = "none") + labs(caption = NULL)) + 
#   plot_layout(widths = c(6,4))
# ggsave("results/lf_deficit_st.png", width = 11, height = 6)
# 
# 
# # and yet another version that putss full and stress test on the same figure
# db1 <- (gg[[1]] + labs(caption = NULL, title = "Full Hydrology")) +
#   (gg[[3]] + labs(caption = NULL, title = "Stress Test Hydrology"))
# 
# 
# db2 <- (gg[[2]] + labs(caption = NULL, title = "Full Hydrology")) /
#   (gg[[4]] + labs(caption = NULL, title = "Stress Test Hydrology"))
# 
# 
# pdf("results/draft_lf_deficit_20200729.pdf", width = 11, height = 8.5)
# print(db1)
# print(db2)
# dev.off()
# 
# # for UCRC workgroup meeting -----------------------------------
# 
# db3 <- (gg[[1]] + 
#    labs(caption = NULL, title = "Full Hydrology (1906-2018)", color = "Demands:") +
#    scale_color_manual(
#      labels = c("2007 UCRC" = "2007 UCRC", "2016 UCRC" = "Preliminary 2016 UCRC"),
#      values = colors[["Full Hydrology"]]
#   ) +
#     theme(
#       axis.text = element_text(size = 12),
#       strip.text = element_text(size = 12),
#       legend.text = element_text(size = 12),
#       legend.title = element_text(size = 12)
#     )
# ) +
#   (gg[[3]] + 
#      labs(caption = NULL, title = "Stress Test Hydrology (1988-2018)", color = "Demands:") +
#      scale_color_manual(
#        labels = c("2007 UCRC" = "2007 UCRC", "2016 UCRC" = "Preliminary 2016 UCRC"),
#        values = colors[["Stress Test Hydrology"]]
#      ) +
#      theme(
#        axis.text = element_text(size = 12),
#        strip.text = element_text(size = 12),
#        legend.text = element_text(size = 12),
#        legend.title = element_text(size = 12)
#      )
#   )

# ggsave("results/lf_risk_v2.png", width = 10, height = 6)
