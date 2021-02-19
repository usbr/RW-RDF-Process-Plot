library(RWDataPlyr)
library(tidyverse)
library(patchwork)

# if (exists("keepscens")) {
#   names(plotColors) <- names(keepscens) # colorNames #### does this work? 
# } else if (exists("scens")){
#   names(plotColors) <- names(scens) # colorNames #### does this work? 
#   keepscens <- scens
# } else {stop("missing keepscens or scens")}

# full_scens <- c("2007 UCRC" = "Aug2020_2021,DNF,2007Dems,IG_DCP,Most",
#                 "2016 UCRC" = "FebRedesign9011,DNF,2016Dems,IG_DCP.9004,MTOM_Most")
# 
# st_scens <-  c(
#   "2007 UCRC" = "Aug2020_2021,ISM1988_2018,2007Dems,IG_DCP,Most",
#   "2016 UCRC" = "FebRedesign9011,ISM1988_2018,2016Dems,IG_DCP.9004,MTOM_Most"
# )

# scen_dir <- "M:/Shared/CRSS/2021/Scenario_dev/"

if (TRUE) {

  rwa <- read_rwd_agg("rw_agg/lf_flow_rwa.csv")
  
  dnf <- rw_scen_aggregate(
    scens,
    agg = rwa, 
    scen_dir = scen_dir,
    file = "data/lf_dnf.feather"
  )
  
  # st <- rw_scen_aggregate(
  #   st_scens,
  #   agg = rwa,
  #   scen_dir = scen_dir,
  #   file = "data/lf_st.feather"
  # )
} else {
  dnf <- readRDS(file.path(ofigs,paste0("lf_dnf.RDS")))
  # st <- feather::read_feather("data/lf_st.feather")
}

saveRDS(dnf,file=file.path(ofigs,paste0("lf_dnf.RDS"))) #prevent neeed to reprocess

zz <- dnf %>% #filter out scens you don't want to keep for plots
  dplyr::filter(Scenario %in% keepscens)

  
# dnf$hydrology <- "Full Hydrology"
# st$hydrology <- "Stress Test Hydrology"

# zz <- bind_rows(dnf, st)

# Plot ---------------------
cap_text <- NULL
var_name <- c(
  "lf_10yr_lt75" = "Risk of 10-year Volume at Lee Ferry < 75 maf", 
  "lf_10yr_lt825" = "Risk of 10-year Volume at Lee Ferry < 82.5 maf"
)

gg <- list()

colors <- mycolors

tomaf <- function(x) {
  x/1000000
}

endyr
zz <- zz %>%
  filter(Year <= endyr) #last year is bad
  

# pdf(file.path(oFigs,paste0("LF10yr_",Figs,".pdf")), width = widths[1],height = heights[1]) #width= width, height= height)

# for (s in keepscens) {
#   
#   tmp <- filter(zz, hydrology == s)
  # tmp_colors <- mycolors #colors[[s]]
  
  # plot 75 maf over 10
  p2 <- zz %>%
    filter(Variable != "lf_10yr") %>%
    mutate(Variable = var_name[Variable]) %>%
    group_by(Year, Scenario, Variable) %>%
    summarise(Value = mean(Value)) %>%
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
  ggsave(filename = file.path(ofigs,paste0("10yrLFvolbyYr.png")), width = widths[1],height = heights[1])#width= width, height= height)
  
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
