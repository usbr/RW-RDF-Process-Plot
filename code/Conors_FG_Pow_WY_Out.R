# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenarios', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
max_date = '2026-12' #'2024-12'

## Trace Select?
single_Trace <- FALSE

# single_Trace <- TRUE
# sel_trace <- 1997 # 2019 #2001, 2011, 1999
# 
# print(paste('-------- Max Date is:', max_date, ';',
#             ifelse(single_Trace, 'Single trace output',
#                    'Cloud of traces output'), '--------'))

slots = c(
  "Powell.Outflow",
  "FlamingGorge.Outflow"#, 
  # "FlamingGorgeData.AccountVolume",
  # "FlamingGorgeData.InitialOutflow"
)

## -- Read in CRMMS results

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = c(rep("res.rdf", length(slots))),
  slot = slots, 
  period = rep("wy", length(slots)),
  summary = rep("sum", length(slots)),
  # period = rep("asis", length(slots)),
  # summary = rep(NA, length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))

# read/process RDFs
df <- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    # keep_cols = 'Unit',
    find_all_slots = F
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = unique(scen_res$TraceNumber)
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df_scens <-
  data.table::as.data.table(df)  %>%
  # mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  # select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  # filter(Date <= as.yearmon(format(ym(max_date), "%Y-%m"))) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + TraceNumber - min(df$TraceNumber))# %>%


#add on Oct 22 releases not in model run 
df <- df_scens  %>%
  mutate(Value = ifelse(Variable == "Powell.Outflow" & Year == 2023, Value + 480.05,
                        ifelse(Variable == "FlamingGorge.Outflow" & Year == 2023, Value + 110.83,Value )))

wymeans <- df   %>%
  # dplyr::filter(Variable == variable) %>%
  dplyr::group_by(Scenario, Year, Variable) %>%
  summarise(Mean = mean(Value)) %>%
  # pivot_wider(names_from = Variable, values_from = Mean) 
pivot_wider(names_from = Scenario, values_from = Mean) 


## Output to excel file

wb1 <- openxlsx::createWorkbook("FGPowWYRel")
openxlsx::addWorksheet(wb1, "wymeans")
openxlsx::writeData(wb1, "wymeans", wymeans)


wyall <-
  df   %>%
  # dplyr::filter(Variable == variable) %>%
  # dplyr::group_by(Scenario, Year, Variable) %>%
  # select(Scenario, Variable, Year, Value) %>%
  # summarise(Mean = mean(Value)) %>%
  pivot_wider(names_from = Scenario, values_from = Value) 


openxlsx::addWorksheet(wb1, "wyall")
openxlsx::writeData(wb1, "wyall", wyall)

openxlsx::saveWorkbook(wb1, file.path(fig_dir, paste0('FGPowWYRel', end_file_nm, '.xlsx')), overwrite = T)

####make no recovery calculation 

df_norecovery <-
  data.table::as.data.table(df)  %>% 
  filter(Scenario == scenarios[1]) 

df_opt1 <-
  data.table::as.data.table(df)  %>% 
  filter(Scenario == scenarios[2]) 

df_opt2 <-
  data.table::as.data.table(df)  %>% 
  filter(Scenario == scenarios[3]) 

df_opt1$Value = df_norecovery$Value - df_opt1$Value

df_opt2$Value = df_norecovery$Value - df_opt2$Value

df_opt12 <- rbind.data.frame(df_opt1,df_opt2)

variable = "Powell.Outflow"

title = paste("WY",variable,"NoRecovery - Opt")

# df_opt12 %>%
  df %>%
  
  dplyr::filter(Variable == variable) %>%
  # dplyr::filter(Year %in% 2023:2024) %>%
  # dplyr::mutate(Value = Value/1000) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  # geom_boxplot()  
  bor_theme() +
  # facet_grid(~ Year) + 
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  labs(title = title, x = variable)


variable = "FlamingGorge.Outflow"

title = paste("WY",variable,"NoRecovery - Opt")

df_opt12 %>%
  dplyr::filter(Variable == variable) %>%
  # dplyr::filter(Year %in% 2023:2024) %>%
  # dplyr::mutate(Value = Value/1000) %>%
  ggplot(aes(x = factor(Year), y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  # geom_boxplot()  
  bor_theme() +
  # facet_grid(~ Year) + 
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  labs(title = title, x = variable)

title = paste("WY Release Diff NoRecovery - Opt")

df_opt12 %>%
  dplyr::filter(Variable %in% slots[1:2]) %>%
  dplyr::filter(Year %in% 2023:2024) %>%
  # dplyr::mutate(Value = Value/1000) %>%
  ggplot(aes(x = Variable, y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  # geom_boxplot()  
  bor_theme() +
  facet_grid(~ Year) +
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  labs(title = title, x = "KAF")

ggsave(filename = file.path(fig_dir, paste0('FGPowRelease_',title, end_file_nm, '.png')), width=7, height=6)

title = paste("WY Release")

df %>%
  dplyr::filter(Variable %in% slots[1:2]) %>%
  dplyr::filter(Year %in% 2023:2024) %>%
  # dplyr::mutate(Value = Value/1000) %>%
  ggplot(aes(x = Variable, y = Value, color = Scenario)) +
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  # geom_boxplot()  
  bor_theme() +
  facet_grid(~ Year) +
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) + 
labs(title = title, x = "KAF")

ggsave(filename = file.path(fig_dir, paste0('FGPowRelease_',title, end_file_nm, '.png')), width=7, height=6)

