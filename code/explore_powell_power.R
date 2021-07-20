#explore_powell_power.R has a few different sections, but those "vulnerability" figures are included pretty early in that file
#get_data.R creates data frames that are used in explore_powell_power.R

library(feather)
library(tidyverse)
library(RWDataPlyr)
library(patchwork)
library(crssplot)

fpath <- "C:/alan/CRSS/crss.2021/results/Jun2021/powell_figs"

mtom <- paste0("Trace", 4:38)
apr_dnf <- rw_scen_gen_names("Apr2021_2022.v5.1,DNF,2016Dems,IG_DCP.v5.1", mtom)
apr_st <- rw_scen_gen_names("Apr2021_2022.v5.1,ISM1988_2019,2016Dems,IG_DCP.v5.1",
                             mtom)
jun_dnf <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCP", mtom)
jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_dnf_nodo <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO", mtom)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom)

st_flat_scens <- rw_scen_gen_names(
  "Jun2021_2022,ISM1988_2019,2016DemsConstant,IG_DCP",
  paste0("Trace", sprintf("%02d", 4:38))
)

xx <- read_feather("C:/alan/crss/crss.2021/results/Jun2021/tempData/MeadPowellPE.feather")
sys_data <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/SysCond.feather")
xtra <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/jun_extra_powell.feather")
unique(xx$Scenario)

xx %>%
  group_by(Year, Agg, Variable) %>%
  filter(
    Variable == "powell_wy_min_lt_3490", Year <= 2026, 
    !(Agg %in% c("Apr 2021 - DNF IG", "Apr 2021 - ST IG"))) %>%
  summarise(Value = mean(Value) * 100) %>%
  pivot_wider(names_from = "Agg", values_from = "Value") %>%
  as.data.frame()

# leb next year
leb <- yy %>% 
  filter(Variable %in% c("lebGt823", "lebLt823", "leb823"), Year == 2022, Value == 1) %>%
  select(Scenario)
leb <- unique(leb$Scenario)

xx %>%
  filter(Scenario %in% leb) %>%
  group_by(Year, Agg, Variable) %>%
  filter(Variable == "powell_wy_min_lt_3490", Year <= 2026,
         !(Agg %in% c("Apr 2021 - DNF IG", "Apr 2021 - ST IG"))) %>%
  summarise(Value = mean(Value) * 100) %>%
  pivot_wider(names_from = "Agg", values_from = "Value") %>%
  as.data.frame()

# scatter plot of traces that fall below 3490 in WY2022 ------------
# trace to year maps

v22 <- xx %>%
  filter(
    Agg %in% c("Jun 2021 - DNF IG noUBDRO", "Jun 2021 - ST IG noUBDRO"),
    Variable == "powell_wy_min_lt_3490", Year == 2022, Value == 1
  )

init_pe <- xx %>%
  filter(
    Variable == "powell_dec_pe", Year == 2021, Scenario %in% unique(v22$Scenario)
  )

v22 <- v22 %>% 
  select(TraceNumber, Scenario, Agg) %>%
  mutate(
    pe = init_pe$Value[match(Scenario, init_pe$Scenario)],
    scen_trace = paste(Scenario, TraceNumber, sep = "_")
  ) 
v22$hydrology <- stringr::str_split_fixed(v22$Scenario, ",", 5)[,2]
v22 <- v22 %>%
  mutate(hydro_start = case_when(
    hydrology == "DNF" ~ TraceNumber + 1905,
    hydrology == "ISM1988_2019" ~ TraceNumber + 1987,
    TRUE ~ -9999
  ))
  

# now get the natural flow for that set of scen_traces 
nf <- xtra %>%
  filter(
    Variable == "lf_nf", Year == 2022,
    Scenario %in% c(jun_dnf_nodo, jun_st_nodo)
  ) %>%
  mutate(scen_trace = paste(Scenario, TraceNumber, sep = "_")) %>%
  filter(scen_trace %in% v22$scen_trace) %>%
  select(scen_trace, Value) %>%
  rename(lf_nf = Value) %>%
  mutate(lf_nf = lf_nf/1000000)

v22 <- left_join(v22, nf, by = "scen_trace")

ggplot(v22, aes(lf_nf, pe, shape = hydrology)) +
  geom_point() +
  geom_label(aes(x = lf_nf, y = 3546, label = hydro_start)) +
  labs(
    title = "Traces with Powell < 3,490 in WY2022",
    x = "modeled WY2022 natural flow (maf)",
    y = "Powell December 2021 elevation (feet)",
    caption = "From June 2021 CRSS with no assumed UB DRO."
  ) +
  scale_y_continuous(labels = scales::comma_format(1))

ggsave(file.path(fpath, "vuln_wy22.png"), width = 9, height = 6)

# scatter plot of traces that fall below 3490 in WY2022 ------------
v23 <- xx %>%
  filter(
    Agg %in% c("Jun 2021 - DNF IG noUBDRO", "Jun 2021 - ST IG noUBDRO"),
    Variable == "powell_wy_min_lt_3490", Year == 2023, Value == 1
  )

init_pe <- xx %>%
  filter(
    Variable == "powell_dec_pe", Year == 2021, Scenario %in% unique(v23$Scenario)
  )

v23 <- v23 %>% 
  select(TraceNumber, Scenario, Agg) %>%
  mutate(
    pe = init_pe$Value[match(Scenario, init_pe$Scenario)],
    scen_trace = paste(Scenario, TraceNumber, sep = "_")
  )
v23$hydrology <- stringr::str_split_fixed(v23$Scenario, ",", 5)[,2]
v23 <- v23 %>%
  mutate(hydro_start = case_when(
    hydrology == "DNF" ~ TraceNumber + 1905,
    hydrology == "ISM1988_2019" ~ TraceNumber + 1987,
    TRUE ~ -9999
  ))

# now get avg natural flow for 22 and 23 for that set of scen_traces
nf <- xtra %>%
  filter(
    Variable == "lf_nf", Year %in% 2022:2023,
    Scenario %in% c(jun_dnf_nodo, jun_st_nodo)
  ) %>%
  group_by(Scenario, TraceNumber) %>%
  summarise(Value = mean(Value)) %>%
  mutate(scen_trace = paste(Scenario, TraceNumber, sep = "_")) %>%
  filter(scen_trace %in% v23$scen_trace) %>%
  ungroup() %>%
  select(scen_trace, Value) %>%
  rename(lf_nf = Value) %>%
  mutate(lf_nf = lf_nf/1000000)

v23 <- left_join(v23, nf, by = "scen_trace")

ll <- v23 %>%
  select(hydro_start, lf_nf) %>%
  distinct()
ll$rank <- rank(ll$lf_nf)

g1 <- ggplot(v23, aes(lf_nf, pe)) +
  geom_point(aes(shape = hydrology)) +
  #geom_label(data = ll,
  #           aes(x = lf_nf, y = 3547 + runif(nrow(ll), -2, 2), label = hydro_start)) +
  labs(
    title = "Traces with Powell < 3,490 in WY2023",
    x = "modeled average WY2022 and WY2023 natural flow (maf)",
    y = "Powell December 2021 elevation (feet)",
    caption = "From June 2021 CRSS with no assumed UB DRO."
  ) +
  scale_y_continuous(labels = scales::comma_format(1)) +
  theme(legend.position = "bottom")

yr_label <- ll$hydro_start
names(yr_label) <- ll$rank

g2 <- ggplot(ll, aes(as.factor(rank), lf_nf)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = yr_label) +
  coord_flip() +
  labs(x = "2-year periods beginning in:",
       y = "Lees Ferry 2-year avg flow (maf)",
       title = "2-year periods")

g1 + g2 +
  plot_layout(widths = c(.8, .2))

ggsave(file.path(fpath, "vuln_wy23.png"), width = 12, height = 6)


# cdf of wy2023 release for problematic traces -------
xtra %>%
  filter(
    Variable == "powell_wy_rel", Year %in% 2023, Month == "December",
    Scenario %in% c(jun_dnf_nodo, jun_st_nodo)
  ) %>%
  mutate(scen_trace = paste(Scenario, TraceNumber, sep = "_")) %>%
  filter(scen_trace %in% v23$scen_trace) %>%
  ggplot(aes(Value)) +
  stat_ecdf() +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  labs(
    title = "Powell WY2023 Release for Traces that fall below 3,490 in WY2023",
    y = "Probability",
    x= "acre-ft"
  ) +
  scale_x_continuous(labels = scales::comma_format(1))

ggsave(file.path(fpath, "cdf_wy23.png"), width = 9, height = 6)

# frequency of year types for problematic traces ------
sys_data %>%
  filter(
    Year %in% 2022:2023, Month == "December",
    Scenario %in% c(jun_dnf_nodo, jun_st_nodo)
  ) %>%
  mutate(scen_trace = paste(Scenario, TraceNumber, sep = "_")) %>%
  filter(scen_trace %in% v23$scen_trace) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(
    eq = eq823 + eq,
    ueb = uebLt823 + uebGt823 + ueb823,
    mer = mer748 + mer823, 
    leb = leb823 + lebGt823 + lebLt823
  ) %>%
  select(scen_trace,Year, eq, ueb, mer, leb) %>%
  pivot_longer(-c("scen_trace", "Year")) %>%
  filter(value == 1) %>%
  ggplot(aes(x = name)) + geom_bar() + facet_wrap(~Year)

# p < 3,490 with CRMMS-ESP ---------------------------------
vv <- c("Powell < 3,490' in Any Month in the WY", "Powell < 3,525' in Any Month in the WY")
names(vv) <- c("powell_wy_min_lt_3490","powell_wy_min_lt_3525")
# st <- read.csv(
#   "C:/alan/CRSS/crss.2021/results/Jun2021/figure_data/Jun2021-ST_CritStats.csv",
#   check.names = FALSE
# ) %>%
#   select_at(c("Year", vv)) %>%
#   mutate(Scenario = "Jun 2021 - ST") %>%
#   pivot_longer(-c("Year", "Scenario"), names_to = "Variable")
# st2 <- read.csv(
#   "C:/alan/CRSS/crss.2021/results/Jun2021/figure_data/Jun2021-ST-noDRO_CritStats.csv",
#   check.names = FALSE
# ) %>% select_at(c("Year", vv)) %>%
#   mutate(Scenario = "Jun 2021 - ST - no DRO") %>%
#   pivot_longer(-c("Year", "Scenario"), names_to = "Variable")

st <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/MeadPowellPE.feather") %>%
  filter(
    Agg %in% c("Jun 2021 - ST IG","Jun 2021 - ST IG noUBDRO"),
    Variable %in% c("powell_wy_min_lt_3490","powell_wy_min_lt_3525")
  ) %>%
  #mutate(Scenario = "Jun ST - flat demands") %>%
  select(-Scenario) %>%
  rename(Scenario = Agg) %>%
  group_by(Scenario, Variable, Year) %>%
  summarise(value = mean(Value)) %>%
  mutate(Variable = vv[Variable])

crmms <- readxl::read_xlsx("C:/alan/MTOM/model/CRMMS_EnsembleOutput _JUN21.xlsm", 
                          "Powell.Pool Elevation", skip = 2) %>%
  rename(time_step = `...1`) %>%
  select_at(c("time_step", paste0("Trace", 4:38))) %>%
  pivot_longer(-time_step, names_to = "Trace") %>%
  mutate(
    year = lubridate::year(time_step), 
    month = lubridate::month(time_step),
    water_year = if_else(month <= 9, year, year + 1)
  ) %>%
  filter(water_year %in% 2022:2060) %>%
  group_by(water_year, Trace) %>%
  summarise(value = min(value)) %>%
  mutate(lt3525 = as.numeric(value <= 3525), lt3490 = as.numeric(value <= 3490)) %>%
  select(-value) %>%
  pivot_longer(c("lt3525", "lt3490"), names_to = "Variable") %>%
  group_by(water_year, Variable) %>%
  summarise(value = mean(value)) %>%
  mutate(Variable = if_else(Variable == "lt3490", vv[1], vv[2]), Scenario = "CRMMS-ESP") %>%
  rename(Year = water_year)

st_flat <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/MeadPowellPE_flatDemands.feather") %>%
  filter(
    Scenario %in% st_flat_scens, Year <= 2060, 
    Variable %in% c("powell_wy_min_lt_3490","powell_wy_min_lt_3525")
  ) %>%
  mutate(Scenario = "Jun ST - flat demands") %>%
  group_by(Scenario, Variable, Year) %>%
  summarise(value = mean(Value)) %>%
  mutate(Variable = vv[Variable])

pr <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/MeadPowellPE_pr.feather") %>%
  filter(
    Variable %in% c("powell_wy_min_lt_3490", "powell_wy_min_lt_3525")
  ) %>%
  mutate(Scenario = "June pluvial removed") %>%
  group_by(Scenario, Variable, Year) %>%
  summarise(value = mean(Value)) %>%
  mutate(Variable = vv[Variable])
  

zz <- bind_rows(st, crmms, st_flat, pr) %>%
  rename(ScenarioGroup = Scenario, Value = value)

cc <- c("Jun 2021 - ST IG" = "#6b8f00", "Jun 2021 - ST IG noUBDRO" = "#9a3324",
        "CRMMS-ESP" = "#4B157A", "Jun ST - flat demands" = "#FF5900",
        "June pluvial removed" = "#90B1E5")
scens_plot_probs(zz, vv, plot_colors = cc)
ggsave("C:/alan/CRSS/crss.2021/results/Jun2021/powell_figs/powell_probs.png", width = 9, height = 6)

# crmms-esp elevations ---------------------------
crmms <- readRDS("C:/alan/CRSS/CRSS.2021/results/Jun2021/AprilvJuneCRMMS_PE.rds") %>%
  select(-Min, -Max, -Most) %>%
  pivot_longer(-c("run", "slot", "Date")) %>%
  filter(Date == as.Date("2021-12-01"))

cc <- c("April 2021" = "#fc8d62", "June 2021" = "#8da0cb")

ggplot(crmms, aes(run, value)) + 
  geom_point(aes(color = run)) + 
  facet_wrap(~slot, nrow = 2, scales = "free_y") +
  scale_color_manual(values = cc) + 
  labs(
    color = NA, y = "feet",
    title = "December 2021 elevations from CRMMS-ESP"
  ) +
  theme(legend.position = 'none')

# natural flow next year ---------------------------
lf <- bind_rows(data.frame(
    years = 1931:2019,
    lf = zoo::coredata(CoRiverNF::cyAnnTot$LeesFerry["1931/"]),
    scenario = "pluvial removed"
  ),
  data.frame(
    years = 1988:2019,
    lf = zoo::coredata(CoRiverNF::cyAnnTot$LeesFerry["1988/"]),
    scenario = "stress test"
  )
)

ggplot(lf, aes(LeesFerry, color = scenario)) +
  stat_ecdf()
