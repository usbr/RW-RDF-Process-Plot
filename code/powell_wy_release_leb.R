library(feather)
library(RWDataPlyr)
library(tidyverse)

sys_data <- read_feather("C:/alan/CRSS/CRSS.2021/results/Jun2021/tempData/SysCond.feather")
zz <- read_feather("C:/alan/crss/CRSS.2021/results/Jun2021/jun_extra_powell.feather")

mtom <- paste0("Trace", 4:38)
apr_dnf <- rw_scen_gen_names("Apr2021_2022.v5.1,DNF,2016Dems,IG_DCP.v5.1", mtom)
apr_st <- rw_scen_gen_names("Apr2021_2022.v5.1,ISM1988_2019,2016Dems,IG_DCP.v5.1",
                            mtom)
jun_dnf <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCP", mtom)
jun_st <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCP", mtom)
jun_dnf_nodo <- rw_scen_gen_names("Jun2021_2022,DNF,2016Dems,IG_DCPnoUBDRO", mtom)
jun_st_nodo <- rw_scen_gen_names("Jun2021_2022,ISM1988_2019,2016Dems,IG_DCPnoUBDRO",
                                 mtom)

june_runs <- c(jun_dnf, jun_st, jun_dnf_nodo, jun_st_nodo)

s2 <- sys_data %>%
  filter(
    Scenario %in% june_runs, 
    Variable %in% c("lebGt823", "lebLt823", "leb823"),
    Year <= 2026
  ) %>%
  group_by(Year, TraceNumber, Scenario) %>%
  summarise(Value = sum(Value)) %>%
  filter(Value != 0) %>%
  mutate(scen_trace_year = paste(Scenario, TraceNumber, Year, sep = "-"))

z2 <- zz %>%
  filter(
    Variable == "powell_wy_rel", Year <= 2026, Month == "December",
    Scenario %in% june_runs
  ) %>%
  mutate(
    scen_trace_year = paste(Scenario, TraceNumber, Year, sep = "-")
  ) %>%
  filter(scen_trace_year %in% unique(s2$scen_trace_year)) 

z2$run <- stringr::str_split_fixed(z2$Scenario, ",", 5)[,4]
run_name <- c("IG_DCP" = "with assumed DRO", "IG_DCPnoUBDRO" = "no assumed DRO")
z2 <- mutate(z2, run = run_name[run])


ggplot(z2, aes(Value, color = run)) + 
  stat_ecdf() +
  scale_x_continuous(labels = scales::comma_format(.1, scale = 1/1000000)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  labs(
    x = "(maf)",
    y = "probability",
    title = "Powell WY Release When in Lower Elevation Balancing",
    subtitle = "2022 - 2026 combined",
    caption = "June 2021 CRSS projections with Stress Test and Full Hydrology"
  ) +
  geom_vline(xintercept = 8230000)

ggplot(z2, aes(Value, color = as.factor(Year))) +
  stat_ecdf() +
  facet_wrap(~run) +
  scale_x_continuous(labels = scales::comma_format(.1, scale = 1/1000000)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  labs(
    x = "(maf)",
    y = "probability",
    title = "Powell WY Release When in Lower Elevation Balancing",
    color = "Year",
    caption = "June 2021 CRSS projections with Stress Test and Full Hydrology"
  ) +
  geom_vline(xintercept = 8230000)

# manual histogram -------------------
z2 <- z2 %>%
  mutate(
    Value = round(Value / 1000000, 1),
    tier = case_when(
      Value < 7 ~ "< 7 maf",
      Value == 7 ~ "7.0 maf",
      Value > 7 & Value <= 8.23 ~ "7.0 - 8.23 maf",
      Value > 8.23 & Value < 9.5 ~ "8.23 - 9.5",
      Value == 9.5 ~ "9.5 maf",
      TRUE ~ "BAD"
    )
  )

z2$tier <- factor(z2$tier, levels = c("< 7 maf", "7.0 maf" ,"7.0 - 8.23 maf",
                                      "8.23 - 9.5","9.5 maf" ))

z2 %>%
  group_by(Year, run, tier) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = Year, y = freq, fill = tier)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~run) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    fill = "WY Release",
    title = "Powell WY Release when in Lower Elevation Balancing Tier"
  )
