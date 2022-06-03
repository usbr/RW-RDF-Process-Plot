library(tidyverse)
library(readxl)

get_mtom_data <- function(file, sheet, max_date, vname = sheet,crmmstraces = 4:33) {
  zz <- read_xlsx(file, sheet = sheet, skip = 2)
  colnames(zz)[1] <- "Date"

  # remove trace1 - 3
  zz <- zz %>%
    select_at(c("Date", paste0("Trace", crmmstraces))) %>%
    filter(Date <= max_date) %>%
    mutate(Date = ymd(Date)) %>%
    pivot_longer(-Date, names_to = "Trace", values_to = "Value") %>%
    mutate(Variable = vname)
  
  zz
}

get_mtom_ond <- function(file, variable, start_date, end_date,crmmstraces) {
  get_mtom_data(file, variable, end_date) %>%
    filter(Date >= start_date)
    #pivot_longer(-Date, names_to = "Trace", values_to = "Value")
}

get_wy_2020 <- function(file, obs_data, scenario_name) {
  
  # get data from MTOM file 
  davis <- get_mtom_data(file, "Mohave.Energy", ymd("2020-09-30"))
  parker <- get_mtom_data(file, "Havasu.Energy", ymd("2020-09-30"))
  
  # insert obs_data
  d_tmp <- cbind(
    obs_data$Date, 
    as.data.frame(replicate(ncol(davis) - 1, obs_data$davis))
  )
  colnames(d_tmp) <- colnames(davis)
  
  p_tmp <- cbind(
    obs_data$Date,
    as.data.frame(replicate(ncol(parker) - 1, obs_data$parker))
  )
  colnames(p_tmp) <- colnames(parker)
  
  davis <- bind_rows(d_tmp, davis) %>%
    pivot_longer(-Date, names_to = "Trace") %>%
    mutate(wy = date_to_wy(Date)) %>%
    group_by(wy, Trace) %>%
    summarise(davis = sum(value))
  
  parker <- bind_rows(p_tmp, parker) %>%
    pivot_longer(-Date, names_to = "Trace") %>%
    mutate(wy = date_to_wy(Date)) %>%
    group_by(wy, Trace) %>%
    summarise(parker = sum(value))
  
  res <- full_join(parker, davis, by = c("wy", "Trace")) %>%
    mutate(Scenario = scenario_name)
  
  res
}

expand_ond_to_crss_scenarios <- function(zz, n_trace, base_scen, 
                                         pad_trace = TRUE) {
  # takes ond data for trace 4-38 and expands to set of scenarios for n_trace
  # first, add 0 padding to trace number
  if (pad_trace) {
    zz <- zz %>%
      mutate(Trace = str_remove(Trace, "Trace")) %>%
      mutate(Trace = paste0("Trace", sprintf("%02d", as.numeric(Trace))))
  }
  
  # for each trace, grab data, and replicate n_trace times, then add in 
  # base_scen
  rep_zz <- lapply(unique(zz$Trace), function(tt) {
    tmp <- filter(zz, Trace == tt)
    tmp$Trace <- NULL
    tmp$Scenario <- paste0(base_scen, ",", tt)
    nt <- nrow(tmp)
    tmp <- bind_rows(replicate(n_trace, tmp, FALSE))
    
    # add in TraceNumber
    tmp$TraceNumber <- as.vector(matrix(t(replicate(nt, seq(n_trace))), ncol = 1))
    tmp
  })
  
  rep_zz <- bind_rows(rep_zz)
  
  rep_zz
}
