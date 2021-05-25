library(tidyselect)

scen_names <- c("dnf" = "Full Hydrology\n(1906-2018)",
                "st" = "Stress Test Hydrology\n(1988-2018)")
scen_color <- rep(c("#138d75", "#f1c40f"))
names(scen_color) <- scen_names

plot_data <- function(zz, plot_years, scens, main_title, sub_title, 
                      yrange = NULL, historical_data = NULL, add_IG = TRUE)
{
  line_names <- c("q10" = "10th", "q50" = "50th", "q90" = "90th")
  
  linetypes <- c("10th" = 3, "50th" = 1, "90th" = 2)
  
  # 10/50/90 
  tmp <- zz %>% 
    ungroup() %>%
    filter(Scenario %in% scens, wy %in% plot_years) %>%
    mutate(Scenario = scen_names[Scenario]) %>%
    #ggplot(aes(wy, Value, linetype = Percentile, color = Scenario)) + 
    ggplot(aes(x = wy)) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = Scenario), alpha = 0.3, linetype = 2) +
    geom_line(aes(y = q50, color = Scenario), size = 1) + 
    scale_linetype_manual(values = linetypes) +
    scale_color_manual(values = scen_color) +
    scale_fill_manual(values = scen_color) +
    #scale_x_continuous(breaks = plot_years) +
    labs(
      title = main_title,
      subtitle = sub_title,
      x = "Fiscal Year",
      y = "GWh"
    ) +
    scale_y_continuous(labels = scales::comma, limits = yrange) 
  
  if (add_IG) {
    tmp <- tmp + 
      geom_vline(xintercept = 2026, linetype = 2)
  }
  
  if (!is.null(historical_data)) {
    new_years <- unique(c(historical_data$wy, plot_years))
    if (length(new_years) > 10) {
      # try every other year
      b1 <- new_years[seq(1, length(new_years), 2)]
      b2 <- new_years
    } else {
      b1 <- new_years
      b2 <- new_years
    }
    
    tmp <- tmp +
      geom_line(
        data = historical_data, 
        aes(wy, Value), 
        size = 1, linetype = 1, color = "black"
      ) +
      scale_x_continuous(
        breaks = b1, 
        minor_breaks = b2, 
        expand = expansion(add = c(.4, .4))
      )
  } else {
    tmp <- tmp + 
      scale_x_continuous(breaks = plot_years, minor_breaks = plot_years)
  }
  
  tmp

}

risk_plot <- function(zz, plot_years, scens, main_title, sub_title, yrange=NULL)
{
  zz %>%
    ungroup() %>%
    filter(Scenario %in% scens, Year %in% plot_years) %>%
    mutate(Scenario = scen_names[Scenario]) %>%
    ggplot(aes(Year, Value, color = Scenario)) +
    geom_line(size = 1) +
    scale_color_manual(values = scen_color) +
    scale_x_continuous(breaks = plot_years) +
    labs(
      title = main_title,
      subtitle = sub_title,
      x = NULL,
      y = NULL
    ) +
    scale_y_continuous(labels = scales::percent, limits = yrange) 
}

# keeps the `keep_vars` and sums them together
compute_105090 <- function(zz, keep_col, long = FALSE)
{
  tmp <- zz %>%
    select_at(c("Scenario", "wy", "TraceNumber", keep_col)) %>%
    group_by(Scenario, wy) %>%
    summarise_at(
      keep_col,
      list(
        q10 = ~quantile(., 0.1), 
        q50 = ~median(.),
        q90 = ~quantile(., 0.9)
      )
    ) 
  
  if (long) 
    tmp <- tmp %>%
      pivot_longer(
        c("q10", "q50", "q90"),
        names_to = "Percentile", 
        values_to = "Value"
      )
  tmp
}

compute_change_in_median <- function(zz, scens, keep_years)
{
  dcp_var <- vars_select(scens, contains("_dcp"))
  nodcp_var <- vars_select(scens, contains("_nodcp"))
  
  tmp <- zz %>%
    filter(Scenario %in% scens, Percentile == "q50", wy %in% keep_years) %>%
    select(-Percentile) %>%
    spread(Scenario, Value) %>%
    mutate(
      prct_diff = (.data[[dcp_var]] - .data[[nodcp_var]]) / .data[[nodcp_var]]
    ) %>%
    gather(Scenario, Value, -wy) %>%
    spread(wy, Value)
    
  row_order <- c(nodcp_var, dcp_var, "prct_diff")
  row_i <- match(row_order, tmp$Scenario)

  tmp[row_i,]  
}
