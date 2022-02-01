### first run Verification_Salt.R

if (F) {
  # LB res 4 panel annual plot
  i=15
  p1 <- df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  i=18
  p2 <- df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  i=19
  p3 <- df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")
  i=20
  p4 <- df_annual %>%
    dplyr::filter(Variable == flownm[i])  %>%
    group_by(DataType,Variable,Year) %>%
    mutate(Value = Value/1000) %>%
    ggplot(aes(x = Year, y = Value, color = DataType)) +
    geom_line() +
    theme_light() +
    theme(axis.title.x = element_blank()) +
    labs(title = paste(nodes[i]), y = "Flow (KAF/yr)",x="")

  p<-grid.arrange(p1,p2,p3,p4,ncol=1)

  if(printfigs==T){ ggsave(plot = p,filename = file.path(file_dir,paste0("LBRes.png")), width = gage_widths[1],height = gage_heights[1])}
}