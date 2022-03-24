#example of issue
scen_res_daily <- readRDS(file = "sample_df.RDS")

ymin <- c(0,0) ; ymax <- c(10000,50000); mybreaks <- c(2000,5000)#old GREAT 

p <- df %>%
  dplyr::group_by(Scenario) %>% 
  ggplot(aes(Value, color = Scenario)) +
  theme_light() + 
  stat_eexccrv() + #function in question 
  scale_color_manual(values = mycolors) +
  coord_cartesian(xlim =c(0,1), ylim = c(ymin[j],ymax[j]), expand = F) + #don't drop data
  scale_x_continuous("Percent Exceedance",labels = scales::percent,breaks=seq(0,1,.2)) + 
  labs(title = title,
       y = y_lab, caption = caption) +
  theme(plot.caption = element_text(hjust = 0)) #left justify 
print(p)


#function underlying 


StatEExcCrv <- ggproto("StatEExcCrv", Stat,
                    compute_group = function(data, scales, n = NULL, pad = TRUE) {
                      # If n is NULL, use raw values; otherwise interpolate
                      if (is.null(n)) {
                        x <- unique(data$x)
                      } else {
                        x <- seq(min(data$x), max(data$x), length.out = n)
                      }
                      
                      if (pad) {
                        x <- c(-Inf, x, Inf)
                      }
                      y <- ecdf(data$x)(x)
                      
                      data.frame(y = x, x = 1-y)
                    },
                    
                    default_aes = aes(y = ..y..),
                    
                    required_aes = c("x")
)

stat_eexccrv <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEExcCrv,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}