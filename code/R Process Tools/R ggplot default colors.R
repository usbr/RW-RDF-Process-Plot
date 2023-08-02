library(scales)

#set margins of plot area
par(mai = c(0.1, 0, 0.1, 0), bg = "grey85")

#create plot with ggplot2 default colors from 1 to 8
gc.grid <- layout(matrix(1:8, nrow = 8))
for(i in 1:8){
  gc.ramp <- hue_pal()(i)
  plot(c(0, 8), c(0,1),
       type = "n", 
       bty="n", 
       xaxt="n", 
       yaxt="n", xlab="", ylab="")
  for(j in 1:i){
    rect(j - 1, 0, j - 0.25, 1, col = gc.ramp[j])
  }
}

library(scales)
#R default pal with 2 colors 
hue_pal()(2)

#R default pal with 5 colors 
hue_pal()(5)