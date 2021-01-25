setwd("C:/Users/SABaker/Documents/4.Master's Report/2015_SalinityWork/FinalFilesForDeliverables")
library(sm) #sm.density
library(ggplot2)

data = read.table("ResidualDiagnostic.txt",header=T)
Yhat = data[,2]
Y = data[,1]
yres = Y - Yhat

df = as.data.frame(cbind(data,yres))


#Plot Lees Ferry vs. Modeled Salinity
p1 = ggplot(data=df, aes(x = Y, y = Yhat)) + 
  #coord_equal() +
  geom_point() +
  xlim(350,650) +
  ylim(350,650) +
  geom_abline(colour = "red") +
  xlab("Lees Ferry Salinity (mg/L)") +
  ylab("Modeled Salinity (mg/L)") +
  ggtitle("Lees Ferry vs. Modeled Salinity") +
  theme_bw()+
  theme(axis.title = element_text(size=8))

#Histogram of residuals
p2 = ggplot(df, aes(x = yres)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 7) +
  ylab('Density') +
  xlab('Residual') +
  geom_density(colour = "red") +
  geom_hline(yintercept = 0) + 
  ggtitle("Histogram of Residuals") +
  theme_bw()+
  theme(axis.title = element_text(size=8))


# qqplot
y <- quantile(yres[!is.na(yres)], c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]

d <- data.frame(resids = yres)

p3 = ggplot(d, aes(sample = resids)) + 
  stat_qq() + 
  geom_abline(slope = slope, intercept = int, colour = "red") +
  ylab('Sample') +
  xlab('Theoretical') +
  ggtitle("Normal Q-Q Plot")+
  theme_bw()+
  theme(axis.title = element_text(size=8))

# ACF
z1=acf(yres, plot = F)
bacfdf <- with(z1, data.frame(lag, acf))
p4 = ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0.135), colour = "red", linetype = 2) +
  geom_hline(aes(yintercept = -0.135), colour = "red", linetype = 2) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  ylab('ACF') +
  xlab('Lag') +
  ggtitle("Autocorrelation Function") +
  geom_hline(yintercept = 0) + 
  theme_bw()+
  theme(axis.title = element_text(size=8))

#Plot Y estimates against residuals - for constant variance or homoskedasticity..
df5 = data.frame(cbind(Yhat,yres))
p5 = ggplot(data = df5, aes(x = Yhat, y = yres)) + 
  geom_point() +
  xlab("Modeled Salinity (mg/L)") +
  ylab("Residuals (mg/L)") +
  ggtitle("Modeled Outflow vs. Residuals") +
  geom_hline(yintercept = 0, colour = "red") + 
  theme_bw() +
  theme(axis.title = element_text(size=8))


library(grid)
library(gridExtra)
test = grid.arrange(p1,p2,p3,p4,p5, ncol = 3)

ggsave(test, file="diagnostics.png")



##### OLD ################################################################################
###Diagnostics Plots
par(mfrow=c(2,3))

#Plot Y historical vs. Modeled
plot(Y,Yhat,xlim = c(350,650), ylim = c(350,650), xlab="Lees Ferry Salinity", ylab="Modeled Salinity",main='Lees Ferry vs. Modeled
     Salinity')
abline(a=0, b=1,col='red')

#Histogram of Residuals
hist(yres,probability=T,xlab='Residuals',main='Historgram of Residuals')
sm.density(yres,col=c('red'),add=T)

#QQ plot - see how good the residuals fit a normal distribution
qqnorm(yres)
qqline(yres,col=c('red')) # Create a qq line

#Autocorrelation function - see if the residuals are related to each other..  IID assumption
z1=acf(yres,main='Autocorrelation Function')

#Plot Y estimates against residuals - for constant variance or homoskedasticity..
plot(Yhat,yres,xlab="Estimate of Y", ylab="Residuals",main='Y Estimate vs. Residual')
abline(a=0, b=0,col='red')

#Cook's Distance plot
N = length(Y)
SSE = sum((yres)^2) 
MSE = SSE/(N - 2) #last value is number of coefficients

#studentized residuals - ri  - equation 12-42
ri = yres/sqrt((1 - (Yhat)) * MSE)

#Compute Cook's Distance Equation 12-44
Di = ri*ri * (Yhat) / ((1-(Yhat)) * 2 #last value is number of coefficients

#Plot the Di 
plot(1:N, Di, xlab="Obs", ylab="Cook's Distance")
#####
#cutoff <- 4/((nrow(mtcars)-2-2)) 
#plot(data,main="", cook.levels=cutoff)
