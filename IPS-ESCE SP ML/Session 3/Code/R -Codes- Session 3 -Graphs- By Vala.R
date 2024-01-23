
rm(list=ls())


library(dplyr)
library("readxl")


setwd("G:/My Drive/IPS/Mestrado/UCs/Aprendizage supervisionada/IPS-ESCE SP ML/Session 3/Data")


 class <- c("numeric", "character", "factor", "numeric", "numeric")
 pollution <- read.csv("avgpm25.csv", colClasses = class)


 head(pollution)
 
 str(pollution)


 summary(pollution$pm25)
 
 #--- Boxplot
 
 boxplot(pollution$pm25, col = "blue")


 #-- Outliers
 
 boxplot(pollution$pm25, col = "blue")$out



#--- Histogram
 
 hist(pollution$pm25, col = "green")
 
 rug(pollution$pm25)
 
 #---- Barplot

 table(pollution$region) %>% barplot(col = "red")
 
 #------ Multiple histograms
 # par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
 
 par(mfrow = c(2, 1))

 
 hist(subset(pollution, region == "east")$pm25, col = "green")
 hist(subset(pollution, region == "west")$pm25, col = "red")
 

 par(mfrow = c(1, 1))
 
 #---- Scatterplots
 
 ?abline
 
  with(pollution, plot(latitude, pm25))
  abline(h = 12, lwd = 1)
 
  
   with(pollution, plot(latitude, pm25, col = region))
   abline(v = 35, lwd = 2, lty = 1)

   par(mfrow = c(1, 2))
   with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West", col="red"))
   with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East", col="blue"))
   
   #------ GGPlot2


library(ggplot2)

qplot(x = carat,                            # x variable
      y = price,                            # y variable
      data = diamonds,                      # Data set
      geom = "point",                       # Plot type
      color = clarity,                      # Color points by variable clarity
      xlab = "Carat Weight",                # x label
      ylab = "Price",                       # y label
      main = "Diamond Carat vs. Price")    # Title

my_data <- as_tibble(diamonds)
my_data


?ggplot


 #--- using ggplot

ggplot(data=diamonds,             # call to ggplot() and data frame to work with
       aes(x=carat, y=price))     # aesthetics to assign

ggplot(data=diamonds, aes(x=carat, y=price)) +  # Initialize plot* 
  geom_point()    # Add a layer of points (make scatterplot)

ggplot(data=diamonds, aes(x=carat, y=price)) +  # Initialize plot 
  geom_point(aes(color = clarity), alpha = 0.1)  +  # Add transparency
  xlim(0,2.5)                                       # Specify x-axis range


ggplot(data=diamonds, aes(x=carat)) +      # Initialize plot 
  
  geom_histogram(fill="skyblue",      # Create histogram with blue bars
                 col="black",         # Set bar outline color to black
                 binwidth = 0.05) +   # Set bin width
  
  xlim(0,3)                           # Add x-axis limits


ggplot(data=diamonds, aes(x=clarity, y=carat)) +  # Initialize plot 
  
  geom_jitter(alpha=0.05,          # Add jittered data points
              color="grey") +    # Set data point color
  
  geom_boxplot(outlier.shape=1,     # Create boxplot and set outlier shape
               alpha = 5  )         # Make inner boxplot area transparent


ggplot(data=diamonds, aes(x=carat)) +       # Initialize plot 
  xlim(0,2.5)                 +       # Limit the x-axis*
  
  geom_density(position="stack",      # Create a stacked density chart
               aes(fill=cut),         # Fill based on cut
               alpha = 0.5)           # Set transparency


ggplot(data=diamonds, aes(x=carat, y=price)) +  # Initialize plot 
  
  geom_point(aes(color=color),            # Color based on diamond color
             alpha=0.5)     +
  
  facet_wrap(~clarity)           +        # Facet on clarity
  
  geom_smooth()                  +        # Add an estimated fit line*
  
  theme(legend.position=c(0.85,0.16))     # Set legend position
