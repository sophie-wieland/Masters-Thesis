# Regression for growth
# August 4, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Dogwood IPM")

# Load and organize growth data (sources: O'Connor, Wedel, KBSF?, Glidden/tracing, aerial imagery, my field experiment)

WedelGrowth <- read.csv("../Literature Search for Model Building/Wedel Growth.csv")
OConnorGrowth <- read.csv("../Literature Search for Model Building/OConnor Growth.csv")

AerialGrowth <- read.csv("../Aerial Imagery/ChangeinSize.csv")

# Create a single merged dataframe with all information and consistent column names
# Columns should be:
# somekinda ID?, Watershed, FRI, Grazer (Bison, Cattle, Ungrazed), Browser (Goat, Unbrowsed), DetectionYear, SizeYear

names(AerialGrowth)[5] <- "Size" 
names(AerialGrowth)[7] <- "Size_tplus1"

AerialGrowth$Grazer <- str_extract(AerialGrowth$Trial_demographic_data.watershed, "[N, C]+")
AerialGrowth$FRI <- str_extract(AerialGrowth$Trial_demographic_data.watershed, "\\d+")







AerialGrowth$ChangeinSize <- (AerialGrowth$Size_tplus1 - AerialGrowth$Size)
AerialGrowth$LogSize <- log(AerialGrowth$Size)
AerialGrowth$LogSize_tplus1 <- log(AerialGrowth$Size_tplus1)
AerialGrowth$Ratio <- (AerialGrowth$Size_tplus1/AerialGrowth$Size)
AerialGrowth$LogRatio <- log(AerialGrowth$Ratio)
####################################################################################
# Plotting size by size_tplus1
ggplot(AerialGrowth, aes(x = Size, y = Size_tplus1)) +
  geom_point()+
  geom_smooth(method = "lm")
####################################################################################
LogRegression <- lm(LogSize_tplus1 ~ LogSize, data = AerialGrowth)
#ChangeRegression <- lm(ChangeinSize ~ Size, data = AerialGrowth)
#LogRatioRegression <- lm(LogRatio ~ LogSize, data = AerialGrowth)
#RatioRegression <- lm(ChangeinSize ~ Size, data = AerialGrowth)

GrazerFireRegression <- lm(LogSize_tplus1 ~ LogSize + Grazer + FRI, data = AerialGrowth)
WatershedRegression <- lm(LogSize_tplus1 ~ LogSize + Trial_demographic_data.watershed, data = AerialGrowth)
####################################################################################
# Variance in growth


####################################################################################
plot(GrazerFireRegression, which = 1)
plot(GrazerFireRegression, which = 2)
plot(GrazerFireRegression, which = 3)
plot(GrazerFireRegression, which = 4)