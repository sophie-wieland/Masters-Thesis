# Fecundity/Reproduction of Roughleaf Dogwood
# August 20, 2026
# Sophie Wieland
getwd()
library(ggplot2)
library(dplyr)
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Dogwood IPM")

# Load and organize reproduction data (sources: Wedel, Orrock, KBSF, Davis, Whelan?, Schulstrom, my germination experiment)

WedelRepro <- read.csv("../Literature Search for Model Building/Wedel Reproduction.csv")
OrrockRepro <- read.csv("../Literature Search for Model Building/Orrock Germination.csv")
DavisRepro <- read.csv("../Literature Search for Model Building/Davis Germination.csv")
SchulstromRepro <- read.csv("../Literature Search for Model Building/Schulstrom Fruit Set.csv")
WielandRepro <- read.csv("../Germination Analysis/CleanedGerminationData.csv")
KBSFRepro <- read.csv("../Literature Search for Model Building/fruits_3yrs.csv")

# Schulstrom fruit set mean and distribution
AverageFruitSet <- mean(SchulstromRepro$Fruit.Set)
hist(SchulstromRepro$Fruit.Set)
###################################################################################
# Wieland germination rate (code from 01 Anlyze Germination R script.R)
TotalGerminants <- (WielandRepro$LiveThinned + WielandRepro$DeadThinned +
                      WielandRepro$LiveThinnedPhotos + WielandRepro$DeadThinnedPhotos +
                      WielandRepro$LiveExtras + WielandRepro$DeadExtras +
                      WielandRepro$LiveFloaters + WielandRepro$DeadFloaters +
                      WielandRepro$LiveTransplant + WielandRepro$DeadTransplant +
                      WielandRepro$PotTransplant)
LiveGerminants <- (WielandRepro$LiveThinned + 
                     WielandRepro$LiveThinnedPhotos + 
                     WielandRepro$LiveExtras + 
                     WielandRepro$LiveFloaters + 
                     WielandRepro$LiveTransplant +
                     WielandRepro$PotTransplant)
SumTotalGerminants <- sum(TotalGerminants)
SumLiveGerminants <- sum(LiveGerminants)

# Germination rate of all data, pooled
AverageInitialGermination <- SumTotalGerminants/ (sum(WielandRepro$SeedCount, na.rm = TRUE))
###################################################################################
# KBSF data: The number of infructescences per stem in 1996, 1998, and 1999
# See, for comparison, Wedel's Mean and SE counts
# These counts, unfortunately, are not tied to *shrubs* of known sizes (just ramets)

hist(KBSFRepro$X99infru)
hist(KBSFRepro$X98infru)
hist(KBSFRepro$X96infru)

mean(KBSFRepro$X99infru)
sd(KBSFRepro$X99infru)/sqrt(length(KBSFRepro$X99infru))

# 1998 falls within the range Wedel found for unburned, unbrowsed shrubs
# The others have higher and more variable values
mean(KBSFRepro$X98infru)
sd(KBSFRepro$X98infru)/sqrt(length(KBSFRepro$X98infru))

mean(KBSFRepro$X96infru)
sd(KBSFRepro$X96infru)/sqrt(length(KBSFRepro$X96infru))

# Calculate likely number of fruits per shrub island, using Wedel and Schulstrom data
# Should this use the average fruit set? Or perhaps pull from the distribution?

WedelRepro$EstFruit2018 <- WedelRepro$EstInflorescences2018 * AverageFruitSet
WedelRepro$EstFruit2019 <- WedelRepro$EstInflorescences2019 * AverageFruitSet

ggplot(WedelRepro, aes(x = Size2018, y = EstFruit2018)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(WedelRepro, aes(x = Size2019, y = EstFruit2019)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(WedelRepro, aes(x = Size2018, y = EstFruit2018)) +
  geom_point()+
  geom_smooth(data = subset(WedelRepro, Treatment == "control"), method = "lm")

ggplot(WedelRepro, aes(x = Size2019, y = EstFruit2019)) +
  geom_point()+
  geom_smooth(data = subset(WedelRepro, Treatment == "control"), method = "lm")