# Preparing germination data from Willson and Whelan 1993
# Sept 30, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

Germ1987 <- read.csv("GermWhelan1987.csv")
Germ1988 <- read.csv("GermWhelan1988.csv")
# The original raw data has been lost to time, these data were digitized from the graphs
# in the publication with Plot Digitizer
# per personal communication with Dr. Whelan, 20 seeds were planted in each treatment at each time

Germ <- rbind(Germ1987, Germ1988)
Germ$Seed <- 20
Germ$Germination <- Germ$AverageNumberGerminants/Germ$Seed

# Calculate average germination for natural soil and potting soil
AverageGermination <- Germ %>%
  group_by(Soil) %>%
  summarize(AverageGermination = mean(Germination))

write.csv(AverageGermination, "WhelanAverageGerm.csv")

# Calculate the difference between the two soil types
# As a lump sum
Difference <- AverageGermination[2, 2] - AverageGermination[1, 2]
# 0.1354878 less germination when grown in natural soil

# By comparing the paired experimental treatments
PairedComparison <- Germ %>%
  group_by(
    Year,
    Planting
    ) %>%
  summarize(
    difference = Germination[Soil == "Potting"] - Germination[Soil == "Natural"],
    .groups = "drop"
  )

write.csv(PairedComparison, "WhelanSoilGerm.csv")

AlternativeAverageDifference <- mean(PairedComparison$difference)
# 0.1354878 this is the same, but might be useful to pull from the distribution?

Distribution <- hist(PairedComparison$difference)

write.csv(Germ, "WhelanGermination.csv")