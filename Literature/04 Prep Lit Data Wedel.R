# Preparing data from Wedel 2021
# Sophie Wieland
library(dplyr)
library(tidyr)
library(ggplot2)
getwd()

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

Islands <- read.csv("Wedel Area and Density.csv")

# Isolate pertinent columns
Growth <- Islands[, c(3:6, 19)]

# Flip data so there is one row per shrub island

# Change in shrub area over time
Area <- pivot_wider(data = Growth, id_cols = c(Watershed, Shrub_id, Treatment), names_from = RecYear, values_from = Shrub_area, names_glue = "size{RecYear}")

# Add columns showing that all shrubs existed every year
Area$exist2018 <- as.integer(!is.na(Area$size2018))
Area$exist2019 <- as.integer(!is.na(Area$size2019))
# Reorganize, just so the years/columns are chronological
Area <- relocate(Area, size2018, .after = Treatment)
Area <- relocate(Area, exist2018, .after = size2018)
Area <- relocate(Area, size2019, .after = exist2018)
Area <- relocate(Area, exist2019, .after = size2019)

write.csv(Area, "Wedel Growth.csv")

###################################################################################
# Flower production
# The number of flowering stems was counted in a 0.25 m wide transect

Islands$FloweringStemDensity <- Islands$Flowering_stems/Islands$Transect_area
Islands$EstTotalFloweringStems <- Islands$FloweringStemDensity*Islands$Shrub_area

TempFlowers <- Islands[, c(3:6, 21)]
Flowers <- pivot_wider(data = TempFlowers, id_cols = c(Watershed, Shrub_id, Treatment), names_from = RecYear, values_from = EstTotalFloweringStems, names_glue = "EstFloweringStems{RecYear}")

# x many flowering stems in a transect of y area
# if the total shrub area is z, then there are a flowering stems in the whole shrub

# Include shrub size
Flowers <- left_join(Flowers, select(Area, Shrub_id, Size2019, Size2018), by = "Shrub_id")

# This regression line ignores those values where there were 0 flowering stems
ggplot(Flowers, aes(x = Size2018, y = EstFloweringStems2018)) +
  geom_point()+
  geom_smooth(data = subset(Flowers, EstFloweringStems2018 != 0), method = "lm")

# Need to combine both years of data
# FlowerRegression <- lm(EstFloweringStems ~ Size, data = Flowers)
###################################################################################
# Inflorescence information from Table S4
# Number of inflorescences per stem

Inflorescences <- data.frame(
  Watershed = c("20C", "20C", "4B", "4B"),
  Treatment = c("browsed", "control", "browsed", "control"),
  MeanInflorescences2018 = c(0.03, 31.33, 0.00, 0.84),
  SEInflorescences2018 = c(0.03, 5.07, 0.00, 0.51),
  MeanInflorescences2019 = c(0.05, 30.69, 0.00, 4.95),
  SEInflorescences2019 = c(0.04, 5.02, 0.00, 1.30)
)

# Number of inflorescences per shrub island

# try making a loop - for every flowering stems quantity, match the watershed, treatment, and year
# multiply the corresponding EstFloweringStems2019 by the mean - SE and mean + SE
# is this a reasonable way to get a range of possible inflorescence numbers?
# does it even matter if there's a range? Just use the mean?
# Or ask Wedel for the raw data so I can pick from a histogram?
Flowers$EstInflorescences2019 <- vector(length = nrow(Flowers))
Flowers$EstInflorescences2018 <- vector(length = nrow(Flowers))

for(i in 1:nrow(Flowers)) {
  TreatmentMatch <- sapply(Inflorescences$Treatment, identical, Flowers$Treatment[[i]])
  WatershedMatch <- sapply(Inflorescences$Watershed, identical, Flowers$Watershed[[i]])
  Flowers$EstInflorescences2019[i] <- 
    Inflorescences$MeanInflorescences2019[TreatmentMatch & WatershedMatch] *
    Flowers$EstFloweringStems2019[i]
  }

for(i in 1:nrow(Flowers)) {
  TreatmentMatch <- sapply(Inflorescences$Treatment, identical, Flowers$Treatment[[i]])
  WatershedMatch <- sapply(Inflorescences$Watershed, identical, Flowers$Watershed[[i]])
  Flowers$EstInflorescences2018[i] <- 
    Inflorescences$MeanInflorescences2018[TreatmentMatch & WatershedMatch] *
    Flowers$EstFloweringStems2018[i]
}

ggplot(Flowers, aes(x = Size2018, y = EstInflorescences2018)) +
  geom_point()+
  geom_smooth(method = "lm")

ggplot(Flowers, aes(x = Size2019, y = EstInflorescences2019)) +
  geom_point()+
  geom_smooth(method = "lm")

# Save reproduction data to combine with Schulstrom
write.csv(Flowers, "Wedel Reproduction.csv")