# Preparing data from O'Connor 2020
# Sophie Wieland
library(dplyr)
library(tidyr)
getwd()

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

Islands <- read.csv("OConnor Area and Density.csv")

# Isolate pertinent columns
Growth <- Islands[, c(1:5)]

# Flip data so there is one row per shrub island

Growth <- group_by(Growth, Watershed, Island, Treatment)
Growth <- mutate(Growth, ID = cur_group_id())
Growth <- ungroup(Growth)
Area <- pivot_wider(data = Growth, id_cols = c(ID, Watershed, Island, Treatment), names_from = Year, values_from = Est_Size_m2, names_glue = "size{Year}")
# Add columns showing that all shrubs existed every year
Area$exist2015 <- as.integer(!is.na(Area$size2015))
Area$exist2016 <- as.integer(!is.na(Area$size2016))
Area$exist2017 <- as.integer(!is.na(Area$size2017))
# Reorganize, just so the years/columns are chronological
Area <- relocate(Area, exist2015, .after = size2015)
Area <- relocate(Area, exist2016, .after = size2016)
Area <- relocate(Area, exist2017, .after = size2017)

write.csv(Area, "OConnor Growth.csv")