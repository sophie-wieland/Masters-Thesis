# Comparing the data Alec collected to my shrub tracing
# August 31, 2026
# Sophie Wieland
getwd()
library(dplyr)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building/Glidden")

Dogwood2023 <- read.csv("Dogwood2023.csv")
Dogwood2024 <- read.csv("Dogwood2024.csv")
Dogwood2025 <- read.csv("Dogwood2025.csv")
Dogwood2026 <- read.csv("Dogwood2026.csv")

# Standardize ID (in 2024 Alec used ID,000 and in 2025 he used ID,011)

Dogwood2024$Id <- substr(Dogwood2024$Id, 1, nchar(Dogwood2024$Id) - 3)
Dogwood2025$Id <- substr(Dogwood2025$Id, 1, nchar(Dogwood2025$Id) - 3)

# some renaming so the dataframes can be combined
Dogwood2024$Id <- as.integer(Dogwood2024$Id)
Dogwood2025$Id <- as.integer(Dogwood2025$Id)
Dogwood2026 <- rename(Dogwood2026, Id = AlecID, Area_m2026 = Area_m)
Dogwood2025 <- rename(Dogwood2025, Area_m2025 = Area_m)
Dogwood2024 <- rename(Dogwood2024, Area_m2024 = Area_m)
Dogwood2023 <- rename(Dogwood2023, Area_m2023 = Area_m)

Combine <- left_join(Dogwood2023, select(Dogwood2024, "Id", "Area_m2024"), by = "Id")
MoreCombining <- left_join(Combine, select(Dogwood2025, "Id", "Area_m2025"), by = "Id")
DogwoodArea <- left_join(MoreCombining, select(Dogwood2026, "Id", "Area_m2026"), by = "Id")

DogwoodArea$Change23_24 <- DogwoodArea$Area_m2024 - DogwoodArea$Area_m2023
DogwoodArea$Change24_25 <- DogwoodArea$Area_m2025 - DogwoodArea$Area_m2024
DogwoodArea$Change25_26 <- DogwoodArea$Area_m2026 - DogwoodArea$Area_m2025

# means?
mean(DogwoodArea$Change23_24, na.rm = TRUE)
mean(DogwoodArea$Change24_25, na.rm = TRUE)
mean(DogwoodArea$Change25_26, na.rm = TRUE)

# anova?
StackedData <- stack(DogwoodArea[, c("Change23_24", "Change24_25", "Change25_26")])
ChangeANOVA <- aov(values ~ ind, data = StackedData)
summary(ChangeANOVA)
# At least one group is stat different from others
TukeyHSD(ChangeANOVA)
# 24-25 and 23-24 are different (meaningless for me)
# 25-26 and 24-25 are different (potentially problematic, but 25-26 and 23-24 aren't different)

write.csv(DogwoodArea, "N4CDogwoodArea.csv")