# Comparing the data Alec collected to my shrub tracing
# August 31, 2026
# Sophie Wieland
getwd()
library(dplyr)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building/Glidden")

AlecData <- read.csv("GliddenPolygons.csv")
SophieData <- read.csv("FirstTrial2026Polygons.csv")

# Select only dogwood
AlecDogwood <- filter(AlecData, Species == "dogwood")
# Standardize ID (in 2024 Alec used ID,000 and in 2025 he used ID,011)

Temp2023 <- subset(AlecDogwood, format(as.Date(Date), "%Y") == "2023")
Temp2024 <- subset(AlecDogwood, format(as.Date(Date), "%Y") == "2024")
Temp2025 <- subset(AlecDogwood, format(as.Date(Date), "%Y") == "2025")

Temp2024$Id <- substr(Temp2024$Id, 1, nchar(Temp2024$Id) - 3)
Temp2025$Id <- substr(Temp2025$Id, 1, nchar(Temp2025$Id) - 3)

Temp2023 <- rename(Temp2023, Area2023 = SHAPE_Area,
                   QGISArea2023 = QGISarea)
Temp2024 <- rename(Temp2024, Area2024 = SHAPE_Area,
                   QGISArea2024 = QGISarea)
Temp2024$Id <- as.integer(Temp2024$Id)
Temp2025 <- rename(Temp2025, Area2025 = SHAPE_Area,
                   QGISArea2025 = QGISarea)
Temp2025$Id <- as.integer(Temp2025$Id)
SophieData <- rename(SophieData, Id = AlecID,
                     Area2026 = SHAPE_Area,
                     QGISArea2026 = GISarea)

Recombine <- left_join(Temp2023, select(Temp2024, "Id", "Area2024", "QGISArea2024"), by = "Id")
MoreCombining <- left_join(Recombine, select(Temp2025, "Id", "Area2025", "QGISArea2025"), by = "Id")
DogwoodArea <- left_join(MoreCombining, select(SophieData, "Id", "Area2026", "QGISArea2026"), by = "Id")

DogwoodArea$Change23_24 <- DogwoodArea$QGISArea2024 - DogwoodArea$QGISArea2023
DogwoodArea$Change24_25 <- DogwoodArea$QGISArea2025 - DogwoodArea$QGISArea2024
DogwoodArea$Change25_26 <- DogwoodArea$QGISArea2026 - DogwoodArea$QGISArea2025

# means?
mean(DogwoodArea$Change23_24, na.rm = TRUE)
mean(DogwoodArea$Change24_25, na.rm = TRUE)
mean(DogwoodArea$Change25_26, na.rm = TRUE)

# anova?
StackedData <- stack(DogwoodArea[, c("Change23_24", "Change24_25", "Change25_26")])
ChangeANOVA <- aov(values ~ ind, data = StackedData)
summary(ChangeANOVA)
TukeyHSD(ChangeANOVA)

# TOST test??