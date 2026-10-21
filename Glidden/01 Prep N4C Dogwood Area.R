# Prep N4C (Glidden) Dogwood Areas
# October 7, 2026
# Sophie Wieland
getwd()
library(dplyr)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building/Glidden")

DogwoodArea <- read.csv("N4CDogwoodArea.csv")
DogwoodNotes <- read.csv("Dogwood ID datasheet.csv")

# Keep only necessary columns
DogwoodArea <- select(DogwoodArea, c('Id', 'Watershed', 'BurnSeason', 'Area_m2023', 'Area_m2024',
                                      'Area_m2025', 'Area_m2026', 'Change23_24', 'Change24_25', 'Change25_26'))

# Rename column to SubWatershed, fix one naming problem
DogwoodArea <- rename(DogwoodArea, SubWatershed = Watershed)
DogwoodArea$SubWatershed <- replace(DogwoodArea$SubWatershed, DogwoodArea$SubWatershed=="su1r", "suf1r")

# Add columns for Watershed, FRI, Grazer (Bison or exclosure)
DogwoodArea$Watershed <- "N4C"
DogwoodArea$FRI <- 1
DogwoodArea$Graze <- ifelse(DogwoodNotes$Exclosure[match(DogwoodArea$Id, DogwoodNotes$Shrub.ID)] == "yes", "ungrazed", "bison")
DogwoodArea$Fruit <- ifelse(DogwoodNotes$Flowering[match(DogwoodArea$Id, DogwoodNotes$Shrub.ID)] == "yes", 1, 0)

# Remove rows for shrubs that are too blob-like to use (i.e. growing into other islands)
# This is a list of the shrubs excluded from Arc GIS
ExcludeShrubs <- c(5, 22, 23, 25, 26, 33, 258, 259, 260, 261, 297, 298, 318, 319, 355, 357)
# Make a list of shrubs to exclude for other reasons? I.e. they were in mowing treatment?
MowShrubs <- 

DogwoodArea <- DogwoodArea[!DogwoodArea$Id %in% ExcludeShrubs, ]

write.csv(DogwoodArea, "ProcessedN4CDogwood.csv")