# Prepare data frame for analysis
# Spring 2026 Roughleaf Dogwood germination
# May 6, 2026
# Sophie Wieland

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Germination Analysis")
library(dplyr)
library(tidyverse)

Germination <- read.csv("GerminationData.csv")
SeedCount <- read.csv("PetriSeedCount.csv")
ThinningCount <- read.csv("Thinning Count.csv")
ThinningfromPhotos <- read.csv("ThinfromPhotosCounting.csv")
Extra <- read.csv("ExtraSeedlingCount.csv")
Floater <- read.csv("Floater.csv")
Transplant <- read.csv("TransplantCount.csv")
PotTransplant <- read.csv("PotTransplant.csv")

# fill in Mother and Scarification columns based on petri dish ID

Germination$Mother <- str_extract_all(Germination$Flat, "[A-Z]+")
Germination$Scarification <- str_extract_all(Germination$Flat, "[c, r, s]+")
Germination$Floater <- str_extract(Germination$Flat, "[f]+")

SeedCount$Mother <- str_extract_all(SeedCount$Petri.Dish, "[A-Z]+")
SeedCount$Scarification <- str_extract_all(SeedCount$Petri.Dish, "[c, r, s]+")
SeedCount$Floater <- str_extract(SeedCount$Petri.Dish, "[f]+")

# Seed counts were done at two times (between scarification and stratification, and between stratification and planting)
# If seeds are known to be lost over the course of stratification, use the second count
# If count was impossible at the second time point because of mold or significant germination, use first count
# Otherwise, use an average of the two counts
# Round to whole numbers in the direction of the second count

AverageRows <- c(1:26, 28:38, 40:86, 88:92, 94:117, 119:120, 122:128, 130:164, 166:234, 236:258)

SeedCount$FinalCount <- NA

Average <- (SeedCount$Initial.Seed.Count[AverageRows] +
              SeedCount$Seed.Count.at.Planting[AverageRows]) / 2

SeedCount$FinalCount[AverageRows] <- ifelse(
  SeedCount$Seed.Count.at.Planting[AverageRows] >= Average,
  ceiling(Average),
  floor(Average))

SeedCount$FinalCount[259] <- (SeedCount$Initial.Seed.Count[259] - 1)
SeedCount$FinalCount[27] <- (SeedCount$Initial.Seed.Count[27] - 2)
SeedCount$FinalCount[165] <- (SeedCount$Seed.Count.at.Planting[165] - 1)

# Initial counts
InitialRows <- c(87, 118, 121, 129)

SeedCount$FinalCount[InitialRows] <- SeedCount$Initial.Seed.Count[InitialRows]

# Second counts
SecondRows <- c(39, 93, 235)

SeedCount$FinalCount[SecondRows] <- SeedCount$Seed.Count.at.Planting[SecondRows]

# Add seed counts

SeedCounts <- vector(length = nrow(Germination))

for(i in 1:nrow(Germination)) {
  
  ID <- match(Germination$Flat[i], SeedCount$Petri.Dish)
  
  if(!is.na(ID)) {
    
    SeedCounts[i] <- SeedCount$FinalCount[ID]
    
  } else {mother_match <- sapply(
    SeedCount$Mother,
    identical,
    Germination$Mother[[i]]
  )
  
  scar_match <- sapply(
    SeedCount$Scarification,
    identical,
    Germination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    SeedCount$Floater,
    identical,
    Germination$Floater[[i]]
  )
  
  SeedCounts[i] <- sum(SeedCount$FinalCount[mother_match & scar_match & floater_match])
    
    }}

Germination$SeedCount <- SeedCounts

# Add thinning counts

Germination$LiveThinned <- ThinningCount$Thinned..number.live.[
  match(Germination$Flat, ThinningCount$ID)]

Germination$LiveThinnedPhotos <- ThinningfromPhotos$ThinSum[
  match(Germination$Flat, ThinningfromPhotos$Petri.Dish)]

Germination$DeadThinned <- ThinningCount$Removed..number.dead.[
  match(Germination$Flat, ThinningCount$ID)]

Germination$DeadThinnedPhotos <- ThinningfromPhotos$Dead[
  match(Germination$Flat, ThinningfromPhotos$Petri.Dish)]

Germination$LiveExtras <- Extra$Number.live[
  match(Germination$Flat, Extra$ID)]

Germination$LiveFloaters <- Floater$Germinant.Number[
  match(Germination$Flat, Floater$ID)]

Germination$DeadExtras <- Extra$Number.dead[
  match(Germination$Flat, Extra$ID)]

Germination$DeadFloaters <- Floater$Dead[
  match(Germination$Flat, Floater$ID)]

Germination$PotTransplant <- PotTransplant$Number.Transplanted[
  match(Germination$Flat, PotTransplant$ID)]

# LiveTransplant is the number remaining after transplanting to the pots
# Not the number originally transplanted
# This scheme may artificially raise the early death value
# (i.e. they may have died because of transplant stress, not that they were going to die anyway)

Germination$LiveTransplant <- Transplant$Number.live[
  match(Germination$Flat, Transplant$ID)]

Germination$DeadTransplant <- Transplant$Number.dead[
  match(Germination$Flat, Transplant$ID)]

# add zeros instead of NAs in columns

Germination$LiveThinned[is.na(Germination$LiveThinned)] <- 0
Germination$DeadThinned[is.na(Germination$DeadThinned)] <- 0
Germination$LiveThinnedPhotos[is.na(Germination$LiveThinnedPhotos)] <- 0
Germination$DeadThinnedPhotos[is.na(Germination$DeadThinnedPhotos)] <- 0
Germination$LiveExtras[is.na(Germination$LiveExtras)] <- 0
Germination$DeadExtras[is.na(Germination$DeadExtras)] <- 0
Germination$LiveFloaters[is.na(Germination$LiveFloaters)] <- 0
Germination$DeadFloaters[is.na(Germination$DeadFloaters)] <- 0
Germination$LiveTransplant[is.na(Germination$LiveTransplant)] <- 0
Germination$DeadTransplant[is.na(Germination$DeadTransplant)] <- 0
Germination$PotTransplant[is.na(Germination$PotTransplant)] <- 0

# remove the bottom rows which have no data

Germination <- head(Germination, -17)

# remove lists from data frame

Germination <- apply(Germination, 2, as.character)

# export as csv

write.csv(Germination, "CleanedGerminationData.csv")   
