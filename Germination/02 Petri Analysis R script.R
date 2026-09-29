# Continue Analyzing Germination Data
# Mold data, petri density, etc.
# Spring 2026 Roughleaf Dogwood germination
# June 10, 2026
# Sophie Wieland

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Germination Analysis")
library(ggplot2)
library(dplyr)
library(tidyverse)

CleanedGermination <- read.csv("CleanedGerminationData.csv")
PetriData <- read.csv("Density_Mold_Peroxide.csv")
FlatSizes <- read.csv("FlatSizes.csv")
###############################################################################
CleanedGermination <- CleanedGermination[ ,-18:-19]
CleanedGermination$FlatSize <- FlatSizes$Size[
  match(CleanedGermination$Flat, FlatSizes$Flat)]

PetriData$Mother <- str_extract_all(PetriData$Petri.Dish, "[A-Z]+")
PetriData$Scarification <- str_extract_all(PetriData$Petri.Dish, "[c, r, s]+")
PetriData$Floater <- str_extract(PetriData$Petri.Dish, "[f]+")

Mold <- vector(length = nrow(CleanedGermination))

for(i in 1:nrow(CleanedGermination)) {
  
  ID <- match(CleanedGermination$Flat[i], PetriData$Petri.Dish)
  
  if(!is.na(ID)) {
    
    Mold[i] <- PetriData$Mold[ID]
    
  } else {mother_match <- sapply(
    PetriData$Mother,
    identical,
    CleanedGermination$Mother[[i]]
  )
  
  scar_match <- sapply(
    PetriData$Scarification,
    identical,
    CleanedGermination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    PetriData$Floater,
    identical,
    CleanedGermination$Floater[[i]]
  )
  
  Mold[i] <- max(PetriData$Mold[mother_match & scar_match & floater_match])
  
  }}

CleanedGermination$Mold <- Mold

Peroxide <- vector(length = nrow(CleanedGermination))

for(i in 1:nrow(CleanedGermination)) {
  
  ID <- match(CleanedGermination$Flat[i], PetriData$Petri.Dish)
  
  if(!is.na(ID)) {
    
    Peroxide[i] <- PetriData$Peroxide[ID]
    
  } else {mother_match <- sapply(
    PetriData$Mother,
    identical,
    CleanedGermination$Mother[[i]]
  )
  
  scar_match <- sapply(
    PetriData$Scarification,
    identical,
    CleanedGermination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    PetriData$Floater,
    identical,
    CleanedGermination$Floater[[i]]
  )
  
  Peroxide[i] <- max(PetriData$Peroxide[mother_match & scar_match & floater_match])
  
  }}

CleanedGermination$Peroxide <- Peroxide

FridgeSprout <- vector(length = nrow(CleanedGermination))

for(i in 1:nrow(CleanedGermination)) {
  
  ID <- match(CleanedGermination$Flat[i], PetriData$Petri.Dish)
  
  if(!is.na(ID)) {
    
    FridgeSprout[i] <- PetriData$FridgeSprout[ID]
    
  } else {mother_match <- sapply(
    PetriData$Mother,
    identical,
    CleanedGermination$Mother[[i]]
  )
  
  scar_match <- sapply(
    PetriData$Scarification,
    identical,
    CleanedGermination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    PetriData$Floater,
    identical,
    CleanedGermination$Floater[[i]]
  )
  
  FridgeSprout[i] <- max(PetriData$FridgeSprout[mother_match & scar_match & floater_match])
  
  }}

CleanedGermination$FridgeSprout <- FridgeSprout

MoldScore <- vector(length = nrow(CleanedGermination))

for(i in 1:nrow(CleanedGermination)) {
  
  ID <- match(CleanedGermination$Flat[i], PetriData$Petri.Dish)
  
  if(!is.na(ID)) {
    
    MoldScore[i] <- PetriData$Mold.Score[ID]
    
  } else {mother_match <- sapply(
    PetriData$Mother,
    identical,
    CleanedGermination$Mother[[i]]
  )
  
  scar_match <- sapply(
    PetriData$Scarification,
    identical,
    CleanedGermination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    PetriData$Floater,
    identical,
    CleanedGermination$Floater[[i]]
  )
  
  MoldScore[i] <- max(PetriData$Mold.Score[mother_match & scar_match & floater_match])
  
  }}

CleanedGermination$MoldScore <- MoldScore

# I summed the area of the petri dishes if they were later combined
# Perhaps not the most accurate, but closest I can do to calculate density effects

PetriSize <- vector(length = nrow(CleanedGermination))

for(i in 1:nrow(CleanedGermination)) {
  
  ID <- match(CleanedGermination$Flat[i], PetriData$Petri.Dish)
  
  if(!is.na(ID)) {
    
    PetriSize[i] <- PetriData$Size..cm2.[ID]
    
  } else {mother_match <- sapply(
    PetriData$Mother,
    identical,
    CleanedGermination$Mother[[i]]
  )
  
  scar_match <- sapply(
    PetriData$Scarification,
    identical,
    CleanedGermination$Scarification[[i]]
  )
  
  floater_match <- sapply(
    PetriData$Floater,
    identical,
    CleanedGermination$Floater[[i]]
  )
  
  PetriSize[i] <- sum(PetriData$Size..cm2.[mother_match & scar_match & floater_match])
  
  }}

CleanedGermination$PetriSize <- PetriSize
################################################################################
# Effects of Mold
# Most were moldy to some extent, so likely don't have enough non-moldy to say with certainty

Mold <- c(0, 1)
MoldGermination <- vector(length = length(Mold))

for(i in 1:length(Mold)) {
  SubData <- CleanedGermination[CleanedGermination$Mold == Mold[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                   SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                   SubData$LiveExtras + SubData$DeadExtras +
                   SubData$LiveFloaters + SubData$DeadFloaters +
                   SubData$LiveTransplant + SubData$DeadTransplant +
                   SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  MoldGermination[i] <- AverageInitialGermination}

MoldGerminationD <- data.frame(Mold, MoldGermination)

ggplot(MoldGerminationD, aes(Mold, MoldGermination))+
  geom_col()

MoldSurvGermination <- vector(length = length(Mold))

for(i in 1:length(Mold)) {
  SubData <- CleanedGermination[CleanedGermination$Mold == Mold[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                       SubData$LiveThinnedPhotos + 
                       SubData$LiveExtras + 
                       SubData$LiveFloaters + 
                       SubData$LiveTransplant + 
                       SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  MoldSurvGermination[i] <- AverageSurvGermination}

MoldSurvGerminationD <- data.frame(Mold, MoldSurvGermination)

ggplot(MoldSurvGerminationD, aes(Mold, MoldSurvGermination))+
  geom_col()

CombinedMold <- data.frame(Mold, MoldGermination, MoldSurvGermination)
CombinedMold <- pivot_longer(CombinedMold, cols = c(MoldGermination, MoldSurvGermination))

ggplot(CombinedMold, aes(Mold, value, fill = name))+
  geom_bar(position = "dodge", stat = "identity")
################################################################################
# Effects of Peroxide
# Again, many were treated with peroxide, so there might not be enough not treated to draw solid conclusions

Peroxide <- c(0, 1)
PeroxideGermination <- vector(length = length(Peroxide))

for(i in 1:length(Peroxide)) {
  SubData <- CleanedGermination[CleanedGermination$Peroxide == Peroxide[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                   SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                   SubData$LiveExtras + SubData$DeadExtras +
                   SubData$LiveFloaters + SubData$DeadFloaters +
                   SubData$LiveTransplant + SubData$DeadTransplant +
                   SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  PeroxideGermination[i] <- AverageInitialGermination}

PeroxideGerminationD <- data.frame(Peroxide, PeroxideGermination)

ggplot(PeroxideGerminationD, aes(Peroxide, PeroxideGermination))+
  geom_col()

PeroxideSurvGermination <- vector(length = length(Peroxide))

for(i in 1:length(Peroxide)) {
  SubData <- CleanedGermination[CleanedGermination$Peroxide == Peroxide[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                       SubData$LiveThinnedPhotos + 
                       SubData$LiveExtras + 
                       SubData$LiveFloaters + 
                       SubData$LiveTransplant + 
                       SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  PeroxideSurvGermination[i] <- AverageSurvGermination}

PeroxideSurvGerminationD <- data.frame(Peroxide, PeroxideSurvGermination)

ggplot(PeroxideSurvGerminationD, aes(Peroxide, PeroxideSurvGermination))+
  geom_col()

CombinedPeroxide <- data.frame(Peroxide, PeroxideGermination, PeroxideSurvGermination)
CombinedPeroxide <- pivot_longer(CombinedPeroxide, cols = c(PeroxideGermination, PeroxideSurvGermination))

ggplot(CombinedPeroxide, aes(Peroxide, value, fill = name))+
  geom_bar(position = "dodge", stat = "identity")
################################################################################
# Mold Score - is worse mold worse?

MoldScore <- c(0:2)
MoldScoreGermination <- vector(length = length(MoldScore))

for(i in 1:length(MoldScore)) {
  SubData <- CleanedGermination[CleanedGermination$MoldScore == MoldScore[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                   SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                   SubData$LiveExtras + SubData$DeadExtras +
                   SubData$LiveFloaters + SubData$DeadFloaters +
                   SubData$LiveTransplant + SubData$DeadTransplant +
                   SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  MoldScoreGermination[i] <- AverageInitialGermination}

MoldScoreGerminationD <- data.frame(MoldScore, MoldScoreGermination)
ggplot(MoldScoreGerminationD, aes(MoldScore, MoldScoreGermination))+
  geom_col()

MoldScoreSurvGermination <- vector(length = length(MoldScore))

for(i in 1:length(MoldScore)) {
  SubData <- CleanedGermination[CleanedGermination$MoldScore == MoldScore[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                       SubData$LiveThinnedPhotos + 
                       SubData$LiveExtras + 
                       SubData$LiveFloaters + 
                       SubData$LiveTransplant + 
                       SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  MoldScoreSurvGermination[i] <- AverageSurvGermination}

MoldScoreSurvGerminationD <- data.frame(MoldScore, MoldScoreSurvGermination)

ggplot(MoldScoreSurvGerminationD, aes(MoldScore, MoldScoreSurvGermination))+
  geom_col()

CombinedMoldScore <- data.frame(MoldScore, MoldScoreGermination, MoldScoreSurvGermination)
CombinedMoldScore <- pivot_longer(CombinedMoldScore, cols = c(MoldScoreGermination, MoldScoreSurvGermination))

ggplot(CombinedMoldScore, aes(MoldScore, value, fill = name))+
  geom_bar(position = "dodge", stat = "identity")
################################################################################
# Fridge Sprout - is germination improved if seeds started germinating in the fridge?

FridgeSprout <- c(0:3)
FridgeSproutGermination <- vector(length = length(FridgeSprout))

for(i in 1:length(FridgeSprout)) {
  SubData <- CleanedGermination[CleanedGermination$FridgeSprout == FridgeSprout[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                   SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                   SubData$LiveExtras + SubData$DeadExtras +
                   SubData$LiveFloaters + SubData$DeadFloaters +
                   SubData$LiveTransplant + SubData$DeadTransplant +
                   SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  FridgeSproutGermination[i] <- AverageInitialGermination}

FridgeSproutGerminationD <- data.frame(FridgeSprout, FridgeSproutGermination)
ggplot(FridgeSproutGerminationD, aes(FridgeSprout, FridgeSproutGermination))+
  geom_col()

FridgeSproutSurvGermination <- vector(length = length(FridgeSprout))

for(i in 1:length(FridgeSprout)) {
  SubData <- CleanedGermination[CleanedGermination$FridgeSprout == FridgeSprout[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                       SubData$LiveThinnedPhotos + 
                       SubData$LiveExtras + 
                       SubData$LiveFloaters + 
                       SubData$LiveTransplant + 
                       SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  FridgeSproutSurvGermination[i] <- AverageSurvGermination}

FridgeSproutSurvGerminationD <- data.frame(FridgeSprout, FridgeSproutSurvGermination)
ggplot(FridgeSproutSurvGerminationD, aes(FridgeSprout, FridgeSproutSurvGermination))+
  geom_col()

CombinedFridgeSprout <- data.frame(FridgeSprout, FridgeSproutGermination, FridgeSproutSurvGermination)
CombinedFridgeSprout <- pivot_longer(CombinedFridgeSprout, cols = c(FridgeSproutGermination, FridgeSproutSurvGermination))

ggplot(CombinedFridgeSprout, aes(FridgeSprout, value, fill = name))+
  geom_bar(position = "dodge", stat = "identity")

################################################################################
# Density
# is germination hindered or improved by seed density, either during stratification or planting?

# First, petri density
# Seed number per cm2
CleanedGermination$PetriDensity <- (CleanedGermination$SeedCount/CleanedGermination$PetriSize)
# Flat Density
# Seed number per cm2
CleanedGermination$FlatDensity <- (CleanedGermination$SeedCount/CleanedGermination$FlatSize)

CleanedGermination$GermRate <- ((CleanedGermination$LiveThinned + CleanedGermination$DeadThinned +
                      CleanedGermination$LiveThinnedPhotos + CleanedGermination$DeadThinnedPhotos +
                      CleanedGermination$LiveExtras + CleanedGermination$DeadExtras +
                      CleanedGermination$LiveFloaters + CleanedGermination$DeadFloaters +
                      CleanedGermination$LiveTransplant + CleanedGermination$DeadTransplant +
                      CleanedGermination$PotTransplant) / CleanedGermination$SeedCount)
CleanedGermination$SurvGermRate <- ((CleanedGermination$LiveThinned + 
                     CleanedGermination$LiveThinnedPhotos + 
                     CleanedGermination$LiveExtras + 
                     CleanedGermination$LiveFloaters + 
                     CleanedGermination$LiveTransplant +
                     CleanedGermination$PotTransplant) / CleanedGermination$SeedCount)

ggplot(CleanedGermination, aes(GermRate, PetriDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(CleanedGermination, aes(GermRate, FlatDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(CleanedGermination, aes(SurvGermRate, PetriDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(CleanedGermination, aes(SurvGermRate, FlatDensity))+
  geom_point()+
  geom_smooth(method = lm)

DenseSeeds <- CleanedGermination[is.na(CleanedGermination$Floater), ]

ggplot(DenseSeeds, aes(GermRate, PetriDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(DenseSeeds, aes(GermRate, FlatDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(DenseSeeds, aes(SurvGermRate, PetriDensity))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(DenseSeeds, aes(SurvGermRate, FlatDensity))+
  geom_point()+
  geom_smooth(method = lm)

################################################################################
# Does peroxide treatment keep mold to a dull roar?
ggplot(CleanedGermination, aes(Peroxide, MoldScore))+
  geom_point(position="jitter")

# Hmmm, how to properly test this - problem is, there are way fewer that were not peroxide treated
# And all that were not moldy were not peroxide treated, of course
# 0, 1, or 2 - proportion 2:1 in the peroxide and non peroxide treatments

PeroxideProportion <- vector(length = length(Peroxide))

for(i in 1:length(Peroxide)) {
  SubData <- CleanedGermination[CleanedGermination$Peroxide == Peroxide[i], ]
  HeavyMold <- sum(SubData$MoldScore == 2)
  LighterMold <- sum(SubData$MoldScore == 1)
  Proportion <- (HeavyMold / LighterMold)
  PeroxideProportion[i] <- Proportion}

PeroxideProportionD <- data.frame(Peroxide, PeroxideProportion)

ggplot(CleanedGermination, aes(Scarification, MoldScore))+
  geom_point(position="jitter")
################################################################################
# Does the number/proportion of early deaths correspond to mold?

CleanedGermination$EarlyDeaths <- (CleanedGermination$DeadThinned +
                                   CleanedGermination$DeadThinnedPhotos +
                                   CleanedGermination$DeadExtras +
                                   CleanedGermination$DeadFloaters +
                                   CleanedGermination$DeadTransplant)
CleanedGermination$EarlyDeathsRate <- ((CleanedGermination$DeadThinned +
                                          CleanedGermination$DeadThinnedPhotos +
                                          CleanedGermination$DeadExtras +
                                          CleanedGermination$DeadFloaters +
                                          CleanedGermination$DeadTransplant) / CleanedGermination$SeedCount)

ggplot(CleanedGermination, aes(EarlyDeaths, MoldScore))+
  geom_point(position="jitter")

write.csv(CleanedGermination, "MuchGerminationData.csv")  