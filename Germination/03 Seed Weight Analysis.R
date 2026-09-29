# Continue Analyzing Germination Data
# Seed Weight
# Spring 2026 Roughleaf Dogwood germination
# June 18, 2026
# Sophie Wieland

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Germination Analysis")
library(ggplot2)
library(dplyr)
library(tidyverse)

Germ <- read.csv("MuchGerminationData.csv")
SeedWeightbyScar <- read.csv("SeedWeightbyScarMethod.csv")
TotalSeedWeight <- read.csv("TotalSeedWeight.csv")
SeedCounts <- read.csv("PetriSeedCount.csv")

SeedCounts$Mother <- str_extract_all(SeedCounts$Petri.Dish, "[A-Z]+")
SeedCounts$Scarification <- str_extract_all(SeedCounts$Petri.Dish, "[c, r, s]+")
SeedCounts$Floater <- str_extract(SeedCounts$Petri.Dish, "[f]+")

SeedWeightbyScar$Mother <- str_extract_all(SeedWeightbyScar$Sample, "[A-Z]+")
SeedWeightbyScar$Scarification <- str_extract_all(SeedWeightbyScar$Sample, "[c, r, s]+")
SeedWeightbyScar$Floater <- str_extract(SeedWeightbyScar$Sample, "[f]+")

SeedCount <- vector(length = nrow(SeedWeightbyScar))

for(i in 1:nrow(SeedWeightbyScar)) {
  
  ID <- match(SeedWeightbyScar$Sample[i], Germ$Flat)
  
  if(!is.na(ID)) {
    
    SeedCount[i] <- Germ$SeedCount[ID]
    
  } else {mother_match <- sapply(
    Germ$Mother,
    identical,
    SeedWeightbyScar$Mother[[i]]
  )
  
  scar_match <- sapply(
    Germ$Scarification,
    identical,
    SeedWeightbyScar$Scarification[[i]]
  )
  
  floater_match <- sapply(
    Germ$Floater,
    identical,
    SeedWeightbyScar$Floater[[i]]
  )
  
  SeedCount[i] <- sum(Germ$SeedCount[mother_match & scar_match & floater_match])
  
  }}

SeedWeightbyScar$SeedCount <- SeedCount

InitialCount <- vector(length = nrow(SeedWeightbyScar))

for(i in 1:nrow(SeedWeightbyScar)) {
  
  ID <- match(SeedWeightbyScar$Sample[i], SeedCounts$Petri.Dish)
  
  if(!is.na(ID)) {
    
    InitialCount[i] <- SeedCounts$Initial.Seed.Count[ID]
    
  } else {mother_match <- sapply(
    SeedCounts$Mother,
    identical,
    SeedWeightbyScar$Mother[[i]]
  )
  
  scar_match <- sapply(
    SeedCounts$Scarification,
    identical,
    SeedWeightbyScar$Scarification[[i]]
  )
  
  floater_match <- sapply(
    SeedCounts$Floater,
    identical,
    SeedWeightbyScar$Floater[[i]]
  )
  
  InitialCount[i] <- sum(SeedCounts$Initial.Seed.Count[mother_match & scar_match & floater_match])
  
  }}

SeedWeightbyScar$InitialCount <- InitialCount
# Seed weights were only taken once, at the beginning
# Seed weight was calculated from the initial quantity of seeds
# Germination rates calculated from the averaged final quantity of seeds

# Individual seed size
SeedWeightbyScar$SeedSize <- (SeedWeightbyScar$Weight.of.Sample..g./SeedWeightbyScar$InitialCount)

# What is happening with fQc???? I have no notes that seeds were lost during treatment
# But for having the same weight as the other fQ treatments, it should have a similar quantity of seeds...
# Exclude from analysis?

Germ$Germinants <- (Germ$LiveThinned + Germ$DeadThinned +
                      Germ$LiveThinnedPhotos + Germ$DeadThinnedPhotos +
                      Germ$LiveExtras + Germ$DeadExtras +
                      Germ$LiveFloaters + Germ$DeadFloaters +
                      Germ$LiveTransplant + Germ$DeadTransplant +
                      Germ$PotTransplant)
Germ$SurvGerminants <- (Germ$LiveThinned + 
                          Germ$LiveThinnedPhotos + 
                          Germ$LiveExtras + 
                          Germ$LiveFloaters + 
                          Germ$LiveTransplant +
                          Germ$PotTransplant)

Germinants <- vector(length = nrow(SeedWeightbyScar))

for(i in 1:nrow(SeedWeightbyScar)) {
  
  ID <- match(SeedWeightbyScar$Sample[i], Germ$Flat)
  
  if(!is.na(ID)) {
    
    Germinants[i] <- Germ$Germinants[ID]
    
  } else {mother_match <- sapply(
    Germ$Mother,
    identical,
    SeedWeightbyScar$Mother[[i]]
  )
  
  scar_match <- sapply(
    Germ$Scarification,
    identical,
    SeedWeightbyScar$Scarification[[i]]
  )
  
  floater_match <- sapply(
    Germ$Floater,
    identical,
    SeedWeightbyScar$Floater[[i]]
  )
  
  Germinants[i] <- sum(Germ$Germinants[mother_match & scar_match & floater_match])
  
  }}

SeedWeightbyScar$Germinants <- Germinants

SurvGerminants <- vector(length = nrow(SeedWeightbyScar))

for(i in 1:nrow(SeedWeightbyScar)) {
  
  ID <- match(SeedWeightbyScar$Sample[i], Germ$Flat)
  
  if(!is.na(ID)) {
    
    SurvGerminants[i] <- Germ$SurvGerminants[ID]
    
  } else {mother_match <- sapply(
    Germ$Mother,
    identical,
    SeedWeightbyScar$Mother[[i]]
  )
  
  scar_match <- sapply(
    Germ$Scarification,
    identical,
    SeedWeightbyScar$Scarification[[i]]
  )
  
  floater_match <- sapply(
    Germ$Floater,
    identical,
    SeedWeightbyScar$Floater[[i]]
  )
  
  SurvGerminants[i] <- sum(Germ$SurvGerminants[mother_match & scar_match & floater_match])
  
  }}

SeedWeightbyScar$SurvGerminants <- SurvGerminants

SeedWeightbyScar$GermRate <- (SeedWeightbyScar$Germinants/SeedWeightbyScar$SeedCount)

SeedWeightbyScar$SurvGermRate <- (SeedWeightbyScar$SurvGerminants/SeedWeightbyScar$SeedCount)

ggplot(SeedWeightbyScar, aes(GermRate, SeedSize))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(SeedWeightbyScar, aes(SurvGermRate, SeedSize))+
  geom_point()+
  geom_smooth(method = lm)
