# Analyze Germination Data
# Spring 2026 Roughleaf Dogwood germination
# May 6, 2026
# Sophie Wieland

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Germination Analysis")
library(dplyr)
library(tidyr)
library(ggplot2)

CleanedGermination <- read.csv("CleanedGerminationData.csv")

TotalGerminants <- (CleanedGermination$LiveThinned + CleanedGermination$DeadThinned +
                      CleanedGermination$LiveThinnedPhotos + CleanedGermination$DeadThinnedPhotos +
                      CleanedGermination$LiveExtras + CleanedGermination$DeadExtras +
                      CleanedGermination$LiveFloaters + CleanedGermination$DeadFloaters +
                      CleanedGermination$LiveTransplant + CleanedGermination$DeadTransplant +
                      CleanedGermination$PotTransplant)
LiveGerminants <- (CleanedGermination$LiveThinned + 
                     CleanedGermination$LiveThinnedPhotos + 
                     CleanedGermination$LiveExtras + 
                     CleanedGermination$LiveFloaters + 
                     CleanedGermination$LiveTransplant +
                     CleanedGermination$PotTransplant)
SumTotalGerminants <- sum(TotalGerminants)
SumLiveGerminants <- sum(LiveGerminants)

# This calculates germination rate for every row
InitialGermination <- (TotalGerminants / CleanedGermination$SeedCount)
min(InitialGermination)
max(InitialGermination)
# Max Germination rate is 0.7980769, of Ns_5

# This calculates germination rate, excluding those that died shortly after sprouting, for every row
SurvGermination <- (LiveGerminants / CleanedGermination$SeedCount)
min(SurvGermination)
max(SurvGermination)
# Max surviving germination rate is 0.7046632, of Br

# This is the germination rate of all the data, pooled
AverageInitialGermination <- SumTotalGerminants/ (sum(CleanedGermination$SeedCount, na.rm = TRUE))
AverageSurvGermination <- SumLiveGerminants/ (sum(CleanedGermination$SeedCount, na.rm = TRUE))

# Make a loop to calculate germination by mother

Mother <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R")
MotherGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- CleanedGermination[CleanedGermination$Mother == Mother[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  MotherGermination[i] <- AverageInitialGermination}

MotherAverageGerminationD <- data.frame(Mother, MotherGermination)

MotherSurvGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- CleanedGermination[CleanedGermination$Mother == Mother[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                   SubData$LiveThinnedPhotos + 
                   SubData$LiveExtras + 
                   SubData$LiveFloaters + 
                   SubData$LiveTransplant + 
                   SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  MotherSurvGermination[i] <- AverageSurvGermination}

MotherSurvGerminationD <- data.frame(Mother, MotherSurvGermination)

# Calculate germination by scarification treatment

Scarification <- c("r", "s", "c")
ScarGermination <- vector(length = length(Scarification))

for(i in 1:length(Scarification)) {
  SubData <- CleanedGermination[CleanedGermination$Scarification == Scarification[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  ScarGermination[i] <- AverageInitialGermination}

ScarGerminationD <- data.frame(Scarification, ScarGermination)

ScarSurvGermination <- vector(length = length(Scarification))

for(i in 1:length(Scarification)) {
  SubData <- CleanedGermination[CleanedGermination$Scarification == Scarification[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                       SubData$LiveThinnedPhotos + 
                       SubData$LiveExtras + 
                       SubData$LiveFloaters + 
                       SubData$LiveTransplant + 
                       SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  ScarSurvGermination[i] <- AverageSurvGermination}

ScarSurvGerminationD <- data.frame(Scarification, ScarSurvGermination)

#########################################################################################
# Just floaters

OnlyFloaters <- CleanedGermination[CleanedGermination$Floater == "f", ]

FloaterGerminants <- (OnlyFloaters$LiveThinned + OnlyFloaters$DeadThinned +
                        OnlyFloaters$LiveThinnedPhotos + OnlyFloaters$DeadThinnedPhotos +
                        OnlyFloaters$LiveExtras + OnlyFloaters$DeadExtras +
                        OnlyFloaters$LiveFloaters + OnlyFloaters$DeadFloaters +
                        OnlyFloaters$LiveTransplant + OnlyFloaters$DeadTransplant +
                        OnlyFloaters$PotTransplant)
FloaterSurvGerminants <- (OnlyFloaters$LiveThinned + 
                        OnlyFloaters$LiveThinnedPhotos + 
                        OnlyFloaters$LiveExtras + 
                        OnlyFloaters$LiveFloaters + 
                        OnlyFloaters$LiveTransplant + 
                        OnlyFloaters$PotTransplant)
SumFloaterGerminants <- sum(FloaterGerminants, na.rm = TRUE)
SumFloaterSurvGerminants <- sum(FloaterSurvGerminants, na.rm = TRUE)

# Calculate germination rate for every row
FloaterGermination <- (FloaterGerminants / OnlyFloaters$SeedCount)
max(FloaterGermination, na.rm = TRUE)
FloaterSurvGermination <- (FloaterSurvGerminants / OnlyFloaters$SeedCount)

AverageFloaterGermination <- SumFloaterGerminants/ (sum(OnlyFloaters$SeedCount, na.rm = TRUE))
AverageFloaterSurvGermination <- SumFloaterSurvGerminants/ (sum(OnlyFloaters$SeedCount, na.rm = TRUE))

# Floater germination by mother

FloaterMotherGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- OnlyFloaters[OnlyFloaters$Mother == Mother[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants, na.rm = TRUE)
  SeedCount <- sum(SubData$SeedCount, na.rm = TRUE)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  FloaterMotherGermination[i] <- AverageInitialGermination}

FloaterMotherGerminationD <- data.frame(Mother, FloaterMotherGermination)

FloaterMotherSurvGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- OnlyFloaters[OnlyFloaters$Mother == Mother[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                   SubData$LiveThinnedPhotos + 
                   SubData$LiveExtras + 
                   SubData$LiveFloaters + 
                   SubData$LiveTransplant + 
                   SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants, na.rm = TRUE)
  SeedCount <- sum(SubData$SeedCount, na.rm = TRUE)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  FloaterMotherSurvGermination[i] <- AverageSurvGermination}

FloaterMotherSurvGerminationD <- data.frame(Mother, FloaterMotherSurvGermination)

# Floater germination by scarification treatment

FloaterScarGermination <- vector(length = length(Scarification))

for(i in 1:3) {
  SubData <- OnlyFloaters[OnlyFloaters$Scarification == Scarification[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants, na.rm = TRUE)
  SeedCount <-  sum(SubData$SeedCount, na.rm = TRUE)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  FloaterScarGermination[i] <- AverageInitialGermination}

FloaterScarGerminationD <- data.frame(Scarification, FloaterScarGermination)

FloaterScarSurvGermination <- vector(length = length(Scarification))

for(i in 1:3) {
  SubData <- OnlyFloaters[OnlyFloaters$Scarification == Scarification[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                   SubData$LiveThinnedPhotos + 
                   SubData$LiveExtras + 
                   SubData$LiveFloaters + 
                   SubData$LiveTransplant + 
                   SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants, na.rm = TRUE)
  SeedCount <-  sum(SubData$SeedCount, na.rm = TRUE)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  FloaterScarSurvGermination[i] <- AverageSurvGermination}

FloaterScarSurvGerminationD <- data.frame(Scarification, FloaterScarSurvGermination)

##############################################################################################
# Excluding floaters

DenseSeeds <- CleanedGermination[is.na(CleanedGermination$Floater), ]

DenseGerminants <- (DenseSeeds$LiveThinned + DenseSeeds$DeadThinned +
                      DenseSeeds$LiveThinnedPhotos + DenseSeeds$DeadThinnedPhotos +
                      DenseSeeds$LiveExtras + DenseSeeds$DeadExtras +
                      DenseSeeds$LiveFloaters + DenseSeeds$DeadFloaters +
                      DenseSeeds$LiveTransplant + DenseSeeds$DeadTransplant +
                      DenseSeeds$PotTransplant)
DenseSurvGerminants <- (DenseSeeds$LiveThinned + 
                      DenseSeeds$LiveThinnedPhotos + 
                      DenseSeeds$LiveExtras + 
                      DenseSeeds$LiveFloaters + 
                      DenseSeeds$LiveTransplant + 
                      DenseSeeds$PotTransplant)
SumDenseGerminants <- sum(DenseGerminants, na.rm = TRUE)
SumLiveDenseGerminants <- sum(DenseSurvGerminants, na.rm = TRUE)

DenseGermination <- (DenseGerminants / DenseSeeds$SeedCount)
max(DenseGermination)
# Max germination rate of dense seeds is 0.7980769, of Lr_1
DenseSurvGermination <- (DenseSurvGerminants / DenseSeeds$SeedCount)
max(DenseSurvGermination)
# Max surviving germination rate of dense seeds is 0.7046632, of Br (same as overall surviving germination rate)

AverageDenseGermination <- SumDenseGerminants/ (sum(DenseSeeds$SeedCount, na.rm = TRUE))
AverageDenseSurvGermination <- SumLiveDenseGerminants/ (sum(DenseSeeds$SeedCount, na.rm = TRUE))

# Dense germination by mother

DenseMotherGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- DenseSeeds[DenseSeeds$Mother == Mother[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  DenseMotherGermination[i] <- AverageInitialGermination}

DenseMotherGerminationD <- data.frame(Mother, DenseMotherGermination)

DenseMotherSurvGermination <- vector(length = length(Mother))

for(i in 1:length(Mother)) {
  SubData <- DenseSeeds[DenseSeeds$Mother == Mother[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                   SubData$LiveThinnedPhotos + 
                   SubData$LiveExtras + 
                   SubData$LiveFloaters + 
                   SubData$LiveTransplant + 
                   SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  DenseMotherSurvGermination[i] <- AverageSurvGermination}

DenseMotherSurvGerminationD <- data.frame(Mother, DenseMotherSurvGermination)

# Dense germination by scarification treatment

DenseScarGermination <- vector(length = length(Scarification))

for(i in 1:length(Scarification)) {
  SubData <- DenseSeeds[DenseSeeds$Scarification == Scarification[i], ]
  Germinants <- (SubData$LiveThinned + SubData$DeadThinned +
                            SubData$LiveThinnedPhotos + SubData$DeadThinnedPhotos +
                            SubData$LiveExtras + SubData$DeadExtras +
                            SubData$LiveFloaters + SubData$DeadFloaters +
                            SubData$LiveTransplant + SubData$DeadTransplant +
                            SubData$PotTransplant)
  TotalGerminants <- sum(Germinants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageInitialGermination <- TotalGerminants / SeedCount
  
  DenseScarGermination[i] <- AverageInitialGermination}

DenseScarGerminationD <- data.frame(Scarification, DenseScarGermination)

DenseScarSurvGermination <- vector(length = length(Scarification))

for(i in 1:length(Scarification)) {
  SubData <- DenseSeeds[DenseSeeds$Scarification == Scarification[i], ]
  SurvGerminants <- (SubData$LiveThinned + 
                   SubData$LiveThinnedPhotos + 
                   SubData$LiveExtras + 
                   SubData$LiveFloaters + 
                   SubData$LiveTransplant + 
                   SubData$PotTransplant)
  TotalSurvGerminants <- sum(SurvGerminants)
  SeedCount <-  sum(SubData$SeedCount)
  AverageSurvGermination <- TotalSurvGerminants / SeedCount
  
  DenseScarSurvGermination[i] <- AverageSurvGermination}

DenseScarSurvGerminationD <- data.frame(Scarification, DenseScarSurvGermination)

########################################################################################

# Percent floaters per mother

QuantityFloaterPercent <- vector(length = length(Mother))

for(i in 1:18) {
  SubData <- CleanedGermination[CleanedGermination$Mother == Mother[i], ]
  
  FloaterSum <- sum(SubData$SeedCount[SubData$Floater == "f"], na.rm = TRUE)
  TotalSum <- sum(SubData$SeedCount, na.rm = TRUE)
  
  QuantityFloaterPercent[i] <- (FloaterSum/TotalSum)}

QuantityFloaterPercentbyMotherD <- data.frame(Mother, QuantityFloaterPercent)

########################################################################################

ggplot(MotherAverageGerminationD, aes(Mother, MotherGermination))+
  geom_col()

ggplot(MotherSurvGerminationD, aes(Mother, MotherSurvGermination))+
  geom_col()

ggplot(DenseMotherGerminationD, aes(Mother, DenseMotherGermination))+
  geom_col()

ggplot(DenseMotherSurvGerminationD, aes(Mother, DenseMotherSurvGermination))+
  geom_col()

ggplot(DenseScarGerminationD, aes(Scarification, DenseScarGermination))+
  geom_col()

ggplot(DenseScarSurvGerminationD, aes(Scarification, DenseScarSurvGermination))+
  geom_col()

ggplot(FloaterMotherGerminationD, aes(Mother, FloaterMotherGermination))+
  geom_col()

ggplot(FloaterMotherSurvGerminationD, aes(Mother, FloaterMotherSurvGermination))+
  geom_col()

ggplot(FloaterScarGerminationD, aes(Scarification, FloaterScarGermination))+
  geom_col()

ggplot(FloaterScarSurvGerminationD, aes(Scarification, FloaterScarSurvGermination))+
  geom_col()

ggplot(ScarGerminationD, aes(Scarification, ScarGermination))+
  geom_col()

ggplot(ScarSurvGerminationD, aes(Scarification, ScarSurvGermination))+
  geom_col()

ggplot(QuantityFloaterPercentbyMotherD, aes(Mother, QuantityFloaterPercent))+
  geom_col()

# Compare germination rates to floater germination rates
# Does it make sense to use dense or total values here??

FloaterGermCompare <- data.frame(Mother, FloaterMotherGermination, DenseMotherGermination)
ggplot(FloaterGermCompare, aes(DenseMotherGermination, FloaterMotherGermination, label=Mother))+
  geom_point()+
  geom_text(hjust=-0.5, vjust=0)

FloaterGermSurvCompare <- data.frame(Mother, FloaterMotherSurvGermination, DenseMotherSurvGermination)
ggplot(FloaterGermSurvCompare, aes(DenseMotherSurvGermination, FloaterMotherSurvGermination, label=Mother))+
  geom_point()+
  geom_text(hjust=-0.5, vjust=0)

# Compare germination rates to floater quantities
FloaterQuantityCompare <- data.frame(Mother, QuantityFloaterPercent, DenseMotherGermination)
ggplot(FloaterQuantityCompare, aes(DenseMotherGermination, QuantityFloaterPercent, label=Mother))+
  geom_point()+
  geom_text(hjust=-0.5, vjust=0)

FloaterQuantitySurvCompare <- data.frame(Mother, QuantityFloaterPercent, DenseMotherSurvGermination)
ggplot(FloaterQuantitySurvCompare, aes(DenseMotherSurvGermination, QuantityFloaterPercent, label=Mother))+
  geom_point()+
  geom_text(hjust=-0.5, vjust=0)

# Floater and dense seed comparison
# Revisualize the QuantityFloaterPercentbyMother graph from above
FloaterDenseD <- QuantityFloaterPercentbyMotherD
FloaterDenseD$Dense <- (1-FloaterDenseD$QuantityFloaterPercent)
FloaterDenseD <- pivot_longer(FloaterDenseD, cols = c(QuantityFloaterPercent, Dense))

ggplot(FloaterDenseD, aes(Mother, value, fill = name))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))

# Visualize floaters and dense seeds that germinated, and those that didn't, in one stacked bar graph
GermQuantity <- CleanedGermination[,c('Flat', 'Mother', 'Scarification', 'Floater')]

GermQuantity$DenseGerm <- (CleanedGermination$LiveThinned + CleanedGermination$DeadThinned +
                             CleanedGermination$LiveThinnedPhotos + CleanedGermination$DeadThinnedPhotos +
                             CleanedGermination$LiveExtras + CleanedGermination$DeadExtras +
                             CleanedGermination$LiveTransplant + CleanedGermination$DeadTransplant +
                             CleanedGermination$PotTransplant)
GermQuantity$FloaterGerm <- (CleanedGermination$LiveFloaters + CleanedGermination$DeadFloaters)
GermQuantity$DenseUngerm <- ifelse(is.na(GermQuantity$Floater), (CleanedGermination$SeedCount - (CleanedGermination$LiveThinned + CleanedGermination$DeadThinned +
                                                                                      CleanedGermination$LiveThinnedPhotos + CleanedGermination$DeadThinnedPhotos +
                                                                                      CleanedGermination$LiveExtras + CleanedGermination$DeadExtras +
                                                                                      CleanedGermination$LiveTransplant + CleanedGermination$DeadTransplant +
                                                                                      CleanedGermination$PotTransplant)), NA)
GermQuantity$FloaterUngerm <- ifelse(is.na(GermQuantity$Floater), NA, (CleanedGermination$SeedCount - (CleanedGermination$LiveFloaters + CleanedGermination$DeadFloaters)))

MotherGermQuantity <- data.frame(matrix(ncol = 5, nrow = 18))
ColumnNames <- c("Mother", "DenseGerm", "FloaterGerm", "DenseUngerm", "FloaterUngerm")
colnames(MotherGermQuantity) <- ColumnNames

for(i in 1:length(Mother)) {
  SubData <- GermQuantity[GermQuantity$Mother == Mother[i], ]
  MotherGermQuantity$DenseGerm[i] <- sum(SubData$DenseGerm, na.rm = TRUE)
  MotherGermQuantity$FloaterGerm[i] <- sum(SubData$FloaterGerm, na.rm = TRUE)
  MotherGermQuantity$DenseUngerm[i] <- sum(SubData$DenseUngerm, na.rm = TRUE)
  MotherGermQuantity$FloaterUngerm[i] <- sum(SubData$FloaterUngerm, na.rm = TRUE)
  MotherGermQuantity$Mother[i] <- Mother[i]}

MotherGermQuantity <- pivot_longer(MotherGermQuantity, cols = c(FloaterGerm, DenseGerm, FloaterUngerm, DenseUngerm))

ggplot(MotherGermQuantity, aes(Mother, value, fill = name))+
  geom_bar(stat = "identity")

MotherGermQuantity$name <- factor(MotherGermQuantity$name, levels = unique(MotherGermQuantity$name))

ggplot(MotherGermQuantity, aes(Mother, value, fill = name))+
  geom_bar(position = "fill", stat = "identity")
