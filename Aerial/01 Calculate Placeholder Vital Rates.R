# Calculating vital rates of woody species using aerial imagery
# Using placeholder years, not true years
# April 23, 2026
# Sophie Wieland

library(dplyr)
library(ggplot2)
library(popbio)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Aerial Imagery")

DatafromPlantTracker <- read.csv("TrackedPolygonsRecodedYears.csv")

# The output from plantTracker should have one row per genet
# That seems to suggest that it is safe to calculate from the columns without risk of duplicates

# Calculate survival rate

# Number of plants alive in time tplus1 / number of plants alive in time t
# survives_tplus1 is NA if there is no year after + other cases - check!
# survives_tplus1 is 0 if they are not present the next year
# survives_tplus1 is 1 if they are present the next year

# so 1/1+0
# could also work by year (ignore for now)
# Organize by size - figure if they are more likely to survive if you are small/med/large to begin

#ggplot(DatafromPlantTracker, aes(x=Trial_demographic_data.basalArea_ramet)) + 
  #geom_histogram( binwidth=3) +
  #ylab("Frequency") + 
  #xlab("Size") +
  #theme_classic()

# these size classes were arbitrarily chosen to be roughly a third of the plants in each category

sizeclasses <- c(0, 13, 40, 40000)
small_plants <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.basalArea_ramet < sizeclasses[2],]
med_plants <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.basalArea_ramet < sizeclasses[3]
                                   & DatafromPlantTracker$Trial_demographic_data.basalArea_ramet > sizeclasses[2], ]
large_plants <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.basalArea_ramet > sizeclasses[3],]


small_survival <- sum(small_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
                  sum(small_plants$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
med_survival <- sum(med_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
                sum(med_plants$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
large_survival <- sum(large_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
                  sum(large_plants$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)

# Overall survival
#overall_survival <- sum(DatafromPlantTracker$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
                    #sum(DatafromPlantTracker$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)

# Calculate growth rate

# Change in basal area / time
# size_tplus1 minus basalArea_ramet gives you the growth between year t and year tplus1
# you are listed multiple times if you survive
# Excluding NA (i.e. no growth or shrinking)

#GrowthOverTime <- DatafromPlantTracker$Trial_demographic_data.size_tplus1 -
                  #DatafromPlantTracker$Trial_demographic_data.basalArea_ramet
#AverageGrowth <- mean(GrowthOverTime, na.rm = TRUE)

# what is the probability of transitioning from one size class to the next size class

#ProbabilitySmall <- sum(small_plants$Trial_demographic_data.size_tplus1 > sizeclasses[2], na.rm = TRUE) /
                    #nrow(small_plants)
#ProbabilityMed <- sum(med_plants$Trial_demographic_data.size_tplus1 > sizeclasses[3], na.rm = TRUE) /
                  #nrow(med_plants)

# make histogram separated by my size classes (starting from each size pool)

SmallHistogram <- hist(small_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses, freq = FALSE)
TranS <- SmallHistogram$counts/
        sum(small_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)

MedHistogram <- hist(med_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
TranM <- MedHistogram$counts/
  sum(med_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)

LargeHistogram <- hist(large_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
TranL <- LargeHistogram$counts/
  sum(large_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)


# Calculate recruitment rate

# Number of new recruits / timestep
# recruit column is 1 if they are new in that year
# recruit column is 0 if they are from a previous year
# recruit column is NA if they are present the first year? seems to be other NAs as well

# total number / year

#Recruitment <- sum(DatafromPlantTracker$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
                #nrow(DatafromPlantTracker)

Year12 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.Trial == 12, ]
Year13 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.Trial == 13, ]
Year14 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.Trial == 14, ]
Year15 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.Trial == 15, ]

Recruitment12 <- sum(Year12$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
                  sum(DatafromPlantTracker$Trial_demographic_data.Trial == 11)
Recruitment13 <- sum(Year13$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
                  sum(DatafromPlantTracker$Trial_demographic_data.Trial == 12)
Recruitment14 <- sum(Year14$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
                  sum(DatafromPlantTracker$Trial_demographic_data.Trial == 13)
Recruitment15 <- sum(Year15$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
                  sum(DatafromPlantTracker$Trial_demographic_data.Trial == 14)

AverageRecruit <- mean(Recruitment12, Recruitment13, Recruitment14, Recruitment15)
RperSize <- AverageRecruit/3

# Calculates lambda for the whole population

OverallMatrix <- matrix(c((RperSize+TranS[[1]]), (RperSize+TranM[[1]]), (RperSize+TranL[[1]]),
                          TranS[[2]], TranM[[2]], TranL[[2]],
                          TranS[[3]], TranM[[3]], TranL[[3]]), 
                          nrow=3, byrow=TRUE)

Lambda <- eigen.analysis(OverallMatrix)

###########################################################################################

# Make a loop to calculate lambda for specific watersheds
Watersheds <- c("1D", "2D", "4F", "C1A", "C3A", "C3B", "C3C", "N1B")

LambdabyWatershed <- vector(length = length(Watersheds))

for(i in 1:8) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed == Watersheds[i], ]
  
  sizeclasses <- c(0, 13, 40, 40000)
  small_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[2],]
  med_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[3]
                                     & SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[2], ]
  large_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[3],]
  
  SmallHistogram <- hist(small_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses, freq = FALSE)
  TranS <- SmallHistogram$counts/
    sum(small_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  MedHistogram <- hist(med_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranM <- MedHistogram$counts/
    sum(med_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  LargeHistogram <- hist(large_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranL <- LargeHistogram$counts/
    sum(large_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  Year12 <- SubData[SubData$Trial_demographic_data.Trial == 12, ]
  Year13 <- SubData[SubData$Trial_demographic_data.Trial == 13, ]
  Year14 <- SubData[SubData$Trial_demographic_data.Trial == 14, ]
  Year15 <- SubData[SubData$Trial_demographic_data.Trial == 15, ]
  
  Recruitment12 <- sum(Year12$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 11)
  Recruitment13 <- sum(Year13$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 12)
  Recruitment14 <- sum(Year14$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 13)
  Recruitment15 <- sum(Year15$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 14)
  
  AverageRecruit <- mean(Recruitment12, Recruitment13, Recruitment14, Recruitment15)
  RperSize <- AverageRecruit/3
  
  OverallMatrix <- matrix(c((RperSize+TranS[[1]]), (RperSize+TranM[[1]]), (RperSize+TranL[[1]]),
                            TranS[[2]], TranM[[2]], TranL[[2]],
                            TranS[[3]], TranM[[3]], TranL[[3]]), 
                          nrow=3, byrow=TRUE)
  
  ResultingLambda <- eigen.analysis(OverallMatrix)
  
  LambdabyWatershed[i] <- ResultingLambda[[1]] }

############################################################################################

DatafromPlantTracker$FRI <- ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("1D", "C1A", "N1B"), 1,
                            ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("2D"), 2,
                            ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("C3A", "C3B", "C3C"), 3,
                            ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("4F"), 4, NA))))
DatafromPlantTracker$Grazer <- ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("C3A", "C1A", "C3B", "C3C"), "Cattle",
                              ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("2D", "1D"), "Ungrazed",
                              ifelse(DatafromPlantTracker$Trial_demographic_data.watershed %in% c("N1B"), "Bison", NA)))

FRI <- c(1, 2, 3, 4)
FireLambda <- vector(length = 4)

for(i in 1:4) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$FRI == FRI[i], ]
  
  sizeclasses <- c(0, 13, 40, 40000)
  small_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[2],]
  med_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[3]
                        & SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[2], ]
  large_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[3],]
  
  SmallHistogram <- hist(small_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses, freq = FALSE)
  TranS <- SmallHistogram$counts/
    sum(small_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  MedHistogram <- hist(med_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranM <- MedHistogram$counts/
    sum(med_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  LargeHistogram <- hist(large_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranL <- LargeHistogram$counts/
    sum(large_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  Year12 <- SubData[SubData$Trial_demographic_data.Trial == 12, ]
  Year13 <- SubData[SubData$Trial_demographic_data.Trial == 13, ]
  Year14 <- SubData[SubData$Trial_demographic_data.Trial == 14, ]
  Year15 <- SubData[SubData$Trial_demographic_data.Trial == 15, ]
  
  Recruitment12 <- sum(Year12$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 11, na.rm = TRUE)
  Recruitment13 <- sum(Year13$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 12, na.rm = TRUE)
  Recruitment14 <- sum(Year14$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 13, na.rm = TRUE)

    denom <- sum(SubData$Trial_demographic_data.Trial == 14, na.rm = TRUE)
    
    Recruitment15 <- ifelse(
      is.finite(denom) && denom > 0,
      sum(Year15$Trial_demographic_data.recruit == 1, na.rm = TRUE) / denom,
      0)
    
  AverageRecruit <- mean(c(Recruitment12, Recruitment13, Recruitment14, Recruitment15))
  RperSize <- AverageRecruit/3
  
  OverallMatrix <- matrix(c((RperSize+TranS[[1]]), (RperSize+TranM[[1]]), (RperSize+TranL[[1]]),
                            TranS[[2]], TranM[[2]], TranL[[2]],
                            TranS[[3]], TranM[[3]], TranL[[3]]), 
                          nrow=3, byrow=TRUE)
  
  ResultingLambda <- eigen.analysis(OverallMatrix, zero = TRUE)
  
  FireLambda[i] <- ResultingLambda[[1]] }
###########################################################################################
# Calculate vital rates for each watershed

WatershedSurvival <- vector(length = length(Watersheds))

for(i in 1:8) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed == Watersheds[i], ]
  
  overall_survival <- sum(SubData$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(SubData$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
  
  WatershedSurvival[i] <- overall_survival}

WatershedRecruitment <- vector(length = length(Watersheds))

for(i in 1:8) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed == Watersheds[i], ]
  
  Year12 <- SubData[SubData$Trial_demographic_data.Trial == 12, ]
  Year13 <- SubData[SubData$Trial_demographic_data.Trial == 13, ]
  Year14 <- SubData[SubData$Trial_demographic_data.Trial == 14, ]
  Year15 <- SubData[SubData$Trial_demographic_data.Trial == 15, ]
  
  Recruitment12 <- sum(Year12$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 11)
  Recruitment13 <- sum(Year13$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 12)
  Recruitment14 <- sum(Year14$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 13)
  Recruitment15 <- sum(Year15$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 14)
  
  AverageRecruit <- mean(Recruitment12, Recruitment13, Recruitment14, Recruitment15)
  
  WatershedRecruitment[i] <- AverageRecruit}

WatershedGrowth <- vector(length = length(Watersheds))

for(i in 1:8) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed == Watersheds[i], ]
  
  GrowthOverTime <- SubData$Trial_demographic_data.size_tplus1 -
                    SubData$Trial_demographic_data.basalArea_ramet

  AverageGrowth <- mean(GrowthOverTime, na.rm = TRUE)
  
  WatershedGrowth[i] <- AverageGrowth}


############################################################################################

Herbivores <- c("Cattle", "Bison", "Ungrazed")
GrazingLambda <- vector(length = 3)

for(i in 1:3) {
  SubData <- DatafromPlantTracker[DatafromPlantTracker$Grazer == Herbivores[i], ]
  
  sizeclasses <- c(0, 13, 40, 40000)
  small_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[2],]
  med_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet < sizeclasses[3]
                        & SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[2], ]
  large_plants <- SubData[SubData$Trial_demographic_data.basalArea_ramet > sizeclasses[3],]
  
  SmallHistogram <- hist(small_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses, freq = FALSE)
  TranS <- SmallHistogram$counts/
    sum(small_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  MedHistogram <- hist(med_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranM <- MedHistogram$counts/
    sum(med_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  LargeHistogram <- hist(large_plants$Trial_demographic_data.size_tplus1, breaks = sizeclasses)
  TranL <- LargeHistogram$counts/
    sum(large_plants$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE)
  
  Year12 <- SubData[SubData$Trial_demographic_data.Trial == 12, ]
  Year13 <- SubData[SubData$Trial_demographic_data.Trial == 13, ]
  Year14 <- SubData[SubData$Trial_demographic_data.Trial == 14, ]
  Year15 <- SubData[SubData$Trial_demographic_data.Trial == 15, ]
  
  Recruitment12 <- sum(Year12$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 11, na.rm = TRUE)
  Recruitment13 <- sum(Year13$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 12, na.rm = TRUE)
  Recruitment14 <- sum(Year14$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
    sum(SubData$Trial_demographic_data.Trial == 13, na.rm = TRUE)
 
  denom <- sum(SubData$Trial_demographic_data.Trial == 14, na.rm = TRUE)
  
  Recruitment15 <- ifelse(
    is.finite(denom) && denom > 0,
    sum(Year15$Trial_demographic_data.recruit == 1, na.rm = TRUE) / denom,
    0)
  
  AverageRecruit <- mean(Recruitment12, Recruitment13, Recruitment14, Recruitment15)
  RperSize <- AverageRecruit/3
  
  OverallMatrix <- matrix(c((RperSize+TranS[[1]]), (RperSize+TranM[[1]]), (RperSize+TranL[[1]]),
                            TranS[[2]], TranM[[2]], TranL[[2]],
                            TranS[[3]], TranM[[3]], TranL[[3]]), 
                          nrow=3, byrow=TRUE)
  
  ResultingLambda <- eigen.analysis(OverallMatrix)
  
  GrazingLambda[i] <- ResultingLambda[[1]] }
############################################################################################

# Run ANOVA to compare FRI crossed with grazing
# FRI as a continuous variable
LambdaDataframe <- data.frame(Watershed = c(1.117647, 1.371795, 1.409091, 1.488889, 1.387097, 1.474576, 1.310175),
                              FRI = c(1, 2, 4, 1, 3, 3, 3),
                              Grazing = c("No", "No", "No", "Cattle", "Cattle", "Cattle", "Cattle"),
                              row.names = c("1D", "2D", "4F", "C1A", "C3A", "C3B", "C3C"))

GrazingFireAnova <- anova(lm(Watershed ~ Grazing * FRI, LambdaDataframe))

############################################################################################

# Recalculate vital rates above, but for specific watersheds and fire and grazing treatments
# Fire Return Intervals: 4, 3, 2, 1

Fire4 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("4F"), ]
Fire3 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("C3C", "C3B", "C3A"), ]
Fire2 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("2D"), ]
Fire1 <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("C1A", "1D", "N1B"), ]

# survival
Fire4Survival <- sum(Fire4$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(Fire4$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
Fire3Survival <- sum(Fire3$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(Fire3$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
Fire2Survival <- sum(Fire2$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(Fire2$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
Fire1Survival <- sum(Fire1$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(Fire1$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)

# growth

Fire4GrowthOverTime <- Fire4$Trial_demographic_data.size_tplus1 -
                        Fire4$Trial_demographic_data.basalArea_ramet
Fire4AverageGrowth <- mean(Fire4GrowthOverTime, na.rm = TRUE)

Fire3GrowthOverTime <- Fire3$Trial_demographic_data.size_tplus1 -
                        Fire3$Trial_demographic_data.basalArea_ramet
Fire3AverageGrowth <- mean(Fire3GrowthOverTime, na.rm = TRUE)

Fire2GrowthOverTime <- Fire2$Trial_demographic_data.size_tplus1 -
                        Fire2$Trial_demographic_data.basalArea_ramet
Fire2AverageGrowth <- mean(Fire2GrowthOverTime, na.rm = TRUE)

Fire1GrowthOverTime <- Fire1$Trial_demographic_data.size_tplus1 -
                        Fire1$Trial_demographic_data.basalArea_ramet
Fire1AverageGrowth <- mean(Fire1GrowthOverTime, na.rm = TRUE)

# probability of transitioning from one size class to the next size class

# working here- make separate objects??
#Fire4ProbabilitySmall <- sum(Fire4$Trial_demographic_data.size_tplus1 > sizeclasses[2], na.rm = TRUE) /
  #nrow(small_plants)
#ProbabilityMed <- sum(med_plants$Trial_demographic_data.size_tplus1 > sizeclasses[3], na.rm = TRUE) /
  #nrow(med_plants)

# Recruitment
# Calculate Recruitment per year??

Fire4Recruitment <- sum(Fire4$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(Fire4)
Fire3Recruitment <- sum(Fire3$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(Fire3)
Fire2Recruitment <- sum(Fire2$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(Fire2)
Fire1Recruitment <- sum(Fire1$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(Fire1)

# Grazing Treatments: Bison, Cattle, Ungrazed

GrazeBison <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("N1B"), ]
GrazeCattle <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("C3C", "C3B", "C3A", "C1A"), ]
GrazeNo <- DatafromPlantTracker[DatafromPlantTracker$Trial_demographic_data.watershed %in% c("2D", "1D", "4F"), ]

# survival
GrazeBisonSurvival <- sum(GrazeBison$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(GrazeBison$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
GrazeCattleSurvival <- sum(GrazeCattle$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(GrazeCattle$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)
GrazeNoSurvival <- sum(GrazeNo$Trial_demographic_data.survives_tplus1 == 1, na.rm = TRUE) /
  sum(GrazeNo$Trial_demographic_data.survives_tplus1 %in% c(0, 1), na.rm = TRUE)

# growth

GrazeBisonGrowthOverTime <- GrazeBison$Trial_demographic_data.size_tplus1 -
  GrazeBison$Trial_demographic_data.basalArea_ramet
GrazeBisonAverageGrowth <- mean(GrazeBisonGrowthOverTime, na.rm = TRUE)

GrazeCattleGrowthOverTime <- GrazeCattle$Trial_demographic_data.size_tplus1 -
  GrazeCattle$Trial_demographic_data.basalArea_ramet
GrazeCattleAverageGrowth <- mean(GrazeCattleGrowthOverTime, na.rm = TRUE)

GrazeNoGrowthOverTime <- GrazeNo$Trial_demographic_data.size_tplus1 -
  GrazeNo$Trial_demographic_data.basalArea_ramet
GrazeNoAverageGrowth <- mean(GrazeNoGrowthOverTime, na.rm = TRUE)

# probability of transitioning from one size class to the next size class

# working here- make separate objects??
#Fire4ProbabilitySmall <- sum(Fire4$Trial_demographic_data.size_tplus1 > sizeclasses[2], na.rm = TRUE) /
  #nrow(small_plants)
#ProbabilityMed <- sum(med_plants$Trial_demographic_data.size_tplus1 > sizeclasses[3], na.rm = TRUE) /
  #nrow(med_plants)

# Recruitment
# Calculate Recruitment per year??

GrazeBisonRecruitment <- sum(GrazeBison$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(GrazeBison)
GrazeCattleRecruitment <- sum(GrazeCattle$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(GrazeCattle)
GrazeNoRecruitment <- sum(GrazeNo$Trial_demographic_data.recruit == 1, na.rm = TRUE) /
  nrow(GrazeNo)

# Make a table
IntermediateSurvivalValues <- data.frame(Fire1Survival, Fire2Survival, Fire3Survival, Fire4Survival,
                             GrazeBisonSurvival, GrazeCattleSurvival, GrazeNoSurvival,
                             large_survival, med_survival, small_survival, overall_survival)
SurvivalValues <- as.data.frame(t(IntermediateSurvivalValues))
  
IntermediateGrowthValues <- data.frame(Fire1AverageGrowth, Fire2AverageGrowth, Fire3AverageGrowth, Fire4AverageGrowth,
                           GrazeBisonAverageGrowth, GrazeCattleAverageGrowth, GrazeNoAverageGrowth,
                           AverageGrowth)
GrowthValues <- as.data.frame(t(IntermediateGrowthValues))

IntermediateRecruitValues <- data.frame(Fire1Recruitment, Fire2Recruitment, Fire3Recruitment, Fire4Recruitment,
                           GrazeBisonRecruitment, GrazeCattleRecruitment, GrazeNoRecruitment,
                           Recruitment12, Recruitment13, Recruitment14, Recruitment15)
RecruitValues <- as.data.frame(t(IntermediateRecruitValues))