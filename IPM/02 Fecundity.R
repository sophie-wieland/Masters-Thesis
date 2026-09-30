# Fecundity/Reproduction of Roughleaf Dogwood
# August 20, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Dogwood IPM")

# Load and organize reproduction data (sources: Wedel, Orrock, KBSF, Davis, Whelan, Schulstrom, my germination experiment)

WedelRepro <- read.csv("../Literature Search for Model Building/Wedel Reproduction.csv")
WedelInflorescences <- read.csv("../Literature Search for Model Building/Wedel Inflorescences.csv")
OrrockRepro <- read.csv("../Literature Search for Model Building/Orrock Germination.csv")
DavisRepro <- read.csv("../Literature Search for Model Building/Davis Germination.csv")
SchulstromRepro <- read.csv("../Literature Search for Model Building/Schulstrom Fruit Set.csv")
WielandRepro <- read.csv("../Germination Analysis/CleanedGerminationData.csv")
KBSFRepro <- read.csv("../Literature Search for Model Building/fruits_3yrs.csv")
WhelanRepro <- read.csv("../Literature Search for Model Building/WhelanAverageGerm.csv")
WhelandSoil <- read.csv("../Literature Search for Model Building/WhelanSoilGerm.csv")

# In chronological order: Flowering, Fruit Set, Germination
#################################################################################
# Flowering
# First, average Wedel's two years (average within study before between studies)
WedelInflorescences$MeanInflorescences <- (WedelInflorescences$MeanInflorescences2018 + WedelInflorescences$MeanInflorescences2019)/ 2
WedelInflorescences$SE <- (WedelInflorescences$SEInflorescences2018 + WedelInflorescences$SEInflorescences2019)/ 2

# Combine Wedel and KBSF data on the number of inflorescenes produced per ramet
# KBSF data: The number of infructescences per stem in 1996, 1998, and 1999
# These counts, unfortunately, are not tied to *shrubs* of known sizes (just ramets)

Mean99 <- mean(KBSFRepro$X99infru)
SE99 <- sd(KBSFRepro$X99infru)/sqrt(length(KBSFRepro$X99infru))

# 1998 falls within the range Wedel found for unburned, unbrowsed shrubs
# The others have higher and more variable values
Mean98 <- mean(KBSFRepro$X98infru)
SE98 <- sd(KBSFRepro$X98infru)/sqrt(length(KBSFRepro$X98infru))

Mean96 <- mean(KBSFRepro$X96infru)
SE96 <- sd(KBSFRepro$X96infru)/sqrt(length(KBSFRepro$X96infru))

MeanKBSF <- mean(c(Mean99, Mean98, Mean96))
SEKBSF <- mean(c(SE99, SE98, SE96))

ReproPerStem <- WedelInflorescences[, c('Watershed', 'Treatment', 'MeanInflorescences', 'SE')]
ReproPerStem[2,3] <- mean(c(ReproPerStem[2,3], MeanKBSF))
ReproPerStem[2,4] <- mean(c(ReproPerStem[2,4], SEKBSF))
#################################################################################
# Fruit Set (per shrub island)

# loop - for every flowering stems quantity, match the watershed, treatment, and year
# multiply the corresponding EstFloweringStems2019 by the mean - SE and mean + SE
# is this a reasonable way to get a range of possible inflorescence numbers?
# does it even matter if there's a range? Just use the mean?
# Or ask Wedel for the raw data so I can pick from a histogram?
WedelRepro$EstInflorescences2019 <- vector(length = nrow(WedelRepro))
WedelRepro$EstInflorescences2018 <- vector(length = nrow(WedelRepro))

for(i in 1:nrow(WedelRepro)) {
  TreatmentMatch <- sapply(ReproPerStem$Treatment, identical, WedelRepro$Treatment[[i]])
  WatershedMatch <- sapply(ReproPerStem$Watershed, identical, WedelRepro$Watershed[[i]])
  WedelRepro$EstInflorescences2019[i] <- 
    ReproPerStem$MeanInflorescences[TreatmentMatch & WatershedMatch] *
    WedelRepro$EstFloweringStems2019[i]
}

for(i in 1:nrow(WedelRepro)) {
  TreatmentMatch <- sapply(ReproPerStem$Treatment, identical, WedelRepro$Treatment[[i]])
  WatershedMatch <- sapply(ReproPerStem$Watershed, identical, WedelRepro$Watershed[[i]])
  WedelRepro$EstInflorescences2018[i] <- 
    ReproPerStem$MeanInflorescences[TreatmentMatch & WatershedMatch] *
    WedelRepro$EstFloweringStems2018[i]
}

# Combine with Schulstrom fruit set per inflorescence
AverageFruitSet <- mean(SchulstromRepro$Fruit.Set)

WedelRepro$EstFruit2018 <- WedelRepro$EstInflorescences2018 * AverageFruitSet
WedelRepro$EstFruit2019 <- WedelRepro$EstInflorescences2019 * AverageFruitSet

min(WedelRepro$EstFruit2018[WedelRepro$EstFruit2018 > 0])
min(WedelRepro$EstFruit2019[WedelRepro$EstFruit2019 > 0])
max(WedelRepro$EstFruit2018)
max(WedelRepro$EstFruit2019)

# The four (presumably averaged) fruit crop numbers reported in Willson and Whelan are 3800, 1300, 4100, and 3900
# Those fall within the very wide range of 19 - 42,000 reported here
###################################################################################
# Germination
# Using just the bird gut (acid and rock tumbling) simulation
WielandGermRate <- 
# Wieland germination rate code in 01 Analyze Germination R script.R

# Applying this one germination value to the fruit crop numbers

# Calculate seed per m2
  
# Put into a matrix with FRI and browsing


#################################################################################
# Create a table showing all the germination percentages from various experiments
