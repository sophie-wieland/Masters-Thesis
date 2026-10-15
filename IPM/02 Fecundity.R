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

GliddenFruit <- read.csv("../Literature Search for Model Building/Glidden/ProcessedN4CDogwood.csv")

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
# multiply the corresponding EstFloweringStems2019 by the mean
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
# Wieland germination rate code in 01 Analyze Germination R script.R
WielandGermRate <- 0.3382358

# Apply this one germination value to the fruit crop numbers

# Calculate seed per m2
# size is in m2
WedelRepro$SeedPerArea2018 <- WedelRepro$EstFruit2018/WedelRepro$size2018
WedelRepro$SeedPerArea2019 <- WedelRepro$EstFruit2019/WedelRepro$size2019

###################################################################################
# Function for fruiting number
WedelRepro <- rename(WedelRepro, Browse = Treatment)
WedelRepro$FRI <- ifelse(WedelRepro$Watershed == "4B", 4, 20)

# stack year in one column
Repro2018 <- select(WedelRepro, c(Watershed, Shrub_id, Browse, EstFloweringStems2018, size2018, EstInflorescences2018, EstFruit2018, SeedPerArea2018, FRI))
Repro2018$Year <- 2018
Repro2018 <- rename(Repro2018, c(EstFloweringStems = EstFloweringStems2018,
                                 size = size2018,
                                 EstInflorescences = EstInflorescences2018,
                                 EstFruit = EstFruit2018,
                                 SeedPerArea = SeedPerArea2018))
Repro2019 <- select(WedelRepro, c(Watershed, Shrub_id, Browse, EstFloweringStems2019, size2019, EstInflorescences2019, EstFruit2019, SeedPerArea2019, FRI))
Repro2019$Year <- 2019
Repro2019 <- rename(Repro2019, c(EstFloweringStems = EstFloweringStems2019,
                                 size = size2019,
                                 EstInflorescences = EstInflorescences2019,
                                 EstFruit = EstFruit2019,
                                 SeedPerArea = SeedPerArea2019))

Repro <- rbind(Repro2018, Repro2019)
# make a subset where fruit number is not 0
FruitRepro <- Repro[which(Repro$EstFruit>0), ]
# because we're going to use the Glidden data first as probability of fruiting,
# and this as the number of fruits (if you make it over the hurdle to fruiting)

num_fruit <- lm(EstFruit ~ size + Browse * FRI, data = FruitRepro)
alt_num_fruit <- lm(EstFruit ~ size + Browse + FRI, data = FruitRepro)
# models are identical - only two browsing data points

# look at residuals
res_num_fruit <- resid(num_fruit)
plot(FruitRepro$size, res_num_fruit)
abline(0, 0)

standardized_res_num_fruit <- rstandard(num_fruit)
plot(FruitRepro$size, standardized_res_num_fruit)
abline(0, 0)
# bad!

# remove the two browsing points, and look again
FruitFireRepro <- FruitRepro[which(FruitRepro$Browse=="control"), ]
fire_fruit <- lm(EstFruit ~ size + FRI, data = FruitFireRepro)
res_fire_fruit <- resid(fire_fruit)
plot(FruitFireRepro$size, res_fire_fruit)
abline(0, 0)

standardized_res_fire_fruit <- rstandard(fire_fruit)
plot(FruitFireRepro$size, standardized_res_fire_fruit)
abline(0, 0)
# also bad!
# use the average by size cop out instead; residuals were not normal, which is why we abandoned this

#predictors <- expand.grid(Browse = unique(WedelRepro$Browse), FRI = unique(WedelRepro$FRI))
# how add size to predictors??

#test <- predict(num_fruit, newdata = predictors, type = "response")

ggplot(FruitRepro, aes(x = size, y = EstFruit, group = Browse, color = Browse)) +
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~FRI)

ggplot(WedelRepro, aes(x = size2019, y = EstFruit2019)) +
  geom_point()+
  geom_smooth(method = "lm")
#################################################################################
# Function for fruiting probability
prob_fruit <- glm(Fruit ~ Area_m2026 + Graze * FRI, family = binomial(link = "logit"), data = GliddenFruit)
res_prob_fruit <- resid(prob_fruit)



#################################################################################
# Create a table showing all the germination percentages from various experiments
