# Preparing data from Schulstrom 2002
# August 5, 2026
# Sophie Wieland
getwd()

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

# The salient bits are Fruit set per bud and the Number of buds per inflorescence

Appendix1 <- read.csv("Schulstrom Appendix 1.csv")
Appendix2 <- read.csv("Schulstrom Appendix 2.csv")
Appendix3 <- read.csv("Schulstrom Appendix 3.csv")
Appendix4 <- read.csv("Schulstrom Appendix 4.csv")
Appendix5 <- read.csv("Schulstrom Appendix 5.csv")

# Each data point is one inflorescence
# Selfed = pollen from same inflorescence
# Out-crossed = pollen from different inflorescence, all at least 10 m away
# Open at night
# Open at day
# Constantly bagged
# Manipulated control = wire frame, but no bag (not sig. different from unmanipulated controls)
# Solitary = no other flowering dogwood stems within 1 m
# Dense = flowering dogwood stems within 1 m
# no difference between same plant controls and other plant controls (SPC, OPC)

# Remove selfed from Appendix 1
ApplicableAppendix1 <- Appendix1[Appendix1$Treatment != "Selfed", ]

# Remove night-open, cont. bag from Appendix 2
ApplicableAppendix2 <- Appendix2[Appendix2$Treatment != "Night-open", ]
ApplicableAppendix2 <- ApplicableAppendix2[ApplicableAppendix2$Treatment != "Cont. bag", ]

# This combines solitary/dense and interior/edge data
# Combine all appendices, and remove duplicate records

# Fix naming convention
colnames(Appendix4)[1] <- "Plant" 
colnames(Appendix5)[1] <- "Plant" 

# Take only applicable columns
TruncatedApp1 <- ApplicableAppendix1[c("Plant", "Bud.no.", "Fruit.Set", "Fruit.set.bud")]
TruncatedApp2 <- ApplicableAppendix2[c("Plant", "Bud.no.", "Fruit.Set", "Fruit.set.bud")]
TruncatedApp3 <- Appendix3[c("Plant", "Bud.no.", "Fruit.Set", "Fruit.set.bud")]
TruncatedApp4 <- Appendix4[c("Plant", "Bud.no.", "Fruit.Set", "Fruit.set.bud")]
TruncatedApp5 <- Appendix5[c("Plant", "Bud.no.", "Fruit.Set", "Fruit.set.bud")]
# Combine
CombinedCounts <- rbind(TruncatedApp1, TruncatedApp2, TruncatedApp3, TruncatedApp4, TruncatedApp5)

InflorescenceCounts <- unique(CombinedCounts)
InflorescenceCounts <- InflorescenceCounts[InflorescenceCounts$Plant != "LDO-04-719", ]
################################################################################
InflorescenceCounts$Fruit.Set <- as.numeric(InflorescenceCounts$Fruit.Set)
AverageFruitSet <- mean(InflorescenceCounts$Fruit.Set)
hist(InflorescenceCounts$Fruit.Set)

write.csv(InflorescenceCounts, "Schulstrom Fruit Set.csv")