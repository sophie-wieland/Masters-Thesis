# Create array to hold all combinations of vital rates
# Oct 28, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Dogwood IPM")

# Create a 5D array of all treatments (maybe 6?)
# FRI: 1, 2, 3, 4, 20/unburned = 5
# Grazer: B, C, Un = 3
# Bison, Cattle, Ungrazed
# Browser: Goat, Early clip, Late clip, Ambient (deer, but no purposeful browsing) = 4
# Fire Season?: Sp, Su = 2

# + dimension for FruitProb, FruitNum, Germ, Surv = 4
d1names <- as.character(c(1, 2, 3, 4, 20))
d2names <- c("B", "C", "Un")
d3names <- c("Goat", "Early", "Late", "Ambient")
d4names <- c("Sp", "Su")
d5names <- c("FruitProb", "FruitNum", "Germ", "Surv")

TreatmentArray <- array(NA, dim = c(5, 3, 4, 2, 4))
dimnames(TreatmentArray) <- list(fire=d1names, graze=d2names, browse=d3names, season=d4names, rate=d5names)

saveRDS(TreatmentArray, "Array.rds")
