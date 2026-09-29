# Preparing data from Orrock 2012
# August 11, 2026
# Sophie Wieland
getwd()

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

Germ <- read.csv("Orrock Germination.csv")

# From John Orrock:
# sum_num_germ is the number of total germinants
# sum_num_not_germ is the total number of seeds that did not germinate
# sum_notgerm_viable_tz is the total number of seeds that did not germinate, but were viable with TZ
# sum_notgerm_inviable_tz is the total number of seeds that did not germinate, and were also not viable with TZ

# Fungicide was not significantly different from control, except in one habitat type (different habitats not shown in this csv)

Germ$Total <- Germ$sum_num_germ + Germ$sum_num_not_germ

Germ$Germination <- Germ$sum_num_germ/Germ$Total

CombinedSeeds <- sum(Germ$Total)
CombinedGerminants <- sum(Germ$sum_num_germ)
CombinedGermination <- CombinedGerminants/CombinedSeeds

# Viability of seeds remaining in the seed bank

Germ$Seedbank <- Germ$sum_notgerm_viable_tz + Germ$sum_notgerm_inviable_tz
Germ$SeedbankViability <- Germ$sum_notgerm_viable_tz/Germ$Seedbank

CombinedSeedbank <- sum(Germ$Seedbank)
CombinedViable <- sum(Germ$sum_notgerm_viable_tz)
CombinedSeedbankViability <- CombinedViable/CombinedSeedbank