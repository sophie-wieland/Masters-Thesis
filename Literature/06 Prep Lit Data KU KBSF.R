# Preparing data from KU KBSF experiment
# Sophie Wieland
# August 19, 2026
library(dplyr)
getwd()
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

# note that the original files (in KU KBSF) have notes at the top of these files
# the csv files (in Literature Search for Model Building) have had the notes deleted
Metal1995 <- read.csv("wd95mttgcor2.csv")
Nursery1996 <- read.csv("wd96nstgcor.csv")
Metal1996 <- read.csv("wd96mttgcor.csv")
Nursery1997 <- read.csv("wd97nstgcor.csv")
Metal1997 <- read.csv("wd97mttgcor.csv")
Nursery1998 <- read.csv("wd98nsrtgcor.csv")
Metal1998 <- read.csv("wd98mttgcor.csv")
Nursery1999 <- read.csv("wd99nsrtgcor.csv")
Metal1999 <- read.csv("wd99mttgcor.csv")
Nursery2000 <- read.csv("wd00nsrtgcor.csv")
Metal2000 <- read.csv("wd00mttgcor.csv")

Nursery1998 <- rename(Nursery1998, SAM = LOCATION)
Metal1998 <- rename(Metal1998, SAM = LOCATION)
Nursery1999 <- rename(Nursery1999, SAM = LOCATION)
Metal1999 <- rename(Metal1999, SAM = LOC)
Nursery2000 <- rename(Nursery2000, SAM = LOCATION)
Metal2000 <- rename(Metal2000, SAM = LOC)

Stems1995 <- read.csv("wd95datcor.csv")
Stems1996 <- read.csv("wd96datcor.csv")
Stems1997 <- read.csv("wd97datcor.csv")
Stems1998 <- read.csv("wd98datcor.csv")
Stems1999 <- read.csv("wd99datcor.csv")
Stems2000 <- read.csv("wd00datcor.csv")

# Find empty locations (locations without dogwood)
# It might be a good idea to also look at the stem count data as a source of empty locations

AllPossibleLocations <- read.csv("KU KBSF Locations.csv")

Metal1995Dogwood <- Metal1995[Metal1995$CODE == "CORDR", ]
Nursery1996Dogwood <- Nursery1996[Nursery1996$CODE == "CORDR", ]
Metal1996Dogwood <- Metal1996[Metal1996$CODE == "CORDR", ]
Nursery1997Dogwood <- Nursery1997[Nursery1997$CODE == "CORDR", ]
Metal1997Dogwood <- Metal1997[Metal1997$CODE == "CORDR", ]
Nursery1998Dogwood <- Nursery1998[Nursery1998$CODE == "CORDR", ]
Metal1998Dogwood <- Metal1998[Metal1998$CODE == "CORDR", ]
Nursery1999Dogwood <- Nursery1999[Nursery1999$CODE == "CORDR", ]
Metal1999Dogwood <- Metal1999[Metal1999$CODE == "CORDR", ]
Nursery2000Dogwood <- Nursery2000[Nursery2000$CODE == "CORDR", ]
Metal2000Dogwood <- Metal2000[Metal2000$CODE == "CORDR", ]

Stems1995Dogwood <- Stems1995[Stems1995$CODE == "CORDR", ]
Stems1996Dogwood <- Stems1996[Stems1996$CODE == "CORDR", ]
Stems1997Dogwood <- Stems1997[Stems1997$code == "CORDR", ]
Stems1998Dogwood <- Stems1998[Stems1998$code == "CORDR", ]
Stems1999Dogwood <- Stems1999[Stems1999$code == "CORDR", ]
Stems2000Dogwood <- Stems2000[Stems2000$code == "CORDR", ]

# Make all the various naming conventions match
Stems1995Dogwood <- rename(Stems1995Dogwood, SAM = LOC)
Stems1995Dogwood <- rename(Stems1995Dogwood, PATCH = PTCH)
Stems1996Dogwood <- rename(Stems1996Dogwood, PATCH = PTCH)
Stems1996Dogwood <- rename(Stems1996Dogwood, SAM = LOC)
Stems1997Dogwood <- rename(Stems1997Dogwood, PATCH = patch)
Stems1997Dogwood <- rename(Stems1997Dogwood, SAM = loc)
Stems1997Dogwood <- rename(Stems1997Dogwood, BLK = block)
Stems1997Dogwood <- rename(Stems1997Dogwood, SIZE = size)
Stems1998Dogwood <- rename(Stems1998Dogwood, PATCH = patch)
Stems1998Dogwood <- rename(Stems1998Dogwood, SAM = loc)
Stems1998Dogwood <- rename(Stems1998Dogwood, BLK = block)
Stems1998Dogwood <- rename(Stems1998Dogwood, SIZE = size)
Stems1999Dogwood <- rename(Stems1999Dogwood, PATCH = patch)
Stems1999Dogwood <- rename(Stems1999Dogwood, SAM = loc)
Stems1999Dogwood <- rename(Stems1999Dogwood, BLK = block)
Stems1999Dogwood <- rename(Stems1999Dogwood, SIZE = size)
Stems2000Dogwood <- rename(Stems2000Dogwood, PATCH = patch)
Stems2000Dogwood <- rename(Stems2000Dogwood, SAM = loc)
Stems2000Dogwood <- rename(Stems2000Dogwood, BLK = block)
Stems2000Dogwood <- rename(Stems2000Dogwood, SIZE = size)

# Find those locations (entire 4x4 m plots) that did not have dogwood in each year
Empty1995 <- anti_join(AllPossibleLocations, Stems1995Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
Empty1996 <- anti_join(AllPossibleLocations, Stems1996Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
Empty1997 <- anti_join(AllPossibleLocations, Stems1997Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
Empty1998 <- anti_join(AllPossibleLocations, Stems1998Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
Empty1999 <- anti_join(AllPossibleLocations, Stems1999Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
Empty2000 <- anti_join(AllPossibleLocations, Stems2000Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))

# Find pairs where there was an empty 4x4 in year t and dogwood in the internal 1x1 in year t+1
# Nursery tags are only applicable for the 1x1 m quadrat - so there could be small stems in the 4x4 quadrat
# But hopefully the buffer helps ensure these are truly new individuals, not ramets growing from elsewhere
NewDogwood1996 <- inner_join(Empty1995, Nursery1996Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
NewDogwood1997 <- inner_join(Empty1996, Nursery1997Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
NewDogwood1998 <- inner_join(Empty1997, Nursery1998Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
NewDogwood1999 <- inner_join(Empty1998, Nursery1999Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))
NewDogwood2000 <- inner_join(Empty1999, Nursery2000Dogwood, by = c("BLK", "PATCH", "SAM", "SIZE"))

# 96 is potentially missing one class 2 individual - A1A4 2 has new stems in the 1x1 m plots, but already(?) had some in the 4x4...
# 97 matches Jin Yao's calculations
# 98 - no new dogwood (while the code returns one plot, that plot had dogwood in 1996)
# 99 - A5A2 2 previously had dogwood, C5C2 2 appears to be a true new. B4A1 6 is unclear - no previous records for that quadrat
# 00 - D4B1 1 appears to be a true new. D4B4 2 also appears to be a true new.

# Exclude those results where there are also new things sprouting up in the 4x4
# Suggests to me that there is a large shrub outside of the 4x4 expanding laterally into the quadrat

# Which leaves only three plants...
# 1996 A1B1 1 - treat both ramets together as one individual (dies after one appearance)
# 1997 D4B4 2 (four years of data)
# 2000 D4B1 1 (only one year of data before monitoring becomes irregular)

# Combine data frames for filtering
CombinedData <- list(Metal1995Dogwood, Nursery1996Dogwood, Metal1996Dogwood, Nursery1997Dogwood, Metal1997Dogwood, Nursery1998Dogwood,
                     Metal1998Dogwood, Nursery1999Dogwood, Metal1999Dogwood, Nursery2000Dogwood, Metal2000Dogwood)
A1B1_1 <- lapply(CombinedData, function(df) {
  filter(df, BLK == "A1" & PATCH == "B1" & SAM == 1)
})
D4B4_2 <- lapply(CombinedData, function(df) {
  filter(df, BLK == "D4" & PATCH == "B4" & SAM == 2)
})
D4B1_1 <- lapply(CombinedData, function(df) {
  filter(df, BLK == "D4" & PATCH == "B1" & SAM == 1)
})

# The few growth data that exist for young dogwood
# Can't use statistically, but would be interesting to compare to general growth seen at Konza
# See if these few points fit within the points found at Konza
A1B1_1_1996 <- filter(Nursery1996Dogwood, BLK == "A1" & PATCH == "B1" & SAM == 1)
D4B1_1_2000 <- filter(Nursery2000Dogwood, BLK == "D4" & PATCH == "B1" & SAM == 1)

D4B4_2_1997 <- filter(Nursery1997Dogwood, BLK == "D4" & PATCH == "B4" & SAM == 2)
D4B4_2_1997$DAB <- as.character(D4B4_2_1997$DAB)
D4B4_2_1997$DBH <- as.character(D4B4_2_1997$DBH)
D4B4_2_1997$TAGNO <- as.integer(D4B4_2_1997$TAGNO)
D4B4_2_1998 <- filter(Nursery1998Dogwood, BLK == "D4" & PATCH == "B4" & SAM == 2)
D4B4_2_1998 <- D4B4_2_1998[, -c(18:19)]
D4B4_2_1999 <- filter(Nursery1999Dogwood, BLK == "D4" & PATCH == "B4" & SAM == 2)
D4B4_2_2000 <- filter(Nursery2000Dogwood, BLK == "D4" & PATCH == "B4" & SAM == 2)
D4B4_2_all <- bind_rows(D4B4_2_1997, D4B4_2_1998, D4B4_2_1999, D4B4_2_2000)

# need to fix character/integer type, then bind all together