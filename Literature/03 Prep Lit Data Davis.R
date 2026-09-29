# Preparing data from Davis 1993
# August 11, 2026
# Sophie Wieland
getwd()

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Literature Search for Model Building")

Germ <- read.csv("Davis Germination.csv")
# Greenhouse rate is similar to the ~30% I reached
# 13.4% is perhaps the most useful rate here to use for wild germination

Survival <- read.csv("Davis Survival.csv")
# When data in September/October is available for my field experiment, compare to 91% seen here

Growth <- read.csv("Davis Growth.csv")
# Compared to my field experiment, this growth is quite low (did mine have a significant head start from greenhouse time?)