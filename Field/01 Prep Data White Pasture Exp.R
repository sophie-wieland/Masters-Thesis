# Prepare White Pasture Field Experiment data
# Nov 4, 2026
# Sophie Wieland
getwd()
library(tidyverse)
setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Field White Pasture Experiment")

AtInstallation <- read.csv("Dogwood_Demography_At_Installation.csv")
June8 <- read.csv("Dogwood_Demography_June_8_2026.csv")
July3 <- read.csv("Dogwood_Demography_July_3_2026.csv")
July16 <- read.csv("Dogwood_Demography_July_16_2026.csv")
PostGoat <- read.csv("Dogwood_Demography_Post_Goat_Clip.csv")
Aug6 <- read.csv("Dogwood_Demography_August_6_2026.csv")
Sept8 <- read.csv("Dogwood_Demography_September_8_2026.csv")
Oct6 <- read.csv("Dogwood_Demography_Oct_6_2026.csv")
PostClip <- read.csv("Dogwood_Demography_Post_Late_Clip.csv")

# Fixing one extra empty column
June8 <- June8[,-10]

# Combining data
Data <- bind_rows(AtInstallation, June8, July3, July16, PostGoat, Aug6, Sept8, Oct6, PostClip)
# Convert the date column to the date class
Data$Date <- as.Date(Data$Date, format = "%B %d, %Y")
# Add a column for treatment
Data <- Data %>%
  mutate(
    Treatment = case_when(
      Plot %in% c(6, 10, 5) ~ "ClipLate",
      Plot %in% c(9, 2, 13) ~ "Control",
      Plot %in% c(12, 15, 11) ~ "Goats",
      Plot %in% c(8, 1, 14) ~ "Fire",
      Plot %in% c(4, 3, 7) ~ "ClipEarly"
    )
  )

ggplot(Data, aes(x = Date, y = Height))+
  geom_point(data = subset(Data, Height == 0), color = "red")+
  geom_line(aes(group = ID))+
  facet_wrap(~Treatment)+
  geom_vline(xintercept = as.numeric(as.Date("2026-07-19")))

ggplot(Data, aes(x = Date, y = Width))+
  geom_point(data = subset(Data, Width == 0), color = "red")+
  geom_line(aes(group = ID))+
  facet_wrap(~Treatment)+
  geom_vline(xintercept = as.numeric(as.Date("2026-07-19")))