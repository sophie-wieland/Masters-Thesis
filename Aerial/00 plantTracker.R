# testing the functionality of plantTracker
# January 15, 2026
# Sophie Wieland
getwd()
library(sf)
library(plantTracker)
library(dplyr)
# load data
# hand traced polygons of all woody vegetation
# includes both trees and shrubs
# includes all species, not just dogwood

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Aerial Imagery")

NaipData2023 <- st_read("polygon_tracing_pack_burn_MASTERKEY/polygon_tracing_pack_burn_MASTERKEY/naip 2023 hand polygons/naip 2023 hand polygons")

NaipData2021 <- st_read("polygon_tracing_pack_burn_MASTERKEY/polygon_tracing_pack_burn_MASTERKEY/naip 2021 hand polygons")

NaipData2017 <- st_read("polygon_tracing_pack_burn_MASTERKEY/polygon_tracing_pack_burn_MASTERKEY/naip 2017 hand polygons")

NaipData2012 <- st_read("polygon_tracing_pack_burn_MASTERKEY/polygon_tracing_pack_burn_MASTERKEY/naip 2012 hand polygons")

NaipData2008 <- st_read("polygon_tracing_pack_burn_MASTERKEY/polygon_tracing_pack_burn_MASTERKEY/naip 2008 hand polygons")

# fix a naming inconsistency
colnames(NaipData2021)[2] <- "woody_type"

# combine all data frames into one sf data frame

NaipDataCombined <- rbind(NaipData2008, NaipData2012, NaipData2017, NaipData2021, NaipData2023)

#st_write(NaipDataCombined, "NaipDataCombined.shp")

# create inventory list
# isolate year and watershed columns

NaipDataCombined$watershed[is.na(NaipDataCombined$watershed)] <- "outside"
AllYearsWatersheds <- cbind(NaipDataCombined$watershed, NaipDataCombined$year)

# remove NAs, observations not within watershed boundaries
# select unique combinations of watershed and year

YearsWatersheds <- unique(AllYearsWatersheds)
InventoryDataFrame <- as.data.frame(YearsWatersheds)

IntermediateList <- split(InventoryDataFrame[[2]], InventoryDataFrame[[1]])
InventoryList <- lapply(IntermediateList, as.numeric)

# turning off spherical geometry fixes error "Loop 0: Edge 1 is degenerate (duplicate vertex)"
# but does not fix error "Edge _ crosses edge _"
# I need to check exactly what this function means, and if this is valid in this use case
sf_use_s2(FALSE)

st_crs(NaipDataCombined)

# transforming the data to add a buffer of zero
# this fixes the error "Edge _ crosses edge _"
# but again, I don't know if this is a legit way to clean the data
st_is_valid(NaipDataCombined, reason = TRUE)
TrialAgain <- st_transform(NaipDataCombined, 3857)
TentativelyFixedData <- st_buffer(TrialAgain, 0.0)
Data_fixed <- st_transform(TentativelyFixedData, st_crs(NaipDataCombined))

VisualizeAll <- st_is_valid(Data_fixed, reason = TRUE)

# the last error is "Self-intersection[-96.567494931001 39.0770557825197]"

# run command to figure out what self-intersects
(Data_fixed[17294,]$geometry[[1]][[1]])
# points 2 and 3

coordinates <- Data_fixed[17294,]$geometry[[1]][[1]]
coordinates <- coordinates[-2, ]

Data_fixed[17294,]$geometry[[1]][[1]] <- coordinates

VisualizeAgain <- st_is_valid(Data_fixed)

# fill in NAs in species column of Data_fixed with unknown category

is.na(Data_fixed$woody_type) #return TRUE if there are NA values

Data_fixed$woody_type[is.na(Data_fixed$woody_type)] <- "unknown"

# delete rows with empty polygons
Doubly_fixed_data <- Data_fixed[-c(75, 85, 350, 370), ]

# this is the actual plantTracker function
#Demographic_data <- trackSpp(Doubly_fixed_data, InventoryList, dorm = 0, buff = 0, clonal = FALSE, species = "woody_type",
         #site = "site", quad = "watershed", year = "year", geometry = "geometry",
         #printMessages = TRUE, flagSuspects = FALSE)

###############################################################
# making a fake version where years are sequential

Doubly_fixed_data$Trial <- recode(Doubly_fixed_data$year, '2008' = 11, '2012' = 12, '2017' = 13, '2021' = 14, '2023' = 15)

InventoryList <- lapply(InventoryList, function(x) {
  recode(x,
         `2008` = 11,
         `2012` = 12,
         `2017` = 13,
         `2021` = 14,
         `2023` = 15)
})

Trial_demographic_data <- trackSpp(Doubly_fixed_data, InventoryList, dorm = 0, buff = 0, clonal = FALSE, species = "woody_type",
                             site = "site", quad = "watershed", year = "Trial", geometry = "geometry",
                             printMessages = TRUE, flagSuspects = FALSE)

write.csv(Trial_demographic_data, "RecodedDogwoodDemographic.csv")

###############################################################
# rearranging the dataframe for continued analysis

PlaceholderDataframe <- data.frame(Trial_demographic_data$watershed, Trial_demographic_data$Trial, 
                                   Trial_demographic_data$year,
                                   Trial_demographic_data$trackID,
                                   Trial_demographic_data$basalArea_ramet, 
                                   Trial_demographic_data$size_tplus1,  
                                   Trial_demographic_data$survives_tplus1)
PlaceholderDataframe <- rename(PlaceholderDataframe, Watershed = Trial_demographic_data.watershed,
                               Year = Trial_demographic_data.year,
                               FakeYear = Trial_demographic_data.Trial,
                               ID = Trial_demographic_data.trackID,
                               size = Trial_demographic_data.basalArea_ramet,
                               size_tplus1 = Trial_demographic_data.size_tplus1,
                               exist_tplus1 = Trial_demographic_data.survives_tplus1)
PlaceholderDataframe$ID <- paste(PlaceholderDataframe$Watershed, PlaceholderDataframe$ID, sep = "_")

Temp2008 <- filter(PlaceholderDataframe, Year == 2008)
Temp2008 <- rename(Temp2008, size2008 = size,
                   size2012 = size_tplus1,
                   exist2012 = exist_tplus1)
Temp2008 <- mutate(Temp2008, exist2008 = NA)
Temp2008 <- relocate(Temp2008, exist2008, .after = size2008)
Temp2012 <- filter(PlaceholderDataframe, Year == 2012)
Temp2012 <- rename(Temp2012, size2012 = size,
                   size2017 = size_tplus1,
                   exist2017 = exist_tplus1)
Temp2017 <- filter(PlaceholderDataframe, Year == 2017)
Temp2017 <- rename(Temp2017, size2017 = size,
                   size2021 = size_tplus1,
                   exist2021 = exist_tplus1)
Temp2021 <- filter(PlaceholderDataframe, Year == 2021)
Temp2021 <- rename(Temp2021, size2021 = size,
                   size2023 = size_tplus1,
                   exist2023 = exist_tplus1)
Temp2023 <- filter(PlaceholderDataframe, Year == 2023)
Temp2023 <- rename(Temp2023, size2023 = size,
                   size20xx = size_tplus1,
                   exist20xx = exist_tplus1)

# rejoin dataframes so there is one row for every unique (ID + watershed) shrub
Recombine <- full_join(x = Temp2008, y = Temp2012, by = c("ID", "Watershed", "FakeYear", "Year", "size2012"))
MoreCombining <- full_join(x = Recombine, y = Temp2017, by = c("ID", "Watershed", "FakeYear", "Year", "size2017"))
ContinuedCombining <- full_join(x = MoreCombining, y = Temp2021, by = c("ID", "Watershed", "FakeYear", "Year", "size2021"))
ChangeinSize <- full_join(x = ContinuedCombining, y = Temp2023, by = c("ID", "Watershed", "FakeYear", "Year", "size2023"))

# fill in missing exist values (1=exist, 0=was not detected)
ChangeinSize$exist2008 <- as.integer(!is.na(ChangeinSize$size2008))
ChangeinSize$exist2012 <- as.integer(!is.na(ChangeinSize$size2012))
ChangeinSize$exist2017 <- as.integer(!is.na(ChangeinSize$size2017))
ChangeinSize$exist2021 <- as.integer(!is.na(ChangeinSize$size2021))
ChangeinSize$exist2023 <- as.integer(!is.na(ChangeinSize$size2023))

write.csv(ChangeinSize, "ChangeinSize.csv")
#write.csv(PlaceholderDataframe, "TrackedPolygonsRecodedYears.csv")  

# Concerns: how is plantTracker calculating area? It doesn't match area calculated with QGIS
# For that matter, how is QGIS calculating area? (It's in something silly like square degrees)