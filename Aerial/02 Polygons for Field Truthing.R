# Making a copy of polygons to use for field validation
# Fixing the invalid geometries
# June 17, 2026
# Sophie Wieland
getwd()
library(sf)
library(dplyr)

setwd("C:/Users/Sophie.Wieland/Documents/KSU/Masters Thesis/Aerial Imagery")

AllPolygons <- st_read("NaipDataCombined.shp")

# turning off spherical geometry fixes error "Loop 0: Edge 1 is degenerate (duplicate vertex)"
# but does not fix error "Edge _ crosses edge _"
# I need to check exactly what this function means, and if this is valid in this use case
sf_use_s2(FALSE)

st_crs(AllPolygons)

# transforming the data to add a buffer of zero
# this fixes the error "Edge _ crosses edge _"
# but again, I don't know if this is a legit way to clean the data
st_is_valid(AllPolygons, reason = TRUE)
TrialAgain <- st_transform(AllPolygons, 3857)
TentativelyFixedData <- st_buffer(TrialAgain, 0.0)
Data_fixed <- st_transform(TentativelyFixedData, st_crs(AllPolygons))

# Error at 17294: "Self-intersection[-96.567494931001 39.0770557825197]"

# run command to figure out what self-intersects
(Data_fixed[17294,]$geometry[[1]][[1]])
# points 2 and 3

coordinates <- Data_fixed[17294,]$geometry[[1]][[1]]
coordinates <- coordinates[-2, ]

Data_fixed[17294,]$geometry[[1]][[1]] <- coordinates

VisualizeAgain <- st_is_valid(Data_fixed)

st_write(Data_fixed, "AllPolygons.shp")