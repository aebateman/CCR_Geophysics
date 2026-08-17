library(here)
library(sf)
library(dplyr)
library(zoo)
library(ggplot2)

# Read your CSV
survey <- read.csv(here("GNSS Data", "20260528", "Positions file", "CCR_20260528_corrected_cleaned.csv"))

# Build a 3D point object: WGS84 lon/lat/ellipsoidal height (EPSG:4979 = WGS84 3D geographic)
pts <- st_as_sf(survey, coords = c("Longitude", "Latitude", "Ellipsoidal.height"),
                crs = 4979)

# Transform straight to NAD83(2011) UTM zone 17N (horizontal) + NAVD88 height (vertical)
# EPSG:6346 = NAD83(2011) / UTM zone 17N,  EPSG:5703 = NAVD88 height
pts_out <- st_transform(pts, "EPSG:6346+5703")

pts_out <- pts_out |>
  select(Description, geometry)

coords <- st_coordinates(pts_out)
survey$Easting    <- coords[, "X"]
survey$Northing   <- coords[, "Y"]
survey$Elevation  <- coords[, "Z"]   # NAVD88 orthometric elevation

#clean df
survey <- survey |>
  select(Name, Description, Easting, Northing, Elevation)

#interpolate for missing points
pt_seq <- data.frame (Name = 1:64)  #max(survey$Name) if last pt good
 
survey <- left_join(pt_seq, survey, by = "Name")

survey <- survey |>
  arrange(by = 'Name')

survey_full <- survey |>
  mutate(
    Easting   = na.approx(Easting, x = Name, na.rm = FALSE),
    Northing  = na.approx(Northing, x = Name, na.rm = FALSE),
    Elevation = na.approx(Elevation, x = Name, na.rm = FALSE)
  )


ggplot() +
  geom_point(data = survey_full, aes(x = Easting, y = Elevation), color = 'steelblue') +
  geom_point(data = survey, aes(x = Easting, y = Elevation)) 

write.csv(survey_full, "survey_data_utm17n_navd88.csv", row.names = FALSE)

