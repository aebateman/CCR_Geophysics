#This script is written to create elevation data files for GPR post-processing
#AEB 2/3/26

library(sf)
library(mapview)
library(terra)


#import .kmz of GPS path
line_gps <- st_read("C:\\Users\\abate\\OneDrive\\Documents\\Carvins_Cove\\GPR data_20260112\\GPS_linepaths.kml")

#Define which line
line <- line_gps[8,-2]

#view line
mapview(st_zm(line))

#Define points for interactive plotting
pts <- st_cast(st_zm(line), "POINT")
mapview(pts)

#remove points from coordinates 
coords <- st_coordinates(line)[,c("X", "Y")]
coords_cln <- coords[-c(3501, 3503), ] #use myFeatureID number from the map view and add to vector
coords_cln <- coords_cln[complete.cases(coords_cln), ] #drop NA rows

line_cln <- st_linestring(coords_cln, dim = "XY") #creates simple feature list
line_cln <- st_sfc(line_cln, crs = st_crs(line))
line_cln <- st_sf(Name = line$Name, geometry = line_cln)

mapview(line_cln)

#convert to appropriate meter based coordinate system (UTM 17N)
line_cln <- st_transform(line_cln, "EPSG:26917")
st_crs(line_cln)

#simplify line
simple <- st_simplify(line_cln, dTolerance = 1)
mapview(simple)

#import DEM (make sure same coordinate system!)
DEM_file <- "C:\\Users\\abate\\OneDrive\\Documents\\Github\\RHESSys_Tutorial\\Spatial_Data\\Inputs\\usgs_dem_ccr_UTM.tif"

DEM <- rast(DEM_file)
st_crs(DEM)
plot(DEM)

#turn back into points
#samples line at "density" per meter
coords_fin <- st_line_sample(
  line_cln,
  density = .5,
  type = "regular"
)

coords_fin <- st_cast(coords_fin, "POINT") #transform from line to point geometry
coords_fin <- st_sf(geometry = coords_fin) #turn points into sf geometry 


#extract DEM elevation for 1 m intervals along the line
elev <- extract(DEM, coords_fin)

#extract easting and northing and attach to DEM elevation
elev_table <- st_coordinates(coords_fin)[,c("X", "Y")] %>%
 as.data.frame() %>%
  rename(
    Easting = X,
    Northing = Y
  ) %>%
  mutate(
    elev_m = elev$usgs_dem_ccr_UTM
  ) 

#convert easting and northing to distance along line
elev_table <- elev_table %>%
  mutate(
    int_dist = sqrt(
      (Easting - lag(Easting))^2 + (Northing - lag(Northing))^2
      ),
    position_m = cumsum(replace_na(int_dist, 0))
    )  
  
#select only needed columns
top_file <- elev_table%>%
  select(position_m, elev_m)

#write position, elevation file
write_delim(
  top_file,
  "C:\\Users\\abate\\OneDrive\\Documents\\Carvins_Cove\\GPR data_20260112\\lineX.top",
  col_names = FALSE
)


# ##USE THIS FOR CLEANING GPS COLLECTED ELEVATION DATA
# #import .kmz of GPS path
# with_z_gps <- st_read("C:\\Users\\abate\\OneDrive\\Documents\\Carvins_Cove\\GPR data_20260112\\Project2.kml")
# 
# 
# #cast to linestring geometry type
# with_z_gps <- st_cast(with_z_gps[1,], "POINTS")
# 
# #grab GPS elevation data
# gps_elev <- st_coordinates(with_z_gps)[,c("X", "Y", "Z")]
# 
# #create feature line
# gps_line <- st_linestring(gps_elev, dim = "XYZ")
# gps_line <- st_sfc(gps_line, crs = st_crs(with_z_gps))
# gps_line <- st_sf(Name = gps_line$Name, geometry = gps_line)
# 
# line_cln <- st_linestring(coords_cln, dim = "XY") #creates simple feature list
# line_cln <- st_sfc(line_cln, crs = st_crs(line))
# line_cln <- st_sf(Name = line$Name, geometry = line_cln)
# 
# #convert to UTM 17N
# #with_z_gps <- st_transform(with_z_gps, "EPSG:26917")


