# 7 reprojecting geographic data----
pacman::p_load(sf, terra, dplyr, spData, spDataLarge, lwgeom)

# * 7.1 introduction----
# geographic crs has units in degrees long and lat
# proj crs is in meters from a datum (the origin of the projection)
# 
# * 7.2 CRSs----
# PROJ is an OS library that transforms CRSs
# proj4 strings are now outdated
# replaced with EPSG, which is future proof
# Open geospatial consortium developed the Well Known Text wkt
# wkts are unambiguous and detailed
# 
# * 7.3 querying and setting coordinate systems----
# starting with vector objects
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
new_vector = read_sf(vector_filepath)
st_crs(new_vector)
st_crs(new_vector)$IsGeographic
st_crs(new_vector)$units_gdal
st_crs(new_vector)$srid
st_crs(new_vector)$proj4string

# raster
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
cat(crs(my_rast))

# if a CRS is not set, then sf does not guess what it is
# geojson will automatically set an unknonw to wgs84
# 
# * 7.4 geometry operations on projected and unprojected data----
# without CRS, sf uses GEOS for operations
# with CRS, sf uses GEOS or S2 spherical geometry engine
# examples below of buffers around london of 100 km or 1 degree
# create a projected and unprojected version of london
london = data.frame(lon = -0.1, lat = 51.5) %>% 
  st_as_sf(coords = c("lon", "lat"))
london_geo = st_set_crs(london, "EPSG:4326")

# create buffers
london_buff_no_crs = st_buffer(london, dist = 1)   # incorrect: no CRS
london_buff_s2 = st_buffer(london_geo, dist = 1e5) # silent use of s2
london_buff_s2_100_cells = st_buffer(london_geo, dist = 1e5, max_cells = 100) 

# turn off S2 and then buffer the projected london
sf::sf_use_s2(FALSE)
london_buff_lonlat = st_buffer(london_geo, dist = 1) # incorrect result
sf::sf_use_s2(TRUE)
# this attempted to use planar geometry operations on lon/lat data
# in situations where curved boundaries are used, best to transform to a proj CRS
# 
# * 7.5 when to reproject?----