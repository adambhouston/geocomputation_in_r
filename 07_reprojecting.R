# 7 reprojecting geographic data----
pacman::p_load(sf, terra, dplyr, spData, spDataLarge, lwgeom, tmap)

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
# transforming to a proj crs is essential if doing geometric operations
# leaflet package requires geographic CRS
# many web mapps require epsg 4326, wgs 84
# but planar geometry operations like buffering may require a projected crs
#
# * 7.6 which crs to use?----
# wgs 84/4326 is the most widely used geographic CRS, also used by GPS
# if using a public data set, probably best to stick with the CRS for that area
# a good projected default is UTM universal transverse mercator
# but best only to use if the area is relatively small and within 6 degrees of 
# central meridian
# crssuggest package takes a geographic crs and returns possible projected crs
# 
# * 7.7 reprojecting vector geometries----
# use st_transform
# 
# * 7.8 reprojecting raster geometries----
# raster reproj means creating a new object, with a different number of columns
# and rows
# attributes must be reestimated
# essentially two operations: changing extent, then resample pixel values
# best to avoid reproject rasters and reproject the vectors instead
# raster reprojection (warping) is done with project()
# raster transofmration can be done with stars
# looking at NLCD for utah, categorical data
cat_raster = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
cat(crs(cat_raster))
unique(cat_raster)
# project into wgs84
cat_raster_wgs84 = project(cat_raster, "EPSG:4326", method = "near")
# if transforming numeric raster, use bilinear or other method rather than
# nearest neighbor
# 
# * 7.9 custom map projections----
# transform zion into custom azimuthal equidistant crs
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
# calculate centroid
zion_centr = st_centroid(zion)
zion_centr_wgs84 = st_transform(zion_centr, "EPSG:4326")
st_as_text(st_geometry(zion_centr_wgs84))
# now modify the wkt definition of the AEQD CRS using this centroid point
# changing the central meridian and the latitude of origin
my_wkt = 'PROJCS["Custom_AEQD",
 GEOGCS["GCS_WGS_1984",
  DATUM["WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["Central_Meridian",-113.0263],
 PARAMETER["Latitude_Of_Origin",37.29818],
 UNIT["Meter",1.0]]'
zion_aeqd = st_transform(zion, my_wkt)

# mollweide projection is good for world mapping while preserving area
world_mollweide = st_transform(world, crs = "+proj=moll")
world_mollweide_gr = st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  lwgeom::st_transform_proj(crs = "+proj=moll")
tm_shape(world_mollweide_gr) +
  tm_lines(col = "gray") +
  tm_shape(world_mollweide) +
  tm_borders(col = "black") 

# * 7.10 exercises
# 1
nz_wgs = st_transform(nz, crs = "EPSG:4326") 
st_crs(nz_wgs)

# 2
world_merc = st_transform(world, crs = "+proj=tmerc")
st_crs(world_merc)
plot(st_geometry(world))
plot(st_geometry(world_merc))
world_wgs = st_transform(world_merc, crs = "EPSG:4326")
plot(st_geometry(world_wgs))

# 3 

# 4
transform = project(cat_raster, "EPSG:4326", method = "bilinear")
# turns a categorical into a continuous raster
# 
