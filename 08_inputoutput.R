# 8 geographic data I/O----
pacman::p_load(sf, terra, dplyr, spData, tmap, rnaturalearth, geodata)

# * 8.1 introduction----
# 
# * 8.2 retrieving open data----
# geoportals
# GEOSS and Copernicus have many raster datasets
# SEDAC and INSPIRE have vector datasets
# USGS earth explorer
# files on static urls can be downloaded with download.file()
# 
# * 8.3 geographic data packages----
# tidygeocoder seems useful
# takes addresses and returns coordinates
# 
# * 8.4 geographic web services----
# OGC has specs for web services
# use package httr
# 
# * 8.5 file formats----
# files can either store vector or raster
# spatial databases like postgis can store both
# GDAL is the base for reading and writing different file formats
# shp files cant be over 2 gb
# geopackage is a suitable replacement
# stores data in SQLite
# Geotiff is a raster format
# COG is cloud optimized geotiff
# las and laz formats for lidar point clouds
# 
# * 8.6 data input----
# * * 8.6.1 vector data----
# geojson and gpkg can be imported directly
# geojsonsf package may be faster for reading .geojson files
# 
# * * 8.6.2 raster data----
# vsicurl prefix in front of url can access web maps
# vsizip gets zipped files from the web without having to unzip them after
# 
# * 8.7 data output----
# * * 8.7.1 vector data----
# write_sf()
# add append to add multiple layers to a file
# 
# * * 8.7.2 raster data----
# writeRaster()
# data type depends on which values you use, categorical or continuous
# 
# * 8.9 exercises
# 1

# 3
c_h = system.file("misc/cycle_hire_xy.csv", package = "spData") |> 
  read.csv() |> 
  st_as_sf(coords = c("X", "Y"))

germany_borders = ne_countries(country = "Germany", returnclass = "sf")
plot(germany_borders)

gmmt = worldclim_global(var = "tmin", res = 5, path = tempdir())
names(gmmt)
june_gmmt = gmmt |> terra::subset("wc2.1_5m_tmin_06")
plot(june_gmmt)
