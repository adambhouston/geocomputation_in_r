#'----
#'title: "Chapter 2, Geocomputation in R"
#'output: github document
#'----
#'

# 2 geographic data in r----
# * 2.1 introduction----
pacman::p_load(sf, terra, spData, spDataLarge)

# * 2.2 vector data ----
# * * 2.2.1 an introduction to simple features
#sf provides same functions as sp, rgdal, and rgeos
#sf interfaces with GEOS, GDAL, proj, and s2
#s2 is a spherical geometry library for non-planar operations,
#as opposed to GEOS
#
#sf objects are stored in a df, with geo data in a special column
#lets look at the world dataset in spData

class(world)
names(world)

#world$geom is a list column that contains the coordinates of country polygons
#treating objects as df is useful because you can use common functions
summary(world$lifeExp)
summary(world["lifeExp"])

#the geom column is sticky because it kept unless removed by user
#sf objects are easy to subset
world_mini <- world[1:2, 1:3]
world_mini

#this subset is of two rows and three columns
#geo metadata is included, such as geom type, dim, bbox, crs

# * * 2.2.2 why simple features?----
# a major advantage is that work is cross transferable
# sf is generally better than sp
# sf can be treated as dfs
# works well with tidyverse
# eg read_sf works like a tidyverse tibble

world_df = st_read(system.file("shapes/world.shp",package = "spData"))
world_tbl = read_sf(system.file("shapes/world.shp",package = "spData"))
class(world_df)
class(world_tbl)

# sf is go-to for vectors in R
# spatstat is stillused for statistics on vectors
# sf is futureproof
# 
# * * 2.2.3 basic map making ----
# created with plot
# multiple variable
plot(world[3:6])

# singl variable
plot(world["pop"])

# make a layer of plots using add = true
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# make a plot of the world with cirlces representing country pops
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000 #set circle size prop. to pop.
world_cents = st_centroid(world, of_largest_polygon = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# stcentroid converts polygons into points, then uses cex o vary their aes
# expandBB expands bbox eg
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "green", lwd = 3)
plot(world_asia[0], add = TRUE)

# * * 2.2.4 geometry types----
# geometrycollection can have multiple types of geometries
# 
# * * 2.2.5 the sf class----
# sfs are made of geometries and non-geographic attributes
# geometries are simple feature groups
# sfgs get combined into sfcs
# sfcs get combined with data frames in order to create sfs
# st_sf is used to create an sf with three attributes:
# temp, place name, and date
# along with geometry: the coordinates of london
lnd_point = st_point(c(0.1, 51.5)) # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326) #sfc object
lnd_attrib = data.frame( #dataframe
  name = "london",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom) #sf object

# coords used to create sfg
# converted to sfc with CRS
# attributes stored in a df
# combined into an sfc object with st_sf
lnd_sf
class(lnd_sf)

# sf objects have two classes, both sf and data.frame
# sfs are data frames with a spatial extension
# 
# * * 2.2.6 simple feature geometries----
# sfgs are the sf geometry types: point, linestring, polygon, and multis
# these can be created with st_nameoftype
# can be made from:
# single point: numeric vector
# matrix: each row represents a point, multipoint, or linestring
# list: collection of objects, such as matrices, multilinestrings, or geo 
# collections
# 
# XY and XYZ coordinates can be speced with an M variable, typically measurement
# accuracy
# 
# st_point creates single points from numeric vectors, eg:
st_point(c(5, 2, 1), dim = "XYM") #XYM point

# matrices are used to create multipoint and linestring objects
# use rbind to create matrices
multipoint_matrix = rbind(c(5,2), c(1,3), c(3,4))
st_multipoint(multipoint_matrix)

# use lists for create of multilinestrings, multipolys, and geo collections
polygon_list = list(rbind(c(1,5), c(2,2), c(4,1), c (4,4), c(1,5)))
st_polygon(polygon_list)

# POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

# GEOMETRYCOLLECTION
gemetrycollection_list = list(st_multipoint(multipoint_matrix),
                              st_linestring(linestring_matrix))
st_geometrycollection(gemetrycollection_list)

# * * 2.2.7 simple feature columns (sfc)
# sfcs are made of a list of sfg objects and the coords ref system
# combine two sfs into one object with two features:
point1 = st_point(c(5,2))
point2 = st_point(c(1,3))
point_sfc = st_sfc(point1, point2, crs = 4326)
point_sfc

# all geometries in sfc objects must have same CRS
# set CRS with identifier
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG: 4326")

# * * 2.2.8 the sfheaders package
# sfheaders speeds up usage of sf objects
# doesn't depend on sf, uses C++ code through header files
# start by creating a simple vector
v = c(1,1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)

v_sfg_sfh

# create sfg objects usig matrices and dataframes in sfheaders
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)

# dataframes
df = data.frame(x = 1:4, y = 4:1)
df
sfheaders::sfg_polygon(obj = df)

# you can also create sfcs and sfs with similar commands
# sfheaders is also good for turning sf objects into dfs and vice versa
# 
# * * 2.2.9 spherical geometry operations with S2
# spherical geometry is based on a round world, while planar geometry is not
# s2 is a type of Discrete Global Grid System
# s2 is default on with sf
# look at what happens when you create buffers with S2 on and off
india_buffer_with_s2 = st_buffer(india, 1)
sf_use_s2(FALSE)
india_buffer_without_s2 = st_buffer(india, 1)
par(mfcol = 2)
plot(st_geometry(india_buffer_with_s2), main = "S2 on")
plot(st_geometry(india_buffer_without_s2), main = "S2 off")
sf_use_s2(TRUE)

# s2 may throw errors occasionally, try turning it off then
# to turn it off for entirety, make a project called .Rprofile in the root 
# directiroy with the sf_use_s2 command off
# 
# * 2.3 raster data
# terra and stars are the two common raster models
# 1) terra uses grids, stars allows other models of data
# terr used with 1 or multi layered rasters
# stars can store data cubes with many layers for many moments in time,
# and many attributes
# 2) both can read raster data into memory or just metadata
# terra based on c++
# stars stores values as lists of arrays
# 3) stars functions are more like sf
# terra has its own class of ojbects called spatvector
# 4) terra has built in functions
# stars uses built in functions, existing r functions, and dplyr functions
# conversion between the two is easy
# 
# * * 2.3.2 an introduction to terra
# lets use datasets from spDataLarge
# looks at zion nationla park
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge", mustWork = TRUE)
my_rast = rast(raster_filepath)
class(my_rast)
my_rast

# * * 2.3.3 basic map making with terra
par(mar = c(1,1,1,1), mfcol = c(1,1))
plot(my_rast)

# other functions include:
# plotRGB for making RGB plots
# tmap for nice maps
# levelplot from rastervis pacakage, for faceting
# nice thing to show change over time
# 
# * * raster classes
# spatraster class is used for terra
# you can make rasters by hand
# default crs is wgs84
# rast fills cells rowwise, unlike matrix
new_raster = rast(nrows = 6, ncols = 6, resolution = 0.5,
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)
plot(new_raster)

# can hold multiple layers, eg multispectrum or time-series
multi_raster = system.file("raster/landsat.tif", package = "spDataLarge", mustWork = TRUE)
multi_rast = rast(multi_raster) 
multi_rast
nlyr(multi_rast)

# layers can be selected with subset by number or name
multi_rast3 = subset(multi_rast,3)
multi_rast4 = subset(multi_rast, "landsat_4")

#combine layers again
multi_rast34 = c(multi_rast3, multi_rast4)

plot(multi_rast)                     
plot(multi_rast34)

# spatraster objects do not store values, they just store pointer to foile
# cannot be directly saved to .rds or used in cluster computing
# 
# * 2.4 geographic and projected coordinate systems
# CRS defines how spatial data is related to the surface of the eart
# 
# * * 2.24.1 geographic crs
# defined by lat lon, which is an angular distance from prime meridian and equator
# not measured in meters, but degrees
# use a perfect sphere or ellipsoid model
# equator radius is 11.5 km longer than polar radius
# 
# the datum relates the cartesian coordinates to location
# geocentric datum (like wgs84) uses the center of the earth
# local datum shifts ellipsoid to aligh with surface at particular location
# 
# * * 2.4.2 projected crs
# based on projecting geographic crs into a flat surface, with oriin, x, y, and
# unit of measurement
# distorts area, direction, distance, or shape
# 
# conic, cylindrical, or planar projections are used
# conic is good for midlatitude
# cyclindirical is used when mapping the world
# planar is used for polar regions
# 
# * 2.5 units
# geometry data in sf objects have native support for units
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)
# returns the area in meters squared
# convert to km sq
units::set_units(st_area(luxembourg), km^2)

# right now only sf supports units, so watch when using raster data

# * 2.6 exercises
# 1
summary(st_geometry(world))
# multipolygon geometry, 177 countries, crs is wgs84(epsg4326)
# 
# 2
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 1000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
# cex is the character expansion factor, sets the size of things relative to default
# world_cents is a point character. adding a cex made the area of the point proportional
# to the size of the population
# im going to make a chloropleth map that shows population per area in colors
# 
head(world)
world = dplyr::mutate(world, 
       pop_per_km2 = pop/area_km2)

library(tmap)
tm_shape(world)+
  tm_polygons("pop_per_km2",
              style = "quantile",
              title = "World population per sq. km.")

# same thing but with plot
plot(world["pop_per_km2"],
     breaks = "quantile", nbreaks = 10)

# create maps of nigeria in context
par(mfcol = c(1,1))
nigeria = world[world$name_long == "Nigeria", ]
world_coords = st_coordinates(world_cents)
plot(st_geometry(nigeria), expandBB = c(1, 1, 1, 3), col = "gray", lwd = 4)
plot(africa[0],
     add = TRUE )
text(world_coords, world$name_long)

# 4 
my_raster = rast(nrow = 10, ncol = 10,
                 vals = runif(100, min = 0, max = 10))
plot(my_raster)

# 5 
nlcd = system.file("raster/nlcd.tif", package = "spDataLarge") %>% 
  rast()
class(nlcd)
crs(nlcd)
nlyr(nlcd)
ncell(nlcd)
summary(nlcd)
res(nlcd)
ext(nlcd)
plot(nlcd)
