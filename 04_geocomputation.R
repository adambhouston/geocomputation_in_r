# 4 spatial data operations----
pacman::p_load(sf, terra, dplyr, spData)

elev = rast(system.file("raster/elev.tif", package = "spData"))

# * 4.1 introduction----
# spatial operations can be done in a number of ways
# spaptial joins with intersects or close by other objects
# type of spatial relationship must be considered
# all spatial objects are related in space, so distance can be used to explore 
# the strength of a relationship
# raster operatoins include subsetting, merging, map algebra
# 
# * 4.2 spatial operations on vector data----
# * * 4.2.1 spatial subsetting----
# take a spatial object and return a new object with only the featurs that
# relate to another object
# use the [] operator
# op = st_intersects() is the default topological relation
# 
canterbury = nz |> filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

nz_full = tm_shape(nz) + tm_polygons(col = "white")+
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.25)+
  tm_layout(main.title = "High points in New Zealand", main.title.size = 1,
            bg.color = "lightblue")
nz_cant = tm_shape(nz) + tm_polygons(col = "white")+
  tm_shape(canterbury) + tm_fill(col = "gray")+
  tm_shape(canterbury_height) + tm_symbols(shape = 2, col = "red", size = 0.25)+
  tm_layout(main.title = "High points in Canterbury", main.title.size = 1,
            bg.color = "lightblue")
tmap_arrange(nz_full, nz_cant, ncol = 2)

# x[y, ] subsets features of x using contents of source y
# x and y must both be geographic objects
# st_intersects includes features in the target that touch, cross, or are within
# the source
# the opposite of st_intersects is st_disjoint

nz_cant_disjoint = nz_height[canterbury, , op = st_disjoint]

nz_cant_disj_map = tm_shape(nz) + tm_polygons(col = "white")+
  tm_shape(canterbury) + tm_fill(col = "gray")+
  tm_shape(nz_cant_disjoint) + tm_symbols(shape = 2, col = "red", size = 0.25)+
  tm_layout(main.title = "High points not in Canterbury", main.title.size = 1,
            bg.color = "lightblue")
tmap_arrange(nz_full, nz_cant_disj_map, ncol = 2)

# the empty argument when subsetting can be used to specify which attribute 
# column to choose
# we can also create objecst with topological operators
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)
sel_sgbp
sel_logical = lengths(sel_sgbp) >0 #this converts the sgbp into a logical vector
canterbury_height2 = nz_height[sel_logical, ] #this is the same as the previous
#subsetting operatoin
canterbury_height == canterbury_height2 # this proves it is true

#sgbp stands for sparse geometry binary predicate, it is the same length as nz_height
# here's one more method
canterbury_height3 = nz_height |> 
  st_filter(y = canterbury, .predicate = st_intersects)
canterbury_height2 == canterbury_height3

# * *4.2.2 topological relations----
# binary topological relationships can only be true or false
# they are about the spatial relationships between two objects defined by
# ordered sets of points
# functions testing topological relations are called binary predicates
# lets create a reprex
polygon_matrix = cbind(
  x = c(0, 0, 1, 1, 0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_matrix
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

line_sfc = st_sfc(st_linestring(cbind(
  x = c(0.4, 1),
  y = c(0.2, 0.5)
)))

point_df = data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)

point_sf = st_as_sf(point_df, coords= c("x", "y"))

par(pty = "s")
plot(polygon_sfc, border = "red", col = "gray", axes = TRUE)                    
plot(line_sfc, lwd = 5, add = TRUE)
plot(point_sf, add = TRUE, lab = 1:4, cex = 2)
text(point_df[,1]+0.02, point_df[,2]+0.04, 1:3, cex = 1.3)

# which points intersect with the polygon?
st_intersects(point_sf, polygon_sfc)
#points 1 and 3 intersect in some way
# this returns a sparse matrix which only registers a relation if one exists
# return dense matrix like this:
st_intersects(point_sf, polygon_sfc, sparse = FALSE)

st_within(point_sf, polygon_sfc) # only point 3 is fully within
st_touches(point_sf, polygon_sfc) # only point 1 just touches the polygon

# which points are within a certain distance of the polygon
st_distance(point_sf, polygon_sfc)
# 1 and 3 are 0 units away because they are in or touch polygon
st_is_within_distance(point_sf, polygon_sfc, dist = 0.2)

# * * 4.2.3 DE-9IM strings----
# dimensionally extended 9-intersection model
# I have no idea how this works
# 
# * * 4.2.4 spatial joining----
# joining spatial datasets relies on spatial relations, not key variables
# adds new columns to the target x from the source y
# add 10 points to the earth's surface, then for the ones on land, which countries?
set.seed(2018) # reproducibility
bb = st_bbox(world) # set bounds

random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)

random_points = random_df |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")
random_points

world_random = world[random_points, ] #spatial subsetting to get countries with random points
nrow(world_random)
world_random

#add the names of the countries to the random points
random_joined = st_join(random_points, world["name_long"])
random_joined

jm0 = tm_shape(world) + tm_borders(lwd = 0.2) + tm_format("World")

jm1 = jm0 +
  tm_shape(shp = random_points, bbox = bb) +
  tm_symbols(col = "black", shape = 4, border.lwd = 2) +
  tm_layout(scale = 1, legend.bg.color = "white", legend.bg.alpha = 0.3, legend.position = c("right", "bottom"))

jm2 = jm0 +
  tm_shape(world_random, bbox = bb) +
  tm_fill(col = "name_long", palette = "Dark2")+
  tm_layout(legend.show = FALSE)

jm3 = jm0 +
  tm_shape(random_joined, bbox = bb)+
  tm_symbols(col = "name_long", shape = 4, border.lwd = 2, palette = "Dark2")+
  tm_layout(legend.show = FALSE)

jm4 = jm0 +
  tm_shape(shp = random_joined, bbox = bb) +
  tm_symbols(col = "name_long", shape = 4, border.lwd = 2, palette = "Dark2") +
  tm_layout(legend.only = TRUE)

tmap_arrange(jm1,jm2,jm3,jm4, nrow = 2, ncol = 2)

# * * 4.2.5 nonoverlapping joins----
# sometimes things dont overlap but they should
# official cycle hire points in london, vs. the same data in openstreetmap
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

# check if they touch each other
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

?cycle_hire
?cycle_hire_osm

# we want to join the capacity variable of OSM with the official points of 
# cyclehire. use a topological operator like is within distance

sel = st_is_within_distance(cycle_hire, cycle_hire_osm, dist = 20) #20 meters
summary(lengths(sel)>0)
# 438 points in cycle hir are within 20 m of cyclehireosm
# 
z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)
#uhoh z has 20 more rows than the target, which means that some points in cyclehire
#have multiple matches in cyclehireosm

z = z |> 
  group_by(id) |> 
  summarise(capacity = mean(capacity))
nrow(cycle_hire) == nrow(z)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# * * 4.2.6 spatial aggregation----
# aggregation condenses data
# find the average height of high points in each region

nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

#nz is the source geometry, nz_height is the target
#geometry of the source defines how target is grouped
#also can do the same with dplyr

nz_agg2 = st_join(x = nz, y = nz_height) |> 
  group_by(Name) |> 
  summarise(elevation = mean(elevation, na.rm = TRUE))

plot(nz_agg["elevation"])
plot(nz_agg2["elevation"])

#results in same plots, but using st_join you keep the names
#
# * * 4.2.7 joining incongruent layers----
# an aggregating object y is congruent with a target object x if the two objects
# have shared borders
# incongruent aggragating objects do not share comon borders
# areal interpolation overcomes incongruence by transferring values from one set
# of areal units to another
# using the incongruent and aggregating zones data set, how can we transfer
# the values othe underlying polygons into the two polygons of the aggregating zones?
# area weighted spatial interpolation:
# transers values from the incongruent object to a new column in the zones in 
# proportion to the area of overlap
iv = incongruent["value"] #keep only the values to be transferred 
agg_aw = st_interpolate_aw(iv, aggregating_zones, ext = TRUE)
agg_aw$value

# * * 4.2.8 distance relations----
nz_heighest = nz_height |> 
  slice_max(n = 1, order_by = elevation)
nz_heighest

cant_centroid = st_centroid(canterbury)
st_distance(nz_heighest, cant_centroid)

# or get distances between multiple points
co = filter(nz, grepl("Canter|Otag", Name))
co
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co))
plot(st_geometry(nz_height)[2:3], add = TRUE)

#2nd and 3rd points in nz height are in otago so the dist is 0
#
# * 4.3 spatial operations on raster data----
# * * 4.3.1 spatial subsetting
# raster objects can be extracted with row and column combos, cell IDs, or 
# coordinates
# You can use cellFromXY to turn coordinates into a cell id, or use terra extract

id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
terra::extract(elev, matrix(c(0.1,0.1), ncol = 2))

#can als subset with another raster object
#
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
plot(elev)
plot(clip)
plot(elev[clip, drop = FALSE])
elev[1:2, drop = FALSE]

# logical subsetting
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)
# this creates rmask with values NA and TRUE
# now we keep only the values of elev which are true
elev[rmask, drop = FALSE]
#or
mask(elev, rmask)
# this kind of masking is technically a Boolean local operation, since we are 
# comparing two rasters cell-wise
# 
# * * 4.3.2 map algebra----
# map algebra are operations that modify or summarise raster cell values,
# with reference to surrounding cells, zones, or global values
# "raster is faster, but vector is corrector"
# if datasets share the same extent, projection, and resolution, you can treat 
# them as matrices
# map algebra has four subclasses
# local or percell operatoins
# focal or neighborhood operatoins. most often a 3x3 block
# zonal operations are like focal but with irregular sized pixel grids
# global or per raster operations
# so therefore ma operations are typed by the number of cells used for each
# pixel processing step and the type of output
# 
# * * 4.3.3 local operations----
plot(elev+elev)
plot(elev^2)
plot(log(elev))
plot(elev>5)

# reclassification, such as elev values into categories of low med high
# first construct a reclass matrix, cols 1 and 2 have the bounds, col 3
# has the new value
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl
recl = classify(elev, rcl = rcl)
plot(recl)

# app = applies function to each cell of raster for multiple layers into one layer
# tapp allows us to do the same but pick a subset of layers
# lapp allows us to apply a function to each cell using layers as arguments
# NDVI is - 1 to 1, >0.2 means living plants
# vegetation absorbs lots of red light in the visible spectrum, but
# it reflects NIR (near infrared) light
# NDVI = (NIR-red)/(NIR+red)
# live plants are dark in PAR region and bright in near infrared region
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)

ndvi_fun = function(nir, red){
  (nir-red)/(nir+red)
}

nlyr(multi_rast)
 # apparently we only want layers 4 and 3 for NIR and red
ndvi_rast = lapp(multi_rast[[c(4,3)]], fun = ndvi_fun)

plot(ndvi_rast)

# predictive mapping is also possible in local raster operations
# response variable is measured or observed points in space, then 
# retrieve predictor variables from rasters (elev, ph, precip, temp)
# model response as a function of predictors using linear models or machine 
# learning
# 
# * * 4.3.4 focal operations----
# focal operations take a focal cell and its designated neibhors
# the neighborhood is also called kernel filter or moving window
# 
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
plot(r_focal)

# * * 4.3.5 zonal operatoins
# takes a categorical raster and calculates all the values that overlay for each
# category
z = zonal(elev, grain, fun = "mean",  as.raster = TRUE)
z
# this is mean altitude fo reach grain size class
plot(z)
