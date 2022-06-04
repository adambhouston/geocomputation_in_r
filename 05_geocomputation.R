# 5 geometry operations----
pacman::p_load(sf, terra, dplyr, spData, tmap, rmapshaper)

# * 5.1 Introduction----
# simplify and convert vector geometries
# unary operations work on a single geometry:
# simplification of lines, buffers, centroids, scaling geometries
# binary operations modify one geometry based on another, like clipping
# or geometry unions
# 
# * 5.2 geometric operations on vector data----
# how do we cahnge the geometry of vector (SF) objects
# functions here work on sfc and sf objects
# * * 5.2.1 simplification----
# simplification makes vector objects smaller, reducing the amount of memory
# also makes them look better on smaller scale maps
# st_simplify uses the GEOS implementation of Dougles Peucker algorithm to 
# reduce the vertex count
# compare simplified seine, reduces number of vertices
seine_simp = st_simplify(seine, dTolerance = 2000)
par(mfcol = c(1,2))
plot(st_geometry(seine))
plot(st_geometry(seine_simp))

# plot again using tmap and labels
p_simp1 = tm_shape(seine) + tm_lines() +
  tm_layout(main.title = "Original data")
p_simp2 = tm_shape(seine_simp) + tm_lines() +
  tm_layout(main.title = "st_simplify")
tmap_arrange(p_simp1, p_simp2, ncol = 2)

#compare object sizes
object.size(seine)
object.size(seine_simp)

# you can also simplify polygons like US states
# first have to a projected CRS, because GEOS only accepts that
us_states2163 = st_transform(us_states, "EPSG:2163")
us_states2163 = us_states2163 %>% 
  mutate(AREA = as.numeric(AREA)) 
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km
# st_simplify simplifies objects on a per geometry basis (for each state),
# so states will shrink
# and appear holey
# ms_simplify from rmapshaper uses the visvalingam algorithm to overcome this
# preserves the overall topology
# keep= indicates the portion of points to retain (0-1; default 0.05)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)
p_us_simp1 = tm_shape(us_states) + tm_polygons()+
  tm_layout(main.title = "original")
p_us_simp2 = tm_shape(us_states_simp1) + tm_polygons()+
  tm_layout(main.title = "st_simplify")
p_us_simp3 = tm_shape(us_states_simp2) + tm_polygons()+
  tm_layout(main.title = "ms_simplify")
tmap_arrange(p_us_simp1, p_us_simp2, p_us_simp3)

# * * 5.2.2 centroids----
# identify the center of complex vectors
# many ways to identify the center
# geographic centroid is most common and is used in st_centroid
# finds the center of mass of an object
# point on surface operations guarantee the centroid will be on the parent object
# avoid the donut problem
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)
nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

p_centr1 = tm_shape(nz) + tm_borders() +
  tm_shape(nz_centroid) + tm_symbols(shape = 1, col = "black", size = 0.5) +
  tm_shape(nz_pos) + tm_symbols(shape = 1, col = "red", size = 0.5)  
p_centr2 = tm_shape(seine) + tm_lines() +
  tm_shape(seine_centroid) + tm_symbols(shape = 1, col = "black", size = 0.5) +
  tm_shape(seine_pos) + tm_symbols(shape = 1, col = "red", size = 0.5)  
tmap_arrange(p_centr1, p_centr2, ncol = 2)
# red circles indicate point on surface centroids
# visual centers and chebyshev centers are the other types of centroids
# 
# * * 5.2.3 buffers----
# polygons that represent the area a given distance from a geometric feature
# st_buffer requires the input and a distance given in units of the CRS
# 
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)
p_buffs1 = tm_shape(seine_buff_5km) + tm_polygons(col = "name") +
  tm_shape(seine) + tm_lines() +
  tm_layout(main.title = "5 km buffer", legend.show = FALSE)
p_buffs2 = tm_shape(seine_buff_50km) + tm_polygons(col = "name") +
  tm_shape(seine) + tm_lines() +
  tm_layout(main.title = "50 km buffer", legend.show = FALSE)
tmap_arrange(p_buffs1, p_buffs2, ncol = 2)

# third argument is nQuadSegs meaning number of lines that are used to create
# the circles that make up the buffers
# reduce this if memory is a concern or increase if precision is needed
# 
# * * 5.2.4 affine transformations----
# affine transformations preserve lines and parallelism, but not angles or
# lenghts
# includes shifting, scaling, and rotation
# sf does this for sfc or sfg objects
# 
nz_sfc = st_geometry(nz)
# this code shifts all the y coordinates 100k meters to the north
nz_shift = nz_sfc + c(0, 100000)

# scaling enlarges or shrinks by a factor
# global scaling increases or decreases all coordinate values in relation to the
# origin
# global scaling preserves topology
# local scaling treats geometries independently and requires centroids around
# which to scale the points
# in this code, the objects are shifted so that center of object is at the origin
# then it is reduced by half
# then moved back to input coordinates
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

# rotation
# requires a rotation matrix, created in function below
# function takes a rotation angle in degrees and converts to radians
# rotation matrix then somehow multiplies by geometry and turns things 
# clockwise
# 
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
st_crs(nz_shift) = st_crs(nz_sfc)
st_crs(nz_scale) = st_crs(nz_sfc)
st_crs(nz_rotate) = st_crs(nz_sfc)
p_at1 = tm_shape(nz_sfc) + tm_polygons() +
  tm_shape(nz_shift) + tm_polygons(col = "red") +
  tm_layout(main.title = "Shift")
p_at2 = tm_shape(nz_sfc) + tm_polygons() +
  tm_shape(nz_scale) + tm_polygons(col = "red") +
  tm_layout(main.title = "Scale")
p_at3 = tm_shape(nz_sfc) + tm_polygons() +
  tm_shape(nz_rotate) + tm_polygons(col = "red") +
  tm_layout(main.title = "Rotate")
tmap_arrange(p_at1, p_at2, p_at3, ncol = 3)

# replace old geometries with newly created geometries using the st_set_geometry()
# 
# * * 5.2.5 clipping----
# clipping is a form of spatial subsetting that works on lines and polygons
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b, border = "grey")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3) # add text
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "grey")
plot(x_and_y, col = "lightgrey", border = "grey", add = TRUE) # intersecting area

# * * 5.2.6 subsetting and clipping----
# st_sample to generate random points in the extent of x and y
# bb and box help us to distribute the points only in the bounding box 
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)
x_and_y = st_intersection(x, y)
plot(b)
plot(p, add = TRUE)

# how do we find the point that is in both x and y
# first method subsets p by the intersection object x_and_y
p_xy1 = p[x_and_y]
# second method finds the intersection of p and x_and_y
p_xy2 = st_intersection(p, x_and_y)
# third method creates an object that includes the binary spatial predicate
# of the intersection of p and x and p and y, then subsets p by that
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]
# first method is simplest obviously
# 
# * * 5.2.7 geometry unions----
# aggregation automatically dissolves boundaries between polygons
# aggregate and summarize functions both combine geometries and dissolve boundaries
# using st_union
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)
plot(texas_union)

# * * 5.2.8 type transformations
# geometry casting transforms geometry types
# behaves differently on sfg, sfc, and sf objects
# create multipoing sfg
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")
p_sc1 = tm_shape(st_sfc(multipoint)) + tm_symbols(shape = 1, col = "black", size = 0.5) +
  tm_layout(main.title = "MULTIPOINT", inner.margins = c(0.05, 0.05, 0.05, 0.05))
p_sc2 = tm_shape(st_sfc(linestring)) + tm_lines() +
  tm_layout(main.title = "LINESTRING")
p_sc3 = tm_shape(st_sfc(polyg)) + tm_polygons(border.col = "black") +
  tm_layout(main.title = "POLYGON")
tmap_arrange(p_sc1, p_sc2, p_sc3, ncol = 3)

# casting sfc and sf objects generally works the same
# create a multilinestring sf with one row
multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring((multilinestring_list))
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf
# cast this into collection of linestrings with 3 rows
linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

# now we can add attributes to the different rows
linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2

p_lc1 = tm_shape(multilinestring_sf) + tm_lines(lwd = 3) +
  tm_layout(main.title = "MULTILINESTRING")
p_lc2 = tm_shape(linestring_sf2) + tm_lines(lwd = 3, col = "name", palette = "Set2") +
  tm_layout(main.title = "LINESTRING", legend.show = FALSE) +
  tm_text("name", along.lines = TRUE)
tmap_arrange(p_lc1, p_lc2, ncol = 2)

# * 5.3 geometric operations on raster data----
# geometric raster operations include shift, flip, mirror, scale, rotate, or warp
# necessary for georeferencing, which you cant do in R because it is manual
# 
# * * 5.3.1 geometric intersections----
# this is for subsetting/clipping a raster
# simple subset, but set drop = FALSE so that spatial element is preserved
elev = rast(system.file("raster/elev.tif", package = "spData"))
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]
# can also use the intersect or crop command
# 
# * * 5.3.2 extent and origin----
# resolution of rasters has to match if map algebra is used
# need to align the rasters
# sometimes we just need to change the extent using extend()
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2))
# try and these two together - you cant
elev_3 = elev + elev_2
# use extend to make them match
elev_4 = extend(elev, elev_2)
# change origin with origin() to make sure they match
# changing the resolution also generally changes the origin
#
# * * 5.3.3 aggregation and disaggregation----
# aggregate decreases resolution, disagg increases resolution
# this decreases resolution by a factor of 5 and uses the mean of the input cells
# to determine the output cell value
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg = aggregate(dem, fact = 5, fun = mean)
p_ar1 = tm_shape(dem) +
  tm_raster(style = "cont", legend.show = FALSE) +
  tm_layout(main.title = "A. Original", frame = FALSE)
p_ar2 = tm_shape(dem_agg) +
  tm_raster(style = "cont", legend.show = FALSE) +
  tm_layout(main.title = "B. Aggregated", frame = FALSE)
tmap_arrange(p_ar1, p_ar2, ncol = 2)

# disagg function increases resolution
# default is to assign output cells the value of the input cells
# bilinear method uses the four nearest pixel centers of the input image to 
# compute an average weighted by distance
dem_disagg = disagg(dem_agg, fact = 5, method = "bilinear")
p_ar3 = tm_shape(dem_disagg) +
  tm_raster(style = "cont", legend.show = FALSE) +
  tm_layout(main.title = "C. Disaggregated", frame = FALSE)
tmap_arrange(p_ar1, p_ar2, p_ar3, ncol = 3)

# * * 5.3.4 resampling----
# resampling takes the values of our original raster and recalcs new values
# for a target raster with custom resolution and origin
# resampling methods include
# -nearest neighbor: assigns nearest cell of original to nearest of target. 
# usualy for resampling categorical rasters
# -bilinear interpolation: described above, fastest for continuous rasters
# -cubic interpolation: uses 16 nearest cells, applys third order polynomial functions
# smoother than bilinear, more intensive
# -cubic spline interpolation: 16 nearest cells, applies piecewise third order
# polynomial functions, coninuous
# -lanczos windowed sinc resampling: 36 nerest cells, continuous
# terra includes resample() which takes the input, the target raster, and method
# create target raster
target_rast = rast(xmin = 794600, xmax = 798200, 
                   ymin = 8931800, ymax = 8935400,
                   resolution = 150, crs = "EPSG:32717")
dem_resampl = resample(dem, y = target_rast, method = "bilinear")
plot(dem)
plot(dem_resampl)

# * 5.4 exercises----
# e.1 was just about simplifying, not doing it
# e.2 how many points in nz_height are within 100 km of canterbury
cant_buff = nz_centroid |> 
  filter(Name == "Canterbury") |> 
  st_buffer(dist = 100000)
cant_highpoints = nz_height[cant_buff, ]


plot(st_geometry(nz))
plot(cant_buff, add = TRUE)
plot(cant_highpoints, add = TRUE)
# this shows the points witin 100 km of canterbury's centroid
# following is correct
canterbury = nz[nz$Name == "Canterbury", ]
cant_buff = st_buffer(canterbury, 100)
nz_height_near_cant = nz_height[cant_buff, ]
nrow(nz_height_near_cant) # 75 - 5 more

# e.3 find centroid of NZ, how far from centroid of Cant
nz_whole_centroid = st_centroid(st_union(nz))
st_distance(nz_whole_centroid, nz_centroid[nz_centroid$Name == "Canterbury", ])

# e.4 make a south up world map with reflection
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
world_sfc = st_as_sfc(world)
world_south_up = world_sfc * c(1, -1)
plot(world_south_up)

# e.6 which state has longest boundary lines in meters
us_states_bor = st_cast(us_states2163, "MULTILINESTRING")
us_states_bor$borders = st_length(us_states_bor)

# e.7 
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
srtm
rast_template = rast(ext(srtm), res = 0.01)
resample(srtm, y = rast_template)
