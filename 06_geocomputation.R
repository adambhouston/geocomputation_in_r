# 6 raster vector interactions----
pacman::p_load(dplyr, terra, sf, tmap, rcartocolor, grid, ggplot2, spData)

# * 6.1 introduction----
# raster cropping and masking using vector objects
# extracting raster values using vector data
# raster vector conversion
# * 6.2 raster cropping----
# read in srtm elevation dem and zion vector, reproject zion to crs of srtm
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, crs(srtm))

# crop the srtm object based on extent of zion
# vect is used to convert sf zion into zionvect, which is stored in terra's
# spatvector class
zion_vect = vect(zion)
srtm_cropped = crop(srtm, zion_vect)

# mask sets values outside the bounds of the second argument to NA
srtm_masked = mask(srtm, zion_vect)

srtm_final = mask(srtm_cropped, zion_vect)
srtm_inv_masked = mask(srtm, zion_vect, inverse = TRUE)

# generate the four different maps
terrain_colors = carto_pal(7, "Geyser")
pz1 = tm_shape(srtm) + 
  tm_raster(palette = terrain_colors, legend.show = FALSE, style = "cont") + 
  tm_shape(zion) + 
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "A. Original", inner.margins = 0)
pz2 = tm_shape(srtm_cropped) +
  tm_raster(palette = terrain_colors, legend.show = FALSE, style = "cont") + 
  tm_shape(zion) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "B. Crop", inner.margins = 0)
pz3 = tm_shape(srtm_masked) + 
  tm_raster(palette = terrain_colors, legend.show = FALSE, style = "cont") + 
  tm_shape(zion) + 
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "C. Mask", inner.margins = 0)
pz4 = tm_shape(srtm_inv_masked) +
  tm_raster(palette = terrain_colors, legend.show = FALSE, style = "cont") + 
  tm_shape(zion) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "D. Inverse mask", inner.margins = 0)
tmap_arrange(pz1, pz2, pz3, pz4, ncol = 4, asp = NA)

# * 6.3 raster extraction----
# extraction is identifying and returning the values of a target raster at 
# specific locations based on a vector selector object
# the reverse of raster extraction is rasterization
# add points to the zion map, then extract the elevation data at each point
# then bind the elevation to the zion_points
data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, vect(zion_points))
zion_points = cbind(zion_points, elevation)

# raster extraction also works for line selectors but is not recommended
# better to instead split the line into many points and extract
# create a line transect between two points in the park
zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>%
  st_linestring() %>% 
  st_sfc(crs = crs(srtm)) %>% 
  st_sf()

# now we add a unique id to the rows of ziontransect (only 1 row rn)
zion_transect$id = 1:nrow(zion_transect)

# segmentize the line into points with a provided density dfMaxLength
zion_transect = st_segmentize(zion_transect, dfMaxLength = 250)

# convert into points with stcast
zion_transect = st_cast(zion_transect, "POINT")

# now we have 257 points. id is used if we have multiple transects to group by
# figure out the distance from first point to each subsequent point in order 
# to make an elevation profile
# 
zion_transect = zion_transect %>% 
  group_by(id) %>% 
  mutate(dist = st_distance(geometry)[, 1]) 
# now extract elevation for each point and bind to zion_transect
zion_elev = terra::extract(srtm, vect(zion_transect))
zion_transect = cbind(zion_transect, zion_elev)

# now plot
# make the line
zion_transect_line = cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>%
  st_linestring() %>% 
  st_sfc(crs = crs(srtm)) %>% 
  st_sf()
# get all the points and add a name
zion_transect_points = st_cast(zion_transect, "POINT")[c(1, nrow(zion_transect)), ]
zion_transect_points$name = c("start", "end")
# plot the elevation raster, zion boundaries, and line transect
rast_poly_line = tm_shape(srtm) +
  tm_raster(palette = terrain_colors, title = "Elevation (m)", 
            legend.show = TRUE, style = "cont") + 
  tm_shape(zion) +
  tm_borders(lwd = 2) + 
  tm_shape(zion_transect_line) + 
  tm_lines(col = "black", lwd = 4) + 
  tm_shape(zion_transect_points) +                                                 
  tm_text("name", bg.color = "white", bg.alpha = 0.75, auto.placement = TRUE) +
  tm_layout(legend.frame = TRUE, legend.position = c("right", "top"))
# make elevation vs distance from first point plot
plot_transect = ggplot(zion_transect, aes(as.numeric(dist), srtm)) + 
  geom_line() +
  labs(x = "Distance (m)", y = "Elevation (m a.s.l.)") + 
  theme_bw() +
  # facet_wrap(~id) +
  theme(plot.margin = unit(c(5.5, 15.5, 5.5, 5.5), "pt"))
# arrange nicely
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(0.25, 5), "null"))))
grid.text("A. Line extraction", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.text("B. Elevation along the line", vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(rast_poly_line, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot_transect, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# final type of geographic vector object for raster extraction uses polygons
# this code extracs all the elv for each pixel inside zion
zion_srtm_values = terra::extract(x = srtm, y = zion_vect)

# this code provides summary statistics across the polygon, would also work
# with multiple polygons just need to group them
group_by(zion_srtm_values, ID) %>% 
  summarize(across(srtm, list(min = min, mean = mean, max = max)))

# this code loads the land cover database and then extracts the categorical data
# inside of the zion polygon and summarizes
nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
zion2 = st_transform(zion, st_crs(nlcd))
zion_nlcd = terra::extract(nlcd, vect(zion2), exact = TRUE)
zion_nlcd %>% 
  group_by(ID, levels) %>%
  count()

# polygons have irregular shapes so they can overlap only some parts of cells
# extract has an arg called exact, when true this adds fraction column
# which contains fraction of each cell covered by the polygon
# useful for calculating weighted mean for continous rasters or more precise for
# categorical rasters

# * 6.4 rasterization----
# convert a vector into a raster object using rasterize and a template vector
# we'll use the cycle hire dataset to create a template raster
cycle_hire_osm = spData::cycle_hire_osm
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 1000,
                       crs = st_crs(cycle_hire_osm_projected)$wkt)
# presence absence raster
# is there a cycle hire point here? specify with field. any cell with a point
# gets assigned a 1
ch_raster1 = rasterize(vect(cycle_hire_osm_projected), raster_template,
                       field = 1)
# specifying the fun argument as length counts the number of hire points in each
# grid cell
ch_raster2 = rasterize(vect(cycle_hire_osm_projected), raster_template, 
                       fun = "length")

# the cycle hire data has capacity of each hire point
# what is the capacity in each grid cell? use the sum fun on field capacity
ch_raster3 = rasterize(vect(cycle_hire_osm_projected), raster_template, 
                       field = "capacity", fun = sum)
# plot
r0p = tm_shape(cycle_hire_osm_projected) + 
  tm_symbols(col = "capacity", title.col = "Capacity: ", size = 0.1) + 
  tm_layout(main.title = "A. Points", main.title.size = 1, 
            legend.position = c("right", "bottom"), legend.frame = TRUE)

r1p = tm_shape(ch_raster1) + 
  tm_raster(legend.show = TRUE, title = "Values: ") + 
  tm_layout(main.title = "B. Presence/absence", main.title.size = 1, 
            legend.position = c("right", "bottom"), legend.frame = TRUE)

r2p = tm_shape(ch_raster2) + 
  tm_raster(legend.show = TRUE, title = "Values: ") + 
  tm_layout(main.title = "C. Count", main.title.size = 1,
            legend.position = c("right", "bottom"), legend.frame = TRUE)

r3p = tm_shape(ch_raster3) + 
  tm_raster(legend.show = TRUE, title = "Values: ") +
  tm_layout(main.title = "D. Aggregated capacity", main.title.size = 1,
            legend.position = c("right", "bottom"), legend.frame = TRUE)

tmap_arrange(r0p, r1p, r2p, r3p, ncol = 2)

# * 6.5 spatial vectorization----
# convert continuous raster data into vector data
# simple thing is to convert centroids of cells into points using as.points
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_point = as.points(elev) %>% 
  st_as_sf()
# creating isolines or contours is useful
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
cl = as.contour(dem)
plot(dem, axes = FALSE)
plot(cl, add = TRUE)
# can also use contour() rasterVis::contourplot, or tmap::tm_iso()
# 
# as.polygons converts each cell into five coordinates (sides and center)
# convert grain object into polygons, then dissolve borders between polygons
# with same attribute classes
grain = rast(system.file("raster/grain.tif", package = "spData"))
grain_poly = as.polygons(grain, dissolve = FALSE) %>% 
  st_as_sf()
grain_poly2 = as.polygons(grain) %>% 
  st_as_sf()

cols = c("clay" = "brown", "sand" = "rosybrown", "silt" = "sandybrown")

p1p = tm_shape(grain) +
  tm_raster(legend.show = FALSE, palette = cols) +
  tm_layout(main.title = "A. Raster", frame = FALSE,
            main.title.size = 1)

p2p = tm_shape(grain_poly) +
  tm_polygons("grain", legend.show = FALSE, palette = cols, lwd = 3) +
  tm_layout(main.title = "B.Polygons", frame = FALSE,
            main.title.size = 1)

p3p = tm_shape(grain_poly2) + 
  tm_polygons("grain", legend.show = FALSE, palette = cols, lwd = 3) +
  tm_layout(main.title = "C. Aggregated polygons", frame = FALSE,
            main.title.size = 1)

tmap_arrange(p1p, p2p, p3p, ncol = 3)

# * 6.6 exercises----
zion_points_path = system.file("vector/zion_points.gpkg", package = "spDataLarge")
zion_points = read_sf(zion_points_path)
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
ch = st_combine(zion_points) %>%
  st_convex_hull() %>% 
  st_as_sf()
# convex hull is the smallest convext polygon that encloses all the points in the
# set

# 1
srtm_points_crop = crop(srtm, zion_points)
srtm_ch_crop = crop(srtm, ch)
srtm_points_mask = mask(srtm, vect(zion_points))
srtm_ch_mask = mask(srtm, vect(ch))

par(mfcol = c(2,2))
plot(srtm_points_crop)
plot(srtm_ch_crop)
plot(srtm_points_mask)
plot(srtm_ch_mask)

# 2
# extract srtm values at points
srtm_points = extract(srtm, vect(zion_points))
points_buff = zion_points |> 
  st_buffer(dist = 90) |> 
  vect()
srtm_buff_points = extract(srtm, points_buff)

# 3
over3.1k = nz_height |> filter(elevation > 3100)
temp_rast = rast(ext(over3.1k), resolution = 3000,
                 crs = st_crs(over3.1k)$wkt)

highpoints = rasterize(vect(over3.1k), temp_rast, field = "elevation", fun = "length") 
plot(highpoints)
maxpoints = rasterize(vect(over3.1k), temp_rast, field = "elevation", fun = "max") 
plot(maxpoints)

# 4
agg_highpoints = aggregate(highpoints, fact = 2, fun = sum)
res(agg_highpoints)
plot(agg_highpoints)
