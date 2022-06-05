# 9 making maps in r----
pacman::p_load(sf, raster, dplyr, terra, spData, spDataLarge, 
               tmap, leaflet, ggplot2, gifski, shiny)

# * 9.1 introduction----
# * 9.2 static maps----
# * * 9.2.1 tmap basics----
# tm_shape defines input data, followed by layer elements
# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 
qtm(nz)
qtm(world_wgs)

# * * 9.2.2 map objects----
# tmap can store objects representing maps
map_nz = tm_shape(nz) + tm_polygons()
map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)
nz_water = st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")
map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()
map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_dots()
tmap_arrange(map_nz1, map_nz2, map_nz3)

# * * 9.2.3 aesthetics----
# 2 types of aesthetics: those that change with the data and the constant
ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# aesthetics that can vary require a character string naming the attribute
tm_shape(nz) + tm_fill(col = "Land_area") 

legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()
map_nza

# * * 9.2.4 color settings----
tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")

# ttyle settings:
# equal breaks is appropriate for uniform distribution
# quantile ensures same number of observations in each category
# jenks identifys similar groups and maxs the difference between them
# cont is good for continuous rasters, order can visualize skewed
# cat represents categorical values
# 
tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")

# palettes are categorical, sequential, or diverging
# 
# * * 9.2.5 layouts----
map_nz + 
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)

map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(scale = 5)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)

tmap_style_catalog()

# tm_style can set cool looking styles
#
# * * 9.2.6 faceted maps----
urb_1970_2030 = urban_agglomerations %>% 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

# * * 9.2.7 inset maps----
# define area of interst
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) %>% 
                    st_as_sfc()
nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scale_bar(position = c("left", "bottom"))
nz_height_map

#now make inset map and use viewport to place it
nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3) # this creates the inset box
library(grid)
nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

# * 9.3 animated maps----
urb_anim = tm_shape(spData::world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE) # use along and free coords is false
tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)

# need gifski for this
# 
# * 9.4 interactive maps----
tmap_mode("view")
map_nz
map_nz + tm_basemap(server = "OpenTopoMap")
world_coffee = left_join(spData::world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets(nrow = 1, sync = TRUE)
mapview::mapview(nz)
tmap_mode("plot")


# * 9.5 mappling applications----
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() %>% 
      # addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)

# * 9.6 other mapping packages----
# 
# * 9.7 exercises----
africa = spData::world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(worldbank_df, by = "iso_a2") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")
zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge")))
data(nlcd, package = "spDataLarge")

breaks = c(0, 0.55, 0.7, Inf)
tm_shape(africa) +
  tm_polygons(col = "HDI", breaks = breaks) 

tm_shape(africa) + 
  tm_polygons(col = "subregion")

tm_shape(zion) + tm_polygons() +
  tm_shape(nlcd) + tm_raster(alpha = 0.3)
