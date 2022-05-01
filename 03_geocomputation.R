# 3 attribute data operations ----

pacman::p_load(sf, terra, tidyverse, spData)

# * 3.1 introduction ----
# attribute data is non spatial information associated with geographic data
# 
# * 3.2 vector attribute manipulation
# sfs have one column per attribute, and one row per observatoin
# unlike dfs, they include a geometry column of class sfc in each row
# key feature of sfs is that they store spatial and nonspatial data in the same way:
# as columns in data.frame
# you can also use tidyverse on them
class(world)
dim(world)

# we can drop the geometry to create a normal df
world_df = st_drop_geometry(world)
# dropping geometry can be useful
# makes manipulation faster
# 
# * * 3.2.1 vector attribute subsetting ----
# subsetting methods:
# base R - [] and subset()
# dplyr - filter() and slice() for rows, select() for columns
# dplyr - pull or $ will return a single attribute but lose the geom
# 
# logical vectors for subsetting
i_small = world$area_km2 < 10000
summary(i_small)
small_countries = world[i_small, ]

# or
small_countries = world[world$area_km2 <10000, ]

# or
small_countries = subset(world, area_km2 < 10000)

# select and rename columns with dplyr
world3 = select(world, name_long, population = pop)

# a bunch of stuff i know already
# 
# * * 3.2.2 chaining commands with pipes----
# i know this already
# 
# * * 3.2.3 vector attribute aggregation----
# grouping attiributes by another grouping variable
# like adding up the people per continent
world_agg = world |> 
  group_by(continent) |> 
  summarise(pop = sum(pop, na.rm = TRUE))
world_agg

# example
world_agg5 = world |> 
  st_drop_geometry() |> 
  select(pop, continent, area_km2) |> 
  group_by(continent) |> 
  summarise(pop = sum(pop, na.rm = TRUE), area = sum(area_km2), n = n()) |> 
  mutate(density = round(pop/area)) |> 
  top_n(n = 3, wt = pop) |> 
  arrange(desc(n))
world_agg5

# * * 3.2.4 vector attribute joining----
# how to join non spatial data to sf objects
head(coffee_data)
world_coffee = left_join(world, coffee_data)
#left join preserves world dataset
#already had the same key variable called name_long

names(world_coffee)
plot(world_coffee["coffee_production_2017"])

#inner join only keeps the rows that have a match for key variable
world_coffee_inner = inner_join(world, coffee_data)
plot(world_coffee_inner["coffee_production_2017"])

# output of a join tends to match first argument
# 
# * * 3.2.5 creating attributes and removing spatial information----
# mutate adds new column to end of existing
# transmute drops all other columns except geometry
# unite pastes together existing columns, separate does the oppotiste
# rename is obvious
# 
# * 3.3 manipulating raster objects ----
# raster of factors example
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, resolution = 0.5, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)
plot(grain)
cats(grain)
nlyr(grain)

# you can also save color tables with coltab()
# 
# * * 3.3.1 raster subsetting-----
# subset rasters with []
# this accepts:
# row column indexing
# cell ids
# coordinates
# another spatial object
elev[1,1]
elev[1]
#both of these subsets return the top left pixel in this raster object
#what if we subset a multilayered object
c(elev, grain)[1]

# * * 3.3.2 summarizing raster objects----
# use global to apply a function to a whole raster
freq(grain)
hist(elev)
hist(grain)

# 3.4 exercises ----
# 1 select only the name column
us_states_name = select(us_states, NAME)
class(us_states_name)

# 2 
names(us_states)
select(us_states, c("total_pop_10", "total_pop_15"))

# 3
head(us_states)
subset(us_states, REGION == "Midwest") |> 
  st_geometry() |> 
  plot()

filter(us_states, REGION == "West" & as.numeric(AREA) < 250000 
                    & total_pop_15 > 5000000) |> st_geometry() |> plot()

filter(us_states, REGION == "South" & c(as.numeric(AREA) > 150000 
                                        | total_pop_15 > 7000000)) |> 
  st_geometry() |> 
  plot()

# 4
head(us_states)
summarise(us_states,
          total_pop_2015 = sum(total_pop_15))
min(us_states$total_pop_15)
max(us_states$total_pop_15)

# 5
us_states |> 
  group_by(REGION) |> 
  summarise(n_region = n())

# 6
us_states |> 
  group_by(REGION) |> 
  summarise(min = min(total_pop_15),
            max = max(total_pop_15),
            total = sum(total_pop_15))

# 7
head(us_states)
head(us_states_df)
us_states_stats = left_join(us_states, us_states_df, by = c("NAME" = "state"))
class(us_states_stats)

# 8 
anti_join(us_states_df, us_states, by = c("state" = "NAME"))

# 9
us_states_stats2 = us_states_stats |> 
  mutate(pop_dens_10 = total_pop_10/AREA,
         pop_dens_15 = total_pop_15/AREA)

# 10
us_states_stats3 = us_states_stats2 |> 
  mutate(pop_dens_delta = (pop_dens_15 - pop_dens_10)/pop_dens_10 * 100)

plot(us_states_stats3["pop_dens_delta"])

# 11
us_states = spData::us_states

us_states = setNames(us_states,tolower(colnames(us_states)))

# 12
us_states_sel = us_states_stats |> 
  select(Income = median_income_15)

# 13
?us_states_df
us_states_poverty = us_states_stats |> 
  mutate(pov_delta = poverty_level_15-poverty_level_10,
         pov_percent_change = (pov_delta/poverty_level_10)*100)

# 14
us_states_stats |> 
  group_by(REGION) |> 
  summarise(min = min(poverty_level_15),
            max = max(poverty_level_15),
            average = mean(poverty_level_15))

us_states_poverty |> 
  group_by(REGION) |> 
  summarise(increase = sum(pov_delta)) |> 
  filter(increase == max(increase))

# 15
foo = rast(ncol = 9, nrow = 9, resolution = 0.5,
           xmin = 0, xmax = 4.5, ymin = 0, ymax = 4.5,
            vals = rnorm(9*9))
plot(foo)

foo[c(1, 9, 81-8, 81)]

# 16 
freq(grain)

# 17
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
hist(dem)
boxplot(dem)
plot(dem)
