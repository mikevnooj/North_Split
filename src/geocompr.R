library(sf)
library(sp)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)
library(spData)


vignette(package = "sf")
vignette("sf1")

nc <- st_read(system.file("shape/nc.shp",package = "sf"))

(nc_geom <- st_geometry(nc))
nc_geom[[1]]

par(mar = c(0,0,1,0))
plot(nc[1],reset = FALSE)
plot(nc[1,1], col = 'grey',add=TRUE)
(w <- which(sapply(nc_geom,length)>1))
plot(nc[w,1],col = 2:7)
summary(world)



# begin geocompr: chapitre un, deux, trois ---------------------------------------------
packageVersion("spData")
plot(world)
summary(world["lifeExp"])
world_mini = world[1:2,1:3]
world_mini

world_sp = as(world,Class = "Spatial")
world_sf = st_as_sf(world_sp)
plot(world[3:6])
plot(world["pop"])
world_asia= world[world$continent == "Asia",]
asia = st_union(world_asia)

# 2.2.3 easy way toucheck geo between layers
plot(world["pop"],reset = FALSE)
plot(asia, add = TRUE,col="red")

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop)/10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world_asia[0], add = TRUE)

#use vectors for st_point creating
st_point(c(5, 2))                 # XY point

st_point(c(5, 2, 3))              # XYZ point

st_point(c(5, 2, 1), dim = "XYM") # XYM point

st_point(c(5, 2, 3, 1))           # XYZM point


## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)

## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)

## POLYGON
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)

## POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

## MULTILINESTRING
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring((multilinestring_list))

## MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)

## GEOMETRYCOLLECTION
gemetrycollection_list = list(st_multipoint(multipoint_matrix),
                              st_linestring(linestring_matrix))
st_geometrycollection(gemetrycollection_list)

# sfc POINT
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)

# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
polygon_sfc
st_geometry_type(polygon_sfc)

# sfc MULTILINESTRING
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
multilinestring_sfc
st_geometry_type(multilinestring_sfc)

# sfc GEOMETRY
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
point_multilinestring_sfc
st_geometry_type(point_multilinestring_sfc)

# EPSG definition
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)
plot(points_sfc_wgs)

# PROJ4STRING definition
st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")

# 2.2.8
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf

raster_filepath = system.file("raster/srtm.tif",package = "spDataLarge")
new_raster = raster(raster_filepath)

new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5,
                      xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                      vals = 1:36)

multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)

raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085, 
                          res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)
r_stack = stack(raster_in_memory, raster_on_disk)
r_stack

# view all projections
View(rgdal::make_EPSG())

vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)
st_crs(new_vector)

new_vector = st_set_crs(new_vector,4326)

projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 
                            +units=m +no_defs" # set CRS

# units stored in crs of sf
luxembourg = world[world$name_long == "Luxembourg", ]

st_area(luxembourg)

# rasters do not, however, store and report units
res(new_raster)

summary(world$geom)

nigeria = world[world$name_long == "Nigeria", ]
africa = world[world$continent == "Africa", ]
plot(st_geometry(nigeria), col = "white", lwd = 3, main = "Nigeria in context", border = "lightgrey",expandBB = c(0.5, 0.2, 0.5, 0.2))
plot(st_geometry(world), lty = 3, add = TRUE, border = "grey")
plot(st_geometry(nigeria), col = "yellow", add = TRUE, border = "darkgrey")
a = africa[grepl("Nigeri", africa$name_long), ]
ncentre = st_centroid(a)
ncentre_num = st_coordinates(ncentre)
text(x = ncentre_num[, 1], y = ncentre_num[, 2], labels = a$name_long)
