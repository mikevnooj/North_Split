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
cex = sqrt(world$pop) / 10000
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

