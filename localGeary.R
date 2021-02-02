library(spdep)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
require(tigris)
library(RColorBrewer)
require(spacetime)

local_g <- localG(q1$ur, swm_w)


q1$gstat <- local_g

q1$osm_id <- unique(panel$osm_id)

locg <- geo_join(a1, q1, by_sp = "osm_id", by_df = "osm_id" )


b <- c(-2.1832, -1.3047,  0.5660,  0.1474,  1.1325,  2.4299)



tm_shape(locg) +
  tm_fill("gstat",
          n=3,
          palette = "RdBu",
          style = "pretty", alpha=0.95) +
  tm_borders(alpha=.4)





tm_shape(locg) +
  tm_polygons("gstat", breaks=b)

local_g$gstat
