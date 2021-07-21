## Vector Data

library(sf)

shp <- '~/data/cb_2016_us_county_5m'
counties <- st_read(shp)

head(counties)

sesync <- st_sfc(
    st_point(c(-76.503394, 38.976546)),
    crs = st_crs(counties))

## Bounding box

st_bbox(counties) #example for bounding box of the counties as a whole

library(dplyr) #for filtering

counties_md <- filter(counties, 
                      STATEFP == "24") #subsets the data to 
#only include maryland counties

st_bbox(counties_md) #looks at altered bounding box coordinates 
#now that we're only in MD

## Grid

grid_md <- st_make_grid(counties_md, n=4)
grid_md


## Plot Layers - if using the add function, they need to have the SAME CRS
#make sure you check this!

plot(grid_md, border="dark grey")
plot(counties_md["NAME"], add = TRUE)
plot(sesync, col = "green", pch = 20, add=TRUE)


## Plotting with ggplot2

library(ggplot2)

ggplot() +
    geom_sf(data = counties_md, aes(fill = ALAND)) +
    geom_sf(data = sesync, size = 3, color = 'red')  

theme_set(theme_bw())

ggplot() +
    geom_sf(data = counties_md, aes(fill = ALAND/1e6), color=NA) +
    geom_sf(data = sesync, size = 3, color = 'red') +
    scale_fill_viridis_c(name = 'Land area (sq. km)') +
    theme(legend.position = c(0.3, 0.3))

## Coordinate Transforms

# this code can filter for overlapping spatial locations
st_filter(counties_md, sesync)


#Importing a new shapefile
shp <- '~/data/huc250k'

#Creating shapefile huc
huc <- st_read(shp)

st_crs(counties_md)
st_crs(huc)

#creates projection for transformation
prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
        +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 \
        +datum=WGS84 +units=m +no_defs'

#projects counties, huc, and sesync point to the same coordinate system
#this can take awhile for big datasets

counties_md <- st_transform(
    counties_md,
    crs=prj)
huc <- st_transform(huc, crs=prj)
sesync <- st_transform(sesync, crs=prj)

plot(st_geometry(counties_md)) #no fill for color - only borders
plot(st_geometry(huc),
     border = 'blue', add = TRUE)
plot(sesync, col = 'green',
     pch = 20, add = TRUE)

## Geometric Operations

state_md <- st_union(counties_md)
plot(state_md)

huc_md <- st_intersection(huc, state_md)

plot(state_md)
plot(st_geometry(huc_md), border = 'blue',
     col = NA, add = TRUE)

## Raster Data

library(stars)
nlcd <- read_stars("~/data/nlcd_agg.tif", proxy=FALSE)
nlcd <- droplevels(nlcd)
plot(nlcd)

## Crop

md_bbox <- st_bbox(huc_md)
nlcd <- st_crop(nlcd, md_bbox) #crops using the bounding box for md_bbox data
plot(nlcd)

ggplot() +
    geom_stars(data = nlcd) +
    geom_sf(data = huc_md, fill = NA) +
    scale_fill_manual(values = attr(nlcd[[1]], "colors")) #this pulls out color scheme

## Raster math

levels(nlcd[[1]])

forest_types <- c('Evergreen Forest', 'Deciduous Forest', 'Mixed Forest')
forest <- nlcd
forest[!(forest %in% forest_types)] <- NA #turns off all pixels that are not forests
#! negates everything within a subsequent logical sequence
plot(forest)

## Downsampling a raster

nlcd_agg <- st_warp(nlcd,
                cellsize = 1500, 
                method = "mode", 
                use_gdal = TRUE) #gdal is a library needed for complex rasters

nlcd_agg <- droplevels(nlcd_agg) 
levels(nlcd_agg[[1]]) <- levels(nlcd[[1]]) 

plot(nlcd_agg) #bigger pixels, lower resolution

## Mixing rasters and vectors

plot(nlcd, reset = FALSE)
plot(sesync, col = 'green',
     pch = 16, cex = 2, add=TRUE)

#tells us what nlcd land cover is at the sesync location
sesync_lc <- st_extract(nlcd, sesync) 
sesync_lc


baltimore_city <- nlcd[counties_md[1, ]] #first row of counties in MD = Baltimore
plot(baltimore_city)

#ALTERNATIVE CODE FOR THE SAME THING
baltimore_city_alt <- st_crop(nlcd, counties_md[1, ])
plot(baltimore_city_alt)

#ONE MORE TIME - SAME THING, BUT USING "PIPELINE" TO GET A TABLE (INSTEAD OF MAP)
nlcd %>%
    st_crop(counties_md[1, ]) %>%
    pull %>%
    table

mymode <- function(x) names(which.max(table(x))) #gets the most abundant value in table of qualatative values

modal_lc <- aggregate(nlcd_agg, huc_md, FUN = mymode) 
modal_lc #this is hard to read

st_as_sf(modal_lc) #this shows us that for each HUC (30 total) the most common LULC for each


#let's map that!!
huc_md <- huc_md %>% 
    mutate(modal_lc = modal_lc[[1]]) #adds outcomes from above to huc_md

ggplot(huc_md, aes(fill = modal_lc)) + 
    geom_sf() #this is cool

## Mapview - can add basemaps

library(mapview)
mapview(huc_md)

mapview(nlcd_agg, legend = FALSE, alpha = 0.5, 
        map.types = 'OpenStreetMap') +
    mapview(huc_md, legend = FALSE, alpa = 0.2)

#alpha determines transparency
#this map is super ugly
