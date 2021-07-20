# PART I: EXPLORE DATA READ AND DISPLAY INPUTS

## Simple Features

library(sf)

lead <- read.csv('data/SYR_soil_PB.csv')

lead <- ...(lead,
  ...,
  crs = 32618)

plot(lead['ppm'])

library(ggplot2)
ggplot(data = lead,
       mapping = aes(...)) +
  ...()

## Feature Collections

blockgroups <- st_read('data/bg_00')

## Table Operations

ggplot(blockgroups,
       aes(...)) +
   ...()

library(dplyr)

census <- ...('data/SYR_census.csv')
census <- mutate(census, 
  ...
)

census_blockgroups <- ...(
  ..., census,
  by = c('BKG_KEY'))

ggplot(...,
       aes(fill = POP2000)) +
  geom_sf()

census_tracts <- census_blockgroups %>%
  group_by(...) %>%
  ...(
    POP2000 = sum(POP2000),
    perc_hispa = sum(HISPANIC) / POP2000)

tracts <- ...('data/ct_00')
ggplot(...,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(...,
          color = 'red', fill = NA)

# PART II: SPATIAL QUERY AND AGGREGATION 

## Spatial Join

ggplot(...,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(..., color = 'red',
          fill = NA, size = 0.1)

lead_tracts <- lead %>%
    st_join(...) %>%
    ...()

lead_tracts <- lead %>%
    st_join(census_tracts) %>%
    st_drop_geometry() %>%
    ... %>%
    ...

census_lead_tracts <- census_tracts %>%
  inner_join(...)

ggplot(...,
       aes(fill = ...)) +
  geom_sf() +
  scale_fill_gradientn(
    colors = heat.colors(7))

library(mapview)
mapview(lead['ppm'],
        map.types = 'OpenStreetMap',
        viewer.suppress = TRUE) +
  mapview(census_tracts,
          label = census_tracts$TRACT,
          legend = FALSE)

# PART III :  KRIGING SURFACES 

## The Semivariogram

library(gstat)

lead_xy <- read.csv('data/SYR_soil_PB.csv')

v_ppm <- ...(
  ppm ~ 1,
  locations = ~ x + y,
  data = lead_xy)
plot(v_ppm)

v_ppm_fit <- ...(
  v_ppm,
  model = ...)
plot(v_ppm, v_ppm_fit)

## Kriging

pred_ppm <- ...(
  lead, cellsize = 400,
  what = 'centers')

pred_ppm <- ...

ggplot(...,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(..., color = 'red', fill = NA)

pred_ppm <- ...(
  ...,
  locations = lead,
  newdata = pred_ppm,
  ...)

ggplot() + 
  geom_sf(data = ...,
          fill = NA) +
  geom_sf(data = ...,
          aes(color = ...))

pred_ppm_tracts <-
  pred_ppm %>%
  ...(census_tracts) %>%
  st_drop_geometry() %>%
  group_by(TRACT) %>%
  summarise(...)
census_lead_tracts <- 
  census_lead_tracts %>%
  ...(pred_ppm_tracts)

ggplot(...,
       aes(x = ..., y = ...)) +
  geom_point() +
  geom_abline(...)

# PART IV : SPATIAL AUTOCORRELATION AND REGRESSION 

ppm.lm <- ...(pred_ppm ~ perc_hispa,
  census_lead_tracts)

census_lead_tracts <- census_lead_tracts %>%
  mutate(...)
plot(census_lead_tracts['lm.resid'])

library(sp)
library(spdep)
library(spatialreg)

tracts <- as(
  st_geometry(census_tracts), 'Spatial')
tracts_nb <- ...(tracts)

plot(census_lead_tracts['lm.resid'],
     reset = FALSE)
plot.nb(tracts_nb, coordinates(tracts),
        add = TRUE)

tracts_weight <- ...(tracts_nb)

...(
  census_lead_tracts[['lm.resid']],
  ...,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)

ppm.sarlm <- ...(
  pred_ppm ~ perc_hispa,
  data = census_lead_tracts,
  tracts_weight,
  tol.solve = 1.0e-30)

# dev.off() # uncomment to try this if moran.plot below fails

moran.plot(
  ...,
  tracts_weight,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)
