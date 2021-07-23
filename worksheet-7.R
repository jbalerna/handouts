# PART I: EXPLORE DATA READ AND DISPLAY INPUTS

## Simple Features

library(sf)

lead <- read.csv('~/data/SYR_soil_PB.csv')
head(lead)

lead <- st_as_sf(lead,
  coords = c("x","y"),
  crs = 32618) #converts non-spatial object to spatial (gives it CRS)
lead #now has geometry column

plot(lead['ppm'])

library(ggplot2)
ggplot(data = lead,
       mapping = aes(color=ppm)) +
  geom_sf()

## Feature Collections

blockgroups <- st_read('~/data/bg_00')
head(blockgroups)
blockgroups[1:5, "BKG_KEY"]

## Table Operations

ggplot(blockgroups,
       aes(fill=Shape_Area)) +
   geom_sf()

library(dplyr)

census <- read.csv('~/data/SYR_census.csv')
head(census)

census <- mutate(census, 
  BKG_KEY= as.character(BKG_KEY)
)

census_blockgroups <- inner_join(
  blockgroups, census,
  by = c('BKG_KEY'))
#if one is a spatial object and the other is not, put the spatial first
#whichever is first, they both will have the properties of that first

head(census_blockgroups)

ggplot(census_blockgroups,
       aes(fill = POP2000)) +
  geom_sf()

census_tracts <- census_blockgroups %>%
  group_by(TRACT) %>%
  summarise(
    POP2000 = sum(POP2000),
    perc_hispa = sum(HISPANIC) / POP2000)
head(census_tracts)

#this gets the %hispanic per census tract
#census tract > census block in terms of area

tracts <- st_read('~/data/ct_00')
#double checking since we have tract data separately


#different visualizations
ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=tracts,
          color = 'red', fill = NA)

ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=lead, color = 'red',
          fill = NA, size = 0.1)

ggplot(census_tracts,
       aes(fill = perc_hispa)) +
  geom_sf() +
  geom_sf(data=lead, color = 'red',
          fill = NA, size = 0.1)

# PART II: SPATIAL QUERY AND AGGREGATION 

## Spatial Join

#when doing spatial joins - make sure data are in the same CRS
st_crs(lead)
st_crs(census_tracts)

#spatial joins can be for when data overlaps spatially but not with other variables
st_join(lead, census_tracts)
st_join()

lead_tracts <- lead %>%
    st_join(census_tracts) %>%
    st_drop_geometry() %>%
    group_by(TRACT) %>%
    summarize(avg_ppm=mean(ppm))
#this code is b/c the lead data needed to be averaged into each tract

head(lead_tracts)

census_lead_tracts <- census_tracts %>%
  inner_join(lead_tracts)

ggplot(census_lead_tracts,
       aes(fill = avg_ppm)) +
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

#this map helps show how there was not random sampling of lead data
#the next part is how to address this non-random/biased sampling

# PART III :  KRIGING SURFACES 

## The Semivariogram

library(gstat)

lead_xy <- read.csv('~/data/SYR_soil_PB.csv')
head(lead_xy)
#same data - no geometry

v_ppm <- variogram(
  ppm ~ 1, #intercept only
  locations = ~ x + y,
  data = lead_xy)

plot(v_ppm)
#shows that variance increases as points get further apart

v_ppm_fit <- fit.variogram(
  v_ppm,
  model = vgm(model="Sph", psill=1, range=900, nugget=1))
  
#1st step = this is a fitted model to compare with our actual data
#are they super different? does our model work?

plot(v_ppm, v_ppm_fit)
#basically this model predicts small to moderate distanced points VERY well
#but does poorly for points further than 4000m away from each other
#BUT how often are points isolated at +4000m?
#this would be something you would need to know about your own data I'd imagine


## Kriging

pred_ppm <- st_make_grid(
  lead, cellsize = 400,
  what = 'centers')
#this creates the grid for evenly placing points

#2nd step = predicting the NEW locations for the predicted values

pred_ppm <- pred_ppm[census_tracts]

#want points to be within census tracts only - not a perfect square grid

ggplot(census_tracts,
       aes(fill = POP2000)) +
  geom_sf() +
  geom_sf(data=pred_ppm, color = 'red', fill = NA)

#visual check of the grid being within census tracts

pred_ppm <- krige(
  formula=ppm~1,   #only an intercept again; no slopes; spatial autocorrelation only
  locations = lead,
  newdata = pred_ppm,
  model=v_ppm_fit)

pred_ppm

ggplot() + 
  geom_sf(data = census_tracts,
          fill = NA) +
  geom_sf(data = pred_ppm,
          aes(color = var1.pred))

#can compare this map with the heat map above of the ppm - pretty close!

pred_ppm_tracts <-
  pred_ppm %>%
  st_join(census_tracts) %>%
  st_drop_geometry() %>%
  group_by(TRACT) %>%
  summarise(pred_ppm = mean(var1.pred))
#same code as above, but with the fitted model data

census_lead_tracts <- 
  census_lead_tracts %>%
  inner_join(pred_ppm_tracts)
#this is for visualization

census_lead_tracts
#has both avg and pred lead ppm


ggplot(census_lead_tracts,
       aes(x = pred_ppm, y = avg_ppm)) +
  geom_point() +
  geom_abline(slope=1)

#DAMN! that's good!!! 1:1 slope cuts right thru the points
#between 5.2-5.5, there is a little deviation but still pretty good!

#let's check 5800 which we noticed had a lot of high values on top of each other and then low values more evenly spaced
census_lead_tracts %>% filter(TRACT==5800)
#avg = 5.44; pred = 5.53 - it went up!


# PART IV : SPATIAL AUTOCORRELATION AND REGRESSION 

ppm.lm <- lm(pred_ppm ~ perc_hispa,
  census_lead_tracts)

#let's check out our model
summary(ppm.lm)
plot(ppm.lm)

plot(ppm.lm$residuals)
abline(h=0)

census_lead_tracts <- census_lead_tracts %>%
  mutate(lm.resid=resid(ppm.lm))
plot(census_lead_tracts['lm.resid'])
#this will plot the residuals SPATIALLY!!!! SO COOL!!!
#Results show that residuals are pretty high AND clustered
#Not great for southern tracts

library(sp)
library(spdep)
library(spatialreg)

tracts <- as(
  st_geometry(census_tracts), 'Spatial')

tracts_nb <- poly2nb(tracts)
#convert spatial object into neighborhood matrix list - wtf?

head(tracts_nb)
#shows neighbors of each polygon


plot(census_lead_tracts['lm.resid'],
     reset = FALSE)
plot.nb(tracts_nb, coordinates(tracts),
        add = TRUE)
#now we can see neighbors on top of lead residuals

tracts_weight <- nb2listw(tracts_nb)
head(tracts_weight)

#gives weights to neighbors - default weight = each neighbor is the same

par(mfrow=c(1,1))
moran.plot(
  census_lead_tracts[['lm.resid']],
  tracts_weight,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)

#visualizes a correlation between the residuals and spatially lagged residuals
#spatially lagged = weighted average of neighbors
#correlation = positive, so spatial autocorrelation = CHECK

ppm.sarlm <- lagsarlm(
  pred_ppm ~ perc_hispa,
  data = census_lead_tracts,
  tracts_weight,
  tol.solve = 1.0e-30)

#fits spatially autoregressive model (SAR)

moran.plot(
  ppm.sarlm$residuals,
  tracts_weight,
  labels = census_lead_tracts[['TRACT']],
  pch = 19)

#still positive but way less so; quentin said "this looks great so"

summary(ppm.sarlm)
plot(ppm.sarlm$fitted.values ~ census_lead_tracts$perc_hispa, pch=21,
     xlab="Percent Hispanic", ylab="Lead ppm (fitted values SAR)", 
     col = "#000000", bg="#FFFFFF", font=2,
     font.lab=2, cex=0.9, cex.lab=1.1, ylim=c(4,5.5))

     