## Tidy Concept

trial <- read.delim(sep = ',', header = TRUE, text = "
block, drug, control, placebo
    1, 0.22,    0.58,    0.31
    2, 0.12,    0.98,    0.47
    3, 0.42,    0.19,    0.40
")

## Pivot wide to long 

library(tidyr)
tidy_trial <- ...(trial,
                  cols = ...,
                  names_to = ...,
                  values_to = ...)

## Pivot long to wide 

survey <- read.delim(sep = ',', header = TRUE, text = "
participant,   attr, val
1          ,    age,  24
2          ,    age,  57
3          ,    age,  13
1          , income,  30
2          , income,  60
")

tidy_survey <- ...(survey,
                   names_from = ...,
                   values_from = ...)

tidy_survey <- pivot_wider(survey,
                           names_from = attr,
                           values_from = val,
                           values_fill = ...)

## Sample Data 

library(data.table)
cbp <- fread('data/cbp15co.csv')

cbp <- fread(
  'data/cbp15co.csv',
  ...,
  ...)

acs <- fread(
  'data/ACS/sector_ACS_15_5YR_S2413.csv',
  colClasses = c(FIPS = 'character'))

## dplyr Functions 

library(...)
cbp2 <- filter(...,
  ...,
  !grepl('------', NAICS))

library(...)
cbp2 <- filter(cbp,
  ...)

cbp3 <- mutate(...,
  ...)

cbp3 <- mutate(cbp2,
  FIPS = str_c(FIPSTATE, FIPSCTY),
  ...)

...
  filter(
    str_detect(NAICS, '[0-9]{2}----')
  ) ...
  mutate(
    FIPS = str_c(FIPSTATE, FIPSCTY),
    NAICS = str_remove(NAICS, '-+')
  )

...
  ...(
    FIPS,
    NAICS,
    starts_with('N')
  )

## Join

sector <- fread(
  'data/ACS/sector_naics.csv',
  colClasses = c(NAICS = 'character'))

cbp <- cbp %>%
  ...

## Group By 

cbp_grouped <- cbp %>%
  ...

## Summarize 

cbp <- cbp %>%
  group_by(FIPS, Sector) %>%
  ...
  ...

acs_cbp <- ... %>%
  ...
