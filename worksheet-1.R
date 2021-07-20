## Editor

vals <- ...

vals <- seq(...,
  ...)

## Load Data

storm <- read.csv(...)

storm <- read.csv(
  ...,
  ...)

## Data Structures

## Lists

x <- list('abc', 1:3, sin)

## Vectors

c(...)

## Data Types

## Factors

education <- ...(
  c('college', 'highschool', 'college', 'middle', 'middle'),
  ... = c('middle', 'highschool', ...))

## Data Frames

... <- c(32000, 28000, 89000, 0, 0)
... <- data.frame(...)

## Parts and Subsets

## Names

...(df) <- c(...)

## Subsetting 

days <- c(
  'Sunday', 'Monday', 'Tuesday',
  'Wednesday', 'Thursday', 'Friday',
  'Saturday')

weekdays <- ...

... <- ...

## Functions

... <- function(...) {
       ...
       return(...)
       }

## Flow Control

if (...) {
  ...
} else {
  ...
}


first <- function(dat) {
  ... {
    result <- dat[[1]]
  } ... {
    result <- dat[1, ]
  }
  return(result)
}

## Distributions and Statistics

...(n = 10)

x <- rnorm(n = 100, ... = 15, sd = 7)
y <- ...(n = 100, ... = 20, prob = .85)



