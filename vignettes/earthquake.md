---
title: "Earthquake Package"
author: "Kevin Sykora"
date: "2017-10-14"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{earthquake}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

This package processes and visualizes NOAA earthquake data.

The package cleans and tidy's the data, creates static timeline plots, and creates dynamic leaflet plots.

Before we begin, let's load in an R dataset:


```r
library(earthquake)
library(dplyr)
library(ggplot2)
library(grid)
data(earthquake_raw)
```


earthquake_raw is a data.frame that holds the raw NOAA data.

## Clean the data

The NOAA datasets do not come by default in a clean format.
The earthquake package includes tools for cleaning data.

One of the tools is the `eq_clean_data` function:


```r
earthquake_clean <- earthquake_raw %>% earthquake::eq_clean_data()
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.2
```

```
## Warning in earthquake::eq_clean_data(.): BCE dates are not supported. We
## are dropping them from the analysis dataset.
```

```r
head(earthquake_clean[["DATE"]])
```

```
## [1] "0010-01-01" "0011-01-01" "0017-01-01" "0023-01-01" "0025-01-01"
## [6] "0027-01-01"
```

Many common R date functions do not support BCE dates. Most earthquake analysis focuses on more recent events.
Therefore we drop BCE dates to maintain user flexibility.

The `eq_clean_data` creates a date column, removes NA columns and drops BCE dates.

Another tool is the `eq_location_clean` function:


```r
earthquake_clean_loc <- earthquake_raw %>% earthquake::eq_location_clean()
#Original Location Name
head(earthquake_raw[["LOCATION_NAME"]])
```

```
## [1] "JORDAN:  BAB-A-DARAA,AL-KARAK"     "TURKMENISTAN:  W"                 
## [3] "SYRIA:  UGARIT"                    "GREECE:  THERA ISLAND (SANTORINI)"
## [5] "ISRAEL:  ARIHA (JERICHO)"          "ITALY:  LACUS CIMINI"
```

```r
#Cleaned Location Name
head(earthquake_clean_loc[["LOCATION_NAME"]])
```

```
## [1] "Bab-A-Daraa,Al-Karak"     "W"                       
## [3] "Ugarit"                   "Thera Island (Santorini)"
## [5] "Ariha (Jericho)"          "Lacus Cimini"
```

The `eq_location_clean` cleans up the location name for future annotations.


## Create timeline plots

The earthquake package also includes new geoms for plotting timelines.

The `geom_timeline` creates timeline plots of the cleaned NOAA data:


```r
ggplot(data=earthquake_clean %>%
         filter(COUNTRY %in% c("CHINA", "USA") ))+
  geom_timeline(aes(x = DATE, color = log(DEATHS, base = 10), y = COUNTRY,
                    alpha = 1, xmin=as.Date("2000-01-01"), xmax=as.Date("2015-12-31"), size = EQ_PRIMARY)) +
  scale_color_continuous()+
  theme_minimal()+theme(legend.position = "bottom")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

You can add in the `geom_timelinelabel` geom to make annotations. It has an additional parameter n_max.
This only labels the highest n_max magnitude earthquakes.


```r
ggplot(data=earthquake_clean %>%
         filter(COUNTRY %in% c("CHINA", "USA") ))+
  geom_timelinelabel(aes(x = DATE, label= LOCATION_NAME, y = COUNTRY,
                         xmin=as.Date("2000-01-01"), xmax=as.Date("2015-12-31"), mag = EQ_PRIMARY), n_max = 4)+
  geom_timeline(aes(x = DATE, color = log(DEATHS, base = 10), y = COUNTRY,
                    alpha = 1, xmin=as.Date("2000-01-01"), xmax=as.Date("2015-12-31"), size = EQ_PRIMARY)) +
  scale_color_continuous()+
  theme_minimal()+theme(legend.position = "bottom")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## Leaflet Plots

The `eq_map` function takes in a cleaned NOAA earthquake dataset, and an annotation column.
It outputs a leaflet plot with the earthquakes on a map and annotations (available by clicking on the point) that describe them.





