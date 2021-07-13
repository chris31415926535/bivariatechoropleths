
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bivariatechoropleths

<!-- badges: start -->
<!-- badges: end -->

The goal of **bivariatechoropleths** is to give easy and “tidy” tools
for bivariate mapping, including static images for publication, dynamic
maps for interactive documents, and a range of tools for creating and
exploring bivariate colour palettes.

## Installation

You can install the developer version of **bivariatechoropleths** with
the following command:

``` r
devtools::install_github("chris31415926535/bivariatechoropleths")
```

## Example

Working examples will go here soon. Here’s a simple call that will make
an interactive bivariate choropleth using Leaflet:

``` r
library(bivariatechoropleths)
library(sf)

map_data <- sf::read_sf("ontario_data.shp")

leaflet::leaflet() %>%
  bivariatechoropleths::addBivariateChoropleth(map_data,
                                               variable_1,
                                               variable_2)
```
