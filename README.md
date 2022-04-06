
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bivariatechoropleths

<!-- badges: start -->
<!-- badges: end -->

The goal of **bivariatechoropleths** is to give easy and “tidy” tools
for bivariate mapping, including static images for publication, dynamic
maps for interactive documents, and a range of tools for creating and
exploring bivariate colour palettes.

**Please note** this is in development. Any bug reports or suggestions
are welcome.

## Installation

You can install the developer version of **bivariatechoropleths** with
the following command:

``` r
devtools::install_github("chris31415926535/bivariatechoropleths")
```

## Example

Here’s an example using a built-in dataset about Renfrew County,
Ontario. It won’t render here, but if you run it locally it will give
you an interactive Leaflet map.

``` r
library(bivariatechoropleths)
library(leaflet)

leaflet::leaflet() %>%
   leaflet::addTiles() %>%
   bivariatechoropleths::addBivariateChoropleth(
     map_data = bivariatechoropleths::renfrew_county,
     var1_name = pop_2016,
     var2_name = median_household_income_2015,
     ntiles= 3,
     var1_label = "Population, 2016",
     var2_label = "Median Household\nIncome, 2015",
     region_name = "CSDNAME",
     weight = 1,
     fillOpacity = 0.7,
     color = "grey",
     highlightOptions = leaflet::highlightOptions(color = "orange",
                                                  weight = 2,
                                                  opacity = 1)) 
```
