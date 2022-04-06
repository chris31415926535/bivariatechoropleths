# prepare sample shapefile for bivariate choropleth demo

renfrew_census <- readr::read_csv("data-raw/renfrew_csd_census_2016.csv")
renfrew_csd <- sf::read_sf("data-raw/renfrew_csds.shp")


renfrew_data <- renfrew_census %>%
  dplyr::filter(TEXT_NAME_NOM %in%
           c( "Median total income of households in 2015 ($)",
              "Population, 2016")) %>%
  dplyr::select(CSDUID = GEO_ID,
                variable = TEXT_NAME_NOM,
                value = T_DATA_DONNEE) %>%
  dplyr::mutate(var_name = dplyr::if_else(
    variable == "Population, 2016",
    "pop_2016",
    "median_household_income_2015"
  )) %>%
  dplyr::select(-variable) %>%
  tidyr::pivot_wider(
                     values_from = "value",
                     names_from = "var_name") %>%
  dplyr::mutate(CSDUID = as.character(CSDUID))


renfrew_county <- dplyr::left_join(renfrew_csd, renfrew_data)   %>%
  dplyr::select(CSDUID, CSDNAME, pop_2016, median_household_income_2015, geometry)

usethis::use_data(renfrew_county, overwrite = TRUE)



