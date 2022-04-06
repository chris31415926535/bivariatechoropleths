#' Geography and 2016 census data for Renfrew County, Ontario census subdivisions
#'
#' A dataset containing the boundary lines and two 2016 census measures
#' (median household 2015 income, 2016 population) for Renfrew County, Ontario's
#' 19 census subdivisions.
#'
#' All data from Statistics Canada.
#'
#' @format A data frame with 19 rows and 5 variables:
#' \describe{
#'   \item{CSDUID}{Unique census subdivision numeric identifier}
#'   \item{CSDNAME}{Census subdivision name}
#'   \item{pop_2016}{Population in 2016}
#'   \item{median_household_income_2015}{Median household income in 2015, $}
#'   \item{geometry}{2016 census subdivision boundary lines}
#'   ...
#' }
#' @source \url{https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/index-eng.cfm}
"renfrew_county"
