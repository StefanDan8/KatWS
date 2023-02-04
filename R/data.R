#' Car accidents in UK in 2021
#'
#' @description This tibble contains information about the car accidents
#' registered in the United Kindgom in 2021
#'
#' @format the tibble has 101.046 rows and the following 7 columns:
#' \describe{
#'   \item{`accident_severity`}{The severity of the accident
#'   (Factor, with levels "Slight", "Serious", "Fatal")}
#'   \item{`day_of_week`}{
#'   The weekday of the accident
#'   (Factor, with levels "Monday", "Tuesday", ... ,"Sunday")}
#'   \item{`road_type`}{
#'   Type of road
#'   (Factor, with levels "Single carriageway", "Dual carriageway",
#'   "One way street", "Slip road", "Roundabout", "Unknown")}
#'   \item{`speed_limit`}{
#'   Maximum speed limit in miles/hour in the place of accident
#'   (Factor, with levels "20", "30", ... , "70")}
#'   \item{`light_conditions`}{
#'   Light conditions at time of accident
#'   (Factor, with levels "Daylight", "Darkness - no lighting",
#'   "Darkness - lights unlit", "Darkness - lights lit", "Darkness - lighting unknown")}
#'   \item{`weather_conditions`}{
#'   Weather conditions at time of accident
#'   (Factor, with levels "Fine + high winds", "Fine no high winds", "Fog or mist",
#'   "Other", "Raining + high winds", "Raining no high winds", "Snowing + high winds",
#'   "Snowing no high winds", "Unknown")}
#'   \item{`urban_or_rural_area`}{
#'   Whether the accident happened in a rural or urban area
#'   (Factor, with levels "Urban", "Rural")}
#' }
#' @source \url{https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data}
"accidents21"

#' Start-ups in Western Europe
#'
#' @description This tibble contains the areas of expertise and the current status
#' of start-ups founded in eight Western European countries.
#'
#' @format This tibble contains 7167 observations and the following 3 columns:
#' \describe{
#'   \item{`category_list`}{Field(s) of activity of the start-up (saved as chr)}
#'   \item{`status`}{current status of the start-up
#'   (Factor, with levels "operating", "closed", "acquired")}
#'   \item{`country_code`}{the country in which the start-up was founded
#'   (Factor, with levels: \cr
#'   "AUT" = Austria \cr
#'   "BEL" = Belgium \cr
#'   "CHE" = Switzerland \cr
#'   "DEU" = Germany \cr
#'   "FRA" = France \cr
#'   "GBR" = Great Britain \cr
#'   "IRL" = Ireland \cr
#'   "SWE" = Sweden)}
#' }
#' @source The data originates from a bigger dataset,
#' `Startup Success/Fail Dataset from Crunchbase`, which can be found on kaggle under
#' the following link:
#'  \url{https://www.kaggle.com/datasets/yanmaksi/big-startup-secsees-fail-dataset-from-crunchbase}
"euro_startups"
