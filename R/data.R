#' Car accidents in UK in 2021
#'
#' @description TODO: write a good description and document this properly
#'
#' @format this data frame has 101.046 rows and the following 7 columns:
#' \describe{
#'   \item{y}{the count for the 2-week period.}
#'   \item{trt}{treatment, "placebo" or "progabide"}
#'   \item{post}{post treatment. 0 for no, 1 for yes}
#'   \item{subject}{subject id}
#'   \item{tj}{time}
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
