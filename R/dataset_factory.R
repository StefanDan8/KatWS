#This is a file that contains several functions that create custom synthetic datasets


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nrows PARAM_DESCRIPTION, Default: 1000
#' @param seed PARAM_DESCRIPTION, Default: 42
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[stats]{Uniform}}, \code{\link[stats]{Binomial}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{recode}}
#' @rdname jobs_colors_independent
#' @export
#' @importFrom tibble tibble
#' @importFrom stats runif rbinom
#' @importFrom dplyr mutate recode_factor
#' @importFrom magrittr %>%
#' @importFrom rlang .data
jobs_colors_independent <- function(nrows=1e3, seed = 42){
  set.seed(seed);
  ds <- tibble::tibble(
    id = 1: nrows,
    fav_color = ceiling(stats::runif(nrows,min = 0, max = 5)),
    job = stats::rbinom(nrows, size = 10, prob = 0.6),
    gender = factor(sample(x = c("Male", "Female"),size = nrows, replace = TRUE, prob = c(0.5, 0.5)))
  ) %>%
    dplyr::mutate(fav_color = dplyr::recode_factor(.data$fav_color, 'Blue', 'Red', 'Green', 'Yellow', 'Black')) %>%
      dplyr::mutate(job = dplyr::recode_factor(.data$job, '0'='Jobless', '1'='Mathematician', '2'='Doctor', '3'='Lawyer',
                             '4'='Plumber','5'= 'Fireman', '6'='Bus driver','7'='Accountant',
                             '8'='Teacher', '9'='Shopkeeper', '10'='IT'));

  return(ds);
}


#yet only for 2 variables.. to be extended...
#Returns a tibble with nrows, distributed after a 2D multivariate
#Normal distribution with mean and cov_matrix

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nrows PARAM_DESCRIPTION, Default: 1000
#' @param mean PARAM_DESCRIPTION
#' @param cov_matrix PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION, Default: 42
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[MASS]{mvrnorm}}
#' @rdname multivariateNormal
#' @export
#' @importFrom tibble tibble
#' @importFrom MASS mvrnorm
multivariateNormal <- function(nrows = 1e3,mean, cov_matrix, seed = 42){
  set.seed(seed);
  df <-  tibble::tibble(var_1 = factor(), var_2 = factor());
  sample <- MASS::mvrnorm(n = nrows, mu = mean, Sigma = cov_matrix)
  df <- tibble::tibble(var_1 =sample[,1], var_2 = sample[,2]);
  return(df)
}

# This turns a continuous, multivariate Normal distribution into a discrete
# distribution, the values being arranged in equal bins between -3*var + mean
#and mean + 3*var. What's below/ above represents one single bin
#The maximal number of bins is 26 as the variables are turned into Letters.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nrows PARAM_DESCRIPTION, Default: 1000
#' @param mean PARAM_DESCRIPTION
#' @param cov_matrix PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION, Default: 42
#' @param vector_bins PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname multivariateNormalDiscrete
#' @export
multivariateNormalDiscrete <- function(nrows = 1e3, mean, cov_matrix, seed = 42, vector_bins){
  df <- multivariateNormal(nrows, mean, cov_matrix, seed)
      df$var_1 = discretise(mean[1], cov_matrix[1,1], vector_bins[1], df$var_1);
      df$var_2 = discretise(mean[2], cov_matrix[2,2], vector_bins[2], df$var_2);
  return(df);
}
#this one does not work mutate somehow passes the entire column
#Too tired to debug today. The function above makes the same and it works.

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param nrows PARAM_DESCRIPTION, Default: 1000
#' @param mean PARAM_DESCRIPTION
#' @param cov_matrix PARAM_DESCRIPTION
#' @param seed PARAM_DESCRIPTION, Default: 42
#' @param vector_bins PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname multi
#' @export
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
multi <- function(nrows = 1e3, mean, cov_matrix, seed = 42, vector_bins){
  lower_bound <- mean - 3*sqrt(diag(cov_matrix));
  upper_bound <- mean + 3*sqrt(diag(cov_matrix));
  df <- multivariateNormal(nrows, mean, cov_matrix, seed) %>%
    dplyr::mutate(var_1 = disc(lower_bound[1],upper_bound[1], vector_bins[1], .data$var_1))
  return(df)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param mean PARAM_DESCRIPTION
#' @param variance PARAM_DESCRIPTION
#' @param nbins PARAM_DESCRIPTION
#' @param vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname discretise
#' @export
discretise <- function(mean, variance, nbins, vec){
  lower_bound <- mean - 3*sqrt(variance);
  upper_bound <- mean + 3*sqrt(variance);
  ans <- sapply(vec, disc, lower_bound = lower_bound, upper_bound= upper_bound, nbins = nbins);
  return(ans);
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param lower_bound PARAM_DESCRIPTION
#' @param upper_bound PARAM_DESCRIPTION
#' @param nbins PARAM_DESCRIPTION
#' @param value PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname disc
#' @export
disc <- function(lower_bound, upper_bound, nbins, value){
  if(value<=lower_bound){
    return("A");
  }
  if(value>=upper_bound){
    return(LETTERS[nbins]);
  }
  thr <-  (upper_bound - lower_bound)/(nbins-2);
  return(LETTERS[ceiling((value-lower_bound)/thr)+1]);
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param format PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{write_delim}}
#' @rdname save_as
#' @export
#' @importFrom readr write_csv write_tsv
save_as <- function(dataframe, format){
  dfAsString <- deparse1(substitute(dataframe))
  switch(format, ".csv" = readr::write_csv(dataframe, paste0("Dataframes/", dfAsString, ".csv")),
         ".rds" =saveRDS(dataframe, paste0("Dataframes/", dfAsString, ".rds")),
         ".tsv" = readr::write_tsv(dataframe, paste0("Dataframes/", dfAsString, ".tsv"))
  );
}
