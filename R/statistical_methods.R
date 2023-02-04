#' @title Independence Test
#' @description The function tests whether two events given as columns of a
#' data frame are independent or not. It uses both a theoretical approach via
#' the chi-squared test from the `stats` package,
#' under the assumption that the values are independent,
#' and a resampling approach via functions from the `infer` package.
#' In the second method, we generate a sample of the events and permute the
#' observations of the second event against the ones of the first. For each
#' repetition, the chi-squared value is computed. The distribution of these values
#' approximates a chi-squared distribution.
#' Lastly, we compare the value obtained through the theoretical approach with the
#' distribution of the resampling method. If they are far from each other, we may
#' reject the Null-Hypothesis of independence.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe` that stores the first event
#' @param var2 name of column in `dataframe` that stores the second event
#' @param sample_size the size of the sample used in the resampling method
#' @param reps number of repetitions in the resampling method
#' @param illustrate if TRUE, prints the result of the theoretical method and
#' plots the chi-squared value obtained theoretically
#' against the distribution from the resampling method and a reference chi-squared
#' distribution with the observed degrees of freedom, Default: TRUE
#' @return A tibble containing a `stat` column of calculated chi-squared values
#' obtained in the resampling method.
#' @details The chi-squared test conducted in the theoretical approach might
#' return an inaccurate response if the relative proportions of the pairs of
#' observations are too skewed.
#' @examples
#' stat1 <- test_independence(euro_startups, country_code, status, 1000, 1000, TRUE)
#' stat2 <- test_independence(accidents21, accident_severity, day_of_week, 2000, 1000, TRUE)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#'  test_independence(euro_startups, "country_code", "status", 1000, 1000, TRUE)
#' # throws an error.
#' }
#' @seealso
#'  \code{\link[infer]{rep_sample_n}}, \code{\link[infer]{specify}},
#'  \code{\link[infer]{hypothesize}}, \code{\link[infer]{generate}},
#'  \code{\link[infer]{calculate}}
#'  \code{\link[stats]{chisq.test}}
#' @rdname test_independence
#' @export
test_independence <- function(dataframe, var1, var2, sample_size, reps,
                              illustrate = TRUE){
  #chi-squared for dataset
  chi2 <- stats::chisq.test(dataframe%>%dplyr::pull({{var1}}),
                            dataframe%>%dplyr::pull({{var2}}));
  x2 <- chi2$statistic;
  df <- chi2$parameter;
  pv <- chi2$p.value;

  # get sample
  sample <- infer::rep_sample_n(dataframe, size = sample_size);

  # approximate chi-squared distribution
  ans <- sample %>%
    infer::specify(response = {{var1}}, explanatory = {{var2}}) %>%
    infer::hypothesise(null = "independence") %>%
    infer::generate(reps = reps, type = "permute") %>%
    infer::calculate(stat = "Chisq");

  # illustrate
  if(illustrate){
    cat("Theoretical approach for the data set:",
        paste0("  X-squared value: ", sprintf(x2, fmt = "%#.3f")),
        paste0("  degrees of freedom: ", df),
        paste0("  p-value: ", signif(pv,digits = 3)), sep = "\n");

    plot(ans%>%ggplot2::ggplot() +
           ggplot2::geom_histogram(mapping = ggplot2::aes(x = .data$stat, y = ggplot2::after_stat(density)), binwidth = 0.5)+
           ggplot2::stat_function(fun=stats::dchisq, args = list(df = df))+
           ggplot2::geom_vline(ggplot2::aes(xintercept = x2, color = "X_squared"), size = 2) +
           ggplot2::scale_color_manual(name = "X-squared",
                                       values = c(X_squared = "blue"),
                                       labels = c(sprintf(x2, fmt = "%#.3f")))
    );
  }
  return(ans);
}
