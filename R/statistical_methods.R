#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param var1 PARAM_DESCRIPTION
#' @param var2 PARAM_DESCRIPTION
#' @param sample_size PARAM_DESCRIPTION
#' @param reps PARAM_DESCRIPTION
#' @param visualize PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[infer]{rep_sample_n}}, \code{\link[infer]{specify}}, \code{\link[infer]{hypothesize}}, \code{\link[infer]{generate}}, \code{\link[infer]{calculate}}
#' @rdname test_independence
#' @export
#' @importFrom dplyr pull
#' @importFrom infer rep_sample_n specify hypothesise generate calculate
test_independence <- function(dataframe, var1, var2, sample_size, reps,
                              visualize = TRUE){
  chi2 <- chisq.test(dataframe%>%dplyr::pull({{var1}}),
                     dataframe%>%dplyr::pull({{var2}}));
  print(chi2);
  x2 <- chi2$statistic;
  df <- chi2$parameter;
  sample <- infer::rep_sample_n(dataframe, size = sample_size);
  ans <- sample %>%
    infer::specify(response = {{var1}}, explanatory = {{var2}}) %>%
    infer::hypothesise(null = "independence") %>%
    infer::generate(reps = reps, type = "permute") %>%
    infer::calculate(stat = "Chisq");
  if(visualize){
    plot(ans%>%ggplot() +
           geom_histogram(mapping = aes(x = stat, y = ..density..),binwidth = 0.5)+
           #ylim(0, 0.2)+
          # xlim(0,20)+
           stat_function(fun="dchisq", args = list(df = df))+
           geom_vline(xintercept = x2, color = "blue", size = 3)
         );
  }
  return(ans);
}