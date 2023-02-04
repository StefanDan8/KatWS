#' @title Variable discovery
#' @description `uniques_per_var()` creates a tibble which displays all unique
#' values of a column corresponding to the observations in another column.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @return `uniques_per_value()` returns an object of type tibble.
#' \itemize{
#' \item{First column, named `<var1>`}{ is made of the unique values
#'of the column passed through `var1` (type `chr`)}
#' \item{Second column, named `values_of_<var1>`}{ contains lists
#' of all unique values corresponding to each value in the first column
#' (type `list`)}
#' \item{Third column, named `display`}{ a sorted string of the
#' corresponding list in second column (type `chr`)}
#' }
#' @details The function is very useful for exploring a data set and, depending
#' on the data, it can be a nice way of illustrating pairs of columns.
#' @examples
#' # Here we display each car model of every manufacturer present in the `mpg`
#' #data set from the `ggplot2` package.
#' uniques_per_var(ggplot2::mpg, manufacturer, model)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#'  uniques_per_var(ggplot2::mpg, "manufacturer", "model")
#' # throws an error.
#' }
#' @seealso
#'  \code{\link[ggplot2]{mpg}}
#' @rdname uniques_per_var
#' @importFrom rlang :=
#' @export
uniques_per_var <- function(dataframe, var1, var2 ){
  n2 <- paste0("values_of_",deparse1(substitute(var2)));
  ans <- dataframe %>%
    dplyr::group_by({{var1}}) %>%
    dplyr::summarise(!!n2 := list(sort(unique({{var2}})))) %>%
    dplyr::mutate(display = purrr::map_chr(get(n2), toString))
  return(ans)
}


#' @title Cross Tabulation
#' @description `contingency_table()` builds a contingency table of the counts at
#' each combination of values from two columns of a data frame.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @return `contingency_table()` returns a contingency table represented as
#' an object of type tibble
#' @details the first column is given by `var1` and the following columns are
#' named by the unique values of `var2`. Each cell represents the count of
#' `var1`-`var2` pairs for the corresponding values.
#' @examples
#' contingency_table(accidents21, accident_severity, urban_or_rural_area)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' contingency_table(accidents21, "accident_severity", "urban_or_rural_area")
#' # throws an error.
#' }
#' @seealso
#'  \code{\link[katws]{contingency_table_scale}} is a extension of this function
#'  that gives the percentage the frequency of each pair among all pairs
#' @rdname contingency_table
#' @export
contingency_table <- function(dataframe, var1, var2){
  ans <- dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
    dplyr::ungroup( ) %>%
    tidyr::spread({{var2}}, n) %>%
    dplyr::mutate_if(is.numeric,~tidyr::replace_na(.,0))
  return(ans)
}


#' @title Cross Tabulation with Percentages
#' @description `contingency_table_scale()` builds a contingency table of the
#' percentages of each pair. If the parameter `all` is set to TRUE, then the
#' percentages are relative to the entire data frame. Otherwise, they are relative
#' to each value given in the `var1` parameter.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @param all whether the percentages are relative to the entire `dataframe`
#' (`all = TRUE`) or to `var1`.
#' @return `contingency_table_scale()` returns a contingency table represented as
#' an object of type tibble.
#' @details The first column is given by `var1` and the following columns are
#' named by the unique values of `var2`. Each cell represents the percentage of
#' `var1`-`var2` pairs for the corresponding values.
#' @examples
#' contingency_table_scale(euro_startups, country_code, status, all = FALSE)
#' contingency_table_scale(euro_startups, country_code, status, all = TRUE)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' contingency_table_scale(euro_startups, "country_code", "status", all = FALSE)
#' # throws an error.
#' }
#' @seealso
#'   \code{\link[katws]{contingency_table}}
#' @rdname contingency_table_scale
#' @export
contingency_table_scale <- function(dataframe, var1, var2, all = FALSE){
  total <- nrow(dataframe);
  func <- if(all) function(n){n/total} else function(n){n/sum(n)};
  n2 <- deparse1(substitute(var1));
  ans <- dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
    dplyr::summarise(n = dplyr::n(),.groups = "drop_last") %>%
    dplyr::mutate(prop = func(n)) %>%
    dplyr::select({{var1}}, {{var2}}, "prop") %>%
    tidyr::spread({{var2}}, "prop") %>%
    dplyr::ungroup()

  return(ans)
}




