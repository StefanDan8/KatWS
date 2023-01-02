#-------NUMERIC METHODS ------- #

#write code to catch exceptions!!

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{count}}
#' @rdname how_many
#' @export
#' @importFrom dplyr count
#' @importFrom magrittr %>%
how_many <- function(dataframe, variable){
  ans <- dataframe %>%
    dplyr::count({{variable}})
  return(ans)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param var1 PARAM_DESCRIPTION
#' @param var2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{n_distinct}}
#' @rdname how_many_distinct_per
#' @export
#' @importFrom dplyr group_by summarise n n_distinct
#' @importFrom magrittr %>%
#' @importFrom rlang :=
how_many_distinct_per <- function(dataframe, var1, var2 ){
  ans <- dataframe %>%
    dplyr::group_by({{var1}}) %>%
      dplyr::summarise(n = dplyr::n(), "n_{{var2}}" := dplyr::n_distinct({{var2}}))
  return(ans)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param var1 PARAM_DESCRIPTION
#' @param var2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map}}
#' @rdname uniques_per_var
#' @export
#' @importFrom dplyr group_by summarise mutate
#' @importFrom purrr map_chr
#' @importFrom magrittr %>%
uniques_per_var <- function(dataframe, var1, var2 ){
  n2 <- paste0("values_of_",deparse1(substitute(var2)));
  ans <- dataframe %>%
    dplyr::group_by({{var1}}) %>%
      dplyr::summarise(!!n2 := list(unique({{var2}}))) %>%
        dplyr::mutate(display = purrr::map_chr(get(n2), toString))
  return(ans)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param var1 PARAM_DESCRIPTION
#' @param var2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{context}}
#'  \code{\link[tidyr]{spread}}
#' @rdname contingency_table
#' @export
#' @importFrom dplyr group_by summarise n ungroup
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
contingency_table <- function(dataframe, var1, var2){
  ans <- dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
        dplyr::ungroup( )%>%
          tidyr::spread({{var2}}, n)

  return(ans)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param dataframe PARAM_DESCRIPTION
#' @param var1 PARAM_DESCRIPTION
#' @param var2 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{spread}}
#'  \code{\link[tibble]{add_column}}
#' @rdname contingency_table_scale
#' @export
#' @importFrom dplyr group_by summarise mutate select ungroup
#' @importFrom tidyr spread
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#' @importFrom rlang .data
contingency_table_scale <- function(dataframe, var1, var2){
  n2 <- deparse1(substitute(var1))
  ans <- dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
      dplyr::summarise(n = n() ,.groups = "drop_last") %>%
        dplyr::mutate(prop = n/sum(n)) %>%
          dplyr::select({{var1}}, {{var2}}, .data$prop) %>%
            tidyr::spread({{var2}}, .data$prop) %>%
              dplyr::ungroup() %>%
                tibble::add_column(n = how_many(dataframe, {{var1}})$n, .after = n2)

  return(ans)
}




