#---------Plots---------#


#' @title BARPLOT1
#' @description `BARPLOT1()` generates a classical barplot
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @return `BARPLOT1()` returns a barplot
#' @details With `var1` on the x-axis `BARPLOT1()` counts the total amount of the variable and
#' it is displayed on the y-axis. With `var2`, `BARPLOT1()` splits the bars in smaller colored bars,
#' that represent the amount of observations regarding the second variable.
#' @examples
#' BARPLOT1(euro_startups, status, country_code)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' BARPLOT1(euro_startups, "status", "country_code")
#' # throws an error.
#' }
#' @seealso
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}}
#' @rdname BARPLOT1
#' @export
#' @importFrom magrittr %>%
BARPLOT1 <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot() +
      ggplot2::geom_bar(mapping = ggplot2::aes(x = {{var1}}, fill = {{var2}}),
                        position = "dodge");
}

# BARPLOT(accidents21, <var1>, <var2>)

#' @title BARPLOT2
#' @description `BARPLOT2()` is similar to `BARPLOT1()` with an additional facet
#' given with `facet_wrap()`
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @return `BARPLOT2()` returns a barplot with facets.
#' @details In extension to `BARPLOT1()`, `BARPLOT2()` divides the plot in more
#' subplots to gain more visibility.
#' @examples
#' BARPLOT2(euro_startups, status, country_code)
#' \dontrun{
#'  # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' BARPLOT2(euro_startups, "status", "country_code")
#' # throws an error.
#' }
#' @seealso
#' \code{\link[katws]{BARPLOT1}} is a extension of this function that has additional facets.
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{facet_wrap}}
#' @rdname BARPLOT2
#' @export
#' @importFrom magrittr %>%
#'
BARPLOT2 <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = {{var1}}, fill = {{var2}}),
                      position = "dodge") +
        ggplot2::facet_wrap(ggplot2::vars({{var2}}), scales = "free");
}

#' @title PIECHART
#' @description `PIECHART()` is a function that generates a piechart as a plot.
#' @param dataframe  a data frame or data frame extension (e.g. a tibble)
#' @param var name of column in `dataframe`
#' @return returns a piechart.
#' @details `PIECHART()` calculates the percentage of the observations of `var1`
#' and displays it onto the plot. The piechart is a barplot with a polar-transformation.
#' @examples
#' PIECHART(euro_startups, status)
#' \dontrun{
# var must be passed as it  is, without quotes or apostrophes.
#' PIECHART(euro_startups, "status")
#' # throws an error.
#' }
#' @seealso
#' \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}},
#' \code{\link[ggplot2]{coord_polar}},\code{\link[ggplot2]{geom_text}},
#' \code{\link[ggplot2]{labs}}
#' @rdname PIECHART
#' @export
#' @importFrom magrittr %>%
PIECHART <- function(dataframe, var){
  len <- nrow(dataframe);
  dataframe %>%
    dplyr::group_by({{var}}) %>%
      dplyr::summarise(cnt = dplyr::n()) %>%
        ggplot2::ggplot(ggplot2::aes(x="", y = .data$cnt, fill = {{var}}))+
          ggplot2::geom_bar(stat="identity", color = "white") +
            ggplot2::coord_polar("y") +
              ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$cnt/len)),
                           position = ggplot2::position_stack(vjust=0.5), size=3)+
                        ggplot2::labs(x = NULL, y = NULL, fill = NULL);
}

#' @title COUNTPLOT
#' @description `COUNTPLOT()` generates a countplot as a plot.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @param stat a char for `prop` or `count`
#' @return `COUNTPLOT()` returns a countplot with either a `prop`-parameter
#' or `count`-parameter.
#' @details `COUNTPLOT()` draws a point at each combination of values from the two
#' variables `var1` and `var2`. The size of the points is mapped to the number
#' of observations with this combinations of values. Meaning: rare observations
#' is equal to small points and frequent observations is equal to large points.
#' With `stat` you can decide whether to use `prop` or `count` as parameter, so
#' that you have two possibilities of plots.
#' @examples
#' COUNTPLOT(euro_startups, status, country_code, "prop")
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' # stat must be passed as either "prop", or "count".
#' COUNTPLOT(euro_startups, "status", "country_code", prop)
#' # throws an error.
#' }
#' @seealso
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_count}},
#' \code{\link[ggplot2]{scale_size_area}}
#' @rdname COUNTPLOT
#' @export
#' @importFrom magrittr %>%
COUNTPLOT <- function(dataframe, var1, var2, stat ){
  if(!(stat =="count" || stat == "prop")){
    warning("unrecognizes parameter. Calculating with the default statistic 'count'");
    }
  my_aes <- if(stat == "prop") ggplot2::aes(size = ggplot2::after_stat(prop), group = {{var2}})
  else NULL;
  dataframe %>%
    ggplot2::ggplot(ggplot2::aes(x = {{var1}}, y = {{var2}})) +
    ggplot2::geom_count(my_aes) +
    ggplot2::scale_size_area(max_size = 10)
}
#' @title HEATMAPS
#' @description `HEATMAPS()` is a function that generates a heatmap as a plot.
#' @param dataframe a data frame or data frame extension (e.g. a tibble)
#' @param var1 name of column in `dataframe`
#' @param var2 name of column in `dataframe`
#' @return `HEATMAPS()` returns a heatmap
#' @details `HEATMAPS()` has a similar functionality like `COUNTPLOT()`, but
#' it differs only in visualization. It doesn't plot points,
#' but colored areas, whose colors visualize the amount of observations of given combinations.
#' @examples
#' HEATMAPS(euro_startups, status, country_code)
#' \dontrun{
#' # var1 and var2 must be passed as they are, without quotes or apostrophes.
#' HEATMAPS(euro_startups, "status", "country_code")
#' # throws an error.
#' }
#' @seealso
#' \code{\link[katws]{COUNTPLOT}} is similar to `COUNTPLOT()`, it differs only in visualization.
#' \code{\link[dplyr]{count}}, \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_tile}}
#' @rdname HEATMAPS
#' @export
#' @importFrom magrittr %>%

HEATMAPS <- function(dataframe, var1, var2){
  dataframe %>%
    dplyr::count({{var1}}, {{var2}}) %>%
      ggplot2::ggplot(ggplot2::aes(x = {{var1}}, {{var2}}, fill = n))+
        ggplot2::geom_tile()+
          ggplot2::scale_fill_viridis_c();
}


