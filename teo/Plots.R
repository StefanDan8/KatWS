#---------Plots---------#
#Plot1 <- ggplot2::ggplot(data = accidents21)+
 # ggplot2::geom_bar(mapping = aes(x = day_of_week, fill = accident_severity), position = "dodge") +
  #ggplot2::labs(title = "Total amount of traffic accidents in 2021 (UK)",
#                subtitle = "Accident severity included on the plot",
 #               caption = "published by the government of the United Kingdom")+
  #scale_fill_brewer(name = "Accident severity", palette = "Set1")+
  #scale_y_continuous(name ="Amount of accidents")+
  #scale_x_discrete(name = "Day of week")

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
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}},\code{\link[ggplot2]{facet_wrap}}
#' @rdname BARPLOT
#' @export#
#' @importFrom ggplot2 ggplot geom_bar facet_wrap
#' @importFrom magrittr %>%
BARPLOT <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot() +
      ggplot2::geom_bar(mapping = ggplot2::aes(x = {{var1}}, fill = {{var2}}),
                        position = "dodge") +
        ggplot2::facet_wrap(vars({{var2}}), scales = "free")
}

# BARPLOT(accidents21, <var1>, <var2>)

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
#' \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summmarise}},
#' \code{\link[dplyr]{mutate}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_bar}},
#' \code{\link[ggplot2]{coord_polar}}, \code{\link[ggplot2]{xlab}},
#' \code{\link[ggplot2]{ylab}}, \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{facet_wrap}}
#' @rdname PIECHART
#' @export
#' @importFrom ggplot2 ggplot geom_bar coord_polar xlab ylab geom_text facet_wrap
#' @importFrom magrittr %>%
PIECHART <- function(dataframe, var1, var2) {
  dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
      dplyr::summarise(cnt = n(), .groups = "drop_last") %>%
        dplyr::mutate(props1 = round(cnt/sum(cnt)*100, 3)) %>%
          ggplot2::ggplot( aes(x = {{var1}}, fill = {{var2 }})) +
            ggplot2::geom_bar( width = 1) +
              ggplot2::coord_polar(theta = "y") +
                ggplot2::xlab(NULL) +
                  ggplot2::ylab(NULL)+
                    geom_text(aes(y = {{var2}}, label = props1), color = "black", size=2.5)+
                      ggplot2::facet_wrap(vars({{var1}}))
}

# PIECHART(accidents21, <var1>, <var2>)

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
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot]{geom_count}},
#' \code{\link[ggplot]{scale_size_area}}
#' @rdname COUNTPLOT
#' @export
#' @importFrom ggplot2 ggplot geom_count scale_size
#' @importFrom magrittr %>%
COUNTPLOT <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot(aes(x = {{var1}}, y = {{var2}})) +
      ggplot2::geom_count(aes(size = after_stat(prop), group = {{var2}})) +
        ggplot2::scale_size_area(max_size = 10)
}

# COUNTPLOT(accidents21, <var1>, <var2>)

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
#' \code{\link[dplyr]{count}}, \code{\link[ggplot]{ggplot}}, \code{\link[ggplot]{geom_tile}}
#' @rdname HEATMAPS
#' @export
#' @importFrom dplyr count
#' @importFrom ggplot2 ggplot geom_tile
#' @importFrom magrittr %>%

HEATMAPS <- function(dataframe, var1, var2){
  dataframe %>%
    dplyr::count({{var1}}, {{var2}}) %>%
      ggplot2::ggplot(aes(x = {{var1}}, {{var2}}, fill = n))+
        ggplot2::geom_tile()
}

#HEATMAPS(accidents21, <var1>, <var2>)
