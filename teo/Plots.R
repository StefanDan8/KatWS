#---------Plots---------#


#' @title BARPLOT1 & BARPLOT2
#' @description BARPLOT1 und BARPLOT2 sind Funktionen, welche Säulendiagramme erstellen.
#' BARPLOT1 erzeugt das klassische Säulendiagramm, wogegen BARPLOT2 mittels facet_wrap das Säulendiagramm für bessere
#' Visualisierung mit Untergruppen auftrennt.
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
BARPLOT1 <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot() +
      ggplot2::geom_bar(mapping = ggplot2::aes(x = {{var1}}, fill = {{var2}}),
                        position = "dodge");
}

# BARPLOT(accidents21, <var1>, <var2>)

BARPLOT2 <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(mapping = ggplot2::aes(x = {{var1}}, fill = {{var2}}),
                      position = "dodge") +
        ggplot2::facet_wrap(vars({{var2}}), scales = "free");
}

#' @title PIECHART
#' @description PIECHART ist im Grunde genommen auch ein Säulendiagramm,
#' aber durch Transformation in Polarkoordiantenn erzeugt man ein Kreisdiagramm.
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
PIECHART <- function(dataframe, var){
  len <- nrow(dataframe);
  dataframe %>%
    dplyr::group_by({{var}}) %>%
      dplyr::summarise(cnt = n()) %>%
        ggplot2::ggplot(ggplot2::aes(x="", y = cnt, fill = {{var}}))+
          ggplot2::geom_bar(stat="identity")+ggplot2::coord_polar("y")+
            ggplot2::geom_text(aes(y = cnt/3 + c(0, cumsum(cnt)[-length(cnt)]),
                           label = scales::percent(cnt/len)),
                           position = position_stack(vjust=0.5), size=4);
}

# PIECHART(accidents21, <var1>, <var2>)

#' @title COUNTPLOT
#' @description COUNTPLOT erstellt einen Graphen für kategorische Variablen.
#' COUNTPLOT erstellt Punkte von jeder Kombination von Werten, die
#' von den zwei eingetragenen Variablen entnommen werden.
#' Die Größe jedes aufgetragenen Punktes hängt von der Anzahl an Beobachtungen von den Kombinationen ab.
#'  Je frequenter die Beobachtung, desto größer der Punkt.
#'  Je seltener die Beobachtung, desto kleiner der Punkt.
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
        ggplot2::scale_size_area(max_size = 10);
}

# COUNTPLOT(accidents21, <var1>, <var2>)

#' @title HEATMAPS
#' @description HEATMAPS hat die ähnliche Funktionalität wie COUNTPLOT,
#' aber unterscheidet sich bei der Visualisierung. Hier werden keine Punkte aufgetragen,
#' sondern gefärbte Flächen, deren Farbe die Menge an Beobachtungen der Kombinationen visualisiert.
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
      ggplot2::ggplot(ggplot2::aes(x = {{var1}}, {{var2}}, fill = n))+
        ggplot2::geom_tile()+
          ggplot2::scale_fill_viridis_c();
}

#HEATMAPS(accidents21, <var1>, <var2>)
