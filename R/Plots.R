#In this script there will be plots

accidents21$day_of_week <- factor(
  accidents21$day_of_week,
  levels = c("Monday","Tuesday" , "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")
)






#Plot1 <- ggplot2::ggplot(data = accidents21)+
 # ggplot2::geom_bar(mapping = aes(x = day_of_week, fill = accident_severity), position = "dodge") +
  #ggplot2::labs(title = "Total amount of traffic accidents in 2021 (UK)",
#                subtitle = "Accident severity included on the plot",
 #               caption = "published by the government of the United Kingdom")+
  #scale_fill_brewer(name = "Accident severity", palette = "Set1")+
  #scale_y_continuous(name ="Amount of accidents")+
  #scale_x_discrete(name = "Day of week")

#Plot1
#Subplot1 <- Plot1 + facet_wrap(~accident_severity , scales = "free")
#Subplot1


##For the next plots I might experiment with heat maps and counter plots, else using standard bar plot...
## I am interested in accident severity
BARPLOT <- function(dataframe, var1, var2){
  dataframe %>%
    ggplot2::ggplot()+
      ggplot2::geom_bar(mapping = aes(x = {{var1}}, fill = {{var2}}), position = "dodge")+
        ggplot2::facet_wrap(vars({{var2}}), scales = "free")
}

# BARPLOT(accidents21, <var1>, <var2>)


PIECHART <- function(dataframe, var1, var2) {
  dataframe %>%
    dplyr::group_by({{var1}}, {{var2}}) %>%
      dplyr::summarise(cnt = n(), .groups = "drop_last") %>%
        dplyr::mutate(props1 = cnt/sum(cnt)) %>%
          ggplot2::ggplot( aes(x = {{var1}}, fill = {{var2 }})) +
           ggplot2::geom_bar( width = 1) +
            ggplot2::coord_polar(theta = "y") +
              ggplot2::xlab(NULL) +
               ggplot2::ylab(NULL)+
                  ggplot2::facet_wrap(vars({{var1}}))

}
# PIECHART(accidents21, <var1>, <var2>)

COUNTPLOT <- function(dataframe, var1, var2, var3){
 ggplot2::ggplot(dataframe, aes(x = {{var1}}, y = {{var2}}))+
  ggplot2::geom_count(aes(size = after_stat(prop), group = {{var2}})) +
    scale_size_area(max_size = 10)
}

# COUNTPLOT(accidents21, <var1>, <var2>, <var3>)

HEATMAPS <- function(dataframe, var1, var2){
  dataframe %>%
    count({{var1}}, {{var2}}) %>%
      ggplot2::ggplot(aes(x = {{var1}}, {{var2}}, fill = n))+
        geom_tile()
}
