#In this script there will be plots

##First plot will showcase the total amount of accidents in GB in the year 2021

#Furthermore, it also shows how high the amount of accident severity was registered:
##----Unfinished: needs some work on labels-------

Plot1 <- ggplot2::ggplot(data = accidents21)+
  ggplot2::geom_bar(mapping = aes(x = day_of_week, fill = accident_severity), position = "dodge")

