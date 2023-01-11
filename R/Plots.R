#In this script there will be plots

library("tidyverse")

load("~/katws/data/accidents21.rda")
accidents21$day_of_week <- factor(
  accidents21$day_of_week,
  levels = c("Monday","Tuesday" , "Wednesday", "Thursday", "Friday" , "Saturday", "Sunday")
)





##First plot will showcase the total amount of accidents in GB in the year 2021

#Furthermore, it also shows how high the amount of accident severity was registered:

##----Unfinished: needs some work on labels-------

Plot1 <- ggplot2::ggplot(data = accidents21)+
  ggplot2::geom_bar(mapping = aes(x = day_of_week, fill = accident_severity), position = "dodge") +
  ggplot2::labs(title = "Total amount of traffic accidents in 2021 (UK)",
                subtitle = "In progress...",
                caption = "published by the government of the United Kingdom")+
  scale_fill_brewer(name = "Accident Severity", palette = "Set1")+
  scale_y_continuous(name ="Amount of accidents")+
  scale_x_discrete(name = "Day of week")

Plot1

Subplot1 <- Plot1 + facet_wrap(~accident_severity , scales = "free")

Subplot1

##For the next plots I might experiment with heat maps and counter plots, else using standard bar plot...
## I am interested in accident severity
 Plot2 <- ggplot2::ggplot(data = accidents21)+
   ggplot2::geom_boxplot(mapping = aes(x = day_of_week, y = number_of_vehicles))+
   ggplot2::labs(title = "",
                 subtitle ="",
                 caption = "published by the government of the United Kingdom")
   ##scale_color_viridis(name = "Number of Vehicles", option = "viridis")

Plot2


Subplot2 <- Plot2 + facet_wrap(~number_of_casualties, scales = "free_x")

Subplot2


Plot3 <- ggplot2::ggplot((data = count(accidents21,accident_severity, number_of_vehicles )))+
  ggplot2::geom_tile(mapping = aes(x = accident_severity, y = number_of_vehicles, fill = n))+

scale_fill_viridis(name = "Amount", option = "rainbow")
Plot3

Plot3 <- ggplot2::ggplot((data = count(accidents21,light_conditions, number_of_casualties )))+
  ggplot2::geom_tile(mapping = aes(x = light_conditions, y = number_of_casualties, fill = n))


Plot3


