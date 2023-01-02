#In this script there will be plots

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




Subplot1 <- Plot1 + facet_wrap(~accident_severity, scales = "free")

Subplot1



