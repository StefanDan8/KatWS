source("dataset_factory.R")

plot <- jobs_colors_independent(1000, 22) %>%
              count(fav_color, job) %>%
              ggplot()+
              geom_tile(mapping = aes(x=fav_color, y = job, fill =n))
