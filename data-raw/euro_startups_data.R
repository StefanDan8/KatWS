startup <- read_csv("data-raw/startup.csv");
startup <- dplyr::select(startup, c("category_list", "status", "country_code"));
euro_startups <- dplyr::filter(startup, !status == "ipo" &
   country_code %in% c("AUT", "BEL", "CHE", "DEU", "FRA", "GBR", "IRL", "SWE") &
   !is.na(category_list));
euro_startups[,2:3] = lapply(euro_startups[, 2:3], as.factor);
#software <- euro_startups%>%filter(stringr::str_detect(category_list, "Software"))
usethis::use_data(euro_startups, overwrite = TRUE)
