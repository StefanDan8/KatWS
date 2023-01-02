raw <- read_csv("Dataset/dft-road-casualty-statistics-accident-2021.csv")
guide <- read_excel("Dataset/Road-Safety-Open-Dataset-Data-Guide.xlsx")
saveRDS(guide, "Dataset/guide.rds")
#Dataset is from here
#https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data
#Columns of raw:
#[1] "accident_index"                              "accident_year"
#[3] "accident_reference"                          "location_easting_osgr"
#[5] "location_northing_osgr"                      "longitude"
#[7] "latitude"                                    "police_force"
#[9] "accident_severity"                           "number_of_vehicles"
#[11] "number_of_casualties"                        "date"
#[13] "day_of_week"                                 "time"
#[15] "local_authority_district"                    "local_authority_ons_district"
#[17] "local_authority_highway"                     "first_road_class"
#[19] "first_road_number"                           "road_type"
#[21] "speed_limit"                                 "junction_detail"
#[23] "junction_control"                            "second_road_class"
#[25] "second_road_number"                          "pedestrian_crossing_human_control"
#[27] "pedestrian_crossing_physical_facilities"     "light_conditions"
#[29] "weather_conditions"                          "road_surface_conditions"
#[31] "special_conditions_at_site"                  "carriageway_hazards"
#[33] "urban_or_rural_area"                         "did_police_officer_attend_scene_of_accident"
#[35] "trunk_road_flag"                             "lsoa_of_accident_location"
good_columns <- raw %>% select(c(9, 10, 11, 13, 20, 21, 28, 29, 30, 31, 33))
saveRDS(good_columns, "Dataset/coded.rds")
accidents <- decode(good_columns)
save_as(accidents, ".rds")
save_as(accidents,".csv")
