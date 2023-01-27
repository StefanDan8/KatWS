raw <- readr::read_csv("data-raw/dft-road-casualty-statistics-accident-2021.csv")
guide <- readxl::read_excel("data-raw/Road-Safety-Open-Dataset-Data-Guide.xlsx")

good_columns <- raw %>% dplyr::select(c(9, 13, 20, 21, 28, 29, 33))

get_vector_guide <- function(column){
  named_vector <-  guide %>%
    dplyr::filter(.data$`field name` == column) %>%
    dplyr::select(c(3,4)) %>%
    tibble::deframe()
  return(named_vector)
}
decode <- function(dataframe){
  columns <- colnames(dataframe)[-4];
  for(column in columns){
    dataframe <- decode_column(dataframe, column)
  }
  return(dataframe)
}

decode_column <- function(dataframe, column){
  get_vector <- function(x){
    return(get_vector_guide(column)[as.character(x)])
  }
  dataframe <- dataframe %>%
    dplyr::mutate_at(column, get_vector)
  return(dataframe)
}

accidents21 <- decode(good_columns)
accidents21[,1:ncol(accidents21)] <- lapply(accidents21[,1:ncol(accidents21)],as.factor);
accidents21 <- accidents21 %>%
  dplyr::filter(!light_conditions=="Data missing or out of range" &
                  !weather_conditions == "Data missing or out of range" &
                  ! urban_or_rural_area == "Unallocated")
usethis::use_data(accidents21, overwrite = TRUE)
