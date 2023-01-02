raw <- readr::read_csv("data-raw/dft-road-casualty-statistics-accident-2021.csv")
guide <- readxl::read_excel("data-raw/Road-Safety-Open-Dataset-Data-Guide.xlsx")

good_columns <- raw %>% dplyr::select(c(9, 10, 11, 13, 20, 21, 28, 29, 30, 31, 33))

get_vector_guide <- function(column){
  named_vector <-  guide %>%
    dplyr::filter(.data$`field name` == column) %>%
    dplyr::select(c(3,4)) %>%
    tibble::deframe()
  return(named_vector)
}
decode <- function(dataframe){
  columns <- colnames(dataframe)[-c(2,3,6)];
  for(column in columns){
    dataframe <- decode_column(dataframe, column)
  }
  dataframe[sapply(dataframe, is.character)] <- lapply(dataframe[sapply(dataframe, is.character)],as.factor)
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

usethis::use_data(accidents21, overwrite = TRUE)
