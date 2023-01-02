guide <- readRDS("Dataset/guide.rds")
get_vector_guide <- function(column){
  named_vector <-  guide%>%filter(.data$`field name` == column)%>%select(c(3,4)) %>%deframe()
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
  dataframe <- dataframe%>%mutate_at(column, get_vector)
  return(dataframe)
}
