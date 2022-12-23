#-------NUMERIC METHODS ------- #

#write code to catch exceptions!!

how_many <- function(dataframe, variable){
  ans <- dataframe %>% count({{variable}})
  return(ans)
}


how_many_distinct_per <- function(dataframe, var1, var2 ){
  ans <- dataframe %>% group_by({{var1}}) %>% summarise(n = n(), "n_{{var2}}" := n_distinct({{var2}}))
  return(ans)
}

uniques_per_var <- function(dataframe, var1, var2 ){
  n2 <- paste0("values_of_",deparse1(substitute(var2)));
  ans <- dataframe %>%
    group_by({{var1}}) %>%
    summarise(!!n2 := list(unique({{var2}})))%>%mutate(display = map_chr(get(n2), toString))
  return(ans)
}

contingencyTable <- function(dataframe, var1, var2){
  ans <- dataframe %>%
    group_by({{var1}}, {{var2}})%>%
    summarise(n = n(), .groups = "drop_last")%>%
    ungroup()%>%
    spread({{var2}}, n)

  return(ans)
}

contingencyTableScale <- function(dataframe, var1, var2){
  n2 <- deparse1(substitute(var1))
  ans <- dataframe %>%
    group_by({{var1}}, {{var2}})%>%
    summarise(n = n() ,.groups = "drop_last")%>%
    mutate(prop = n/sum(n))%>%
    select({{var1}}, {{var2}}, prop)%>%
    spread({{var2}}, prop) %>%
    ungroup() %>% add_column(n = how_many(dataframe, {{var1}})$n, .after = n2)

  return(ans)
}




