#
# ggplot2::ggplot(accidents21)+geom_bar(aes(x=accident_severity));
# sample <- infer::rep_sample_n(accidents21, size = 100);
# sample_r <- infer::rep_sample_n(accidents21, size = 100, reps = 1000);
# report <- sample_r %>% summarise(prop = sum(accident_severity == "Fatal")/100);
# ggplot2::ggplot(report, aes(x=prop)) +geom_histogram(binwidth = 0.01);
# mean(report$prop); #0.01454
# #0.01455629
# #for day_of_week and accident_severity
# set.seed(42)
# accidents_non_fatal <- accidents21 %>% filter(!accident_severity == "Fatal");
# #How does one find a good size?
# #for exaple, for size=1000, the test is not conlcudent at all
# #for size = 10000, the test is
# sample_r <- infer::rep_sample_n(accidents_non_fatal, size = 2000, reps = 1000);
# report <- sample_r %>% summarize(p_value = helper(accident_severity, urban_or_rural_area));
# report %>%
#   ggplot(aes(x = p_value)) +
#   geom_histogram(binwidth = 0.01, color = "white")
# mean_p_value <- mean(report$p_value)
#
# boot_ki <- report %>%
#   infer::get_confidence_interval(level = 0.95, type = "percentile")
# report %>% ggplot(aes(x = seq_along(p_value), y = sort(p_value))) +
#        geom_bar(stat = "identity") +
#       labs(x = "Durchlauf", y = "p-Wert", title = "Simulationsbasierte p-Werte")

#---------------------ONE VARIABLE FIXED, THE OTHER SAMPLED -------------------
# works poorly
test_no_bootstrap <- function(dataframe, var_fixed, var_sampled, sample_size, reps){
  set.seed(42);
  fixed <- dataframe %>%
    dplyr::select({{var_fixed}}) %>%
    infer::rep_sample_n(size = sample_size)%>%
    dplyr::pull({{var_fixed}});
  sample_r <- dataframe %>%
    dplyr::select({{var_sampled}}) %>%
    infer::rep_sample_n(size = sample_size, reps = reps);
  p_values <- sample_r %>% dplyr::summarise(p_value = get_p(fixed,{{var_sampled}}))

  return(p_values);
}
test_bootstrap <- function(dataframe, var_fixed, var_sampled, sample_size, reps){
  set.seed(42);
  sample <- dataframe %>%
    infer::rep_sample_n(size = sample_size);
  fixed <- sample %>%
    dplyr::pull({{var_fixed}});
  sample_r <- sample %>%
    dplyr::select({{var_sampled}})%>%
    infer::rep_sample_n(size = sample_size, replace = TRUE, reps = reps);
  p_values <- sample_r %>% dplyr::summarise(p_value = get_p(fixed,{{var_sampled}}))

  return(p_values);
}



#---------------DOUBLE SAMPLE ----------------------------------------------
calculate_p_values <- function(sample, var1, var2){
  report <- sample %>% dplyr::summarise(p_value = get_p({{var1}}, {{var2}}));
}

#Quite a bad approach tbh..
p_value_estimator_no_bootstrap <- function(dataframe, var1, var2, sample_size, reps){
  set.seed(42);
  sample <- infer::rep_sample_n(dataframe, size = sample_size, reps = reps);
  p_values <- calculate_p_values(sample, {{var1}}, {{var2}});
  #return(mean(p_values$p_value));
  plot(p_values%>%ggplot(aes(x = p_value)) +
       geom_histogram(binwidth = 0.01, color = "white"));
  return(p_values);
}

p_value_estimator_bootstrap <- function(dataframe, var1, var2, initial_sample_size, bootstrap_sample_size, reps){
 # set.seed(42);
  sample <- infer::rep_sample_n(dataframe, size = initial_sample_size);
  bootstrapped <- infer::rep_sample_n(sample, size = bootstrap_sample_size, replace = TRUE, reps = reps);
  p_values <- calculate_p_values(bootstrapped, {{var1}}, {{var2}});
  plot(p_values%>%ggplot(aes(x = p_value)) +
         geom_histogram(binwidth = 0.01, color = "white"));
  return(mean(p_values$p_value));
  #return(p_values);
}
# accidents_non_fatal <- accidents21 %>% filter(!(accident_severity == "Fatal"));
# accidents_non_fatal_mon_fri <- accidents_non_fatal %>% dplyr::filter(!(day_of_week %in%c("Saturday", "Sunday")))
# INDEPENDENT
# a4 <- p_value_estimator_bootstrap(accidents_non_fatal_mon_fri, accident_severity, day_of_week, 2000, 2000, 1000)
# DEPENDENT
# a4 <- p_value_estimator_bootstrap(accidents_non_fatal, accident_severity, day_of_week, 2000, 2000, 1000)
# INDEPENDENT
# b4 <- p_value_estimator_no_bootstrap(accidents_non_fatal_mon_fri, accident_severity, day_of_week, 2000,  1000)
# DEPENDENT
# b4 <- p_value_estimator_no_bootstrap(accidents_non_fatal, accident_severity, day_of_week, 2000,  1000)
# One can see a difference here.

get_p <- function(x,y){
  ans <- chisq.test(x,y);
  return(ans$p.value)
}

#function takes a contingency table as argument
# turns it into a matrix for simpler calculation
# obs are the observed values (numeric columns of table as matrix)
# t represents the matrix of chisq values
# sum(t) is then the X-squared value and the answer
chi2test <- function(df, return_only_p = TRUE){
  obs <- unname(data.matrix(select_if(df, is.numeric)));
  df <- as.integer(prod(dim(obs)-1));
  x <- rowSums(obs);
  y <- colSums(obs);
  total <- sum(x);
  expected <- (x%o%y)/total;
  t <- (obs-expected)^2 / expected;
  x <- sum(t);
  p <- pchisq(x, df = df, lower.tail=FALSE);
  x <- prettyNum(x, scientific = FALSE, digits = 6);
  p <- as.numeric(prettyNum(p, scientific = TRUE, digits = 4));
  answer <- c("X-squared" = x, "df" = df, "p-value" =p);
  if(return_only_p){
    return(p);
  }
  return(answer);
}
