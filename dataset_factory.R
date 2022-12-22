#This is a file that contains several functions that create custom synthetic datasets


jobs_colors_independent <- function(nrows=1e3, seed = 42){
  set.seed(seed);
  ds <- tibble(
    id = 1: nrows,
    fav_color = ceiling(runif(nrows,min = 0, max = 5)),
    job = rbinom(nrows, size = 10, prob = 0.6),
    gender = factor(sample(x = c("Male", "Female"),size = nrows, replace = TRUE, prob = c(0.5, 0.5)))
  )%>%mutate(fav_color = recode_factor(fav_color, 'Blue', 'Red', 'Green', 'Yellow', 'Black'))%>%
    mutate(job = recode_factor(job, '0'='Jobless', '1'='Mathematician', '2'='Doctor', '3'='Lawyer',
                             '4'='Plumber','5'= 'Fireman', '6'='Bus driver','7'='Accountant',
                             '8'='Teacher', '9'='Shopkeeper', '10'='IT'));

  return(ds);
}


#yet only for 2 variables.. to be extended...
#Returns a tibble with nrows, distributed after a 2D multivariate
#Normal distribution with mean and cov_matrix
multivariateNormal <- function(nrows = 1e3,mean, cov_matrix, seed = 42){
  set.seed(seed);
  df <-  tibble(var_1 = factor(), var_2 = factor());
  sample <- MASS::mvrnorm(n = nrows, mu = mean, Sigma = cov_matrix)
  df <- tibble(var_1 =sample[,1], var_2 = sample[,2]);
  return(df)
}

# This turns a continuous, multivariate Normal distribution into a discrete
# distribution, the values being arranged in equal bins between -3*var + mean
#and mean + 3*var. What's below/ above represents one single bin
#The maximal number of bins is 26 as the variables are turned into Letters.
multivariateNormalDiscrete <- function(nrows = 1e3, mean, cov_matrix, seed = 42, vector_bins){
  df <- multivariateNormal(nrows, mean, cov_matrix, seed)
      df$var_1 = discretise(mean[1], cov_matrix[1,1], vector_bins[1], df$var_1);
      df$var_2 = discretise(mean[2], cov_matrix[2,2], vector_bins[2], df$var_2);
  return(df);
}
#this one does not work mutate somehow passes the entire column
#Too tired to debug today. The function above makes the same and it works.
multi <- function(nrows = 1e3, mean, cov_matrix, seed = 42, vector_bins){
  lower_bound <- mean - 3*sqrt(diag(cov_matrix));
  upper_bound <- mean + 3*sqrt(diag(cov_matrix));
  df <- multivariateNormal(nrows, mean, cov_matrix, seed)%>%
    mutate(var_1 = disc(lower_bound[1],upper_bound[1], vector_bins[1], var_1))
  return(df)
}


discretise <- function(mean, variance, nbins, vec){
  lower_bound <- mean - 3*sqrt(variance);
  upper_bound <- mean + 3*sqrt(variance);
  ans <- sapply(vec, disc, lower_bound = lower_bound, upper_bound= upper_bound, nbins = nbins);
  return(ans);
}


disc <- function(lower_bound, upper_bound, nbins, value){
  if(value<=lower_bound){
    return("A");
  }
  if(value>=upper_bound){
    return(LETTERS[nbins]);
  }
  thr <-  (upper_bound - lower_bound)/(nbins-2);
  return(LETTERS[ceiling((value-lower_bound)/thr)+1]);
}

saveAs <- function(save_as){
  switch(saveAs, ".csv" = write_csv(ds, "Synthetic Dataframes/jobs_and_colors.csv"),
         ".rds" =saveRDS(ds,"Synthetic Dataframes/jobs_and_colors.rds"),
         ".tsv" = write_tsv(ds,"Synthetic Dataframes/jobs_and_colors.tsv")
  );
}
