#This is a file that contains several functions that create custom synthetic datasets


jobs_colors_independent <- function(nrows=1e3, seed = 42, saveAs="don't save"){
  set.seed(seed)
  ds <- tibble(
    id = 1: nrows,
    fav_color = ceiling(runif(nrows,min = 0, max = 5)),
    job = rbinom(nrows, size = 10, prob = 0.6),
    gender = factor(sample(x = c("Male", "Female"),size = nrows, replace = TRUE, prob = c(0.5, 0.5)))
  )%>%mutate(fav_color = recode_factor(fav_color, 'Blue', 'Red', 'Green', 'Yellow', 'Black'))%>%
    mutate(job = recode_factor(job, '0'='Jobless', '1'='Mathematician', '2'='Doctor', '3'='Lawyer',
                             '4'='Plumber','5'= 'Fireman', '6'='Bus driver','7'='Accountant',
                             '8'='Teacher', '9'='Shopkeeper', '10'='IT'));
  switch(saveAs, ".csv" = write_csv(ds, "Synthetic Dataframes/jobs_and_colors.csv"),
         ".rds" =saveRDS(ds,"Synthetic Dataframes/jobs_and_colors.rds"),
         ".tsv" = write_tsv(ds,"Synthetic Dataframes/jobs_and_colors.tsv")
      );

  return(ds);
}
