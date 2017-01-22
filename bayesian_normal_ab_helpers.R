######## 
# HELPER FUNCTIONS
#  keeping these seperate to run the program
#------------------------------------------------
library(bayesAB)
library(stringi)
library(data.table)

###############################################
## generate fake data for prog
gen_fake_data <- function(obs, mu1, mu2){
  # generate fake data for program test
  
  DF <- data.table::data.table(
    ids = stringi::stri_rand_strings(n, 4)
    ,groups = stringi::stri_rand_strings(n, 1, pattern = "[A-D]")
    ,test = rnorm(n, mean=mu1)
    ,control = rnorm(n, mean=mu2)
  )
  
  return(DF)
}

###############################################
## preagg and then sum by columns
agg_by_keys <- function(DF, group_vars) {
  ## preaggregate and then sum by columns
  data.table::setkeyv(DF, group_vars)
  DF <- DF[DF, lapply(.SD, sum, na.rm=T), by= group_vars]
  
  return(as.data.frame(DF))
}

##########################################
# function to run ab_norm_test via BayesAB
norm_ab_test <- function(A_data = test_A
                         , B_data = test_B
                         , priors = priors_to_use
                         , n_samples = samples_for_run
                         , distribution = dist_to_use){
  
  ab_test <- bayesAB::bayesTest(A_data
                                , B_data
                                , priors
                                , n_samples
                                , distribution)
  
  ab_test_results <- summary(ab_test)
  
  results_to_df <- c(
    ab_test_results$probability$Mu[1]
    ,as.numeric(ab_test_results$interval$Mu[1])
    ,as.numeric(ab_test_results$interval$Mu[2])
    ,ab_test_results$posteriorExpectedLoss$Mu)
  
  return(results_to_df)
}

#####################################################
## Loop and run AB test on group
get_test_results <- function(df, group_vars){
  # run Bayesian Norm AB
  
  # set number of groups
  unique_groups <- unique(df[, group_vars[2]])
  num_of_groups <- length(unique_groups)
  
  
  results_list <- list()
  
  for(i in 1:num_of_groups){
    
    group_to_pull <- unique_groups[i]
    filter_for_group <- df[ group_vars[2]] == group_to_pull
    sub_df <- df[filter_for_group, ]
    test_output <- norm_ab_test(A_data = sub_df$test, B_data = sub_df$control)
    results_list[[i]] <- c(group_to_pull, test_output)
    
  }
  return(results_list)
}