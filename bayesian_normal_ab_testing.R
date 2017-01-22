set.seed(123)
source("bayesian_normal_ab_helpers.R")

# set grouping vars - 2nd is test grouping
group_vars <- c("ids","groups")
# set alg params
priors_to_use <- c('m0' = 5, 'k0' = 1, 's_sq0' = 3, 'v0' = 1)
samples_for_run <- 1e+06
dist_to_use <- "normal"

#------------------------------------------------
# cooking up test data
# will need to pregroup by group and id and the preaggregate
n <- 2000000
df <- gen_fake_data(obs=n, mu1=45.5, mu2=43.5)
df <- agg_by_keys(df, group_vars)

### run ab testing on segments
#------------------------------------------------
ab_results <- get_test_results(df, group_vars)

ab_df <- as.data.frame(do.call("rbind", ab_results))
colnames(ab_df) <- c("segment", "probablity","lower_bound","upper_bound", "expected_loss")
ab_df

