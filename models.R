library(peopleanalyticsdata)
library(brms)
library(bayesplot)
library(ggplot2)

# set brms options
options(
  rms.backend = "cmdstanr",
  brms.iter = 10000,
  brms.chains = 4,
  brms.seed = 123,
  brms.refresh = 0,
  brms.save_pars = save_pars('all')
)

## NAIVE BAYES MODEL

# run simple Bayesian logistic regression
naive_model <- readRDS("naive_model.RDS")
# naive_model <- brm(
#   formula = Hire ~ Test,
#   data = selection,
#   family = bernoulli()
# )

# view posterior for Test coefficient
mcmc_areas(
  as.matrix(naive_model),
  pars = "b_Test",
  prob = 0.95
) +
  theme_minimal()

# coefficient statistics
posterior_samples_naive <- as.data.frame(naive_model)
mean(posterior_samples_naive$b_Test > 0)
median(posterior_samples_naive$b_Test)
exp(median(posterior_samples_naive$b_Test))

## CAUSAL BAYES MODEL

# run simple Bayesian logistic regression
causal_model <- brm(
  formula = Hire ~ Test + GPA,
  data = selection,
  family = bernoulli()
)

# view posterior for Test coefficient
mcmc_areas(
  as.matrix(causal_model),
  pars = "b_Test",
  prob = 0.95
) +
  theme_minimal()

# coefficient statistics
posterior_samples_causal <- as.data.frame(causal_model)
mean(posterior_samples_causal$b_Test > 0)
median(posterior_samples_causal$b_Test)
exp(median(posterior_samples_causal$b_Test))
