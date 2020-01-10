
## supercomputer_failures_model

modelString <- "
  data {
    int <lower=0> Nobs;
    int <lower=0> count[Nobs];
  }

  parameters {
    real <lower=0> lambda;
  }

  model {
    count ~ poisson(lambda);

    lambda ~ gamma(5, 1);
  }

  generated quantities {
    //sample predicted values from the model for posterior predictive checks
    real y_rep[Nobs];

    for(n in 1:Nobs)
      y_rep[n] = poisson_rng(lambda);
  }
"
supercomputer_failures_model <- rstan::stan_model(model_code = modelString)

usethis::use_data(supercomputer_failures_model, overwrite = TRUE)


##
