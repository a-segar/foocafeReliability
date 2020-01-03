



get_bayesian_chi_squared <- function(x) {

  # INCOMPLETE
  ############################

  ## Arguments needed:
  #   - Raw data
  #   - Results from Bayesian sampling
  #   - Distribution e.g. weibull, poisson...
  #   - Parameter values for the specified dist

  ## To make it general, you need to specify the distribution, then include
  ## a vector that contains all the parameters used in that dist. Then you call
  ## that dist and include the data and params as args.

  # Raw data
  n <- NROW(lcd_projector_failures)

  # Bayesian results
  output_vals <- extract(output)


  #############################


  K <- round(n^0.4)
  a <- seq(0,1,1/K)
  p <- diff(a)
  m <- numeric(K)

  chi_val <- qchisq(0.95,K-1)
  r_b <- numeric(NROW(output_vals$lambda))

  for (jj in 1:NROW(r_b)) {

    thisLambda <- output_vals$lambda[jj]

    probabilities <- pexp(lcd_projector_failures$projection_hours, thisLambda)

    for (ii in 1:length(m)){
      m[ii] <- sum(probabilities > a[ii] & probabilities < a[ii+1])
    }

    r_b[jj] <- sum(((m - n*p)^2)/(n*p))

  }

  sum(r_b > chi_val)/NROW(r_b) * 100



}
