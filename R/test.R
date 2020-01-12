

# three types
#   - continuous
#   - discrete
#   - censored

# Assume the user knows what they're doing

# Also need data (with optional censored indicator column)
# and need to specify the function that will be used for the distribution

# Note that names of columns in the parameter dataframe should match those used in the function specified.

get_bayesian_chi_squared(data, distribution_fun, params, test_type, censored = NULL, chisq_quantile = 0.95){

  n <- NROW(y)
  K <- round(n^0.4)
  a <- seq(0,1,1/K)
  p <- diff(a)
  m <- numeric(K)

  chi_val <- qchisq(chisq_quantile,K-1)

  # Create a function that can create a distribution function of the form required:
  dist_fun_generator <- function(fun){
    output_fun <- function(x, params){

      string <- paste("fun( x, ",
                      paste(paste(colnames(params), "=", params), collapse = ", "),
                      ")")
      return(eval(str2expression(string)))

      #fun(x, params)
    }
    return(output_fun)
  }
  this_dist_fun <- dist_fun_generator(distribution_fun)

  # Convert the dataframe of parameters to a list so we can apply the distribution function
  params_list <- split(params, seq(NROW(params)))


  if (test_type == "continuous"){
    probabilities <- lapply(params_list, function(x){

      paste(colnames(x), "=", x)

      string <- paste("this_dist_fun( data, ", paste(paste(colnames(thisParams), "=", thisParams), collapse = ", "), ")")
      eval(str2expression(string))

      this_dist_fun(data, x )}

      )
  }


  if (test_type == "discrete"){
    probabilities <- lapply(params_list, function(x){
      temp_df <- data.frame(probabilities_minus_1 = this_dist_fun(data - 1, x),
                            probabilities = this_dist_fun(data, x))
      probs <- temp_df %>% dplyr::mutate(g = apply(temp_df, 1, function(x) {runif(1, min = x["probabilities_minus_1"], max = x["probabilities"])} ))

      return(probs$g)
      })
  }


  if (test_type == "censored"){

    if (censored = NULL){
      stop("If applying censored method a vector indicating which data points are censored is required")
    }
    if (NROW(data) != NROW(censored)){
      stop("Length of censored vector be the same length as the data")
    }

    probabilities <- lapply(params_list, function(x){
      probs <- data.frame(
        probabilities = this_dist_fun(data, x)
      )

      probs <- probs %>%
        dplyr::mutate(g = apply(probs, 1, function(x) {runif(1, min = x[1], max = 1)} )) %>%
        dplyr::mutate(censored = censored) %>%
        dplyr::mutate(test = ifelse(censored == TRUE, g, probabilities))

      return(probs$test)
    })
  }

  m_list <- lapply(probabilities, function(x){
    m <- numeric(K)

    for (ii in 1:K){
      m[ii] <- sum(x > a[ii] & x < a[ii+1])
    }

    return(m)
  })

  rb_list <- lapply(m_list, function(x) { sum(((x - n*p)^2)/(n*p)) })

  r_b_new <- unlist(rb_list)

  return(r_b)

}

sum(r_b > chi_val)/NROW(r_b) * 100
sum(r_b_new > chi_val)/NROW(r_b) * 100

