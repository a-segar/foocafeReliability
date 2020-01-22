

# three types
#   - continuous
#   - discrete
#   - censored

# Assume the user knows what they're doing

# Also need data (with optional censored indicator column)
# and need to specify the function that will be used for the distribution

# Note that names of columns in the parameter dataframe should match those used in the function specified.

#' Bayesian Chi-squared test
#'
#' @param y Independent and identically distributed samples drawn from a given distribution
#' @param distribution_fun The cumulative distribution function of the model posterior.
#' @param params Randomly sampled draws of from the parameters posterior distribution.
#' @param data_type There are three choices of data_type; "continuous", "discrete", and "censored".
#' @param censored A vector the same length as y indicating whether the data is censored.
#' @param chisq_quantile Quantile of the Chi-squared distribution to be used in the test.
#'
#' @return The proportion of R^b values that exceed the specified critical value from
#'  their known Chi-squared reference distribution.
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom Rcpp cpp_object_initializer
#'

bayesian_chi_squared_test <- function(y, distribution_fun, params, data_type = "continuous", censored = NULL, chisq_quantile = 0.95){

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


  if (data_type == "continuous"){
    probabilities <- lapply(params_list, function(x){

      this_dist_fun(y, x )}

      )
  }


  if (data_type == "discrete"){
    probabilities <- lapply(params_list, function(x){
      temp_df <- data.frame(probabilities_minus_1 = this_dist_fun(y - 1, x),
                            probabilities = this_dist_fun(y, x))
      probs <- temp_df %>% dplyr::mutate(g = apply(temp_df, 1, function(x) {runif(1, min = x["probabilities_minus_1"], max = x["probabilities"])} ))

      return(probs$g)
      })
  }


  if (data_type == "censored"){

    if (is.null(censored)){
      stop("If applying censored method a vector indicating which data points are censored is required")
    }
    if (NROW(y) != NROW(censored)){
      stop("Length of censored vector be the same length as the data")
    }

    probabilities <- lapply(params_list, function(x){
      probs <- data.frame(
        probabilities = this_dist_fun(y, x)
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

  r_b <- unlist(rb_list)

  output <- sum(r_b > chi_val)/NROW(r_b) * 100
  return(output)

}
