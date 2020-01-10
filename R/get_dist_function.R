#' get_dist_fun
#'
#' @param dist_type The type of distribution. Must be one of the following:
#'   Poisson, exponential, Weibull, lognormal, or gamma.
#' @param thisParams The parameters required for the specified distribution.
#'
#' @return A distribution function corresponding to the specified type and
#'   parameters.
#' @export
#'

get_dist_fun <- function(dist_type, thisParams){

  # Could use R 'family' feature

  if (tolower(dist_type) == "poisson"){

    if ("lambda" %in% colnames(thisParams)){
      stop("Parameters dataframe for Poisson distribution
           must include column named 'lambda' (see ?Poisson).")
    }

    output_fun <- function(x) {ppois(x, thisParams$lambda)}
  }

  else if (tolower(dist_type) == "exponential") {

    if ("rate" %in% colnames(thisParams)){
      stop("Parameters dataframe for exponential distribution
           must include column named 'rate' (see ?Exponential).")
    }

    output_fun <- function(x) {pexp(x, thisParams$rate)}
  }

  else if (tolower(dist_type) == "weibull") {

    if (all(c("shape", "scale") %in% colnames(thisParams))){
      stop("Parameters dataframe for Weibull distribution
           must include columns named 'shape' and 'scale' (see ?Weibull).")
    }

    output_fun <- function(x) {pweibull(x, thisParams$shape, thisParams$scale)}
  }

  else if (tolower(dist_type) == "lognormal") {

    if (all(c("meanlog", "sdlog") %in% colnames(thisParams))){
      stop("Parameters dataframe for lognormal distribution
           must include columns named 'meanlog' and 'sdlog' (see ?Lognormal).")
    }

    output_fun <- function(x) {plnorm(x, thisParams$meanlog, thisParams$sdlog)}
  }

  else if (tolower(dist_type) == "gamma") {

    if (all(c("shape", "rate") %in% colnames(thisParams))){
      stop("Parameters dataframe for gamma distribution
           must include columns named 'shape' and 'rate' (see ?GammaDist).")
    }

    output_fun <- function(x) {pgamma(x, thisParams$shape, thisParams$rate)}
  }

  else {
    stop("Distribution must be one of those specified in the function help page")
  }

  return(output_fun)
}
