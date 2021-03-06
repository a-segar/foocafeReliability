

lcd_projector_failures <- tibble::tibble(
  lcd_model = c(1,1,1,1,1,2,2,2,1,2,1,3,3,2,2,3,
                3,2,1,2,1,3,1,2,1,2,1,1,2,2,2),
  projection_hours = c(387,182,244,600,627,332,418,300,798,584,660,39,274,174,50,34,
                       1895,158,974,345,1755,1752,473,81,954,1407,230,464,380,131,1205)
)


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


library(shiny)
library(rstan)
library(Rcpp)
library(ggplot2)
library(magrittr)

stan_models <- readRDS("stan_models.RDS")

modelStr <- list()
modelStr[["exponential"]] <- "
  data {
    int<lower=0> Nobs;
    real<lower=0> projection_hours[Nobs];
  }

  parameters {
    real<lower=0> lambda;

    real<lower=0> lambda_prior;
  }

  transformed parameters {
    real MTTF;

    MTTF = 1 / lambda;
  }

  model {
    projection_hours ~ exponential(lambda);

    lambda ~ gamma(2.5, 2350);

    lambda_prior ~ gamma(2.5, 2350);
  }

  generated quantities {
    //sample predicted values from the model for posterior predictive checks
    real y_rep[Nobs];

    for(n in 1:Nobs)
      y_rep[n] = exponential_rng(lambda);
  }
"

modelStr[["weibull"]] <- "
  data {
    int<lower=0> Nobs;
    vector<lower=0>[Nobs] projection_hours;
  }

  parameters {
    real<lower=0> shape;
    real<lower=0> scale;

  }

  model {
    target += weibull_lpdf(projection_hours | shape, scale);

    shape ~ gamma(1, 1);
    scale ~ gamma(2.5, 0.006);
  }

  generated quantities {
    //sample predicted values from the model for posterior predictive checks
    real y_rep[Nobs];

    for(n in 1:Nobs)
      y_rep[n] = weibull_rng(shape, scale);
  }
  "

modelStr[["lognormal"]] <- "
  data {
    int<lower=0> Nobs;
    vector<lower=0>[Nobs] projection_hours;
  }

  parameters {
    real mu;
    real sigma_2;
  }

  transformed parameters {
    real<lower=0> sigma;

    sigma = sqrt(sigma_2);
  }

  model {

    target += lognormal_lpdf(projection_hours | mu, sigma);

    mu ~ normal(6, 5);
    sigma_2 ~ inv_gamma(6.5, 23.5);
  }

  generated quantities {
    //sample predicted values from the model for posterior predictive checks
    real y_rep[Nobs];

    for(n in 1:Nobs)
      y_rep[n] = lognormal_rng(mu, sigma);
  }
  "

# stan_models <- list()
# stan_models[["exponential"]] <- rstan::stan_model(model_code = modelStr[["exponential"]])
# stan_models[["weibull"]] <- rstan::stan_model(model_code = modelStr[["weibull"]])
# stan_models[["lognormal"]] <- rstan::stan_model(model_code = modelStr[["lognormal"]])


ui <- shiny::shinyUI(fluidPage(
  shiny::titlePanel("LCD projector failure analysis"),

  shiny::mainPanel("",
                   shiny::tabsetPanel(type = "tabs",
                                      shiny::tabPanel("View data",
                                                      shiny::plotOutput("raw_data_histogram")),
                                      shiny::tabPanel("Select model",
                                                      shinyWidgets::pickerInput("model", "",
                                                                                choices = c("Exponential", "Lognormal", "Weibull", "Custom"),
                                                                                selected = c("Exponential"),
                                                                                options = list(`actions-box` = TRUE), multiple = FALSE),
                                                      shiny::verbatimTextOutput("model_text")
                                      ),
                                      shiny::tabPanel("Sample from posterior distribution",
                                                      shiny::actionButton("sample_from_posterior", "Sample from posterior"),
                                                      shiny::plotOutput("stanPlot"),
                                                      shiny::plotOutput("prior_comparison")
                                      ),
                                      shiny::tabPanel("Check model fit",
                                                      shiny::textOutput("chi_squared_val"),
                                                      shiny::textOutput("BIC_val"),
                                                      shiny::tabsetPanel(type = "tabs",
                                                                         shiny::tabPanel("Graphical posterior predictive check",
                                                                                         shiny::plotOutput("ppc_plot")),
                                                                         shiny::tabPanel("PPP test statistic",
                                                                                         shinyWidgets::pickerInput("ppp_test_statistic", "",
                                                                                                                   choices = c("mean", "max", "min", "custom"),
                                                                                                                   selected = c("mean"),
                                                                                                                   options = list(`actions-box` = TRUE), multiple = FALSE),
                                                                                         shiny::textInput("custom_ppp_test_statistic", "Custom PPP test statistic"),
                                                                                         shiny::textOutput("ppp_val"),
                                                                                         shiny::plotOutput("ppp_plot"))
                                                      )
                                      )
                   )
  )
)
)



server <- shiny::shinyServer(function(input, output, session) {


  ####################
  ## Plot raw data: ##
  ####################

  output$raw_data_histogram <- shiny::renderPlot({

    lcd_projector_failures %>%
      ggplot(aes(x = projection_hours)) +
      geom_histogram(fill = "turquoise", binwidth = 100) +
      labs(x = "Failure time", y = "Count")

  })

  #################
  ## STAN model: ##
  #################

  output$model_text <- shiny::renderText({

    modelStr[["custom"]] <- input$caption
    text <- modelStr[[tolower(input$model)]]
  })

  ## Select STAN model and sample from posterior:
  #----------------------------------------------

  stanModel <- shiny::eventReactive(input$sample_from_posterior, {

    if(tolower(input$model) == "custom"){
      showModal(modalDialog("Building STAN model", footer=NULL))

      stan_models[["custom"]] <- rstan::stan_model(model_code = input$caption)

      removeModal()
    }

    showModal(modalDialog("Sampling from posterior", footer = NULL))

    lcd_fail_list <- list(Nobs = NROW(lcd_projector_failures$projection_hours),
                          projection_hours = lcd_projector_failures$projection_hours
    )
    lcd_output <- rstan::sampling(stan_models[[tolower(input$model)]], data = lcd_fail_list, chains = 4, iter = 4000, control = list(adapt_delta = 0.9))

    removeModal()


    return(lcd_output)

  })

  ## Plots:
  #==========

  output$stanPlot <- shiny::renderPlot({

    model_output <- stanModel()

    params_to_plot <- names(model_output)[substr(names(model_output),1,5) != "y_rep" &
                                            substr(names(model_output),1,5) != "lp__" &
                                            substr(names(model_output),
                                                   nchar(names(model_output))-4,
                                                   nchar(names(model_output))) != "prior"]
    stan_hist(model_output, params_to_plot)

  })

  output$prior_comparison <- shiny::renderPlot({
    model_output <- stanModel()
    output_vals <- rstan::extract(model_output)

    if(tolower(input$model) == "exponential"){
      data <- data.frame(output_vals["lambda"],
                         output_vals["lambda_prior"])

      ggplot(data = data) +
        geom_density(aes(lambda)) +
        geom_density(aes(lambda_prior), linetype = "dashed")
    }

  })


  ##################
  ## Review model ##
  ##################

  output$chi_squared_val <- shiny::renderText({

    if (tolower(input$model) == "exponential"){
      params <- data.frame(lambda = rstan::extract(stanModel())["lambda"])
      colnames(params) <- "rate"

      t <- bayesian_chi_squared_test(y = lcd_projector_failures$projection_hours,
                                     distribution_fun = pexp,
                                     params = params,
                                     data_type = "continuous")


    } else if (tolower(input$model) == "lognormal"){
      params <- data.frame("meanlog" = rstan::extract(stanModel())["mu"],
                           "sdlog" = rstan::extract(stanModel())["sigma"])
      colnames(params) <- c("meanlog","sdlog")

      t <- bayesian_chi_squared_test(y = lcd_projector_failures$projection_hours,
                                     distribution_fun = plnorm,
                                     params = params,
                                     data_type = "continuous")


    } else if (tolower(input$model) == "weibull"){
      params <- data.frame("shape" = rstan::extract(stanModel())["shape"],
                           "scale" = rstan::extract(stanModel())["scale"])
      colnames(params) <- c("shape", "scale")

      t <- bayesian_chi_squared_test(y = lcd_projector_failures$projection_hours,
                                     distribution_fun = pweibull,
                                     params = params,
                                     data_type = "continuous")


    } else if (tolower(input$model) == "custom"){

      t <- "not available for custom models"


    }

    paste("Bayesian Chi-squared value is", t)

  })

  output$BIC_val <- shiny::renderText({

    if (tolower(input$model) == "exponential"){
      expo <- rstan::extract(stanModel())
      lambda <- mean(expo$lambda)
      BIC <- -2*log(prod(dexp(lcd_projector_failures$projection_hours, lambda))) + log(NROW(lcd_projector_failures$projection_hours))*1

    } else if (tolower(input$model) == "lognormal"){
      logn <- rstan::extract(stanModel())
      mu <- mean(logn$mu)
      sigma <- mean(logn$sigma)
      BIC <- -2*log(prod(dlnorm(lcd_projector_failures$projection_hours, mu, sigma))) + log(NROW(lcd_projector_failures$projection_hours))*2

    } else if (tolower(input$model) == "weibull"){
      weib <- rstan::extract(stanModel())
      shape <- mean(weib$shape)
      scale <- mean(weib$scale)
      BIC <- -2*log(prod(dweibull(lcd_projector_failures$projection_hours, shape, scale))) + log(NROW(lcd_projector_failures$projection_hours))*2

    } else if (tolower(input$model) == "custom"){
      # If using custom fit then evaluate using posterior predictive check
      BIC <- "not available for custom models"
    }

    paste("BIC is", BIC)

  })

  output$ppc_plot <- shiny::renderPlot({
    model_output <- stanModel()
    x <- list(y = lcd_projector_failures$projection_hours,
              yrep = rstan::extract(model_output, "y_rep")$y_rep)
    bayesplot::ppc_dens_overlay(x[["y"]], x[["yrep"]][1:100,, drop = FALSE])

  })

  output$ppp_val <- shiny::renderText({

    model_output <- stanModel()
    y <- lcd_projector_failures$projection_hours
    output_vals <- rstan::extract(model_output)

    if (input$ppp_test_statistic == "custom"){
      test_stat <- input$custom_ppp_test_statistic
    } else {
      test_stat <- input$ppp_test_statistic
    }

    T_y <- eval(str2expression(paste(test_stat, "(y)")))
    T_yrep <- apply(output_vals$y_rep, 1, test_stat)
    sum(T_yrep > T_y)/NROW(output_vals$y_rep)

  })

  output$ppp_plot <- renderPlot({
    model_output <- stanModel()
    y <- lcd_projector_failures$projection_hours
    output_vals <- rstan::extract(model_output)

    if (input$ppp_test_statistic == "custom"){
      test_stat <- input$custom_ppp_test_statistic
    } else {
      test_stat <- input$ppp_test_statistic
    }

    T_y <- eval(str2expression(paste(test_stat, "(y)")))
    T_yrep <- apply(output_vals$y_rep, 1, test_stat)

    ggplot() +
      aes(T_yrep) +
      geom_histogram() +
      geom_vline(xintercept = T_y)

  })

}
)

shinyApp(ui, server)
