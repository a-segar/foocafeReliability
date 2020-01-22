
modelStr <- list()
modelStr[["exponential"]] <- "
  data {
    int <lower=0> Nobs;
    real <lower=0> projection_hours[Nobs];
  }

  parameters {
    real <lower=0> lambda;

  }

  transformed parameters {
    real MTTF;

    MTTF = 1 / lambda;
  }

  model {
    projection_hours ~ exponential(lambda);

    lambda ~ gamma(2.5, 2350);
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
  "

library(shiny)
library(rstan)
library(Rcpp)


ui <- shiny::shinyUI(fluidPage(
  shiny::titlePanel("LCD Analysis"),

  shiny::mainPanel("",
                   shiny::tabsetPanel(type = "tabs",
                                      shiny::tabPanel("Select model",
                                                      shiny::tabsetPanel(type = "tabs",
                                                                         shiny::tabPanel("Model structure",
                                                                                         shinyWidgets::pickerInput("model", "",
                                                                                                                   choices = c("Exponential", "Lognormal", "Weibull", "Custom"),
                                                                                                                   selected = c("Exponential"),
                                                                                                                   options = list(`actions-box` = TRUE), multiple = FALSE),
                                                                                         shiny::verbatimTextOutput("model_text")
                                                                         ),
                                                                         shiny::tabPanel("Custom model",
                                                                                         textAreaInput("caption", "Caption", modelStr[["exponential"]], width = "1000px", height = "600px")
                                                                         )
                                                      )
                                      ),
                                      shiny::tabPanel("Review posterior distribution",
                                                      shiny::actionButton("sample_from_posterior", "Sample from posterior"),
                                                      shiny::textInput("params_to_plot", ""),
                                                      shiny::plotOutput("stanPlot")
                                      ),
                                      shiny::tabPanel("Check model fit",
                                                      shiny::textOutput("chi_squared_val"),
                                                      shiny::textOutput("BIC_val")
                                      )
                   )
  )
)
)



server <- shiny::shinyServer(function(input, output, session) {

  output$model_text <- shiny::renderText({

    modelStr[["custom"]] <- input$caption
    text <- modelStr[[tolower(input$model)]]
  })

  ####################
  ## Fit STAN model: ##
  ####################

  stanModel <- shiny::eventReactive(input$sample_from_posterior, {

    showModal(modalDialog("Building STAN model", footer=NULL))

    stan_model <- rstan::stan_model(model_code = modelStr[[tolower(input$model)]])

    removeModal()



    showModal(modalDialog("Sampling from posterior", footer = NULL))

    lcd_fail_list <- list(Nobs = NROW(lcd_projector_failures$projection_hours),
                          projection_hours = lcd_projector_failures$projection_hours
    )
    lcd_output <- rstan::sampling(stan_model, data = lcd_fail_list, chains = 4, iter = 4000, control = list(adapt_delta = 0.9))

    removeModal()


    return(lcd_output)

  })


  ###############
  ## Analysis: ##
  ###############

  ## Plots:
  #==========

  output$stanPlot <- shiny::renderPlot({

    model_output <- stanModel()

    if (input$params_to_plot == ""){
      stan_hist(model_output)
    } else {
      temp <- strsplit(sub(" ", "", input$params_to_plot), ",")
      stan_hist(model_output, temp[[1]])
    }

  })

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

}
)

shinyApp(ui, server)
