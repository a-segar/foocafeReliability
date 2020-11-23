
modelStr <- list()
modelStr[["weibull"]] <- "
data {
int<lower=0> Nobs;
}

parameters {
real<lower=0> shape;
}

model {
shape ~ gamma(1, 1);
}

"

stan_models <- list()
stan_models[["weibull"]] <- rstan::stan_model(model_code = modelStr[["weibull"]])

# failures = strex::str_extract_numbers('102 246 125 247 169 305 138 262 179 182 221 267 227 174 247  94 101 197 203 108 122 136 199 290 305 155 110 171 230 151 213 264 185 109  91  95 254 175')[[1]]
# suspensions = strex::str_extract_numbers('138 228 187 207 105   5  22  33 109   2  17 270  27  36 194  17 100  34 180 101  36  22 105  47')[[1]]
# # rate <- 0.006
# fail_susp_list <- list(Nobs = NROW(failures),
#                        yobs = failures,
#                        Ncen = NROW(suspensions),
#                        ycen = suspensions,
#                        rate = rate)
# sampling_output <- rstan::sampling(stan_models[[tolower("weibull")]], data = fail_susp_list, chains = 10, iter = 4000, control = list(adapt_delta = 0.9))
# output_vals <- rstan::extract(sampling_output)



library(shiny)
library(rstan)
library(Rcpp)
library(ggplot2)
library(magrittr)

stan_model = stan_models[["weibull"]]
print(stan_model@dso)

ui <- shiny::shinyUI(fluidPage(
  shiny::titlePanel("Bayesian survival analysis"),

  shiny::mainPanel("",
                   shiny::tabsetPanel(type = "tabs",
                                      shiny::tabPanel("Input data",
                                                      shiny::numericInput("rate", "Gamma prior rate", "0.006"),
                                                      shiny::textAreaInput("failures_data", "Failures", '102 246 125 247 169 305 138 262 179 182 221 267 227 174 247  94 101 197 203 108 122 136 199 290 305 155 110 171 230 151 213 264 185 109  91  95 254 175', width = "500px", height = "100px"),
                                                      shiny::textAreaInput("suspensions_data", "Suspensions", '138 228 187 207 105   5  22  33 109   2  17 270  27  36 194  17 100  34 180 101  36  22 105  47', width = "500px", height = "100px")),
                                      shiny::tabPanel("View input data",
                                                      shiny::plotOutput("input_data_disp")),
                                      shiny::tabPanel("Select model",
                                                      shiny::tabsetPanel(type = "tabs",
                                                                         shiny::tabPanel("Model structure",
                                                                                         shinyWidgets::pickerInput("model", "",
                                                                                                                   choices = c("Weibull", "Exponential", "Lognormal", "Custom"),
                                                                                                                   selected = c("Weibull"),
                                                                                                                   options = list(`actions-box` = TRUE), multiple = FALSE),
                                                                                         shiny::verbatimTextOutput("model_text")
                                                                                         ),
                                                                         shiny::tabPanel("Custom model",
                                                                                         textAreaInput("caption", "Caption", modelStr[["exponential"]], width = "1000px", height = "600px")
                                                                                         )
                                                                      )
                                                      ),
                                      shiny::tabPanel("Sample from posterior distribution",
                                                      shiny::actionButton("sample_from_posterior", "Sample from posterior"),
                                                      shiny::plotOutput("stanPlot"),
                                                      shiny::plotOutput("prior_comparison")
                                                      ),
                                      shiny::tabPanel("View posterior distibution",
                                                      shiny::plotOutput("posterior")
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

  output$input_data_disp <- shiny::renderPlot({

    failures <- strex::str_extract_numbers(input$failures_data)[[1]]
    suspensions <- strex::str_extract_numbers(input$suspensions_data)[[1]]

    data <- tibble::tibble(time_since_event = c(failures, suspensions),
                           failure_suspension = c(rep("F", length(failures)), rep("S", length(suspensions)))
                           )

    data <- as.data.frame(data)

    data %>%
      ggplot(aes(x = time_since_event, fill=failure_suspension)) +
      geom_histogram(binwidth = 10) +
      facet_grid(failure_suspension~.) +
      labs(x = "Failure time", y = "Count") +
      scale_fill_manual(labels=c("F", "S"), values=c("green","brown"))

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


    failures <- strex::str_extract_numbers(input$failures_data)[[1]]
    suspensions <- strex::str_extract_numbers(input$suspensions_data)[[1]]
    rate <- input$rate

    fail_susp_list <- list(Nobs = NROW(failures),
                           yobs = failures,
                           rate = rate)

    sampling_output <- rstan::sampling(stan_models[[tolower(input$model)]], data = fail_susp_list, chains = 10, iter = 4000, control = list(adapt_delta = 0.9))

    removeModal()


    return(sampling_output)

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

    if(tolower(input$model) == "weibull"){
      data <- data.frame(output_vals["shape"],
                         output_vals["shape_prior"])

      ggplot(data = data) +
        geom_density(aes(shape)) +
        geom_density(aes(shape_prior), linetype = "dashed")
    }

  })

  output$posterior <- shiny::renderPlot({

    showModal(modalDialog("Generating plot", footer = NULL))

    model_output <- stanModel()
    output_vals <- rstan::extract(model_output)

    tableList <- list()

    for (ii in 1:NROW(output_vals$shape)){

      thisShape <- as.numeric(output_vals$shape[ii])
      thisScale <- as.numeric(output_vals$scale[ii])

      get_reliability <- function(shape, scale){
        f1 <- function(t){
          exp(1)^(-(t/scale)^shape)
        }
        return(f1)
      }


      t <- c(seq(0.1, 2.4, 0.1) * 200)
      thisTable <- data.frame("t" = t,
                              "reliability" = numeric(NROW(t)),
                              "mean_life" = numeric(NROW(t)),
                              "preventive_cost" = numeric(NROW(t)),
                              "corrective_cost" = numeric(NROW(t)),
                              "total_cost" = numeric(NROW(t)))

      reliability <- get_reliability(thisShape, thisScale)
      thisTable$reliability <- reliability(t)

      tableList[[ii]] <- thisTable

    }

    finalTable <- plyr::aaply(plyr::laply(tableList, as.matrix), c(2, 3), mean) %>% as.data.frame()
    finalTableLQ <- plyr::aaply(plyr::laply(tableList, as.matrix), c(2, 3), function(x){quantile(x, 0.01)}) %>% as.data.frame()
    finalTableHQ <- plyr::aaply(plyr::laply(tableList, as.matrix), c(2, 3), function(x){quantile(x, 0.99)}) %>% as.data.frame()

    output <- data.frame(t = finalTable$t,

                         lower_bound = finalTableLQ$reliability,
                         mean_val = finalTable$reliability,
                         upper_bound = finalTableHQ$reliability

    )

    removeModal()

    ggplot(data = output, aes(t)) +
      geom_line(aes(y = mean_val)) +
      geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), alpha=0.05) +
      xlab("Running hours") +
      ylab("Reliability")

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

    failures <- strex::str_extract_numbers(input$failures_data)[[1]]

    model_output <- stanModel()
    x <- list(y = failures,
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
