#' Launches analysis app
#'
#' @description
#' Launches Shiny app
#'
#'
#' @export
#'

lcd_projectors_app <- function() {

  appDir <- system.file("lcd_projectors_app", package = "foocafeReliability")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `foocafeReliability`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

