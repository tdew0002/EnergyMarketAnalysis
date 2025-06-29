# R/run_app.R

#' @title Launch the NEM Dashboard
#' @description
#'   Run the bundled Shiny application shipped with AemoETL.
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "AemoETL")
  if (app_dir == "") {
    stop("Cannot find app directory. Try re-installing AemoETL.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

