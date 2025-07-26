#' Launch the Non-Symmetric Confidence Interval Shiny App
#'
#' This wrapper launches an interactive Shiny application for visualizing non-symmetric confidence intervals.
#' The app allows users to select variables from the DDT dataset, adjust CI parameters, and view real-time plots
#' and diagnostics. Clickable plots let users update the hypothesized mean \code{mu0} interactively.
#'
#' @name launchCiApp
#' @description Wrapper to launch the \code{ciNonSym()} Shiny app.
#'
#' @returns Runs the Shiny app in a browser or viewer pane.
#'
#' @details
#' Required objects:
#' \itemize{
#'   \item The function \code{ciNonSym()} must be available in the workspace.
#'   \item The dataset \code{ddt} must be loaded and include variables: \code{RIVER}, \code{SPECIES}, and a numeric variable like \code{LENGTH}, \code{WEIGHT}, or \code{DDT}.
#' }
#'
#' @seealso \code{\link{ciNonSym}}
#'
#' @importFrom shiny shinyAppDir
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data first
#' data(ddt)
#'
#' # Then launch the Shiny app
#' launchCiApp()
#' }
utils::globalVariables(c("ui", "server", "data"))

launchCiApp <- function() {
  app_dir <- system.file("app", package = "Math4753KORBLab11")
  shinyAppDir(appDir = app_dir)
}

