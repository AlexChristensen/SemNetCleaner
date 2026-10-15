#' Point-and-Click \code{textcleaner()} Wizard
#'
#' @description \strong{Internal and experimental:} not exported while the
#' app's known bugs are triaged. Call it as
#' \code{SemNetCleaner:::textcleaner_shiny()}.
#'
#' Launches a Shiny app wrapping the entire
#' \code{\link{textcleaner}} pipeline end to end -- upload, options,
#' automated spell-check, manual spell-check wizard, automated-corrections
#' review, and download -- as a point-and-click alternative to the
#' interactive console wizard. Every menu option becomes a real clickable
#' button (plus a free-text box for typing a custom word/string directly);
#' the "GOOGLE WORD"/"GOOGLE STRING"/"GOOGLE IT" options open an embedded
#' in-app search instead of a new browser window.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()} (e.g.
#' \code{port}, \code{launch.browser}).
#'
#' @return Called for its side effect (launching the Shiny app). Nothing is
#' returned to the calling R session -- results are downloaded from within
#' the app itself (see the app's own Export tab).
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Launch the {shiny} wizard ----
# Guards `shiny` and `DT` like `automated_edit()`, since the app embeds that function's UI and server
# Updated 14.10.2026
textcleaner_shiny <- function(
    ... # arguments passed to shiny::runApp
)
{

  # Set required packages
  required_packages <- c("shiny", "DT")

  # Find missing packages
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  # Check for missing packages
  if(length(missing_packages) > 0){

    # Send error
    stop(
      paste0(
        "In `textcleaner_shiny()`: the following packages are required but aren't installed: ",
        paste0(missing_packages, collapse = ", "), ".\n",
        "Install them with `install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))`."
      ),
      call. = FALSE
    )

  }

  # Run the app
  shiny::runApp(system.file("Shiny", package = "SemNetCleaner"), ...)

}
