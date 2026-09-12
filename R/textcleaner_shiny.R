#' Point-and-Click \code{textcleaner()} Wizard
#'
#' @description Launches a Shiny app wrapping the entire
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
#' @export
# New (Phase 11). Guards `shiny`/`DT` exactly like `automated_edit()`
# (`R/automated_edit.R`), since the app embeds that function's own UI/server
# as one of its steps.
# Updated 12.09.2026
textcleaner_shiny <- function(...)
{

  required_packages <- c("shiny", "DT")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if(length(missing_packages) > 0){
    stop(
      paste0(
        "In `textcleaner_shiny()`: the following packages are required but aren't installed: ",
        paste0(missing_packages, collapse = ", "), ".\n",
        "Install them with `install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))`."
      ),
      call. = FALSE
    )
  }

  shiny::runApp(system.file("Shiny", package = "SemNetCleaner"), ...)

}
