#' Embedded-Search \code{search_fn} Factory for \code{textcleaner_shiny()}
#'
#' @description Returns a \code{search_fn}-shaped closure (matching
#' \code{textcleaner()}'s own \code{search_fn} contract) for
#' \code{\link{textcleaner_shiny}}'s wizard: instead of opening a real
#' browser window, it writes a DuckDuckGo results URL into \code{store} for
#' the app's own search modal to render in an embedded iframe. See this
#' file's own header comment for why a real Google results page cannot be
#' embedded this way at all.
#'
#' Exported (not just internal) because \code{inst/Shiny/}'s own module
#' files are sourced into the global environment by Shiny's own app-loading
#' mechanism, not into the package namespace -- an unexported function is
#' not reachable from there by its bare name.
#'
#' @param store Function.
#' A \code{shiny::reactiveVal()} (or any function accepting one argument)
#' to write \code{list(query =, url =)} into whenever the returned closure
#' is called
#'
#' @return A \code{function(query, site, rlang)} suitable for
#' \code{textcleaner()}'s own \code{search_fn} argument
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
# Search function for the {shiny} wizard ----
# Creates the `search_fn` for the internal Shiny wizard. The GOOGLE options call
# `search_fn(query, site = "google", rlang = FALSE)`, which normally opens a new browser
# window; Google results pages cannot be embedded, so this returns a function that builds a
# DuckDuckGo results URL and stores it for the app's search modal to show in an iframe
# (`site` and `rlang` are accepted and ignored)
# Updated 14.10.2026
make_shiny_search_fn <- function(
    store # function that stores the search
)
{

  # Create search function that stores the query instead of opening a browser
  function(query, site = "google", rlang = TRUE)
  {

    # Build search URL
    url <- paste0(
      "https://duckduckgo.com/html/?q=",
      utils::URLencode(query, reserved = TRUE)
    )

    # Store query and URL
    store(list(query = query, url = url))

    # Return URL silently
    invisible(url)

  }

}
