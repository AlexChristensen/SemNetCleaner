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
#' @export
# `search_fn` factory for `textcleaner_shiny()` (Phase 11).
#
# `spellcheck_menu()`'s GOOGLE WORD/STRING/IT options call
# `search_fn(query, site = "google", rlang = FALSE)`, matching
# `searcher::search_site()`'s own contract (the real default). Confirmed
# directly (`searcher:::search_google()`/`searcher:::browse_url()`) that
# this ultimately just calls `utils::browseURL()` -- opening a real new
# browser window/tab, not something that can be embedded inside a Shiny
# app's own page. A real Google results page also cannot be embedded via
# iframe at all (it blocks framing via response headers) -- confirmed no
# viable "Google, but embedded" option exists.
#
# Resolution: `make_shiny_search_fn()` returns a `search_fn`-shaped closure
# that, instead of opening a browser window, builds a DuckDuckGo HTML
# results URL (DuckDuckGo's lightweight, iframe-friendly endpoint) and
# writes it into the injected `store` (a `shiny::reactiveVal()`) -- the
# app's own search-modal module (`mod_search_modal.R`) observes `store()`
# and renders the URL in an `shiny::tags$iframe()`. `site`/`rlang` are
# accepted (matching the real call contract every `spellcheck_menu()` call
# site uses) but not otherwise used -- DuckDuckGo is the only embeddable
# target this function ever produces, regardless of which "site" argument
# `spellcheck_menu()` happens to pass.
# Updated 12.09.2026
make_shiny_search_fn <- function(store)
{

  function(query, site = "google", rlang = TRUE)
  {

    url <- paste0(
      "https://duckduckgo.com/html/?q=",
      utils::URLencode(query, reserved = TRUE)
    )

    store(list(query = query, url = url))

    invisible(url)

  }

}
