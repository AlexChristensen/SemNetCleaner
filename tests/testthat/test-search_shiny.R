test_that("make_shiny_search_fn() writes a DuckDuckGo URL and the original query into the store", {

  captured <- NULL
  store <- function(x = NULL){
    if(is.null(x)) return(captured)
    captured <<- x
  }

  search_fn <- make_shiny_search_fn(store)

  search_fn("category 'dog'", site = "google", rlang = FALSE)

  expect_equal(captured$query, "category 'dog'")
  expect_true(grepl("^https://duckduckgo\\.com/html/\\?q=", captured$url))

})

test_that("make_shiny_search_fn()'s closure matches spellcheck_menu()'s real search_fn(query, site, rlang) call contract", {

  # Every real call site (spellcheck_menu.R) calls
  # search_fn(query, site = "google", rlang = FALSE) -- confirming this
  # closure accepts that exact call shape without error, regardless of
  # which "site" is requested (DuckDuckGo is the only embeddable target
  # this factory ever produces)
  store <- function(x = NULL) invisible(x)
  search_fn <- make_shiny_search_fn(store)

  expect_no_error(search_fn("dog", site = "google", rlang = FALSE))
  expect_no_error(search_fn("dog", site = "bing", rlang = TRUE))

})

test_that("make_shiny_search_fn() URL-encodes special characters in the query", {

  captured <- NULL
  store <- function(x){ captured <<- x }
  search_fn <- make_shiny_search_fn(store)

  search_fn("cat & dog", site = "google", rlang = FALSE)

  expect_true(grepl("cat", captured$url))
  expect_false(grepl(" ", captured$url))
  expect_false(grepl("&", captured$url, fixed = TRUE))

})
