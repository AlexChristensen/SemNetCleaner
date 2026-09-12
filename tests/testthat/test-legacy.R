test_that("every legacy dotted name is exported and delegates to its new-named replacement", {

  expect_equal(bad.response(c("hello", "", "NA")), bad_response(c("hello", "", "NA")))

  expect_equal(
    best.guess("bomba", full.dictionary = SemNetDictionaries::animals.dictionary),
    best_guess("bomba", full_dictionary = SemNetDictionaries::animals.dictionary)
  )

  rmat <- matrix(c(1, 0, 0, 1), nrow = 2, dimnames = list(c("P1", "P2"), c("cat", "dog")))
  expect_equal(bin2resp(rmat), binary_to_responses(rmat))

  expect_equal(qwerty.dist("big", "pig"), qwerty_distance("big", "pig"))

  resp <- matrix(c("cat", "dog", "cat", "bird"), nrow = 2, dimnames = list(c("P1", "P2"), NULL))
  expect_equal(resp2bin(resp), responses_to_binary(resp))

})

test_that("qwerty_distance() and singularize() are real package exports, not just internal objects (real bug fix)", {

  # Real bug fixed during Phase 8's documentation audit: a roxygen
  # doc-comment block's `@export` tag was, in both of these files, actually
  # attached to an unrelated private helper placed between the doc comment
  # and the function it was meant to document (`.qwerty_keyboard_cache`/
  # `additional_irregular_singulars` respectively) -- `qwerty_distance()`
  # and `singularize()` were silently never added to NAMESPACE, invisible
  # throughout development since `devtools::load_all()` exposes internal
  # objects the same as exported ones. Checking `getNamespaceExports()`
  # directly (rather than just calling the function, which would still
  # work under `load_all()`/`testthat` regardless) is what actually
  # catches this class of bug.
  exports <- getNamespaceExports("SemNetCleaner")

  expect_true("qwerty_distance" %in% exports)
  expect_true("singularize" %in% exports)
  # `all.names = TRUE`: dot-prefixed objects are hidden from a plain `ls()`
  # by R's own convention (not a bug) -- omitting it here would make this
  # assertion pass vacuously, defeating the point of the check.
  expect_true(".qwerty_keyboard_cache" %in% ls(asNamespace("SemNetCleaner"), all.names = TRUE))
  expect_false(".qwerty_keyboard_cache" %in% exports)
  expect_false("additional_irregular_singulars" %in% exports)

})

test_that("every pre-2.0.0 exported name is still a real export (binding decision #6)", {

  exports <- getNamespaceExports("SemNetCleaner")

  old_names <- c(
    "bad.response", "best.guess", "bin2resp", "convert2snafu", "pluralize",
    "qwerty.dist", "read.data", "resp2bin", "singularize", "textcleaner"
  )

  for(name in old_names){
    expect_true(name %in% exports, info = name)
  }

})
