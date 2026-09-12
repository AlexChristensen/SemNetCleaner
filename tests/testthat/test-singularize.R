test_that("singularize() matches the original singularize() on all documented examples", {

  expect_equal(singularize("dogs"), "dog")
  expect_equal(singularize("foxes"), "fox")
  expect_equal(singularize("wolves"), "wolf")
  expect_equal(singularize("octopi"), "octopus")
  expect_equal(singularize("taxa"), "taxon")
  expect_equal(singularize("wives"), "wife")
  expect_equal(singularize("fezzes"), "fez")
  expect_equal(singularize("roofs"), "roof")
  expect_equal(singularize("photos"), "photo")
  expect_equal(singularize("children"), "child")
  expect_equal(singularize("teeth"), "tooth")
  expect_equal(singularize("mice"), "mouse")

})

test_that("singularize() derives the shared irregular_plurals reverse-mapping correctly", {

  expect_equal(singularize("geese"), "goose")
  expect_equal(singularize("men"), "man")
  expect_equal(singularize("women"), "woman")
  expect_equal(singularize("feet"), "foot")
  expect_equal(singularize("people"), "person")
  expect_equal(singularize("lice"), "louse")

})

test_that("singularize() handles its own 16 singularize-only irregular entries", {

  expect_equal(singularize("valves"), "valve")
  expect_equal(singularize("trees"), "tree")
  expect_equal(singularize("scribbles"), "scribble")
  # "peduncle"/"mallee" (unlike the other 14 entries in this table) aren't
  # in SemNetDictionaries::coca.dictionary, so the function's OLD
  # dictionary-gate architecture rejected the correctly-matched irregular
  # singular and fell back to the original word unchanged -- fixed (per
  # direct request) by having a curated irregular-table match bypass the
  # dictionary gate entirely, the same treatment pluralize() now gets.
  expect_equal(singularize("peduncles"), "peduncle")
  expect_equal(singularize("mallees"), "mallee")
  expect_equal(singularize("panicles"), "panicle")
  expect_equal(singularize("ridges"), "ridge")
  expect_equal(singularize("petioles"), "petiole")
  expect_equal(singularize("angles"), "angle")
  expect_equal(singularize("bristles"), "bristle")
  expect_equal(singularize("edges"), "edge")
  expect_equal(singularize("fissures"), "fissure")
  expect_equal(singularize("sutures"), "suture")
  expect_equal(singularize("occurrences"), "occurrence")
  expect_equal(singularize("bees"), "bee")
  expect_equal(singularize("asses"), "ass")

})

test_that("singularize() handles the new shared irregular_plurals entries in reverse", {

  expect_equal(singularize("oxen"), "ox")
  expect_equal(singularize("deer"), "deer")
  expect_equal(singularize("moose"), "moose")
  expect_equal(singularize("sheep"), "sheep")
  expect_equal(singularize("salmon"), "salmon")
  expect_equal(singularize("elk"), "elk")
  expect_equal(singularize("bison"), "bison")
  expect_equal(singularize("geniuses"), "genius")
  expect_equal(singularize("octopi"), "octopus")

})

test_that("singularize() handles multi-word phrases, singularizing only the last word", {

  expect_equal(singularize("polar bears"), "polar bear")

})

test_that("singularize(dictionary = FALSE) skips the dictionary check", {

  expect_equal(singularize("dogs", dictionary = FALSE), "dog")

})

test_that("singularize() validates its inputs", {

  expect_error(singularize(1), "singularize")
  expect_error(singularize(c("dogs", "cats")), "singularize")
  expect_error(singularize("dogs", dictionary = "yes"), "singularize")

})
