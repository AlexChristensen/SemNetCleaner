test_that("pluralize() matches the original pluralize() on all documented examples", {

  expect_equal(pluralize("dog"), "dogs")
  expect_equal(pluralize("fox"), "foxes")
  expect_equal(pluralize("wolf"), "wolves")
  expect_equal(pluralize("taxon"), "taxa")
  expect_equal(pluralize("wife"), "wives")
  expect_equal(pluralize("roof"), "roofs")
  expect_equal(pluralize("photo"), "photos")
  expect_equal(pluralize("child"), "children")
  expect_equal(pluralize("tooth"), "teeth")
  expect_equal(pluralize("mouse"), "mice")

})

test_that("pluralize('octopus') now matches the package's own original documented example (rule improvement)", {

  # Previously returned "octopus" unchanged (see git history / REFACTOR_PLAN.md
  # for the original investigation): "octopi" isn't in coca.dictionary, and
  # every dictionary/spell-checker checked (coca, hunspell, cocaspell, real
  # hunspell_check()) also rejects it -- it's a folk-etymology plural. Now an
  # explicit irregular_plurals entry, which bypasses the dictionary gate
  # entirely and finally matches what this function's own documentation has
  # always claimed.
  expect_equal(pluralize("octopus"), "octopi")

})

test_that("pluralize() handles the remaining irregular table entries (via shared irregular_plurals)", {

  expect_equal(pluralize("goose"), "geese")
  expect_equal(pluralize("man"), "men")
  expect_equal(pluralize("woman"), "women")
  expect_equal(pluralize("foot"), "feet")
  expect_equal(pluralize("person"), "people")
  expect_equal(pluralize("louse"), "lice")
  expect_equal(pluralize("ox"), "oxen")

})

test_that("pluralize() correctly leaves invariant-plural animal names unchanged (new exception words)", {

  # All confirmed empirically to be mishandled by the regular suffix rules
  # before this fix: their naive "+s" candidate is coincidentally a real
  # dictionary entry (e.g. "deers"/"salmons"/"elks"/"bisons"/"trouts" are
  # all genuine coca.dictionary entries, and several even pass the real
  # hunspell spell-checker), so no dictionary-membership check alone could
  # ever have told them apart from a correctly-formed regular plural --
  # this package's primary use case is verbal-fluency ANIMAL naming (see
  # the bundled open.animals data), so these are weighted toward that domain
  expect_equal(pluralize("deer"), "deer")
  expect_equal(pluralize("moose"), "moose")
  expect_equal(pluralize("sheep"), "sheep")
  expect_equal(pluralize("swine"), "swine")
  expect_equal(pluralize("salmon"), "salmon")
  expect_equal(pluralize("trout"), "trout")
  expect_equal(pluralize("elk"), "elk")
  expect_equal(pluralize("bison"), "bison")
  expect_equal(pluralize("fish"), "fish")
  expect_equal(pluralize("shrimp"), "shrimp")
  expect_equal(pluralize("buffalo"), "buffalo")
  expect_equal(pluralize("species"), "species")

})

test_that("pluralize() handles other rule-engine-mishandled irregular words (new exception words)", {

  expect_equal(pluralize("bus"), "buses")
  expect_equal(pluralize("walrus"), "walruses")
  expect_equal(pluralize("matrix"), "matrices")
  expect_equal(pluralize("vertex"), "vertices")
  expect_equal(pluralize("iris"), "irises")
  expect_equal(pluralize("hippopotamus"), "hippopotamuses")
  expect_equal(pluralize("platypus"), "platypuses")
  # "genius" -> the rule engine's own "-us/-i" candidate ("genii") is a
  # real, hunspell-recognized word, but the archaic sense (guardian
  # spirits) rather than the standard modern plural for people
  expect_equal(pluralize("genius"), "geniuses")

})

test_that("pluralize()'s broadened final gate (+es fallback, coca+hunspell) fixes previously-uncovered -us words", {

  # These are NOT in the irregular table -- fixed purely by the rule
  # improvement (trying "+es" in addition to "+s", checked against both
  # coca.dictionary and, when installed, the real hunspell spell-checker)
  expect_equal(pluralize("virus"), "viruses")
  expect_equal(pluralize("campus"), "campuses")
  expect_equal(pluralize("bonus"), "bonuses")
  expect_equal(pluralize("census"), "censuses")
  expect_equal(pluralize("chorus"), "choruses")
  expect_equal(pluralize("circus"), "circuses")
  expect_equal(pluralize("status"), "statuses")

})

test_that("pluralize() still correctly handles the Latin -us/-i pattern words", {

  expect_equal(pluralize("cactus"), "cacti")
  expect_equal(pluralize("alumnus"), "alumni")
  expect_equal(pluralize("fungus"), "fungi")
  expect_equal(pluralize("nucleus"), "nuclei")
  expect_equal(pluralize("radius"), "radii")
  expect_equal(pluralize("stimulus"), "stimuli")
  expect_equal(pluralize("syllabus"), "syllabi")

})

test_that("pluralize() handles -is words via the broadened gate", {

  expect_equal(pluralize("crisis"), "crises")
  expect_equal(pluralize("analysis"), "analyses")
  expect_equal(pluralize("axis"), "axes")
  expect_equal(pluralize("thesis"), "theses")

})

test_that("pluralize() returns the original word when no plural form is found", {

  # An unrecognizable non-word should fall through every rule and both
  # dictionary fail-safe attempts, returning the original input unchanged
  expect_equal(pluralize("zzzqqxx"), "zzzqqxx")

})

test_that("pluralize() common regular animals still work correctly (no regression)", {

  expect_equal(pluralize("cat"), "cats")
  expect_equal(pluralize("bird"), "birds")
  expect_equal(pluralize("lion"), "lions")
  expect_equal(pluralize("tiger"), "tigers")
  expect_equal(pluralize("zebra"), "zebras")
  expect_equal(pluralize("giraffe"), "giraffes")
  expect_equal(pluralize("elephant"), "elephants")
  expect_equal(pluralize("monkey"), "monkeys")
  expect_equal(pluralize("rabbit"), "rabbits")
  expect_equal(pluralize("horse"), "horses")
  expect_equal(pluralize("cow"), "cows")
  expect_equal(pluralize("pig"), "pigs")
  expect_equal(pluralize("calf"), "calves")
  expect_equal(pluralize("leaf"), "leaves")

})

test_that("pluralize() validates its input", {

  expect_error(pluralize(1), "pluralize")
  expect_error(pluralize(c("dog", "cat")), "pluralize")

})
