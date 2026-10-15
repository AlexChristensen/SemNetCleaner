# The wizard's deliberate pauses (see `pause()` in R/helpers.R) were most of
# the suite's run time; switch them off for every test.
old_options <- options(SemNetCleaner.pause = FALSE)
withr::defer(options(old_options), teardown_env())
