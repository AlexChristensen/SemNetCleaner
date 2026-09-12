# Sourced once by Shiny's own autoload mechanism, before ui.R/server.R --
# every file directly under inst/Shiny/R/ is also auto-sourced this way, so
# no manual source() loop is needed for the modules themselves.

suppressMessages(library(SemNetCleaner))

# All cores the host has, minus one left free for the OS/main Shiny process.
# Defined here (not in mod_shared.R) because global.R is sourced before any
# inst/Shiny/R/*.R file (Shiny's own loadSupport() order) and needs this
# immediately, below, to size the app-wide future plan.
max_app_cores <- function()
{
  max(1, parallel::detectCores() - 1)
}

# Without an explicit plan here, promises::future_promise() (used by every
# ExtendedTask in this app) falls back to future::sequential -- every
# "background" task would actually run synchronously in this same process,
# blocking Shiny's entire event loop for its full duration. multisession
# dispatches each task to a real background R process instead.
# parallel_process()'s own internal plan() switch (for a single task's OWN
# requested `cores`) is scoped via on.exit() and composes correctly with
# this outer plan -- see helpers.R's own nested-worker-cap comment.
future::plan(future::multisession, workers = max_app_cores())

# Shiny's default upload cap is a silent 5MB; explicit and generous enough
# for realistic fluency/free-association datasets
options(shiny.maxRequestSize = 50 * 1024^2)
