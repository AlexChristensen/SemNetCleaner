### SemNetCleaner 2.0.0

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner) [![Downloads per month](http://cranlogs.r-pkg.org/badges/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner)

# How To Install
```
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNetCleaner", dependencies = c("Imports", "Suggests"))
```

# How To Use
See our tutorial: https://psyarxiv.com/eht87/

Christensen, A. P., & Kenett, Y. N. (in press). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*. doi:10.31234/osf.io/eht87. OSF:10.17605/OSF.IO/HQXTC

# SemNetCleaner
Implements several functions that automatize the cleaning, de-pluralizing, binarizing, de-stringing, converging,
and finalizing linguistic data for semantic network analysis.

SemNetCleaner enables researchers to efficiently and feasibly preprocess their verbal fluency data into a format that can be used with any semantic network estimation method (e.g., Zemla & Austerweil, 2018). Additionally, this package encourages open science practices by offering output that can be used in the peer-review process (i.e., response corrections and inclusions). More broadly, SemNetCleaner seeks to achieve more reliable results and promotes a standardized approach to the preprocessing of verbal fluency responses. This means that researchers who are not necessarily interested in performing SemNA can still use SemNetCleaner to ensure the reliability and validity of their verbal fluency results.

## Version 2.0.0: A Rebuilt Package

Version 2.0.0 is a from-scratch rebuild of the package's internals, aimed at
long-term reliability rather than new features. Highlights:

* **One function, one file.** Every exported function (e.g. `textcleaner()`,
  `best_guess()`) now lives in its own file alongside its own input
  validation, instead of being scattered across one 7,500-line
  `utils-SemNetCleaner.R`.
* **Real bugs fixed.** More than 60 confirmed correctness issues in the
  pre-2.0.0 codebase are fixed in this version -- a silently-shadowed
  duplicate function definition, a ~38x undercount in perseveration
  counting, a manual spell-check edit that was silently discarded for
  fluency data, several reversed/dropped loop ranges on degenerate inputs,
  and more. See `NEWS.md` for the full list.
* **The wizard is testable for the first time.** Every interactive,
  `readline()`/`menu()`-driven function (the manual spell-check wizard,
  `textcleaner()` itself) now takes an injectable input reader, so the whole
  pipeline can be driven end to end by a scripted queue of answers in tests
  instead of relying on manual smoke-testing alone. The package had zero
  tests before this rebuild; it now has an extensive `testthat` suite.
* **Old code keeps working.** Every pre-2.0.0 exported name (e.g.
  `bad.response()`, `qwerty.dist()`, `resp2bin()`) still works, as a thin
  wrapper around its new-named replacement. Existing scripts do not need to
  change.
* **Fewer, better-declared dependencies.** `pbapply`/`parallel` are dropped
  in favor of `future`/`future.apply`/`progressr` (guaranteed worker
  cleanup, no core-count-floor crash on a 1-core machine); the unconditional
  `easycsv` dependency in `convert2snafu()` is dropped for an explicit
  `path =` argument; `shiny`/`DT`/`tcltk`/`rstudioapi` moved to
  `Suggests`-only, each guarded by `requireNamespace()` at its one real call
  site.
* **The Shiny companion app (`automated_edit()`, formerly `AutomatedEdit()`) is
  fully rebuilt.** It now takes its data as a real R argument instead of a
  `tempdir()`/CSV round-trip, fixing a real cross-platform path bug at the root
  and making it unit-testable. `correct_changes()`'s old Linux-vs-everywhere-else
  OS fork for this step is gone -- every OS now defaults to the Shiny app (Linux
  users now need `shiny`/`DT` installed for the interactive default; pass
  `edit_fn = edit` for the old, dependency-free behavior). See `SHINY_PLAN.md`
  for the full development record.
* **A full point-and-click `textcleaner()` wizard: `textcleaner_shiny()`.**
  Wraps the entire pipeline -- upload, options, automated spell-check, manual
  spell-check wizard, corrections review, download -- as a Shiny app. Every
  console menu option is a real clickable button (plus a free-text box for
  typing a custom word/string directly); "GOOGLE WORD"/"GOOGLE STRING"/
  "GOOGLE IT" open an embedded in-app search instead of a new browser
  window. Built on the console wizard's own save/resume mechanism -- no
  wizard logic is reimplemented. See `SHINY_PLAN.md` for the full
  development record.

# References
Zemla, J. C., & Austerweil, J. L. (2018). Estimating semantic networks of groups and
individuals from fluency data. *Computational Brain & Behavior*, *1*, 36–58.
https://doi.org/10.1007/s42113-018-0003-7
