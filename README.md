### SemNetCleaner 2.0.0

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner) [![Downloads per month](http://cranlogs.r-pkg.org/badges/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner)

# How To Install
```
install.packages("SemNetCleaner")
```

The development version is on GitHub:

```
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNetCleaner", dependencies = c("Imports", "Suggests"))
```

SemNetCleaner needs R 4.0.1 or later and the *SemNetDictionaries* package (0.3.0 or later), which supplies the dictionaries.

# How To Use
See our tutorial: https://doi.org/10.1037/met0000463

Christensen, A. P., & Kenett, Y. N. (2021). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*. https://doi.org/10.1037/met0000463

A quick start with the verbal fluency data in the package (one row per participant, one response per column):

```
library(SemNetCleaner)

clean <- textcleaner(
  data = open.animals[, -c(1:2)], partBY = "row",
  dictionary = "animals", spelling = "US"
)

clean$responses$clean   # cleaned responses, one row per participant
clean$responses$binary  # binary matrix (participants by responses)
clean$behavioral        # perseverations, intrusions, appropriate responses
```

Responses that cannot be corrected automatically are sent to a manual spell-check in the console. A help window opens beside the console at the start (press `H` at any decision to open it again, `B` to go back, and `X` to exit and continue later with `continue =`).

# SemNetCleaner
Implements several functions that automatize the cleaning, de-pluralizing, binarizing, de-stringing, converging,
and finalizing linguistic data for semantic network analysis.

SemNetCleaner enables researchers to efficiently and feasibly preprocess their verbal fluency data into a format that can be used with any semantic network estimation method (e.g., Zemla & Austerweil, 2018). Additionally, this package encourages open science practices by offering output that can be used in the peer-review process (i.e., response corrections and inclusions). More broadly, SemNetCleaner seeks to achieve more reliable results and promotes a standardized approach to the preprocessing of verbal fluency responses. This means that researchers who are not necessarily interested in performing SemNA can still use SemNetCleaner to ensure the reliability and validity of their verbal fluency results.

## What changed in 2.0.0

Version 2.0.0 rebuilds the package's internals for reliability. See `NEWS` for the full list. In short:

* **Many bugs are fixed**, including a perseveration count that was far too low, manual edits that were discarded, menus that crashed on narrow consoles, and several failures on unusual data layouts.
* **New functions and arguments.** `bad_response()`, `best_guess()`, `binary_to_responses()`, `qwerty_distance()`, `read_data()`, and `responses_to_binary()` replace the dotted names (`bad.response()`, `best.guess()`, `bin2resp()`, `qwerty.dist()`, `read.data()`, `resp2bin()`), which still work. `textcleaner()` gains `keepRepeats`.
* **One change that can break a script.** The `lowercase` argument of `textcleaner()` was removed because responses are always lowercased; delete it from existing calls.
* **The manual spell-check help** opens in its own window, and `options(SemNetCleaner.pause = FALSE)` turns off the short pauses in the console wizard.
* **The review of automated corrections** opens the Shiny/DT review app on every operating system (it needs `shiny` and `DT`); pass `edit_fn = edit` to use base R's `edit()` instead.
* **Parallel processing** uses `future`; with `cores > 1` the package must be installed (not loaded with `devtools::load_all()`).

# References
Zemla, J. C., & Austerweil, J. L. (2018). Estimating semantic networks of groups and
individuals from fluency data. *Computational Brain & Behavior*, *1*, 36–58.
https://doi.org/10.1007/s42113-018-0003-7
