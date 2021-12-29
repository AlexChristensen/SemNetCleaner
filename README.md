### CRAN 1.3.4 | GitHub 1.3.5

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner) [![Downloads per month](http://cranlogs.r-pkg.org/badges/SemNetCleaner?color=brightgreen)](https://cran.r-project.org/package=SemNetCleaner) 

# How To Install
```
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNetCleaner")
```

# How To Use
See our tutorial: https://psyarxiv.com/eht87/

Christensen, A. P., & Kenett, Y. N. (in press). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*. doi:10.31234/osf.io/eht87. OSF:10.17605/OSF.IO/HQXTC

# SemNetCleaner
Implements several functions that automatize the cleaning, de-pluralizing, binarizing, de-stringing, converging,
and finalizing linguistic data for semantic network analysis.

SemNetCleaner enables researchers to efficiently and feasibly preprocess their verbal fluency data into a format that can be used with any semantic network estimation method (e.g., Zemla & Austerweil, 2018). Additionally, this package encourages open science practices by offering output that can be used in the peer-review process (i.e., response corrections and inclusions). More broadly, SemNetCleaner seeks to achieve more reliable results and promotes a standardized approach to the preprocessing of verbal fluency responses. This means that researchers who are not necessarily interested in performing SemNA can still use SemNetCleaner to ensure the reliability and validity of their verbal fluency results.

# References
Zemla, J. C., & Austerweil, J. L. (2018). Estimating semantic networks of groups and
individuals from fluency data. *Computational Brain & Behavior*, *1*, 36–58.
https://doi.org/10.1007/s42113-018-0003-7
