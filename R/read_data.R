#' Read in Common Data File Extensions
#'
#' @description A single function to read in common data file extensions.
#' Note that this function is specialized for reading in text data in the
#' format necessary for functions in SemNetCleaner
#'
#' File extensions supported:
#' \itemize{
#' \item{.Rdata} \item{.rds} \item{.csv} \item{.xlsx}
#' \item{.xls} \item{.sav} \item{.txt} \item{.mat} \item{.dat}
#' }
#'
#' @param file Character.
#' A path to the file to load.
#' Defaults to interactive file selection using \code{\link{file.choose}}
#'
#' @param header Boolean.
#' A logical value indicating whether the file contains the
#' names of the variables as its first line.
#' If missing, the value is determined from the file format:
#' header is set to \code{TRUE} if and only if the first row
#' contains one fewer field than the number of columns
#'
#' @param sep Character.
#' The field separator character.
#' Values on each line of the file are separated by this character.
#' If sep = "" (the default for \code{\link{read.table}}) the separator
#' is a 'white space', that is one or more spaces, tabs, newlines or
#' carriage returns
#'
#' @param ... Additional arguments.
#' Allows for additional arguments to be passed onto
#' the respective read functions. See documentation in the list below:
#'
#' \itemize{
#' \item{.Rdata}
#' {\code{\link{load}}}
#' \item{.rds}
#' {\code{\link{readRDS}}}
#' \item{.csv}
#' {\code{\link[utils]{read.table}}}
#' \item{.xlsx}
#' {\code{\link[readxl]{read_excel}}}
#' \item{.xls}
#' {\code{\link[readxl]{read_excel}}}
#' \item{.sav}
#' {\code{\link[foreign]{read.spss}}}
#' \item{.txt}
#' {\code{\link[utils]{read.table}}}
#' \item{.mat}
#' {\code{\link[R.matlab]{readMat}}}
#' \item{.dat}
#' {\code{\link[utils]{read.table}}}
#' }
#'
#' @return A data frame containing a representation of the data in the file.
#' If file extension is ".Rdata", then data will be read to the global environment
#'
#' @examples
#' # Use this example for your data
#' if(interactive())
#' {read_data()}
#'
#' # Example for CRAN tests
#' ## Create test data
#' test1 <- c(1:5, "6,7", "8,9,10")
#'
#' ## Path to temporary file
#' tf <- tempfile()
#'
#' ## Create test file
#' writeLines(test1, tf)
#'
#' ## Read in data
#' read_data(tf)
#'
#' # See documentation of respective R functions for specific examples
#'
#' @references
#' # R Core Team
#'
#' R Core Team (2019). R: A language and environment for
#' statistical computing. R Foundation for Statistical Computing,
#' Vienna, Austria. URL https://www.R-project.org/.
#'
#' # readxl
#'
#' Hadley Wickham and Jennifer Bryan (2019). readxl: Read Excel
#' Files. R package version 1.3.1.
#' https://CRAN.R-project.org/package=readxl
#'
#' # R.matlab
#'
#' Henrik Bengtsson (2018). R.matlab: Read and Write MAT Files
#' and Call MATLAB from Within R. R package version 3.6.2.
#' https://CRAN.R-project.org/package=R.matlab
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom utils read.table read.csv
#' @importFrom tools file_ext
#'
#' @export
# Read data
# Ported from `read.data()`. Added up-front extension validation via a real
# `read_data_errors()` (the original inlined its extension check first thing
# in the function body -- moved out, same check, matching the file-per-
# function `<name>_errors()` convention).
# Updated 08.09.2026
read_data <- function(file = file.choose(), header = TRUE, sep = ",", ...)
{

  # Grab extension
  ext <- tolower(file_ext(file))

  read_data_errors(ext)

  # Determine data load
  if(ext != "")
  {
    data <- switch(ext,
                   rdata = load(file, envir = .GlobalEnv),
                   rds = readRDS(file),
                   csv = read.csv(file, header = header, sep = sep, as.is = TRUE, ...),
                   xlsx = as.data.frame(readxl::read_xlsx(file, col_names = header, ...)),
                   xls = as.data.frame(readxl::read_xls(file, col_names = header, ...)),
                   sav = foreign::read.spss(file, to.data.frame = TRUE, stringAsFactors = FALSE, ...),
                   txt = read.table(file, header = header, sep = sep, ...),
                   mat = as.data.frame(R.matlab::readMat(file, ...)),
                   dat = read.table(file, header = header, sep = sep, ...)
    )
  }else{data <- read.table(file, header = header, sep = sep, ...)}

  return(data)

}

#' @noRd
# Up-front validation for `read_data()`
# Updated 08.09.2026
read_data_errors <- function(ext)
{
  supported <- c("rdata", "rds", "csv", "xlsx", "xls", "sav", "txt", "mat", "dat", "")

  # Robustness fix (Phase 2 review, per direct request): the original's
  # bare "File extension not supported" gave no indication of what was
  # actually received or what's actually supported -- every other
  # `<name>_errors()` message in this rebuild names both. Matched here for
  # consistency, and given `call. = FALSE` like the rest of the family.
  if(!ext %in% supported){
    stop(
      paste0(
        "In `read_data()`: file extension '", ext, "' is not supported.\n",
        "Supported extensions: ", paste0("'", supported[supported != ""], "'", collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
}
