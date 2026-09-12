#' Converts \code{\link[SemNetCleaner]{textcleaner}} object
#' to a SNAFU GUI format
#'
#' @description Converts \code{\link[SemNetCleaner]{textcleaner}} object
#' to a SNAFU GUI format (only works for fluency data)
#'
#' @param ... Matrix or data frame.
#' A clean response matrices
#'
#' @param category Character.
#' Category of verbal fluency data
#'
#' @param path Character.
#' Directory to save the SNAFU-formatted \code{.csv} file to
#'
#' @param reader Function.
#' The input reader used to prompt for the output file name. Defaults to
#' \code{readline}. Exposed so this can be driven programmatically (e.g. in
#' automated tests) instead of real console input
#'
#' @return A .csv file formatted for SNAFU
#'
#' @details The format of the file has 7 columns:
#' \itemize{
#'
#' \item id --- Defaults to the row names of the inputted \code{data}
#'
#' \item listnum --- The list number for the fluency category. Defaults to 0.
#' Future implementations will allow more lists
#'
#' \item category --- The verbal fluency category that is input into the
#' \code{category} argument
#'
#' \item item --- The verbal fluency responses for every participant
#'
#' \item RT --- Response time. Currently not implemented. Defaults to 0
#'
#' \item RTstart --- Start of response time. Currently not implemented.
#' Defaults to 0
#'
#' \item group --- Names of groups. Defaults to the names of the objects
#' input into the function (\code{...})
#'
#' }
#'
#' @examples
#' # Convert data to SNAFU
#' if(interactive())
#' {convert2snafu(open.clean, category = "animals", path = tempdir())}
#'
#' @references
#' # For SNAFU, see:
#' Zemla, J. C., Cao, K., Mueller, K. D., & Austerweil, J. L. (2020).
#' SNAFU: The Semantic Network and Fluency Utility.
#' \emph{Behavior Research Methods}, 1-19.
#' https://doi.org/10.3758/s13428-019-01343-w
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom utils write.table
#'
#' @export
# Convert data to SNAFU
# Real bug fixed: `if(length(data.list) == 1){}else{...}` -- the untouched
# original's single-dataset branch is completely EMPTY, so the function's
# own documented single-argument use (`convert2snafu(open.clean, category =
# "animals")`) silently wrote out an empty, header-only CSV instead of the
# real data. Confirmed directly that the `else` branch's own group-name
# derivation (`as.character(substitute(list(...)))`, minus the `"list"`
# element -- already audited safe in Phase 6, always deparses with `"list"`
# first for any number of arguments per R's own semantics) works correctly
# for a single dataset too, so the special case was never necessary; merged
# into one unconditional branch.
# Binding decision #5: drops the `easycsv` dependency (`Suggests`-only but
# called unconditionally -- an undeclared-dependency bug in its own right)
# in favor of an explicit `path =` argument, and the interactive filename
# prompt is rebuilt on `read_input()`/a `reader = readline` parameter per
# binding decision #3.
# Bug #8 audit: both `for(i in 1:length(data.list))` and `for(j in
# 1:nrow(target.data))` reverse-range to `1:0` on a 0-dataset/0-row input --
# fixed via `seq_along()`/`seq_len()`, same class as bugs #15-17.
# Updated 12.09.2026
convert2snafu <- function(..., category, path, reader = readline)
{

  data_list <- list(...)

  snafu_mat <- matrix(0, nrow = 0, ncol = 7)
  colnames(snafu_mat) <- c("id", "listnum", "category", "item", "RT", "RTstart", "group")

  if(length(data_list) != 0){

    name <- as.character(substitute(list(...)))
    name <- name[-which(name == "list")]

    for(i in seq_along(data_list)){

      target_data <- as.matrix(data_list[[i]])

      if(is.null(row.names(target_data))){
        id <- paste("A", seq_len(nrow(target_data)), sep = "")
      }else{
        id <- paste("A", formatC(as.numeric(row.names(target_data)), digits = 2, format = "d", flag = 0), sep = "")
      }

      for(j in seq_len(nrow(target_data))){

        target_part <- target_data[j, ]

        item <- na.omit(target_part)

        target_id <- rep(id[j], length(item))
        listnum <- rep(0, length(item))
        categorey <- rep(category, length(item))
        RT <- rep(0, length(item))
        RTstart <- rep(0, length(item))
        group <- rep(name[i], length(item))

        target_mat <- cbind(target_id, listnum,
                            categorey, item,
                            RT, RTstart, group)

        row.names(target_mat) <- NULL
        colnames(target_mat) <- colnames(snafu_mat)

        snafu_mat <- rbind(snafu_mat, target_mat)

      }

    }

  }

  filename <- read_input("Name of file: ", reader = reader)

  full_path <- file.path(path, paste(filename, "csv", sep = "."))

  write.table(snafu_mat, file = full_path,
              quote = FALSE, sep = ",", row.names = FALSE)

  message(paste("SNAFU formatted file was saved in: "), full_path)

}
