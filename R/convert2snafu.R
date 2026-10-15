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
#' # Convert data to SNAFU (\code{reader} supplies the file name)
#' convert2snafu(
#'   open.clean[1:5, ], category = "animals", path = tempdir(),
#'   reader = function(prompt = "") "snafu_example"
#' )
#'
#' if(interactive())
#' {
#'   # Asks for the file name
#'   convert2snafu(open.clean, category = "animals", path = tempdir())
#' }
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
# Convert to SNAFU format ----
# Converts one or more response matrices to the SNAFU format and writes a csv file to
# `path`; each dataset is a group and the file name is asked for
# Updated 14.10.2026
convert2snafu <- function(
    ..., # datasets to convert
    category, # name of the category
    path, # directory to save file
    reader = readline # function used to read file name
)
{

  # Check for missing arguments
  if(missing(category) || missing(path)){

    # Send error
    stop(
      "In `convert2snafu()`: `category` (e.g., \"animals\") and `path` (the folder to save the file in) are required.",
      call. = FALSE
    )

  }

  # Get datasets
  data_list <- list(...)

  # Initialize SNAFU matrix
  snafu_mat <- matrix(0, nrow = 0, ncol = 7)

  # Name columns
  colnames(snafu_mat) <- c("id", "listnum", "category", "item", "RT", "RTstart", "group")

  # Check for datasets
  if(length(data_list) != 0){

    # Get dataset names
    name <- as.character(substitute(list(...)))

    # Remove list name
    name <- name[-which(name == "list")]

    # Loop over datasets
    for(i in seq_along(data_list)){

      # Get dataset as matrix
      target_data <- as.matrix(data_list[[i]])

      # Check for row names
      if(is.null(row.names(target_data))){

        # Create IDs from row numbers
        id <- paste("A", seq_len(nrow(target_data)), sep = "")

      }else{

        # Create IDs from row names
        id <- paste("A", formatC(as.numeric(row.names(target_data)), digits = 2, format = "d", flag = 0), sep = "")

      }

      # Loop over participants
      for(j in seq_len(nrow(target_data))){

        # Get participant responses
        target_part <- target_data[j, ]

        # Remove missing responses
        item <- na.omit(target_part)

        # Set participant ID
        target_id <- rep(id[j], length(item))

        # Set list number
        listnum <- rep(0, length(item))

        # Set category
        categorey <- rep(category, length(item))

        # Set response time
        RT <- rep(0, length(item))

        # Set response time start
        RTstart <- rep(0, length(item))

        # Set group
        group <- rep(name[i], length(item))

        # Combine columns
        target_mat <- cbind(target_id, listnum,
                            categorey, item,
                            RT, RTstart, group)

        # Remove row names
        row.names(target_mat) <- NULL

        # Match column names
        colnames(target_mat) <- colnames(snafu_mat)

        # Add participant to SNAFU matrix
        snafu_mat <- rbind(snafu_mat, target_mat)

      }

    }

  }

  # Get file name
  filename <- read_input("Name of file: ", reader = reader)

  # Create full path
  full_path <- file.path(path, paste(filename, "csv", sep = "."))

  # Write file
  write.table(snafu_mat, file = full_path,
              quote = FALSE, sep = ",", row.names = FALSE)

  # Send location of saved file
  message(paste("SNAFU formatted file was saved in: "), full_path)

}
