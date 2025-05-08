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
#' {convert2snafu(open.clean, category = "animals")}
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
# Updated 19.06.2022
convert2snafu <- function (..., category)
{
  # Data list
  data.list <- list(...)

  # Initialize snafu matrix
  snafu.mat <- matrix(0, nrow = 0, ncol = 7)
  colnames(snafu.mat) <- c("id", "listnum", "category", "item", "RT", "RTstart", "group")

  if(length(data.list) == 1)
  {

  }else{

    # Get group names
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]

    for(i in 1:length(data.list))
    {
      # Target data
      target.data <- as.matrix(data.list[[i]])

      # Number of possible responses
      n <- ncol(target.data)

      # IDs
      if(is.null(row.names(target.data)))
      {id <- paste("A", 1:nrow(target.data), sep = "")
      }else{id <- paste("A", formatC(as.numeric(row.names(target.data)), digits = 2, format = "d", flag = 0), sep = "")}

      for(j in 1:nrow(target.data))
      {
        # Target participant
        target.part <- target.data[j,]

        # Item
        item <- na.omit(target.part)

        # Target ID
        target.id <- rep(id[j], length(item))

        # List number
        listnum <- rep(0, length(item))

        # Category
        categorey <- rep(category, length(item))

        # RT
        RT <- rep(0, length(item))

        # RTstart
        RTstart <- rep(0, length(item))

        # Group
        group <- rep(name[i], length(item))

        # Bind data
        target.mat <- cbind(target.id, listnum,
                            categorey, item,
                            RT, RTstart, group)

        row.names(target.mat) <- NULL
        colnames(target.mat) <- colnames(snafu.mat)

        # Append snafu matrix
        snafu.mat <- rbind(snafu.mat, target.mat)
      }
    }

  }

  # Choose directory
  DIR <- easycsv::choose_dir()

  # Get file name
  FILENAME <- readline("Name of file: ")

  # Set up path
  PATH <- paste(DIR, FILENAME, sep = "/")
  PATH <- gsub("\\\\", "/", PATH)
  PATH <- paste(PATH, "csv", sep = ".")

  write.table(snafu.mat, file = PATH,
              quote = FALSE, sep = ",", row.names = FALSE)

  # Message to user
  message(paste("SNAFU formatted file was saved in: "), PATH)
}
