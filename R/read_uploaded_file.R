#' Read a File Uploaded Through Shiny
#'
#' @description Reads a file uploaded via \code{shiny::fileInput()} (Phase
#' 11, \code{textcleaner_shiny()}'s own upload step). Shiny stores an
#' uploaded file at a temporary path (\code{datapath}) that does not
#' necessarily carry the original file's extension, but \code{\link{read_data}}
#' dispatches purely by sniffing the path's own extension -- this function
#' bridges the two by renaming/copying \code{datapath} to one ending in the
#' real, original extension (from \code{filename}, the name the browser
#' reported) before handing off.
#'
#' @param datapath Character.
#' The temporary file path \code{shiny::fileInput()} stored the upload at
#' (\code{input$file$datapath})
#'
#' @param filename Character.
#' The original file name the browser reported
#' (\code{input$file$name}) -- used only for its extension
#'
#' @param header Boolean.
#' Passed through to \code{\link{read_data}}. Defaults to \code{TRUE}
#'
#' @param sep Character.
#' Passed through to \code{\link{read_data}}. Defaults to \code{","}
#'
#' @return The data read from the uploaded file (a data frame for every
#' supported extension except \code{.Rdata}, an unmodified round-trip)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @keywords internal
#' @importFrom tools file_ext
# Read uploaded file ----
# Reads a file uploaded to the internal Shiny app; `.Rdata` is loaded into a private
# environment (the global one is shared in a long-running Shiny process) and must hold
# exactly one object
# Updated 14.10.2026
read_uploaded_file <- function(
    datapath, # temporary path of uploaded file
    filename, # original name of uploaded file
    header = TRUE, # first row contains names
    sep = "," # separator for delimited files
)
{

  # Check for errors
  read_uploaded_file_errors(datapath, filename)

  # Get file extension
  ext <- tools::file_ext(filename)

  # The temporary name of an upload does not carry the real extension that `read_data()`
  # dispatches on, so a copy with the real extension is read (the upload itself is left alone)
  staged_path <- if(nzchar(ext)){

    # Create temporary path with the real extension
    staged <- tempfile(fileext = paste0(".", ext))

    # Copy uploaded file
    file.copy(datapath, staged, overwrite = TRUE)

    # Return staged path
    staged

  }else{

    # Use uploaded path
    datapath

  }

  # Check for R data file
  if(tolower(ext) == "rdata"){

    # Create empty environment
    load_env <- new.env(parent = emptyenv())

    # Load file into environment
    load(staged_path, envir = load_env)

    # Get object names
    objects <- ls(load_env)

    # Check for exactly one object
    if(length(objects) != 1){

      # Send error
      stop(
        paste0(
          "In `read_uploaded_file()`: \"", filename, "\" must contain exactly ",
          "one object to be used as uploaded data (found ", length(objects), ")."
        ),
        call. = FALSE
      )

    }

    # Return the object
    return(load_env[[objects]])

  }

  # Read all other formats
  return(read_data(staged_path, header = header, sep = sep))

}

#' @noRd
# Read uploaded file errors ----
# Up-front validation for `read_uploaded_file()`
# Updated 14.10.2026
read_uploaded_file_errors <- function(
    datapath, # temporary path of uploaded file
    filename # original name of uploaded file
)
{

  # Check path type
  typeof_error(datapath, "character", "read_uploaded_file")

  # Check path length
  length_error(datapath, 1, "read_uploaded_file")

  # Check name type
  typeof_error(filename, "character", "read_uploaded_file")

  # Check name length
  length_error(filename, 1, "read_uploaded_file")

  # Check for existing file
  if(!file.exists(datapath)){

    # Send error
    stop(
      paste0("In `read_uploaded_file()`: \"", datapath, "\" does not exist."),
      call. = FALSE
    )

  }

}
