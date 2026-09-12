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
#' @export
#' @importFrom tools file_ext
# Read a Shiny-uploaded file
# New (Phase 11). `.Rdata` is deliberately NOT dispatched to `read_data()`:
# that function's own `.Rdata` branch does `load(file, envir = .GlobalEnv)`
# -- writing directly into the GLOBAL environment is a real hazard inside a
# shared, long-running Shiny process (one session's upload could clobber
# another's globals, or the app's own). Loaded into a private `new.env()`
# instead, erroring clearly unless the file contains exactly one object
# (ambiguous otherwise -- which object was the user's actual dataset?).
# Updated 12.09.2026
read_uploaded_file <- function(datapath, filename, header = TRUE, sep = ",")
{

  read_uploaded_file_errors(datapath, filename)

  ext <- tools::file_ext(filename)

  # `datapath`'s own name is a shiny-managed temp filename (e.g. "0.dat" or
  # similar), not guaranteed to carry `filename`'s real extension --
  # `read_data()` dispatches purely off the path's own extension, so a copy
  # with the real extension is made before handing off (never a rename: the
  # original `datapath` isn't ours to move, shiny manages its own cleanup).
  staged_path <- if(nzchar(ext)){
    staged <- tempfile(fileext = paste0(".", ext))
    file.copy(datapath, staged, overwrite = TRUE)
    staged
  }else{
    datapath
  }

  if(tolower(ext) == "rdata"){

    load_env <- new.env(parent = emptyenv())
    load(staged_path, envir = load_env)

    objects <- ls(load_env)

    if(length(objects) != 1){
      stop(
        paste0(
          "In `read_uploaded_file()`: \"", filename, "\" must contain exactly ",
          "one object to be used as uploaded data (found ", length(objects), ")."
        ),
        call. = FALSE
      )
    }

    return(load_env[[objects]])

  }

  read_data(staged_path, header = header, sep = sep)

}

#' @noRd
# Up-front validation for `read_uploaded_file()`
# Updated 12.09.2026
read_uploaded_file_errors <- function(datapath, filename)
{

  typeof_error(datapath, "character", "read_uploaded_file")
  length_error(datapath, 1, "read_uploaded_file")
  typeof_error(filename, "character", "read_uploaded_file")
  length_error(filename, 1, "read_uploaded_file")

  if(!file.exists(datapath)){
    stop(
      paste0("In `read_uploaded_file()`: \"", datapath, "\" does not exist."),
      call. = FALSE
    )
  }

}
