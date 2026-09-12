# Small helpers shared across modules -- app/UI glue only, no SemNetCleaner
# pipeline logic lives here. Ported from ../SemNeT/inst/Shiny/R/mod_shared.R,
# with its progress-file option renamed to SemNetCleaner.progress_file to
# match this package's own parallel_process() (helpers.R).

# A fresh per-invocation progress file pair: `file` holds the current total
# (pre-populated with "1" so a poller reading it before the task's first
# real write sees a valid, if uninformative, value rather than an empty
# file), and the sibling "<file>.progress" is appended to once per completed
# iteration by parallel_process() (see helpers.R for why a completion
# *count* there, rather than the raw iteration index here, is what stays
# monotonic under `cores > 1`).
new_progress_file <- function()
{
  file <- tempfile()
  writeLines("1", file)
  file.create(paste0(file, ".progress"))
  return(file)
}

# Live per-iteration progress for a long-running ExtendedTask. progressr's
# own cross-process relay only reaches a caller that synchronously blocks
# waiting on the future -- it delivers nothing to a
# shiny::ExtendedTask/promises::future_promise() caller, which never blocks
# that way. Instead, the task itself is invoked with
# options(SemNetCleaner.progress_file = <this file>) set INSIDE its own
# future_promise() body (see mod_auto_progress.R), so parallel_process()
# (helpers.R) writes completion counts there directly from the background
# worker. `file_reactive` is a reactiveVal() holding the current task's file
# path (NULL before any task has been invoked yet).
progress_poller <- function(session, file_reactive)
{

  shiny::reactivePoll(
    400, session,
    checkFunc = function(){
      file <- file_reactive()
      if(is.null(file) || !file.exists(file)){
        return(NULL)
      }
      progress_file <- paste0(file, ".progress")
      if(file.exists(progress_file)) file.info(progress_file)$size else NA
    },
    valueFunc = function(){

      file <- file_reactive()

      if(is.null(file) || !file.exists(file)){
        return(NULL)
      }

      total <- suppressWarnings(as.numeric(readLines(file, n = 1, warn = FALSE)))

      if(length(total) != 1 || anyNA(total)){
        return(NULL)
      }

      progress_file <- paste0(file, ".progress")
      current <- if(file.exists(progress_file)) file.info(progress_file)$size else 0

      return(list(current = current, total = total))

    }
  )

}

# A real progress bar while `task` is running (falling back to a plain
# "Running..." message before the first real progress value has been
# written yet), or an error display. `progress` is a reactive as returned
# by progress_poller().
progress_status_ui <- function(task, progress)
{

  status <- task$status()

  if(status == "error"){
    message <- tryCatch(task$result(), error = function(e) conditionMessage(e))
    return(shiny::tags$p(style = "color: #a94442;", paste("Failed:", message)))
  }

  if(status != "running"){
    return(NULL)
  }

  info <- progress()

  if(is.null(info) || info$total <= 0){
    return(shiny::tags$p(shiny::tags$em("Running... this tab will update when it's done.")))
  }

  percent <- round(100 * min(info$current / info$total, 1))

  shiny::tags$div(
    style = "width: 100%; max-width: 400px; background: #e9ecef; border-radius: 4px; overflow: hidden; margin-bottom: 10px;",
    shiny::tags$div(
      style = paste0(
        "width: ", percent, "%; background: #337ab7; color: white; text-align: center; ",
        "padding: 4px 0; white-space: nowrap; transition: width 0.3s ease;"
      ),
      paste0(percent, "% (", info$current, " / ", info$total, ")")
    )
  )

}
