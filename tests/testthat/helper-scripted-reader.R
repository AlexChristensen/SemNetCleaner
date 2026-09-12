# Builds a `reader` function -- the contract every interactive SemNetCleaner
# function accepts via its `reader = readline` argument, `function(prompt = "")`
# -- that pops scripted answers off a fixed queue instead of reading real
# console input. This is what makes the console wizard testable end-to-end
# (see decision #3 / read_input() in R/helpers.R).
scripted_reader <- function(answers)
{

  answers <- as.character(answers)
  index <- 0L

  function(prompt = "")
  {
    index <<- index + 1L

    if(index > length(answers)){
      stop(
        "scripted_reader() exhausted: no more scripted answers (prompt: '", prompt, "')",
        call. = FALSE
      )
    }

    answers[[index]]
  }

}
