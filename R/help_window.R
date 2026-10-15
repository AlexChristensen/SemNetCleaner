#' @noRd
# The manual spell-check help as a separate browser window that can sit beside the console
# It is a static legend of every menu option, numbered for the current `keepStrings`
# setting, so it can stay open between decisions. Labels come from
# `wizard_menu_labels()` so they cannot drift from the menu; `help_descriptions`
# supplies the explanation of each label
#
# Used when R is interactive (or `options(SemNetCleaner.help_window = TRUE)`);
# `options(SemNetCleaner.help_window = FALSE)` keeps the console help, which is also the
# fallback when the window cannot be opened
# The instructions for reviewing the automated corrections (`correct_changes()`) use the same
# window setting (`build_review_html()` and `open_review_window()` below)
# Descriptions of each menu option
help_descriptions <- c(
  # Single-response options
  "SKIP" = "Keeps the target word 'as is' and moves on to the next response",
  "ADD TO DICTIONARY" = "Adds the target word to the dictionary for future spell-checking",
  "TYPE MY OWN" = "Lets you type your own correction for the target word (separate several words with commas)",
  "GOOGLE IT" = "Opens your default browser and 'Googles' the target word",
  "BAD WORD" = "Marks the target word as an inappropriate response (NA)",
  # Word options of multi-word responses
  "SKIP WORD" = "Keeps the target word 'as is' and moves on to the next word to be spell-checked",
  "ADD WORD TO DICTIONARY" = "Adds the target word to the dictionary for future spell-checking",
  "TYPE MY OWN WORD" = "Lets you type your own correction for the target word (separate several words with commas)",
  "GOOGLE WORD" = "Opens your default browser and 'Googles' the target word",
  # String options of multi-word responses
  "KEEP ORIGINAL" = "Reverts the string back to the original string the participant provided",
  "KEEP AUTO-CORRECT" = "Keeps the string 'as is', with the auto-corrected string provided by the automated spell-check",
  "TYPE MY OWN STRING" = "Lets you type your own correction for the whole original string (separate several words with commas)",
  "GOOGLE STRING" = "Opens your default browser and 'Googles' the original string",
  "BAD STRING" = "Marks the entire string as an inappropriate response (NA)"
)

#' @noRd
# Check for help window ----
# Updated 14.10.2026
help_window_enabled <- function()
{

  # Get user setting
  option <- getOption("SemNetCleaner.help_window", NULL)

  # Check for user setting
  if(!is.null(option)){

    # Return user setting
    return(isTRUE(option))

  }

  # Default to interactive sessions
  return(interactive())

}

#' @noRd
# Escape HTML ----
# Updated 14.10.2026
help_html_escape <- function(
    x # text to escape
)
{

  # Escape ampersands
  x <- gsub("&", "&amp;", x, fixed = TRUE)

  # Escape opening brackets
  x <- gsub("<", "&lt;", x, fixed = TRUE)

  # Escape closing brackets and return
  return(gsub(">", "&gt;", x, fixed = TRUE))

}

#' @noRd
# Help page frame ----
# The top of a self-contained page (inline CSS, no external requests, light and dark
# themes, readable in a narrow side-by-side window); the page body follows it
# Updated 14.10.2026
help_page_top <- function(
    title # title of the page
)
{

  # Build the top of the page
  top <- c(
    # Document header
    "<!DOCTYPE html>",
    '<html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    paste0("<title>", title, "</title>"),
    # Page styles
    "<style>",
    ":root{--bg:#f6f7f9;--fg:#1c2430;--muted:#5d6877;--line:#d9dee6;--card:#fff;--accent:#1b6b57;--keybg:#e4eee9}",
    "@media (prefers-color-scheme: dark){:root{--bg:#12171d;--fg:#e6eaf0;--muted:#9aa5b4;--line:#2b3541;--card:#1a2129;--accent:#4fbf9f;--keybg:#1f3a32}}",
    "*{box-sizing:border-box}",
    "body{background:var(--bg);color:var(--fg);font:15px/1.5 system-ui,-apple-system,'Segoe UI',sans-serif;margin:0;padding:16px}",
    "main{max-width:34rem;margin:0 auto;display:flex;flex-direction:column;gap:14px}",
    "h1{font-size:1.25rem;margin:0}h2{font-size:.78rem;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);margin:0 0 8px}",
    "section{background:var(--card);border:1px solid var(--line);border-radius:6px;padding:12px 14px}",
    "ul{list-style:none;margin:0;padding:0;display:flex;flex-direction:column;gap:8px}",
    "li{display:flex;gap:10px;align-items:flex-start}.key{flex:none;min-width:2.1rem;text-align:center;font:600 .9rem ui-monospace,Menlo,Consolas,monospace;background:var(--keybg);color:var(--accent);border-radius:4px;padding:2px 6px}",
    ".what{min-width:0;overflow-wrap:anywhere}p{margin:0;color:var(--muted)}code{font-family:ui-monospace,Menlo,Consolas,monospace}"
  )

  # Return the top of the page
  return(top)

}

#' @noRd
# Build help page ----
# The self-contained HTML page (inline CSS, no external requests, light and
# dark themes, readable in a narrow side-by-side window)
# Updated 14.10.2026
build_help_html <- function(
    keep_strings = FALSE # whether strings are kept
)
{

  # Create list of options with keys
  legend <- function(labels, first = 1){

    # Fill list items
    sprintf(
      '<li><span class="key">%d</span><span class="what"><strong>%s</strong><br>%s</span></li>',
      seq_along(labels) + first - 1, help_html_escape(labels), help_html_escape(unname(help_descriptions[labels]))
    )

  }

  # Get single-response labels
  single <- wizard_menu_labels(NULL, keep_strings)

  # Get multi-word labels
  multi <- wizard_menu_labels(c("a", "b"), keep_strings)

  # Get word labels
  word <- multi[1:5]

  # Get string labels
  string <- multi[-(1:5)]

  # Get letters for suggestions
  letters_used <- wizard_response_letters(10)

  # Build page
  html <- c(
    help_page_top("SemNetCleaner help"),
    # Page body
    "</style></head><body><main>",
    "<h1>SemNetCleaner: manual spell-check help</h1>",
    "<p>Keep this window beside your R console. Type the number or letter in the console and press ENTER. Lowercase is fine.</p>",
    # Single-response options
    "<section><h2>One-word responses</h2><ul>", legend(single), "</ul></section>",
    # Word options
    "<section><h2>Multi-word responses: word options</h2>",
    "<p>The <em>target word</em> is the highlighted word inside the response.</p><br><ul>", legend(word), "</ul></section>",
    # String options
    sprintf("<section><h2>Multi-word responses: string options</h2><ul>%s</ul></section>", paste(legend(string, first = 6), collapse = "")),
    # Suggestion options
    sprintf("<section><h2>Response options</h2><p>The letters %s are the best guesses for the target word, shown under <em>Response options</em> in the console. Type the letter to use that guess.</p></section>",
            paste0("<code>", letters_used, "</code>", collapse = " ")),
    # Other keys
    "<section><h2>Other keys</h2><ul>",
    '<li><span class="key">B</span><span class="what"><strong>GO BACK</strong><br>Takes you back one decision (use it if you make a mistake). From the first decision of a response it returns to the previous response and asks about it again from its first word.</span></li>',
    '<li><span class="key">H</span><span class="what"><strong>HELP</strong><br>Opens this window again.</span></li>',
    '<li><span class="key">X</span><span class="what"><strong>EXIT</strong><br>Exits <code>textcleaner()</code> and saves your progress, so you can continue later with <code>continue = </code>.</span></li>',
    "</ul></section>",
    # Note about the keepStrings setting
    if(keep_strings) "<p>This session uses <code>keepStrings = TRUE</code>, so there is no KEEP AUTO-CORRECT option and the string options are numbered 6 to 9.</p>" else "<p>This session uses <code>keepStrings = FALSE</code>, so the string options are numbered 6 to 10.</p>",
    # Close page
    "</main></body></html>"
  )

  # Return page as one string
  return(paste(html, collapse = "\n"))

}

#' @noRd
# Open help window ----
# Writes the page and opens it with `utils::browseURL()` (a separate browser
# window or tab; in RStudio, the default browser). Returns whether it opened.
# Updated 14.10.2026
open_help_window <- function(
    keep_strings = FALSE, # whether strings are kept
    open_fn = utils::browseURL # function used to open the page
)
{

  # Set path to the page
  path <- file.path(tempdir(), "SemNetCleaner-help.html")

  # Write and open page (without stopping on failure)
  opened <- try({
    # Write page
    writeLines(build_help_html(keep_strings), path, useBytes = TRUE)
    # Open page
    open_fn(path)
    # Mark as opened
    TRUE
  }, silent = TRUE)

  # Return whether the page opened
  return(isTRUE(opened))

}

#' @noRd
# Build review page ----
# The instructions for reviewing the automated corrections, as a page for a separate
# window; `last_column` is the number of the last column with automated responses
# Updated 14.10.2026
build_review_html <- function(
    last_column # number of the last column of the review table
)
{

  # Build page
  html <- c(
    help_page_top("SemNetCleaner review"),
    # Page body
    "</style></head><body><main>",
    "<h1>SemNetCleaner: review of the automated corrections</h1>",
    "<p>A spreadsheet opens next so that you can correct the changes made during the automated spell-check. Keep this window beside it, and press ENTER in the console when you are ready to continue.</p>",
    # Columns
    "<section><h2>The columns</h2><ul>",
    '<li><span class="key">1</span><span class="what"><strong>Row number</strong><br>The row number in the output object <code>$spellcheck$correspondence</code> (see <code>?textcleaner</code> for more information about this output).</span></li>',
    '<li><span class="key">2</span><span class="what"><strong>Original response</strong><br>The response the participant provided.</span></li>',
    sprintf(
      '<li><span class="key">3-%d</span><span class="what"><strong>Automated responses</strong><br>The responses from the automated spell-check, in columns named <code>to_#</code>.</span></li>',
      last_column
    ),
    "</ul></section>",
    # Editing
    "<section><h2>How to edit</h2><ul>",
    sprintf(
      '<li><span class="key">3-%d</span><span class="what"><strong>Type your corrections</strong><br>Change these columns by typing responses.</span></li>',
      last_column
    ),
    '<li><span class="key">NA</span><span class="what"><strong>Inappropriate response</strong><br>Type <code>NA</code> for a response that should be removed.</span></li>',
    '<li><span class="key">Done</span><span class="what"><strong>Finish</strong><br>Click <em>Done</em> when you are finished (or the <em>X</em> if you use the lightweight spreadsheet editor with <code>edit_fn = edit</code>).</span></li>',
    "</ul></section>",
    # Close page
    "</main></body></html>"
  )

  # Return page as one string
  return(paste(html, collapse = "\n"))

}

#' @noRd
# Open review window ----
# Writes the review instructions and opens them with `utils::browseURL()` (a separate
# browser window or tab). Returns whether it opened.
# Updated 14.10.2026
open_review_window <- function(
    last_column, # number of the last column of the review table
    open_fn = utils::browseURL # function used to open the page
)
{

  # Set path to the page
  path <- file.path(tempdir(), "SemNetCleaner-review.html")

  # Write and open page (without stopping on failure)
  opened <- try({
    # Write page
    writeLines(build_review_html(last_column), path, useBytes = TRUE)
    # Open page
    open_fn(path)
    # Mark as opened
    TRUE
  }, silent = TRUE)

  # Return whether the page opened
  return(isTRUE(opened))

}
