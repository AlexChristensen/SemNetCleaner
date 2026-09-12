.onAttach <- function(libname, pkgname)
{
    msg <- style_text(style_text(paste("\nSemNetCleaner (version ", packageVersion("SemNetCleaner"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')
    msg <- paste(msg, "New: try the point-and-click Shiny app with `textcleaner_shiny()`\n")
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNetCleaner/issues>\n\n")
    
    packageStartupMessage(msg)
}