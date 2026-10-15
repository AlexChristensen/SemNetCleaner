# OnAttach ----
# Updated 14.10.2026
.onAttach <- function(libname, pkgname)
{

    # Create bold, underlined version header
    msg <- style_text(style_text(paste("\nSemNetCleaner (version ", packageVersion("SemNetCleaner"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    # Add pointer to the package paper
    msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')

    # Add pointer to the issue tracker
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNetCleaner/issues>\n\n")
    
    # Print startup message
    packageStartupMessage(msg)

}
