.onload <- function(libname, pkgname)
{library.dynam("SemNetCleaner",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- styletext(styletext(paste("\nSemNetCleaner (version ", packageVersion("SemNetCleaner"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.1037/met0000463> \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNetCleaner/issues>\n\n")
    
    packageStartupMessage(msg)
}