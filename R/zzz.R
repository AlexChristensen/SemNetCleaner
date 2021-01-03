.onload <- function(libname, pkgname)
{library.dynam("SemNetCleaner",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- styletext(styletext(paste("\nSemNetCleaner (version ", packageVersion("SemNetCleaner"), ")", sep = ""), defaults = "underline"), defaults = "bold")
    
    msg <- paste(msg, '\nFor help getting started, see <https://doi.org/10.31234/osf.io/eht87> \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNetCleaner/issues>\n\n")
    msg <- paste(msg, "WARNING: There have been major updates to the SemNetCleaner package.\n Please see 'Package NEWS' for a detailed list of updates (see 'Changes in version 1.2.0')\n", sep = "")
    
    packageStartupMessage(msg)
}