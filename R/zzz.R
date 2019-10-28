.onload <- function(libname, pkgname)
{library.dynam("SemNetCleaner",package=pkgname,lib.loc=libname)}

.onAttach <- function(libname, pkgname)
{
    msg <- paste('For help getting started, type `browseVignettes("SemNetCleaner")` \n')
    msg <- paste(msg,"For bugs and errors, submit an issue to <https://github.com/AlexChristensen/SemNetCleaner/issues>")
    packageStartupMessage(msg)
}