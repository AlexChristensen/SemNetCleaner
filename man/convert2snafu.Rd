% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert2snafu.R
\name{convert2snafu}
\alias{convert2snafu}
\title{Converts \code{\link[SemNetCleaner]{textcleaner}} object
to a SNAFU GUI format}
\usage{
convert2snafu(..., category)
}
\arguments{
\item{...}{Matrix or data frame.
A clean response matrices}

\item{category}{Character.
Category of verbal fluency data}
}
\value{
A .csv file formatted for SNAFU
}
\description{
Converts \code{\link[SemNetCleaner]{textcleaner}} object
to a SNAFU GUI format (only works for fluency data)
}
\details{
The format of the file has 7 columns:
\itemize{

\item id --- Defaults to the row names of the inputted \code{data}

\item listnum --- The list number for the fluency category. Defaults to 0.
Future implementations will allow more lists

\item category --- The verbal fluency category that is input into the
\code{category} argument

\item item --- The verbal fluency responses for every participant

\item RT --- Response time. Currently not implemented. Defaults to 0

\item RTstart --- Start of response time. Currently not implemented.
Defaults to 0

\item group --- Names of groups. Defaults to the names of the objects
input into the function (\code{...})

}
}
\examples{
# Convert data to SNAFU
if(interactive())
{convert2snafu(open.clean, category = "animals")}

}
\references{
# For SNAFU, see:
Zemla, J. C., Cao, K., Mueller, K. D., & Austerweil, J. L. (2020).
SNAFU: The Semantic Network and Fluency Utility.
\emph{Behavior Research Methods}, 1-19.
https://doi.org/10.3758/s13428-019-01343-w
}
\author{
Alexander Christensen <alexpaulchristensen@gmail.com>
}
