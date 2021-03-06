% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_meta.R
\name{add_code_of_conduct}
\alias{add_code_of_conduct}
\title{Add a CODE_OF_CONDUCT.md file to the project directory}
\usage{
add_code_of_conduct(path = ".", package = FALSE,
  coc_email = getOption("bcgovr.coc.email", default = NULL))
}
\arguments{
\item{path}{Directory path (default \code{"."})}

\item{package}{Is this a package or a regular project? (Default \code{FALSE}). 
If \code{TRUE}, "CODE_OF_CONDUCT.md" will be added to .Rbuildignore}

\item{coc_email}{Contact email address(es) for the Code of Conduct.}
}
\value{
\code{TRUE} (invisibly)
}
\description{
Add a CODE_OF_CONDUCT.md file to the project directory
}
\seealso{
\code{\link{add_readme}}, \code{\link{add_license}}, \code{\link{add_license_header}}
}
