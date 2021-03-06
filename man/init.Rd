% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/courseR.r
\name{init}
\alias{init}
\title{Initialize a new project}
\usage{
init(path = getwd(), name = basename(path), overwrite = TRUE,
  examples = TRUE)
}
\arguments{
\item{path}{path to use to create a new project}

\item{name}{name of the new project}

\item{overwrite}{overwrite existing files or not}

\item{examples}{should example content and assignment files be created}
}
\description{
Creates the needed file structure at the given path
}
