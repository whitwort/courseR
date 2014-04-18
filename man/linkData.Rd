\name{linkData}
\alias{linkData}
\title{Post processor that adds <a href> tags to all strings referencing data files.}
\usage{
linkData(text, context, templates)
}
\arguments{
  \item{text}{a character string containing the text of the
  output document}

  \item{context}{a list containing post processing
  environment variables}

  \item{templates}{a list containing strings holding the
  contents of template documents}
}
\value{
a string character containing the new text of the output
document
}
\description{
This post processor assumes that content files referencing
external data containg a 'data' field in their headers with
relative paths to data files; these directory holding data
files should also be included in the 'sourceExternals'
list.
}
\details{
All post processor take exactly three arguments (passed in
by the courseR framework).
}

