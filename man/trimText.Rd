\name{trimText}
\alias{trimText}
\title{Post processor that removes long <p> elements}
\usage{
trimText(text, context, templates)
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
This post processor assumes that a trimTextCutoff binding
is defined on the templating data context.
}
\details{
All post processor take exactly three arguments (passed in
by the courseR framework).
}

