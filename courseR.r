## TODO: depends
#library(devtools); install_github('knitr', 'yihui')
#install.packages('knitr', dependencies = TRUE)
library(knitr)
library(markdown)
library(highlight)

render <- function(sourcePath) {
  render_html()
  mdFile <- knit(sourcePath)
  renderMarkdown(file = mdFile, output = "example.html")
}

drawMath <- function(expr) {
  
  #Make an empty plot with a single white point
  plot(c(0), xlab="", ylab="", axes=FALSE, col="white")
  
  #Put the equation text in the center
  text(c(0,0), expr)
  
}