## TODO: depends
#library(devtools); install_github('knitr', 'yihui')
library(knitr)
library(markdown)
library(highlight)

makeSite <- function(html.template, 
                     pages,
                     page.css,
                     page.js,
                     index,
                     index.section,
                     index.entry,
                     index.css,
                     index.js
                     ) {
  
  #for each page, build the HTML resource
  for (i in 1:nrow(pages)) {
    
    #The row containing information about this page from pages table
    page <- pages[i,]
    
    #Start building this source
    message("Building page for ", page$name)
    
    #renderHTML from the rmd precursor
    renderHTML(sourcePath   = page$file,
               templatePath = html.template,
               savePath     = paste(splitExt(page$file), ".html", sep=""),
               css          = page.css,
               js           = page.js,
               title        = page$name
               )
  
  }
  
  #Compile the index by sections
  sections <- by(pages, pages$section,
                function(entry) {
                  
                  #use index.entry template to render the md representation of entries in this section
                  sectionLines <- by(entry, entry$name,
                                     function(page) {
                                       
                                       #For some reason MD won't accept relative refs 
                                       #(like "page.html") as a link, so we'll
                                       #hard code it...
                                       pageLink <- paste("<a href=", 
                                                         paste(splitExt(page$file), ".html", sep=""),
                                                         ">",
                                                         page$name, 
                                                         "</a>", 
                                                         sep="")
                                       
                                       renderTemplate(
                                         template    = readLines(index.entry), 
                                         name        = pageLink,
                                         description = page$description
                                          )
                                     })
                  
                  #use index.section ro render the md respresentation of this section
                  return(
                    renderTemplate(
                      template = readLines(index.section),
                      section  = entry$section[1], 
                      entries  = sectionLines
                      )
                    )
                })
  
  #Render the final compiled markdown using the index template
  indexMD <- renderTemplate(template = readLines(index), 
                            index    = sections)
  
  #Now compile the markdown to html, and save as index.html
  innerIndex <- rawToChar(renderMarkdown(text = indexMD))
  indexHTML <- renderTemplate(template  = readLines(html.template), 
                              title     = 'Table of contents',
                              content   = innerIndex,
                              css       = index.css,
                              js        = index.js
                              )
  
  write(indexHTML, "index.html")
  
}

#Renders a single HTML page from md source and a page template
renderHTML <- function(sourcePath, templatePath, 
                       savePath   = sub(".rmd", ".html", basename(sourcePath), fixed = TRUE),
                       imagePath  = "img/",
                       css        = "", 
                       js         = "",
                       title      = splitExt(savePath),
                       cleanup    = TRUE
                       ) {
  
  #Put knitr in HTML mode (however, it only emits HTML for code blocks)
  render_html()
  
  #Override knitr hooks to make things prettier
  knit_hooks$set(
    
    #Add code highlighting to source (the documentation makes it sound like this
    #should already be happening, but it isn't)
    source  = function(x, options) {
      sprintf("<div class=\"%s\">%s</div>", 'source', codeHighlight(x))
    }
    
  )
  
  #Setup knitr package options
  opts_knit$set(
    progress = FALSE          #Make knitr less verbose
  )
  
  #Setup knitr chunk options
  opts_chunk$set(
    fig.width   = 5,
    fig.height  = 5,
    fig.path    = imagePath,
    comment     = "",         #Get rid of leading "##" on each line
    tidy        = FALSE       #Hopefully our code is more tidy than formatR's!
  )
  
  #Produce the markdown intermediate using knitr
  message("Running knitr... (", sourcePath, ")")
  mdFile <- knit(input = sourcePath)
  
  #Use the markdown package to convert the md intermediate to HTML
  message("Running markdown... (", mdFile, ")")
  contentHTML <- rawToChar(renderMarkdown(file = mdFile))
  
  #Load template lines
  template <- readLines(templatePath)
  
  #Use the template engine to produce the final HTML
  html <- renderTemplate(template = template,
                         css      = css,
                         js       = js,
                         content  = contentHTML,
                         title    = title
                         )
  
  #Save the html to the a file
  message(paste("Output HTML file:", savePath))
  write(html, savePath)
  
  #Clean up intermediate files
  if (cleanup) {
    file.remove(mdFile)
  }
  
}

#Templating engine (uses handlebars to match replacements)
renderTemplate <- function(template, ...,  rep = TRUE) {
  
  #Listify ... to get replacements
  replacements <- list(...)
  
  #For each replacement we've been given
  for (tag in names(replacements)) {
    
    #Calculate the pattern using our handlebars-like syntax
    pattern <- paste("{{", tag, "}}", sep="")
    
    if (rep) {
      
      replacementLines <- replacements[[tag]]
      
    } else {
    
      #If rep = FALSE, collapse replacementLines into a single string so that matched lines aren't repeated
      replacementLines <- paste(replacements[[tag]], collapse="\n", sep="")
      
    }
    
    #For each line in the template
    for (line in template) {
      
      #Get the template lines in which this pattern is found (numeric vector)
      matchLines <- grep(pattern  = pattern,
                         x        = template, 
                         fixed    = TRUE, 
                         value    = FALSE)
      
      #If there were hits
      if (length(matchLines) > 0) {
        
        #A little functional alchemy here: this will produce a Matrix where each
        #row contains the character vector with new lines to replace originals
        newText <- sapply(replacementLines,  
                          FUN = function(replacement) {
                            sub(pattern     = pattern, 
                                replacement = replacement, 
                                x           = template[matchLines], 
                                fixed       = TRUE
                                )
                          },
                          USE.NAMES = FALSE
                          )
        
        #If more than one instance of the pattern was found, we'll have a matrix
        if (class(newText) == "matrix") {
        
          #Now all we need to do is collapse each row into a string with "\n" and
          #shove the new strings back into template at the original match
          #locations
          template[matchLines] <-
            apply(newText,
                  MARGIN = 1, 
                  FUN    = function(x) { paste(x, collapse = "\n", sep = "") }
                  )
        
        #Otherwise, we'll just have a character vector; collapse it and shove into template
        } else {
          template[matchLines] <- paste(newText, collapse = "\n", sep = "")
        }
        
      }
    }
  
  }
  
  #Now collapse the lines of template into a single string with "\n"
  return( paste(template, collapse = "\n", sep = "") )
  
}

#Extensions to knitr hooks
codeHighlight <- function(code) {
  
  #There doesn't seem to be any way around dumping the data to a file
  write(x = code, file = 'highlight-tmp.r')
  
  #Little closure to heep highlight from adding cruft before and after spans
  blank <- function(doc) { "" }
  
  #Send serialized code to highlight, which will parse(...) and return a char vector
  prettyCode <- paste(
      highlight(file      = 'highlight-tmp.r',
                output    = 'output-tmp.html',
                renderer  = renderer_html(header = blank, footer = blank)
                ),
      collapse  = "",
      sep       = ""
      )
  #Clean up our temporary files
  file.remove("highlight-tmp.r","output-tmp.html")
  
  #return the html
  return(prettyCode)
  
}

#Convenience functions for use in R-chunks
drawMath <- function(expr) {
  
  #Make an empty plot with a single white point
  plot(c(0), xlab = "", ylab = "", axes = FALSE, col = "white")
  
  #Put the equation text in the center
  text(c(0,0), expr)
  
}

#utilities
splitExt <- function(filePath) {
  
  strsplit(
    basename(filePath),
    split = ".",
    fixed = TRUE)[[1]][1]
  
}
