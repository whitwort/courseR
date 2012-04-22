## TODO: depends
library(knitr)
library(markdown)
library(highlight)

makeSite <- function(html.template, 
                     pages,
                     css,
                     js,
                     index,
                     index.section,
                     index.entry,
                     
                     #Class and id attributes used in page template and main.js
                     link.class     = 'courserLink',
                     
                     #post-processors finalize the html for pages
                     postProcessor  = revealSlides
                     
                     ) {
  
  #for each page, build the HTML resource
  for (i in 1:nrow(pages)) {
    
    #The row containing information about this page from pages table
    page <- pages[i,]
    
    #Calculate an html file name
    htmlPath <- paste(splitExt(page$file), ".html", sep="")
    
    #Start building this source
    message("------ Building page for ", page$name, " ------")
    
    #renderHTML from the rmd precursor
    renderHTML(sourcePath   = page$file,
               templatePath = html.template,
               savePath     = htmlPath,
               css          = css,
               js           = js,
               title        = page$name
               )
    
    #If there is a post-processor run it
    if (!is.null(postProcessor)) {
      
      html <- readLines(htmlPath)
      write(postProcessor(html), htmlPath)
      
    }
  
  }
  
  #Compile the index by sections
  message("----- Building index.html -----")
  sections <- by(pages, pages$section,
                function(entry) {
                  
                  #use index.entry template to render the md representation of entries in this section
                  sectionLines <- by(entry, 1:length(entry$name),
                                     function(page) {
                                       print(page$name)
                                       pageLink <- paste("<a href=", 
                                                         paste(splitExt(page$file), ".html", sep=""),
                                                         " class='", link.class,"'>",
                                                         page$name, 
                                                         "</a>", 
                                                         sep="")
                                       renderTemplate(
                                         template    = readLines(index.entry), 
                                         name        = pageLink,
                                         description = page$description
                                          )
                                     })
                  
                  #use index.section to render the md respresentation of this section
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
                              css       = css,
                              js        = js
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
  oldChunk <- knit_hooks$get('chunk')
  
  knit_hooks$set(
    
    #Add code highlighting to source (the documentation makes it sound like this
    #should already be happening, but it isn't)
    source  = function(x, options) {
      sprintf("<div class=\"%s\">%s</div>", 'source', codeHighlight(x))
    },
    
    #Fixes a bug that introduces a new line between images and </pre> tag which
    #breaks markdown rendering.
    chunk = function(x, options) {
      txt <- oldChunk(x,options)
      return (gsub("\n</pre>", "</pre>", txt, fixed = TRUE))
      
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

#Templating engine (uses {{..}} to match replacements)
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

#Postprocessors (for finalized html)
revealSlides <- function(html) {
  
  #Get the index of lines that contain H1 elements
  sectionStarts <- grep(pattern = "<[Hh]1>.*</[Hh]1>", x = html)
  
  #Find the line that ends the courser block (2 lines behind end of last section)
  blockEnd <- grep("<!-- end of courser content -->", html)
  
  #Calculate the end indexes of all of the sections
  if (length(sectionStarts) > 1) {
    sectionEnds <- c(sectionStarts[2:length(sectionStarts)] - 1, blockEnd - 2)
  } else {
    sectionEnds <- blockEnd - 2
  }
  
  #Prepend <section> to each start and re-inject into the html lines
  html[sectionStarts] <- paste("<section>", html[sectionStarts], sep="")
  
  #Append </section> to each end section ending line
  html[sectionEnds] <- paste(html[sectionEnds], "</section>", sep="")
  
  #Get the index of all subsection start lines
  subsectionStarts <- grep(pattern = "<[Hh]2>.*</[Hh]2>", x = html)
  
  #Calculate possible subsection ends (if they end at another subsection)
  if (length(subsectionStarts) > 1) {
      possibleEnds = sort( c(sectionEnds,
                           subsectionStarts[2:length(subsectionStarts)] - 1)
                         )
  } else {
    possibleEnds = c(sectionEnds)
  }
  
  #For each subsection
  for (start in subsectionStarts) {
    
    #Prepend <section> at the start line
    html[start] <- paste("<section>", html[start], sep="")
    
    #Find first possible end value that's bigger than the start
    subsectionEnd <- possibleEnds[possibleEnds > start][1]
    
    #if subsectionEnd didn't happen at a valid id then this subsection is at the
    #end of the document, so we'll have to slap another </section> there
    if (is.na(subsectionEnd)) {
      subsectionEnd <- blockEnd -2
    }
    
    html[subsectionEnd] <- paste("</section>", html[subsectionEnd], sep="")
    
  }
  
  return(html)
  
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
