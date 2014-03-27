# TODO documentation
library(RJSONIO)
library(XML)
library(whisker)
library(markdown)
library(knitr)

library(tools)

## API
courseR.init  <- function(overwrite = FALSE) {
  
}

courseR.build <- function( projectPath  = getwd()
                         , buildPath    = file.path(projectPath, "build")
                         , configJSON   = file.path(projectPath, "courseR.json")
                         , config       = fromJSON(configJSON)
                         , templates    = loadFiles(file.path(projectPath, config$templates))
                         , content      = loadFiles(file.path(projectPath, config$content))
                         , name         = config$name
                         , toc          = config$toc
                         , css          = config$css
                         , js           = config$js
                         , img          = config$img
                         , data         = config$data
                         , optimize     = config$optimize
                         ) {
  
  if (optimize) {
    warning("Optimization hasn't been implemented yet.")   # TODO
  }
  
  # knitr customization
  opts_knit$set( progress = FALSE 
               , base.dir = buildPath
               , root.dir = projectPath
               )
  
  opts_chunk$set( fig.width   = 8
                , fig.height  = 5
                , fig.path    = config$img
                , comment     = ""
                , tidy        = FALSE
                , autodep     = TRUE
                )
  
  # if the build path exists, clear it out
  if (file.exists(buildPath)) {
    unlink(buildPath, recursive = TRUE)
  }
  
  # setup build path & copy resources directories; the path to files in each of 
  # the resource paths are available as a file list on templates (with ignored 
  # files filtered out).
  globalData <- sapply( c("img", "js", "css", "data")
                      , function(p) {
                        
                          resourceDir <- eval(parse(text = p))
                          sourcePath  <- file.path(projectPath, resourceDir)
                          buildPath   <- file.path(buildPath, resourceDir)
                          
                          dir.create( buildPath
                                    , showWarnings = FALSE
                                    , recursive    = TRUE
                                    )
                          
                          sourceFiles <- file.path( sourcePath
                                                  , list.files(sourcePath)
                                                  )
                          
                          if ( length(sourceFiles) > 0 ) {
                            file.copy( from = sourceFiles
                                     , to   = buildPath
                                     )
                          }
                          
                          lapply( listFiles(sourcePath)
                                , function(filePath) {
                                    list( path = file.path(p, filePath) ) 
                                })
                      }
                      
                      , simplify = FALSE
                      )
  
  # write each content page
  lapply( names(content)
        , function(pageName) {
            
            pageContent <- content[[pageName]]
          
            if (grepl('class="r output error"', pageContent, fixed = TRUE)) {
              warning("Page `", pageName, "` contains output with errors.")
            }
            if (grepl('class="r output warning"', pageContent, fixed = TRUE)) {
              warning("Page `", pageName, "` contains output with warnings.")
            }
            
            pageData    <- list( content = pageContent
                               , title   = pageName
                               )
            
            write( whisker.render(templates$page, c(globalData, pageData), templates)
                 , file.path(buildPath, paste(pageName, "html", sep = "."))
                 )
        })
  
  
  # check for errors in TOC list
  tocNames        <- sapply(toc, function(entry) { rootName(entry[['content']]) })
  contentNames    <- names(content)
  
  missingContent  <- setdiff(tocNames, contentNames)
  if (length(missingContent) > 0) {
    warning("Files listed in TOC but missing from 'content': ", paste(missingContent, sep = ", "))
  }
  
  missingTOC      <- setdiff(contentNames, tocNames)
  if (length(missingTOC) > 0) {
    warning("Files found in 'content' but not listed in TOC: ", paste(missingTOC, sep = ","))
  }
  
  # write the index
  sections <- tapply( toc
                    , sapply( toc
                            , function(entry) { 
                                if (is.null(entry[["section"]])) {
                                  entry[["section"]] <- "Contents"
                                }
                               
                                entry[["section"]]
                              }
                            )
                    , function(entries) {
                        lapply( entries
                              , function(entry) {
                                  url <- paste( rootName(entry['content'])
                                              , "html"
                                              , sep = "."
                                              )
                                  entry['url'] <- url
                                  entry
                              })
                      }
                    )
  
  sectionData <- lapply( names(sections)
                       , function(name) {
                           list( section = name, entries = sections[[name]] ) 
                       })
  
  indexContent <- whisker.render( templates$index
                                , c( globalData
                                   , list(sections = sectionData)
                                   )
                                , templates
                                )
    
  write( whisker.render( templates$page
                       , c( globalData
                          , list( title = name, content = indexContent )
                          )
                       , templates
                       )
       , file.path(buildPath, "index.html")
       )
  
  # restore knitr defaults
  opts_knit$restore()
  opts_chunk$restore()
  
}

listFiles <- function(rootPath) {
  ignores     <- "courser-ignore"
  ignoreFile  <- file.path(rootPath, ignores)
  
  if (file.exists(ignoreFile)) {
    ignores = c(ignores, readLines(ignoreFile))
  }
  
  files <- list.files(rootPath, recursive = TRUE)
  setdiff(files, ignores)
  
}

loadFiles <- function(rootPath) {
  
  files <- listFiles(rootPath)
  
  mapply( preprocess
        , rootName(files)
        , text      = file.path(rootPath, files)
        , SIMPLIFY  = FALSE
        )
  
}

preprocessors <- list(
      'html'  = function(text) { text }
    , 'md'    = function(text) { 
                  preprocess( markdownToHTML( text          = text
                                            , fragment.only = TRUE
                                            , options       = c( "use_xhtml"
                                                               , "smartypants"
                                                               )
                                            )
                            , 'html'
                            )
                  }
    , 'rmd'   = function(text) { 
                  preprocess( knit(text = text)
                            , 'md'
                            )
                  }
    , 'r'     = function(text) { 
                  preprocess( toString(eval(parse(text = text)))
                            , 'rmd'
                            )
                  }
    )

preprocess    <- function(text, type) {
  
  # If text is a file path
  if (file.exists(text)) {
    opts_knit$set( unnamed.chunk.label = rootName(text) )
    preprocess( paste(readLines(text, warn = FALSE), collapse="\n")
              , tolower(file_ext(text))
              )
  
  # Otherwise, pick a preprocessing path
  } else {
    preprocessors[[type]](text)
    
  }
  
}

# Utilities
rootName <- function(filePath) { file_path_sans_ext(basename(filePath)) }

