library(RJSONIO)
library(XML)
library(whisker)
library(markdown)
library(knitr)
library(tools)

#' Initialize a new courseR project directory  
#' 
#' After initializing a new project see the 'courseR.json' file and '/content'
#' subdirectory for examples.
#' 
#' @export
#' 
#' @param path character string path on the local file system where a new
#'   project folder should be written.  Defaults to the current working
#'   directory.
#' @param overwrite a logical value specifying whether or not existing files
#'   should be overwritten.
courseR.init  <- function( path         = getwd()
                         , overwrite    = FALSE
                         ) {
  
  sourcePath  <- file.path( find.package("courseR")
                          , "project-template"
                          )
  
  lapply( file.path( path
                   , list.dirs( sourcePath
                              , full.names = FALSE
                              )
                   )
        , function(p) { dir.create( p
                                  , showWarnings = FALSE
                                  , recursive = TRUE
                                  )
                      }
        )
  
  file.copy( list.files( sourcePath
                       , full.names = TRUE
                       )
           , path
           , overwrite = overwrite
           , recursive = TRUE
           , copy.mode = FALSE
           )
  
  cat("New courseR project initialized.  To get started, see 'courseR.json' and '/content' in the new project directory.")
  
}

#' (Re)build a courseR project
#' 
#' Generates a new website package for a courseR project.  After editing the 
#' 'courseR.json' configuration file and adding content to the 'content' 
#' directory, simply call this function with defaults to build a website package
#' in a 'build' subfolder.
#' 
#' Many of the optional arguments to this function can also be specified in the 
#' 'courseR.json' project file.  However, explicitly setting an option in a call
#' to this function will override the settings in the configuration file.
#' 
#' If files are present in project subfolders that you'd like to the build 
#' script ignore, simply create a 'courser-ignore' text file in the subfolder 
#' and list the files you want to skip (one file name per line).
#' 
#' @export
#' 
#' @param projectPath a character string with the root path to a project folder.
#'   This folder should contain the courseR.json configuration file along with 
#'   required resource subfolders (unless alternative locations are specified). 
#'   Defaults to the current working directory.
#' @param buildPath a character string with the path of the destination folder
#'   for this build.  Defaults to a '/build' subfolder.
#' @param configJSON a character string the name of the configuration file. 
#'   Defaults to 'courseR.json'.
#' @param config a list containing configuration settings.  Use to directly pass
#'   in a configuration profile instead of reading options in from a .json file.
#' @param templates a character vector containing template file paths.  Use to
#'   override the default enumeration of the 'templates' directory.
#' @param buildPath a character vector containing content file paths.  Use to
#'   override the default enumeration of the 'contents' directory.
#' @param name a character string containing the project name (defaults to
#'   configuration file setting).
#' @param toc a list containing the table of contents structure (defaults to
#'   configuration file setting).
#' @param css a character string with the directory path where css files are
#'   found (defaults to configuration file setting).
#' @param js a character string with the directory path where javascript files
#'   are found (defaults to configuration file setting).
#' @param img a character string with the directory path where image files are
#'   found (defaults to configuration file setting).
#' @param data a character string with the directory path where data files are
#'   found (defaults to configuration file setting).
#' @param optimize a logical indicating whether or not web resources should be
#'   run through an optimizer.
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

