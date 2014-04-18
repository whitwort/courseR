buildFinal <- function( targetPath
                      , buildCache
                      , manifest
                      , context
                      , templates
                      , dumpContext
                      ) {
  by( manifest
    , manifest$file
    , function(sourceManifest) {
        
        cat("\nBuilding", rootName(sourceManifest$file), "content ...")
        
        # attach information about this content source to the root of
        # templateData
        contentInfo <- buildContent(sourceManifest)
        context     <- listMerge(context, contentInfo)
        
        # for each output file, expand templates or just copy to destination
        lapply( names(contentInfo$outputFiles)
              , function(type) {
                  context$type      <- type
                  context$fileName  <- contentInfo$outputFiles[[type]]
                  
                  sourceFile <- file.path(buildCache, context$fileName)
                  destFile   <- file.path(targetPath, context$fileName)
                  
                  format  <- contentInfo$formats[[type]]
                  text    <- loadTextFile(sourceFile)
                  
                  if (dumpContext) {
                    write( as.yaml(context)
                         , paste(destFile, "context.yaml", sep="-")
                         )
                  }
                  
                  if (!is.null(format$post_processors)) {
                    text <- Reduce(function(text, f) { 
                                      f(text, context, templates) 
                                    }
                                  , lapply( format$post_processors
                                          , function(n) {eval(parse(text = n))}
                                          )
                                  , init = text
                                  )
                  }
                  
                  # write text to file
                  write(text, destFile)
                  
                })
      })
}

loadTemplates <- function(path, templateData) {
  partials <- lapply( list.files(path)
                    , function(fileName) {
                        if (grepl("(^.*\\.r$)|(^.*\\.R$)", fileName)) {
                          eval( parse(file.path(path, fileName))
                              , envir = templateData
                              )
                        } else {
                          loadTextFile(file.path(path, fileName))
                        }
                      }
                    )
  names(partials) <- rootName(list.files(path))
  partials
}

buildContent <- function(content) {
  content       <- as.list(content)
  content$name  <- rootName(content$file)
  
  content$formats     <- yaml.load(content$formats)
  content$outputFiles <- yaml.load(content$outputFiles)
  
  if (length(content$outputFiles) > 0) {
    content$primaryFile <- content$outputFiles[[1]]
  }
  if (length(content$outputFiles) > 1) {
    content$otherFormats <- 
      lapply( names(content$outputFiles)[2:length(content$outputFiles)]
            , function(name) {
                list( type = name
                    , file = content$outputFiles[[name]]
                    )                
            })
  }
  
  content
}

buildContents <- function(manifest) {
  # whisker templating chokes on named lists where arrays are expected (naming
  # list elements creates an addition context tier), so we strip names here
  # before returning
  
  buildsection <- function(section) {
    content <- dlply(section, "file", buildContent)
    names(content) <- NULL
    
    list( name    = section[1, 'section']
        , content = content
        )
  }
  
  # filter entries with no section annotation (will be empty strings)
  manifest <- manifest[!manifest$section == "",]
  
  sections <- dlply(manifest, "section", buildsection)
  names(sections) <- NULL
  sections
}
