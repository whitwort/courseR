renderCache  <- function( targetPath
                        , sourceFiles
                        , contentPath
                        , defaultFormats
                        , annotations
                        , sourceExternals
                        , outputExternals
                        , config
                        ) {
  
  # Create a manifest and merge in pre-existing data if it exists
  build.manifest <- file.path(targetPath, 'build.manifest')
  manifest       <- data.frame(file = sourceFiles, stringsAsFactors = FALSE)
  if ( file.exists(build.manifest) ) {
    oldManifest  <- read.table(build.manifest, stringsAsFactors = FALSE)
    manifest     <- merge(manifest, oldManifest, by = "file", all.x = TRUE, all.y = FALSE)
  }
  
  # Parse yaml headers from sources into a list
  headers <- lapply(file.path(contentPath, manifest$file), parseHeader)
  
  # update manifest annotation columns
  for (colName in annotations) {
    manifest[[colName]] <- sapply( headers
                                 , function(h) {
                                     v <- h[[colName]]
                                     if (is.null(v)) { "" } else { v }
                                 })
  }
  
  # Convert NA's to empty string for comparisons needed to evaluate rebuilds
  for (colName in c('sourceHash', 'formats')) {
    if (is.null(manifest[[colName]])) {
      manifest[[colName]] <- ""
    } else {
      manifest[[colName]][is.na(manifest[[colName]])] <- ""
    }
  }
  
  # Calculate source file hashes including external dependencies
  sourceHash <- hashSources(sourceFiles, headers, sourceExternals, contentPath)
  
  # Calculate merged output formats list for each source
  outputFormats <- lapply( headers
                         , mergeFormats
                         , defaultFormats  = defaultFormats
                         , outputExternals = outputExternals
                         , contentPath     = contentPath
                         )
  
  # Render output formats for each content file
  renders <- mapply( renderOutput
                   , sourceFile   = file.path(contentPath, manifest$file)
                   , fileHeader   = headers
                   , formats      = outputFormats
                   , rebuilds     = cacheInvalid( manifest
                                                , sourceHash
                                                , outputFormats
                                                )
                   , MoreArgs     = list(targetPath = targetPath, config = config)
                   , SIMPLIFY     = FALSE
                   , USE.NAMES    = TRUE
                   )
  
  # add output file name annotations to the manifest
  manifest$outputFiles <- sapply( renders
                                , function(r) { 
                                    if (length(r) > 0) { 
                                      as.yaml(as.list(r))
                                    } else { "" }
                                })
  
  # Update manifest to the current file & data hashes and outputFormats in yaml
  manifest$sourceHash <- sourceHash
  manifest$formats    <- sapply( outputFormats
                               , function(format) { as.yaml(format) } 
                               )
  
  # Serialize manifest
  writeManifest(manifest, annotations, build.manifest)
  
  # return the manifest
  manifest
  
}

writeManifest <- function(manifest, annotations, build.manifest) {
  
  for (colName in annotations) {
    manifest[[colName]] <- sapply(manifest[[colName]], as.yaml)
  }
  
  # Serialize the current manifest (used in the next build step)
  write.table(manifest, build.manifest)
  
}

cacheInvalid <- function(manifest, sourceHash, outputFormats) {
  
  # NULL compares to NULL as false-ish, but rmkardown choose NULL as the value 
  # assignment needed to skip automatic resource inclusion (ex. theme: null); 
  # the choice here should have been false given the intended semantics of NULL,
  # but we hack a solution by changing NULL (~ in YAML) to a unique-ish string
  # so that the "NULL"ish values in output settings compare as equal.
  deNULL <- function(l) {
    yaml.load(gsub("~", "~NULL~", as.yaml(l)))
  }
  
  mapply( function(oldSource, newSource, oldOutput, newOutput) {
            formatCount <- length(newOutput)
            if (!oldSource == newSource) {
              rep(TRUE, times = formatCount)
            } else {
              comp <- compareEqual( deNULL(newOutput)
                                  , deNULL(oldOutput)
                                  , ignoreComponentOrder = TRUE
                                  )
              !comp$detailedResult
            }
          }
        , manifest$sourceHash 
        , sourceHash
        , lapply(manifest$formats, yaml.load)
        , outputFormats
        )
}

hashSources <- function(sourceFiles, headers, sourceExternals, contentPath) {
  mapply( function(file, header) {
            externalRefs <- intersect(names(header), sourceExternals)
            if (length(externalRefs) > 0) {
              sourceFiles  <- c(file, header[[externalRefs]], recursive = TRUE
              )
            } else { 
              sourceFiles = file 
            }
            fileHash(file.path(contentPath, sourceFiles))
          }
        , sourceFiles
        , headers
        )
}


mergeFormats <- function( fileHeader
                        , defaultFormats
                        , outputExternals
                        , contentPath
                        ) {
  
  # File header settings can append/overwrite defaults
  if (!is.null(fileHeader$output_formats)) {
    formats <- listMerge(defaultFormats, fileHeader$output_formats)
  } else {
    formats <- defaultFormats
  }
  
  for (name in names(formats)) {
    format <- formats[[name]]
    
    # remove format keys that are false
    if (is.logical(format) && format == FALSE) {
      formats[[name]] <- NULL
      
      # calculate externals hash if externals are present
    } else {
      renderArgs   <- format$render[[1]]
      externalRefs <- intersect(names(renderArgs), outputExternals)
      if (length(externalRefs) > 0) {
        files <- c(renderArgs[[externalRefs]], recursive = TRUE)
        formats[[name]]$externalsHash <- fileHash(file.path(contentPath, files))
      }
    }
  }
  
  formats
}

renderOutput <- function( sourceFile
                        , fileHeader
                        , formats
                        , rebuilds
                        , targetPath
                        , config
                        ) {
  
  fileRoot     <- rootName(sourceFile)
  
  renderFormat <- function(format, rebuild) {
    outputFile <- paste(fileRoot, format$file_extension, sep = "")
    outputPath <- file.path(targetPath, outputFile)
    
    # if the file doesn't exist or the cache for this format is invalid render
    # the file
    if ( (!file.exists(outputPath) || rebuild) && file.exists(sourceFile)) {
      cat("\nRendering", outputFile, "...")
      if (class(format$render) == "character") {
        renderFormat <- do.call(get(format$render), list())
      } else {
        renderFormat <- do.call(get(names(format$render)), format$render[[1]]) 
      }
      
      if (!is.null(config$knitr) && !is.null(renderFormat$knitr)) {
        for (name in names(config$knitr)) {
          renderFormat$knitr[[name]] <- listMerge(renderFormat$knitr[[name]], config$knitr[[name]])
        }
      }
      
      # rmarkdown::render
      render( input         = sourceFile
            , output_format = renderFormat
            , output_file   = outputFile
            , output_dir    = targetPath
            , quiet         = TRUE
            )
    }
    
    outputFile
  }
  
  mapply(renderFormat, formats, rebuilds)
}
