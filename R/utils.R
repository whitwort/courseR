loadTextFile    <- function(filePath, warn = FALSE) {
  paste(readLines(filePath, warn = warn), sep = "\n")
}

rootName    <- function(filePath) { file_path_sans_ext(basename(filePath)) }

fileHash    <- function(path, wd = getwd()) {
  
  oldwd <- getwd()
  setwd(wd)
  
  # If there are multiple paths, hash each then digest the resulting vector of
  # hashes
  if (length(path) > 1) {
    hash <- digest( sapply(path, function(p) { fileHash(p, wd) }) )
  } else {
    
    # If the file is large (>1mb), hash the file info instead of the contents
    info <- file.info(path)
    if (is.na(info$size) || info$size > 1024 * 1024) {
      hash <- digest(info)
      
      # If the file is small, hash the contents (to avoid recompiling if no actual
      # content changes have been made even if the file has been touched)
    } else {
      hash <- md5sum(path)
    }  
  }
  
  setwd(oldwd)
  hash
  
}

recursiveCopy <- function(fromPath, toPath, overwrite = TRUE) {
  lapply( file.path( toPath, list.dirs(fromPath, full.names = FALSE) )
        , function(p) { dir.create(p, showWarnings = FALSE, recursive = TRUE) }
        )
  file.copy( list.files(fromPath, full.names = TRUE)
           , toPath
           , overwrite = overwrite
           , recursive = TRUE
           , copy.mode = FALSE
           )
}

parseHeader   <- function(filePath) {
  lines       <- readLines(filePath)
  headerIdx   <- grep("^---\\s*$", lines)
  if (length(headerIdx) > 1) {
    headerLines <- lines[(headerIdx[1] + 1):(headerIdx[2] - 1)]
    yaml.load( paste(headerLines, collapse = "\n") )
  }
}

listMerge     <- function(a, b) {
  # http://stackoverflow.com/questions/13811501/r-merge-lists-with-overwrite-and-recursion
  a.names <- names(a)
  b.names <- names(b)
  m.names <- sort(unique(c(a.names, b.names)))
  sapply( m.names
        , function(i) {
            if (is.list(a[[i]]) & is.list(b[[i]])) { 
              listMerge(a[[i]], b[[i]])
            } else {
              if (i %in% b.names) {
                b[[i]]
              } else { 
                a[[i]]
              } 
            }
          }
        , simplify = FALSE
        )
}
