renderTemplate <- function(template, data, file = NULL, partials = list()) {
  if (file.exists(template)) {
    template <- readFile(template)
  }
  s <- whisker::whisker.render( template = template
                              , data     = data
                              , partials = partials
                              )
  
  # un-escape stuff whisker escaped
  s <- gsub("&amp;", "&", s)
  s <- gsub("&lt;", "<", s)
  s <- gsub("&gt;", ">", s)
  s <- gsub("&quot;", "\"", s)
  
  if (!is.null(file)) { cat(s, "\n", file = file) }
  
  invisible(s)
}

loadPartials <- function(path, recursive = TRUE) {
  paths <- list.files(path, recursive = recursive)
  x <- lapply( paths
             , function(file) { readFile(file.path(path, file)) }
             )
  
  names(x) <- splitext(paths)
  x
}

readFile <- function(path) {
  paste0(readLines(path), collapse = "\n")
}

loadConfig <- function(path) {
  config <- yaml::yaml.load_file(file.path(path, "courseR.yml"))
  config$templates$data <- c(config$templates$data, config)
  config
}

getHeader <- function(path) {
  lines <- readLines(path)
  exts  <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  yaml::yaml.load(paste0(lines[(exts[1] + 1):(exts[2] - 1)], collapse = "\n"))
}

subHeader <- function(path, replacement) {
  lines <- readLines(path)
  exts  <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  paste( "---"
       , replacement
       , "---"
       , paste0(lines[(exts[2] + 1):length(lines)], collapse = "\n")
       , sep = "\n"
       )
}

mergeHeader <- function(a, b) {
  m <- mergeLists(a, b)
  yaml::as.yaml(m)
}

splitext <- function(path) {
  s <- strsplit(basename(path), ".", fixed = TRUE)
  sapply(s, function(x) x[1])
}

newSource <- function(tmpl, dest, path, ...) {
  
  config   <- loadConfig(path)
  renderTemplate( template = file.path(path, "templates", "site", tmpl)
                , data     = c(config$templates$data, list(...))
                , file     = file.path(path, dest)
                , partials = loadPartials(file.path(path, "templates", "site", "partials"))
                )
  
  update(path)
  
}

# only for named lists; recursive only works for lists
mergeLists <- function(a, b, overwrite = FALSE, recursive = TRUE) {
  for (name in names(b)) {
    if (!is.null(a[[name]])) {
      if (class(a[[name]]) == 'list') {
        a[[name]] <- mergeLists(a[[name]], b[[name]])
      } else {
        if (overwrite) {
          a[[name]] <- b[[name]]
        }
      }
    } else {
      a[[name]] <- b[[name]]
    }
  }
  a
}
