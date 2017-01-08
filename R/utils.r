renderTemplate <- function(template, data, file = NULL, partials = list()) {
  s <- whisker::whisker.render( template = readFile(source)
                              , data     = data
                              , partials = partials
                              )
  
  if (!is.null(file)) { cat(s, file = file) }
  
  invisible(s)
}

loadPartials <- function(path, recursive = TRUE) {
  lapply( list.files(file.path(path, "partials"), recursive = recursive)
        , function(file) { readFile(file.path(path, file)) }
        )
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
  yaml::yaml.load(paste0(lines[(exts[1] + 1):(exts[2] - 1)]))
}

splitext <- function(path) {
  s <- strsplit(basename(path), ".", fixed = TRUE)
  s[[1]][1]
}

newSource <- function(tmpl, dest, path = getwd(), ...) {
  
  config   <- loadConfig(path)
  renderTemplate( template = file.path(path, "templates", "site", tmpl)
                , data     = c(config$templates$data, list(...))
                , file     = file.path(path, dest)
                , partials = loadPartials(file.path(path, "templates", "site", "partials"))
                )
  update(path)
  
}