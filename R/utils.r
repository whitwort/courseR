renderTemplate <- function(template, data, file = NULL, partials = list(), post = function(s) { s }) {
  if (file.exists(template)) {
    template <- readFile(template)
  }
  s <- whisker::whisker.render( template = template
                              , data     = data
                              , partials = partials
                              )
  
  # un-escape stuff whisker escaped
  s <- whisker.unescape(s)
  s <- post(s)
  
  if (!is.null(file)) { cat(s, "\n", file = file) }
  
  invisible(s)
}

# this could be smarter
stripEmptyH <- function(s) {
  s <- gsub("\\n#\\n", "", s)
  s <- gsub("\\n##\\n", "", s)
  s
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

getHeaderString <- function(path) {
  lines <- readLines(path)
  exts  <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  paste0(lines[(exts[1] + 1):(exts[2] - 1)], collapse = "\n")
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

listify <- function(v) { sapply(v, function(x) x, simplify = FALSE) }

getRMDFile <- function(name, path, onlyone = TRUE, exists = TRUE) {
  if (file.exists(file.path(path, name))) {
    file.path(path, name)
  } else {
    f <- list.files(path, pattern = paste0("^", name, "\\.[Rr]md$"), full.names = TRUE)
    if (onlyone && (length(f) > 1)) {
      stop("More than one markdown file matches that assignment name: ", f)
    }
    if (exists && length(f) == 0) {
      stop("No markdown file exists that matches that assignment name here: ", path)
    }
    if (!exists && length(f) == 0) {
      f <- file.path(path, paste0(name, ".Rmd"))
    }
    f
  }
  
}

smartSuppress <- function(expr, warningGrep) {
  h <- function(w) {
    if (grepl(warningGrep, w)) {
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers(expr, warning = h)
}

whisker.unescape <- function(s) {
  
  # un-escape stuff whisker escaped
  s <- gsub("&amp;", "&", s)
  s <- gsub("&lt;", "<", s)
  s <- gsub("&gt;", ">", s)
  s <- gsub("&quot;", "\"", s)
  
  s
}

# l list of lists; add <p> for new lines
markdownify <- function(l, de.p = FALSE, add.p = TRUE) {
  x <- lapply( 1:length(l)
             , function(i) {
               if (class(l[[i]]) == "character") {
                 s <- markdown::markdownToHTML(text = l[[i]], fragment.only = TRUE)
                 
                 if (de.p) {
                   s <- gsub("<p>", "", s, fixed = TRUE)
                   s <- gsub("</p>\n", "", s, fixed = TRUE)
                 }
                 if (add.p) {
                   s <- gsub("\\n$", "", s)
                   s <- gsub("\n", "</p><p>", s, fixed = TRUE)
                 }
                 
                 s
               } else if (class(l[[i]]) == "list") {
                 markdownify(l[[i]])
               } else {
                 l[[i]]
               }
             }
            )
  names(x) <- names(l)
  x
}

# there really must be an easier way to do this
shinyToNode <- function(s) {
  xml_child(xml_child(read_html(as.character(s))))
}
