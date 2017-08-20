renderTemplate <- function(template, data, file = NULL, partials = list(), post = function(s) { s }, overwrite = TRUE) {
  if (file.exists(template)) {
    template <- readFile(template)
  }
  s <- whisker::whisker.render( template = template
                              , data     = data
                              , partials = partials
                              )
  
  # un-escape stuff whisker escaped
  # s <- whisker.unescape(s)
  s <- post(s)
  
  if (!is.null(file)) { 
    if (overwrite || !file.exists(file)) {
      cat(s, "\n", file = file) 
    }
  }
  
  invisible(s)
}

# this could be smarter
stripEmptyH <- function(s) {
  s <- gsub("\\n#\\n", "", s)
  s <- gsub("\\n##\\n", "", s)
  s
}

getUser <- function() {
  Sys.info()["user"]
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

hash <-function(filePaths) {
  contents <- vapply(filePaths, readFile, FUN.VALUE = "" , USE.NAMES = FALSE)
  openssl::md5(contents)
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
  if (name == "") {
    stop("You need to enter an assignment name.  See `listAssignments()` for names.")
  }
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

# This is a reduce; why doesn't the base R Reduce work this way?
reducer <- function(funcs) {
  function(x) {
    for (f in funcs) { x <- f(x) }
    x
  }
}

nonames <- function(l) {
  names(l) <- NULL
  l
}

# adapted nearly verbatim from https://github.com/dtkaplan/checkr
capture.code.envir <- function(code_text, envir) {
  
  #this could break
  insert_magrittr_input <- function(command, input_name) {
    res <- sub("(+[a-zA-Z0-9._]*\\()", sprintf("\\1%s, ", input_name), 
               command)
    gsub(", \\)", ")", res)
  }
  
  expand_chain <- function(chain_string) {
    components <- unlist(strsplit(chain_string, "%>%", fixed = TRUE))
    if (length(components) <= 1) 
      return(chain_string)
    assigned_to <- stringr::str_match(components[1], "^( *[a-zA-Z0-9._]*) *<-")[2]
    if (is.na(assigned_to)) 
      first_bit <- components[1]
    else components[1] <- stringr::str_match(components[1], "^.*<- *(.*) *$")[2]
    components[1] <- paste(components[1], "-> ..tmp1..")
    for (k in 2:length(components)) {
      components[k] <- insert_magrittr_input(components[k], 
                                             sprintf("..tmp%d..", k - 1))
      if (k == length(components) && !is.na(assigned_to)) {
        components[k] <- paste(components[k], sprintf("-> %s", 
                                                      assigned_to))
      }
      else {
        components[k] <- paste(components[k], sprintf("-> ..tmp%d..", 
                                                      k))
      }
    }
    paste0(components, collapse = "; ")
  }
  
  code_text <- as.character(parse(text = code_text))
  for (k in seq_along(code_text)) {
    code_text[k] <- expand_chain(code_text[k])
  }
  code_text <- paste0(code_text, "\n")
  commands <- parse(text = paste(code_text, collapse = "\n"))
  R <- list()
  environments <- list()
  statements <- as.character(commands)
  for (k in seq_along(commands)) {
    if (k > 1) {
      parent_environment <- environments[[k - 1]]
    }
    else {
      parent_environment <- envir
    }
    environments[[k]] <- new.env(parent = parent_environment)
    res <- try(eval(commands[k], envir = environments[[k]]), 
               silent = TRUE)
    R[[k]] <- res
  }
  statements <- lapply(statements, FUN = as.character)
  res <- list(returns = R, names = environments, statements = statements, 
              expressions = commands, valid_lines = 1:length(R), passed = TRUE, 
              line = 0, message = "", created_by = "capturing code")
  class(res) <- "capture"
  res
}
