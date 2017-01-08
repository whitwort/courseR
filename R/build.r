getRMD <- function(path) {
  lines <- readLines(path)
  exts  <- grep("^(---|\\.\\.\\.)\\s*$", lines)
  lines[(exts[2] + 1):length(lines)]
}

slideRMD <- function(lines, config) {
  if (config$build$site$`max-chars`) {
    collapseRMD( lines[nchar(lines) < config$build$site$`max-chars`])
  } else {
    collapseRMD(lines)
  }
}

solutionRMD <- function(lines) {
  s <- collapseRMD(lines)
  
  gsub( pattern     = "```\\{(.*?)\\}(.*?)```"
      , replacement = "```{\\1, echo = FALSE}\n\\2```"
      , x = s
      )
  
}

taskRMD <- function(lines) {
  s <- collapseRMD(lines)
  
  gsub( pattern     = "```\\{(.*?)\\}(.*?)```"
      , replacement = "```{\\1}\n# Enter your code here!\n\n```"
      , x = s
      )
}

collapseRMD <- function(lines) {
  paste0(paste0(lines, collapse = "\n"), "\n")
}
