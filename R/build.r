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
  assnRMD(lines, "solution", ", echo=FALSE")
}

taskRMD <- function(lines) {
  s <- assnRMD(lines, "answer", "")
  s <- gsub( pattern     = "\\n```(\\{r answer.*\\}).*?\\n```"
           , replacement = "\n```\\1\n# Enter your code here!\n\n```"
           , x = s
           )
  s <- gsub( pattern     = "\n```\\{r checkr(.*?)\n```(\\s*?)\n"
           , replacement = "\n"
           , x = s
           )
  
  s
}

assnRMD <- function(lines, prefix, postfix) {
  starts <- grep(pattern = "^```\\{r solution(.*?)\\}", x = lines)
  for (i in 1:length(starts)) {
    lines[starts[i]] <- sub( pattern     = "^```\\{r solution([^,\\}\\s]*)(,?.*?)\\}"
                           , replacement = paste0("```{r ", prefix, "-", i, ", ", prefix, "=TRUE", ", task=", i, postfix, "\\2}")
                           , x           = lines[starts[i]]
                           )
    
    checkrStop  <- if (i < length(starts)) {
                     starts[i + 1] - 1 
                   } else {
                     length(lines)
                   }
    checkrLines <- grep(pattern = "^```\\{r checkr(.*?)\\}", x = lines[starts[i]:checkrStop])
    for (k in 1:length(checkrLines)) {
      linesIdx <- starts[i] + checkrLines[k] - 1
      lines[linesIdx] <- sub( pattern     = "^```\\{r checkr([^,\\}\\s]*)(,?.*?)\\}"
                            , replacement = paste0("```{r checkr", "-", i, ".", k, ", task=", i, ", checkr=", k, ", echo=FALSE, include=FALSE\\2}")
                            , x           = lines[linesIdx]
                            )
    }
    
  }
  
  collapseRMD(lines)
}

collapseRMD <- function(lines) {
  paste0(paste0(lines, collapse = "\n"), "\n")
}

