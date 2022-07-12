library(plumber)

# Returns a list containing "user" and "groups" information 
# populated by incoming request data.
getUserMetadata <- function(req) {
  rawUserData <- req[["HTTP_RSTUDIO_CONNECT_CREDENTIALS"]]
  if (!is.null(rawUserData)) {
    jsonlite::fromJSON(rawUserData)
  } else {
    list()
  }
}

#* @get /hello
function(req, res){
  user <- getUserMetadata(req)
  username <- user[["user"]]
  if (!is.null(username)) {
    list(message = paste0("So nice to see you, ", username, "."))
  } else {
    list(message = paste0("Howdy, stranger."))
  }
}
   
#* @get /static
#* @serializer html
function(req, res) {
  s <- readLines("inst/project-template/templates/site/table.html")
  paste0(s, collapse = "\n")
}

#* @get /files
function(req, res) {
  list( path = list.files()
      , parent = list.files("../")
      , grandparent = list.files("../../")
      )
}
