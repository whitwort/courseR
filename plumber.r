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
    return(list(message = paste0("So nice to see you, ", username, ".")))
  } else {
    return(list(message = paste0("Howdy, stranger.")))
  }
}