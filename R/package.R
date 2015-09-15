updatePackage <- function(config, manifest, projectPath) {
  
  # Paths needed by package functions
  
  # Project
  contentPath  <- file.path(projectPath, config$paths$content)
  dataPath     <- file.path(contentPath, config$package$dataPath)
  
  # Students
  homePath     <- config$package$homePath
  submitPath   <- config$package$submitPath
  
  # Instructor
  exercisePath <- file.path(projectPath, config$package$exercisePath)
  studentList  <- file.path(exercisePath, config$package$studentList)
  
  studentPath <- function(username) {
    file.path(homePath, username, config$package$studentProject)
  }
  
  packageData <- file.path( projectPath
                          , config$package$packagePath
                          , "R"
                          , "sysdata.rda"
                          )
  save( config
      , manifest
      , contentPath
      , dataPath
      , homePath
      , submitPath
      , exercisePath
      , studentList
      , studentPath
      , file = packageData
      )
 
  message("Package data updated: ", packageData) 
}