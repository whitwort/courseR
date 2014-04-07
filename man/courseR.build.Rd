\name{courseR.build}
\alias{courseR.build}
\title{(Re)build a courseR project}
\usage{
courseR.build(projectPath = getwd(), buildPath = file.path(projectPath,
  "build"), configJSON = file.path(projectPath, "courseR.json"),
  config = fromJSON(configJSON),
  templates = loadFiles(file.path(projectPath, config$templates)),
  content = loadFiles(file.path(projectPath, config$content)),
  name = config$name, toc = config$toc, css = config$css,
  js = config$js, img = config$img, data = config$data,
  optimize = config$optimize)
}
\arguments{
  \item{projectPath}{a character string with the root path
  to a project folder.  This folder should contain the
  courseR.json configuration file along with required
  resource subfolders (unless alternative locations are
  specified).  Defaults to the current working directory.}

  \item{buildPath}{a character string with the path of the
  destination folder for this build.  Defaults to a
  '/build' subfolder.}

  \item{configJSON}{a character string the name of the
  configuration file.  Defaults to 'courseR.json'.}

  \item{config}{a list containing configuration settings.
  Use to directly pass in a configuration profile instead
  of reading options in from a .json file.}

  \item{templates}{a character vector containing template
  file paths.  Use to override the default enumeration of
  the 'templates' directory.}

  \item{buildPath}{a character vector containing content
  file paths.  Use to override the default enumeration of
  the 'contents' directory.}

  \item{name}{a character string containing the project
  name (defaults to configuration file setting).}

  \item{toc}{a list containing the table of contents
  structure (defaults to configuration file setting).}

  \item{css}{a character string with the directory path
  where css files are found (defaults to configuration file
  setting).}

  \item{js}{a character string with the directory path
  where javascript files are found (defaults to
  configuration file setting).}

  \item{img}{a character string with the directory path
  where image files are found (defaults to configuration
  file setting).}

  \item{data}{a character string with the directory path
  where data files are found (defaults to configuration
  file setting).}

  \item{optimize}{a logical indicating whether or not web
  resources should be run through an optimizer.}
}
\description{
Generates a new website package for a courseR project.
After editing the 'courseR.json' configuration file and
adding content to the 'content' directory, simply call this
function with defaults to build a website package in a
'build' subfolder.
}
\details{
Many of the optional arguments to this function can also be
specified in the 'courseR.json' project file.  However,
explicitly setting an option in a call to this function
will override the settings in the configuration file.

If files are present in project subfolders that you'd like
to the build script ignore, simply create a
'courser-ignore' text file in the subfolder and list the
files you want to skip (one file name per line).
}

