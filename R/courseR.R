#' Initialize a new courseR project directory  
#' 
#' After initializing a new project see the '/content' subdirectory for example 
#' content pages.  See the 'courseR.yaml' configuration file and /templates' 
#' subdirectory for ways to customize the visual look and structure of the 
#' project site.
#' 
#' Run (\code{\link{courseR.build}}) to render and build the site.
#' 
#' @export
#' 
#' @param path character string path on the local file system where a new
#'   project folder should be written.  Defaults to the current working
#'   directory.
#' @param overwrite a logical value specifying whether or not existing files
#'   should be overwritten.
#' 
init  <- function( path         = getwd()
                 , overwrite    = FALSE
                 ) {
  
  sourcePath  <- file.path(find.package("courseR"), "project-template")
  recursiveCopy(sourcePath, path, overwrite = overwrite)
  
  cat("New courseR project initialized.  To get started, see the '/content' and 
      '/template' folders in the new project directory.")
  
}

#' (Re)build a courseR project
#' 
#' All of the arguments to this function can be provided in the project 
#' courseR.yaml configuration file.  By default the current working directory is
#' assumed to be the project root.  See the courseR.yaml file provided in the 
#' template project for a full description of all of the project build options.
#' 
#' @export
#' 
#' @param projectPath a character string with the root path to a project folder.
#'   This folder should contain the required resource subfolders (unless 
#'   alternative locations are specified). Defaults to the current working 
#'   directory.
#' @param config a list structure containing a full set of project configuration
#'   settings.  Defaults to the yaml parse of the file found on configPath.
#' @param configPath a character string path to a yaml configuration file. 
#'   Defaults to courseR.yaml.
#' @param content a character string containing the path to the directory
#'   containing content files.
#' @param app a character string containing the path to a directory containing
#'   web application resources.
#' @param templates a character string containing the path to the directory
#'   containing templates to be made available to post processing calls.
#' @param buildCache a character string containing the path to the build cache
#'   directory.
#' @param build a character string containing the path to the final build
#'   directory.
#' @param formats a list containing the default output formats data structure
#'   (see the default courseR.yaml for examples).
#' @param contents a list containing a full site contents structure.  If NULL 
#'   then one is built using content headers; the order of both the sections and
#'   content within sections is determined by an alphabetical sorting of content
#'   file names.
#' @param templateData a list containing additional data bindings that should be
#'   made available to the post-processing environments.
#' @param annotations a character vector containing the names of yaml header
#'   fields that should be flagged as content annotations; these bindings are
#'   made available to the post-processing environments.
#' @param sourceExternals a character vector containing the names of yaml header
#'   fields that reference external files that should be considered as integral
#'   parts of the source content (for example, data files that are loaded when
#'   code in an RMD file is executed).  Sources are rebuilt when these files
#'   chage and they are bundled when the final site is built.
#' @param outputExternals a character vector containing the names of fields on
#'   output formats that reference external files that should be considered as
#'   integral to the format rendering stage (for example, pandoc templates). 
#'   Outputs will be rebuilt when these files change.
#' @param clearCache a logical indicating whether or not the entire project
#'   should be re-rendered and rebuilt, irrespective of the state of the
#'   intermediate cache.  Mostly useful for debugging purposes.
#' @param dumpContext a logical indicating whether or not data bindings
#'   available to post-processors should be dumped to YAML files for each output
#'   being produced.  Useful during development for discovering what variables
#'   are available for use in post-processing functions.
#' 
#' @import digest

#' @import rmarkdown
#' @import yaml
#' @import whisker
#' @import plyr
#' @import compare
#' @import shiny
#' @import XML
#' @import selectr
#' 
build <- function( projectPath      = getwd()
                 , config           = yaml.load_file(configPath)
                 , configPath       = file.path(projectPath, "courseR.yaml")
                 , content          = file.path(projectPath, config$paths$content)
                 , app              = file.path(projectPath, config$paths$app)
                 , templates        = file.path(projectPath, config$paths$templates)
                 , buildCache       = file.path(projectPath, config$paths$buildCache)
                 , build            = file.path(projectPath, config$paths$build)
                 , formats          = config$output_formats
                 , contents         = config$contents
                 , templateData     = config$templateData
                 , annotations      = config$annotations
                 , sourceExternals  = config$sourceExternals
                 , outputExternals  = config$outputExternals
                 
                 , clearCache       = FALSE
                 , dumpContext      = FALSE
                 ) {
  
  # We take r scripts, rmd and md files as possible sources
  contentFiles <- list.files( content
                            , pattern     = "(^.*\\.r$)|(^.*\\.rmd$)|(^.*\\.md$)"
                            , ignore.case = TRUE
                            )
  
  # The intermediate build step contains the results of the R Markdown rendering
  # (via knitr and Pandoc).  We cache the results of the intermediate build to
  # avoid having to re-execute (potentially) costly R between build updates when
  # only the presentation or overall structure of the project has changed.
  if (clearCache) { unlink(buildCache, recursive = TRUE) }
  if ( !file.exists(buildCache) ) { dir.create(buildCache) }
  
  manifest <- renderCache( targetPath      = buildCache
                         , sourceFiles     = contentFiles
                         , contentPath     = content
                         , defaultFormats  = formats
                         , annotations     = annotations
                         , sourceExternals = sourceExternals
                         , outputExternals = outputExternals
                         )

  # Build a contents structure from the manifest if one is not provided
  if (is.null(contents)) {
    contents <- buildContents(manifest)
  }
  templateData$sections <- contents
  
  # The final build contains the results of post processing calls.
  if (file.exists(build)) { unlink(build, recursive = TRUE) }
  dir.create(build)
  
  final <- buildFinal( targetPath  = build
                     , buildCache  = buildCache
                     , manifest    = manifest
                     , context     = templateData
                     , templates   = loadTemplates(templates, templateData)
                     , dumpContext = dumpContext
                     )
  
  # copy over all web resources found in app and source externals
  recursiveCopy(app, build)
  if (length(sourceExternals) > 0) {
   s <- mapply( recursiveCopy
              , file.path(content, sourceExternals)
              , file.path(build, sourceExternals)
              )
  }
  
  if (!is.null(config$package)) {
    updateScript <- file.path( projectPath
                             , config$package$packagePath
                             , "R"
                             , config$package$updateScript
                             )
    if (file.exists(updateScript)) {
      source(updateScript, local = TRUE)
      updatePackage(config, manifest, projectPath)  
    }
  }
  
}

#' Get project content manifest
#'
#' @param projectPath a character string with the root path to a project folder.
#'   This folder should contain the required resource subfolders (unless 
#'   alternative locations are specified). Defaults to the current working 
#'   directory.
#' @param config a list structure containing a full set of project configuration
#'   settings.  Defaults to the yaml parse of the file found on configPath.
#' @param configPath a character string path to a yaml configuration file. 
#'   Defaults to courseR.yaml.
#' @param buildCache a character string containing the path to the build cache 
#'   directory.
#'   
#' @return A data.frame
#' @export
#'
manifest <- function( projectPath = getwd()
                    , config      = yaml.load_file(configPath)
                    , configPath  = file.path(projectPath, "courseR.yaml")
                    , buildCache  = file.path(projectPath, config$paths$buildCache)
                    ) {
  read.table(file.path(buildCache, 'build.manifest'), stringsAsFactors = FALSE)
}

