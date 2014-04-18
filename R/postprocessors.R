#' Post processor that uses the whisker package to render mustache-style 
#' templates.
#' 
#' All post processor take exactly three arguments (passed in by the courseR 
#' framework).
#' 
#' @export
#' 
#' @param text a character string containing the text of the output document
#' @param context a list containing post processing environment variables
#' @param templates a list containing strings holding the contents of template 
#'   documents
#'   
#' @return a string character containing the new text of the output document
#'   
renderWhisker <- function(text, context, templates) {
  whisker.render( template = text
                , data     = context
                , partials = templates
                )
}

#' Post processor that restores whisker/mustache partials tags
#' 
#' According to the pandoc documentation, it should be possible to preserve the 
#' mustache partials include syntax by escaping but this doesn't seem to be
#' working.  This post processor restores these tags by aassume that the HTML
#' encoded result only occurs in the document when the whisker/mustache tag was
#' meant.  This is useful to include before the renderWhisker post_processor in
#' output format configurations.
#' 
#' All post processor take exactly three arguments (passed in by the courseR 
#' framework).
#' 
#' @export
#' 
#' @param text a character string containing the text of the output document
#' @param context a list containing post processing environment variables
#' @param templates a list containing strings holding the contents of template 
#'   documents
#'   
#' @return a string character containing the new text of the output document
#'   
fixPartialsMarkup <- function(text, context, templates) {

  gsub( "{{&gt;", "{{>", text, fixed = TRUE)
}

#' Post processor that adds <a href> tags to all strings referencing data files.
#' 
#' This post processor assumes that content files referencing external data
#' containg a 'data' field in their headers with relative paths to data files;
#' these directory holding data files should also be included in the
#' 'sourceExternals' list.
#' 
#' All post processor take exactly three arguments (passed in by the courseR 
#' framework).
#' 
#' @export
#' 
#' @param text a character string containing the text of the output document
#' @param context a list containing post processing environment variables
#' @param templates a list containing strings holding the contents of template 
#'   documents
#'   
#' @return a string character containing the new text of the output document
#'   
linkData <- function(text, context, templates) {
  dataPaths <- as.character(context$data)
  for (path in dataPaths) {
    if (nchar(path) > 0) {
      text <- gsub( path
                    , a(path, href = path)
                    , text
                    , fixed = TRUE
      ) 
    }
  }
  text
}

#' Post processor that adds centered and responsive classes to images
#' 
#' This post processor adds the classes 'img-responsive' and 'center-block' to
#' all img tags on a page.
#' 
#' All post processor take exactly three arguments (passed in by the courseR 
#' framework).
#' 
#' @export
#' 
#' @param text a character string containing the text of the output document
#' @param context a list containing post processing environment variables
#' @param templates a list containing strings holding the contents of template 
#'   documents
#'   
#' @return a string character containing the new text of the output document
#'   
responsiveImages <- function(text, context, templates) {
  doc      <- htmlParse(text)
  imgNodes <- querySelectorAll(doc, "img")
  for (node in imgNodes) {
    currentClass <- xmlGetAttr(node, 'class')
    if (is.null(currentClass)) {
      newClass <- "img-responsive center-block"
    } else {
      newClass <- paste(currentClass, "img-responsive center-block")
    }
    addAttributes(node, class = newClass)
  }
  saveXML(doc, indent = FALSE)
}

#' Post processor that removes long <p> elements
#' 
#' This post processor assumes that a trimTextCutoff binding is defined on the
#' templating data context.
#' 
#' All post processor take exactly three arguments (passed in by the courseR 
#' framework).
#' 
#' @export
#' 
#' @param text a character string containing the text of the output document
#' @param context a list containing post processing environment variables
#' @param templates a list containing strings holding the contents of template 
#'   documents
#'   
#' @return a string character containing the new text of the output document
#'   
trimText <- function(text, context, templates) {
  doc    <- htmlParse(text)
  pNodes <- querySelectorAll(doc, "p")
  for (node in pNodes) {
    count      <- nchar(xmlValue(node))
    parentTag  <- xmlName(xmlParent(node))
    if (count > context$trimTextCutoff && !parentTag == "blockquote") {
      removeNodes(node)
    }
  }
  saveXML(doc, indent = FALSE)
}