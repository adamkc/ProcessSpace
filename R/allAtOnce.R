#' allAtOnce
#'
#' This function runs in sequence most of the work that is done with
#' \code{ProcessSpace}. The individual functions can be run individually for
#' more control. Before running this function you must run
#' \code{generateCrossSections} to generate a transectObject.
#'
#' @param transectObject Object created by the \code{generateCrossSections} function.
#' @param outputFilename This is the name of the pdf file that is generated as
#'  well as the KMZ if spatial files are exported.
#' @param detrendElevs Logical (\strong{TRUE}). Calculated a detrended raster file.
#'  Can be computationally expensive, especially with long target stream lengths.
#' @param returnObject Logical (\strong{FALSE}). Whether or not to return an R object
#'  for additional analysis. If FALSE, NULL is returned.
#' @param doExportSpatial Logical (\strong{FALSE}). Whether or not to generate KML
#'  exports. If both this and \code{returnObject} are FALSE, a pdf map and cross
#'  section plot are exported and nothing else.
#' @param ... Additonal arguments to be passed to \code{addTopoLines},
#'   \code{addCrossSectionElevations}, \code{buildXSectionPlot}, or
#'   \code{rasterPlotter}.
#'
#' @return Either a transectObject or NULL
#' @export
#'
#' @examples

allAtOnce <- function(transectObject,
                      outputFilename,
                      detrendElevs=TRUE,
                      returnObject=TRUE,
                      doExportSpatial=FALSE,
                      ...){

  pdfName <- paste0(tools::file_path_sans_ext(outputFilename),".pdf")

  suppressWarnings(
    suppressMessages({
      transectObject <- addTopoLines(transectObject, ...)
      transectObject <- addCrossSectionElevations(transectObject, ...)
      transectObject <- addProcessSpace(transectObject)
      transectObject <- buildXSectionPlot(transectObject,
                                          plotFileName = pdfName, ...)

      if(detrendElevs){
        transectObject <- rasterPlotter(transectObject, ...)
      }
    })
  )
  if(doExportSpatial)
    exportSpatials(transectObject,
                   sectionName = tools::file_path_sans_ext(outputFilename))

  if(returnObject){
    return(transectObject)}
  else{
    return()
  }
}
