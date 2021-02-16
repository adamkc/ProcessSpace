#' allAtOnce
#'
#' @param transectObject Object created by the \code{generateCrossSections} function
#' @param outputFilename
#' @param detrendElevs
#' @param returnObject
#' @param doExportSpatial
#'
#' @return
#' @export
#'
#' @examples

allAtOnce <- function(transectObject,
                      outputFilename,
                      detrendElevs=TRUE,
                      returnObject=FALSE,
                      doExportSpatial=FALSE,
                      ...){

  pdfName <- paste0(tools::file_path_sans_ext(outputFilename),".pdf")

  suppressWarnings(
    suppressMessages({
      transectObject <- addTopoLines(transectObject,
                                     ...)
      transectObject <- addCrossSectionElevations(transectObject,
                                                  ...)
      transectObject <- addProcessSpace(transectObject)
      transectObject <- buildXSectionPlot(transectObject,
                                          plotFileName = pdfName,
                                          ...)

      if(detrendElevs){
        transectObject <- rasterPlotter(transectObject,
                                        ...)
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
