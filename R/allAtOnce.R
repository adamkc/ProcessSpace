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
  suppressWarnings(
    suppressMessages({
      transectObject <- transectObject %>%
        # mapPlotter() %>%
        #exportSpatials(folderName = "finger1b")
        #longitudinalElevation(plotFileName = "Finger1LongitudinalPIPE.png") %>%
        addTopoLines(...) %>%
        addStreamChannels(...) %>%
        addCrossSectionElevations(...) %>%
        addProcessSpace() %>%
        buildXSectionPlot(plotFileName = outputFilename,...)
      if(detrendElevs){
        transectObject <- transectObject %>%
          rasterPlotter(...)
      }
      # transectObject <- transectObject %>%
      #   buildXSectionPlot(plotFileName = outputFilename,
      #                     returnData=TRUE)
    })
  )
  #print(names(transectObject))
  if(doExportSpatial)
    exportSpatials(transectObject,
                   sectionName = tools::file_path_sans_ext(outputFilename))

  if(returnObject){
    return(transectObject)}
  else{
    return()
  }
}
