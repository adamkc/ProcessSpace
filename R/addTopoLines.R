#' addTopoLines
#'
#' @param transectObject
#' @param rasterDir
#'
#' @return
#' @export
#'
#' @examples
addTopoLines <- function(transectObject,
                         rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                         ...){

  cat(crayon::green("Adding TopoLines"))
  startTime <- Sys.time()
  r <- raster::raster(rasterDir)

  bufferPoly <- sf::st_union(transectObject$leftSide,transectObject$rightSide) %>%
    sf::st_transform(raster::crs(r)) %>%
    extent.sf()

  r <- r %>% raster::crop(bufferPoly)

  contours1ft <- r %>%
    raster::rasterToContour(levels = seq(r@data@min,
                                         r@data@max,1)) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs=4326)

  contours5ft <- r %>%
    raster::rasterToContour(levels = seq(r@data@min,
                                         r@data@max,5)) %>%
    sf::st_as_sf()  %>%
    sf::st_transform(crs=4326)

  # newPlot<- transectObject$mapPlot +
  #   geom_sf(data=contours1ft,col=alpha("white",0.4),
  #           inherit.aes = FALSE) +
  #   geom_sf(data=contours5ft,col=alpha("black",0.6),
  #           inherit.aes = FALSE)
  transectObject$plotDataContours5ft <- contours5ft
  transectObject$plotDataContours1ft <- contours1ft

  outputTimer(startTime)

  return(transectObject)
}

