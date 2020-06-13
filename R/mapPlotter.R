#' mapPlotter
#'
#' @param transectObject
#' @param returnData
#'
#' @return
#' @export
#'
#' @examples

mapPlotter <- function(transectObject,
                       returnData=TRUE,
                       streamChannelFile,
                       ...){
  cat(crayon::green("Generating Map Plot"))

  startTime <- Sys.time()

  rasterBbox <- transectObject$satImage %>%
    attr("bb") %>%
    matrix(2,byrow = TRUE) %>% data.frame() %>%
    sf::st_as_sf(coords=c("X2","X1"),crs=4326) %>% sf::st_bbox()


  channels <- sf::read_sf(streamChannelFile) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_crop(rasterBbox)


  mapPlot <- ggmap::ggmap(transectObject$satImage)

  if(!is.null(transectObject$plotDataContours5ft)){
    mapPlot <- mapPlot +
      ggplot2::geom_sf(data=transectObject$plotDataContours5ft,
                       col=ggplot2::alpha("black",0.6),
                       inherit.aes = FALSE)
  }

  if(!is.null(transectObject$plotDataContours1ft)){
    mapPlot <- mapPlot +
      ggplot2::geom_sf(data=transectObject$plotDataContours1ft,
                       col=ggplot2::alpha("black",0.3),linetype=2,size=.2,
                       inherit.aes = FALSE)
  }

  if(!is.null(transectObject$ProcessPolygons_2ft)){
    mapPlot <- mapPlot +
      ggplot2::geom_sf(data=transectObject$ProcessPolygons_2ft,
                       col=ggplot2::alpha("green4",0.5),size=.2,
                       fill=ggplot2::alpha("green4",0.3),
                       inherit.aes = FALSE)
  }

  if(!is.null(transectObject$ProcessPolygons_1ft)){
    mapPlot <- mapPlot +
      ggplot2::geom_sf(data=transectObject$ProcessPolygons_1ft,
                       col=ggplot2::alpha("blue4",0.6),size=.2,
                       fill=ggplot2::alpha("blue4",0.5),
                       inherit.aes = FALSE)
  }


  mapPlot <- mapPlot +
    ggplot2::geom_sf(data=transectObject$mainLine %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE,
                     show.legend = FALSE,size=1,color="blue") +
    ggplot2::geom_sf(data=transectObject$leftSide %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE,col=ggplot2::alpha("red4",.5)) +
    ggplot2::geom_sf(data=transectObject$rightSide %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE,col=ggplot2::alpha("purple4",.5)) +
    ggplot2::geom_sf(data=transectObject$ls0 %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE,col=ggplot2::alpha("red1",.4)) +
    ggplot2::geom_sf(data=transectObject$rs0 %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE,col=ggplot2::alpha("purple1",.4)) +
    ggplot2::geom_sf(data=channels,col="skyblue",alpha=.1,
                     inherit.aes=FALSE,size=.6,linetype=3)+
    ggplot2::geom_sf(data = transectObject$sampledPoints %>%
                       sf::st_transform(crs=4326),
                     inherit.aes = FALSE, color = "turquoise4",size=.1) +
    ggplot2::geom_sf_text(transectObject$sampledPoints %>%
                            sf::st_transform(crs=4326),
                          mapping=ggplot2::aes(label=pointID),
                          inherit.aes = FALSE,col="black",size=3)+
    ggplot2::theme(panel.grid=ggplot2::element_line(color="transparent"))

  outputTimer(startTime)

  if(returnData == TRUE){
    transectObject$mapPlot <- mapPlot
    return(transectObject)
  }

}
