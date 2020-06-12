#' rasterPlotter
#'
#' @param transectObject
#' @param rasterDir
#' @param interpSmoothness
#'
#' @return
#' @export
#'
#' @examples

rasterPlotter <- function(transectObject,
                          rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                          interpSmoothness = "Smooth",
                          ...){
  cat(crayon::green("Generating Detrended Elevation Raster"))
  startTime <- Sys.time()

  r <- raster::raster(rasterDir)
  rasterBbox <- transectObject$satImage %>%
    attr("bb") %>%
    matrix(2,byrow = TRUE) %>%
    data.frame() %>%
    sf::st_as_sf(coords=c("X2","X1"),crs=raster::crs(r)) %>%
    sf::as_Spatial() %>%
    sp::spTransform(raster::crs(r))

  mainLine_projected <- transectObject$mainLine %>%
    sf::st_transform(raster::crs(r))

  bufferPoly <- sf::st_union(transectObject$leftSide,transectObject$rightSide) %>%
    sf::st_transform(raster::crs(r)) %>%
    extent.sf()

  r.clip <- raster::crop(x = r, y=bufferPoly)# %>%
    #raster::projectRaster(crs = raster::crs(r))

  # r.clip.df <- r.clip %>% methods::as("SpatialPixelsDataFrame") %>%
  #   as.data.frame()
  # names(r.clip.df) <- c("Value","x","y")
  # r.clip.df <- r.clip.df %>%
  #   dplyr::mutate(Zscore = abs(Value - mean(Value,na.rm=TRUE))) %>%
  #   dplyr::mutate(Value= dplyr::case_when(Zscore < 3 * sd(Value,na.rm=TRUE) ~ Value,
  #                                         TRUE ~ mean(Value,na.rm=TRUE)))

  ##Detrended Raster
  ## Interpolation from:
  ## https://mgimond.github.io/Spatial/interpolation-in-r.html
  #library(gstat)
  #library(sp)


  deltaElPts <- transectObject$XSectionPlotData %>%
    dplyr::select(geometry,deltaEl) %>%
    sf::as_Spatial() %>%
    sp::spTransform(raster::crs(r))

  r.new <- raster::raster(ext=raster::extent(r.clip),
                          ncol=ncol(r.clip),nrow=nrow(r.clip),
                          crs=raster::crs(r.clip)) %>%
    methods::as('SpatialPixels')

  print("DeltaEL")
  print(sf::st_crs(deltaElPts))
  print(raster::crs(deltaElPts))

  print("r.new")
  print(sf::st_crs(r.new))
  print(raster::crs(r.new))

  #x.interp <- gstat::idw(deltaEl~1,locations=deltaElPts,newdata=r.new,idp=2.0)
  if(interpSmoothness=="Smooth")
    suppressMessages(
      x.interp.krige <- gstat::krige(deltaEl~1,locations=deltaElPts,
                                     newdata=r.new,
                                     nmax=20,nmin=10,maxdist=40)
    )
  if(interpSmoothness=="Coarse")
    suppressMessages(
      x.interp.krige <- gstat::krige(deltaEl~1,locations=deltaElPts,
                                     newdata=r.new,
                                     nmax=1,nmin=1,maxdist=40)
    )
  #x.npsp.interp <- npsp::interp

  names(x.interp.krige) <- c("DetrendedElevation","KrigeVar")
  #plot(x.interp.krige)

  transectObject$detrendedRaster <- x.interp.krige

  outputTimer(startTime)

  return(transectObject)
}

