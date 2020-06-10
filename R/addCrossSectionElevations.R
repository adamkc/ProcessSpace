#' addCrossSectionElevations
#'
#' @param transectObject Object created by the \code{generateCrossSections} function
#' @param rasterDir Directory containing the LiDAR raster dataset
#'
#' @return transectObject with the cross section elevations added.


addCrossSectionElevations <- function(transectObject,
                                      rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                                      ...){
  {
    # Extremely fast method but doesn't have "along=TRUE" option:
    # transectObject$ls0_projected <- transectObject$ls0 %>%
    #   sf::st_transform(crs = r$crs)
    #
    # transectObject$rs0_projected <- transectObject$rs0 %>%
    #   sf::st_transform(crs = r$crs)
    #
    # q <- r$copy()
    # q$crop(transectObject$ls0_projected)
    # elevations.ls <- q$extract(transectObject$ls0_projected)
    # names(elevations.ls) <- paste0("arcid_",transectObject$ls0$pointID)
    #
    # q <- r$copy()
    # q$crop(transectObject$rs0_projected)
    # elevations.rs <- q$extract(transectObject$rs0_projected)
    # names(elevations.rs) <- paste0("arcid_",transectObject$rs0$pointID)
  } # Fast Elevation extraction that doesn't go "along". maybe convert to pts?
  cat(crayon::green("Extracting Cross Section Elevations"))
  startTime <- Sys.time()

  r <- raster::raster(rasterDir)

  transectObject$sampledPoints$streamEl <- raster::extract(r,
                                                           transectObject$sampledPoints%>%
                                                             sf::st_transform(raster::crs(r)))

  ls0_points <- transectObject$ls0 %>%
    sf::st_transform(raster::crs(r)) %>%
    sf::st_line_sample(density = units::as_units(1,"m")) %>% #1pts/meter
    sf::st_sf() %>%
    dplyr::mutate(Transect =transectObject$sampledPoints$pointID,
                  Side = "ls",
                  length=as.numeric(sf::st_length(transectObject$ls0))) %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    dplyr::group_by(Transect) %>%
    dplyr::mutate(pointCount=dplyr::n()) %>%
    dplyr::ungroup()

  rs0_points <- transectObject$rs0 %>%
    sf::st_transform(raster::crs(r)) %>%
    sf::st_line_sample(density = units::as_units(1,"m")) %>% #1pts/meter
    sf::st_sf() %>%
    dplyr::mutate(Transect =transectObject$sampledPoints$pointID,
                  Side = "rs",
                  length=as.numeric(sf::st_length(transectObject$rs0))) %>%
    sf::st_cast("POINT",warn=FALSE) %>%
    dplyr::group_by(Transect) %>%
    dplyr::mutate(pointCount=dplyr::n()) %>%
    dplyr::ungroup()

  ##Raster Extracting:
  ls0_points$El <- raster::extract(r,ls0_points)
  rs0_points$El <- raster::extract(r,rs0_points)

  XsectionElevations <-  rbind(ls0_points,rs0_points) %>%
    dplyr::mutate(El = ifelse(El==0,yes=NA,no=El)) %>%
    #mutate(Index=row_number())  %>%
    dplyr::left_join(data.frame(Transect=transectObject$sampledPoints$pointID,
                                streamEl=transectObject$sampledPoints$streamEl,
                                stringsAsFactors = FALSE),
                     by="Transect") %>%
    dplyr::group_by(Transect) %>%
    dplyr::mutate(deltaEl = El-streamEl,
                  deltaElCat = dplyr::case_when(deltaEl<.30 ~ "Water",
                                                deltaEl<0.60 ~ "Veg",
                                                TRUE ~"Dry"),
                  deltaElCat = factor(deltaElCat, levels=c("Water","Veg","Dry"))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Transect,Side) %>%
    dplyr::mutate(test = which.min(abs(c(dplyr::first(deltaEl),dplyr::last(deltaEl)))),
                  Index = ifelse(test==1,
                                 yes = 1:dplyr::n(),
                                 no= dplyr::n():1),
                  percentLength = ifelse(test==1,
                                         yes = seq(from=0,to=1,length.out = dplyr::n()),
                                         no=seq(from=1,to=0,length.out = dplyr::n())),
                  metersLength = percentLength*length,
                  metersLength = dplyr::case_when(Side=="rs" ~ -metersLength,
                                                  TRUE ~ metersLength)) %>%
    dplyr::select(-test) %>%
    dplyr::arrange(Index) %>%
    dplyr::ungroup()


  outputTimer(startTime)

  transectObject$XsectionElevations <- XsectionElevations
  return(transectObject)

}


