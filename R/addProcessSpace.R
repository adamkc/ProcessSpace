#' addProcessSpace
#'
#' @param transectObject Object created by the \code{generateCrossSections} function
#'
#' @return transectObject that can be passed on to other functions or passed to exportSpatials


addProcessSpace <- function(transectObject){
  cat(crayon::green("Calculating Process Space Polygons"))
  startTime <- Sys.time()

  ##ErrorCheck:
  if(is.null(transectObject$XsectionElevations)){
    return(cat(crayon::red("\nNot generating Process Space Polygons: Xsection plot data missing. \nRun 'addCrossSectionElevations()' first. ")))
  }

  BufferLengths <- transectObject$XsectionElevations %>% data.frame() %>%
    dplyr::select(Transect,Side,metersLength,deltaElCat,Index) %>%
    dplyr::mutate(Transect=factor(Transect)) %>%
    dplyr::group_by(Transect,Side) %>%
    dplyr::arrange(Index) %>%
    dplyr::mutate(waterEdge = (deltaElCat=="Water"|Index==1)&
                    dplyr::lead(deltaElCat!="Water",1,default = TRUE)&
                    dplyr::lead(deltaElCat!="Water",2,default = TRUE),
                  vegEdge = (deltaElCat %in% c("Water","Veg")|Index==1)&
                    dplyr::lead(deltaElCat=="Dry",1,default = TRUE)&
                    dplyr::lead(deltaElCat=="Dry",2,default = TRUE)) %>%
    tidyr::gather(Edge,EdgeTF,c(waterEdge,vegEdge)) %>%
    dplyr::filter(EdgeTF==TRUE) %>%
    dplyr::select(Transect,Side,metersLength,Edge) %>%
    dplyr::ungroup() %>% dplyr::group_by(Transect,Side,Edge) %>%
    dplyr::summarize(metersLength = min(abs(metersLength),na.rm=TRUE)) %>%
    dplyr::ungroup()  %>%
    tidyr::spread(Edge,metersLength) %>%
    dplyr::mutate(vegEdge = tidyr::replace_na(vegEdge,replace = 0),
                  waterEdge = tidyr::replace_na(waterEdge,replace = 0))



  buffers <- rbind(transectObject$sampledPoints %>%
                     dplyr::mutate(Side="ls"),
                   transectObject$sampledPoints %>%
                     dplyr::mutate(Side="rs")) %>%
    dplyr::mutate(pointID = as.character(pointID)) %>%
    dplyr::right_join(BufferLengths,by = c("Side", "pointID"="Transect")) #%>%
  #This next thing is a bug.  We need a test for elevation never gets to Veg threshold or
  # elevation exceeds Veg in first pixel so never Veg.
  # Maybe the move is to define Veg as the first that exceeds the limit...
  # replace_na(list(Veg = max(transectObject$XsectionElevations$length),
  #                 Dry = max(transectObject$XsectionElevations$length))) %>%
  # mutate(VegBuff = ifelse(!is.na(Veg),
  #                     yes=Veg,
  #                     no=ifelse(is.na(Dry),
  #                               yes = max(transectObject$XsectionElevations$length),
  #                               no = 0.1)),
  #        DryBuff = ifelse(!is.na(Dry),
  #                     yes=Dry,
  #                     no=max(transectObject$XsectionElevations$length)))

  ##1ft:
  buffers_1ft <- sf::st_buffer(x = buffers,
                               dist=units::as_units(buffers$waterEdge,"m"))
  ProcessLines_1ft <- rbind(transectObject$ls0,transectObject$rs0) %>%
    sf::st_intersection(buffers_1ft) %>%
    dplyr::filter(Side==Side.1 & pointID == pointID.1)
  ProcessPolygon.ls_1ft <- concavemanWrapper(ProcessLines_1ft %>%
                                               dplyr::filter(Side=="ls")) %>%
    dplyr::mutate(Side = "ls",
                  Name = paste0(pointID,".",Side))

  ProcessPolygon.rs_1ft <- concavemanWrapper(ProcessLines_1ft %>%
                                               dplyr::filter(Side=="rs")) %>%
    dplyr::mutate(Side = "rs",
                  Name = paste0(pointID,".",Side))
  ProcessPolygons_1ft <- rbind(ProcessPolygon.ls_1ft,ProcessPolygon.rs_1ft) %>%
    sf::st_sf() %>%
    dplyr::arrange(pointID,Side)

  ##2ft:
  buffers_2ft <- sf::st_buffer(x = buffers,
                               dist=units::as_units(buffers$vegEdge,"m"))
  ProcessLines_2ft <- rbind(transectObject$ls0,transectObject$rs0) %>%
    sf::st_intersection(buffers_2ft) %>%
    dplyr::filter(Side==Side.1 & pointID == pointID.1)
  ProcessPolygon.ls_2ft <- concavemanWrapper(ProcessLines_2ft %>%
                                               dplyr::filter(Side=="ls")) %>%
    dplyr:: mutate(Side = "ls",
                   Name = paste0(pointID,".",Side))
  ProcessPolygon.rs_2ft <- concavemanWrapper(ProcessLines_2ft %>%
                                               dplyr::filter(Side=="rs")) %>%
    dplyr::mutate(Side = "rs",
                  Name = paste0(pointID,".",Side))
  ProcessPolygons_2ft <- rbind(ProcessPolygon.ls_2ft,ProcessPolygon.rs_2ft) %>%
    sf::st_sf()  %>% dplyr::arrange(pointID,Side)

  #pointPopup <- bind_cols(BufferLengths,)


  transectObject$ProcessPolygons_1ft <- ProcessPolygons_1ft
  transectObject$ProcessPolygons_2ft <- ProcessPolygons_2ft

  outputTimer(startTime)

  return(transectObject)

}

