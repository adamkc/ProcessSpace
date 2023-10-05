#' exportSpatials
#'
#' @param transectObject
#' @param sectionName
#' @param addXSectionPlots
#' @param rasterPaletteLimits
#'
#' @return
#' @export
#'
#' @examples
exportSpatials <- function(transectObject,
                           sectionName="temp",
                           addXSectionPlots=TRUE,
                           rasterPaletteLims = c(-2,2)){

  colorPalatte <- plotKML::SAGA_pal[[21]]#$SG_COLORS_GREEN_RED_BLUE
  suppressMessages({
    suppressWarnings({
      plotKML::plotKML.env(silent = TRUE, kmz = TRUE,
                           colour_scale_numeric = colorPalatte)
      plotKML::kml_open(paste0(sectionName,"_temp_"),
                        overwrite=TRUE,kml_visibility = FALSE)
      cat(crayon::green("Writing kml to:\n"))
      cat(crayon::green(paste0(sectionName,".kml")))
      cat("\n")
      #--
      if(!is.null(transectObject$mainLine))
        transectObject$mainLine %>%
        sf::st_cast("MULTILINESTRING") %>%
        sf::st_line_merge() %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="mainLine",
                           colour="blue",width=5)
      #--
      if(!is.null(transectObject$sampledPoints)){
        if(addXSectionPlots){
          exportIndividualXSectionPlots(transectObject,sectionName)
          temp <- transectObject$sampledPoints %>% sf::as_Spatial()
          plotKML::kml_layer(temp,
                             subfolder.name="sampledPoints",
                             points_names = temp$pointID,
                             html.table = paste0("<img style=\"max-width:800px;\" src=\"",
                                                 sectionName,"-Images/Transect_",
                                                 temp$pointID,"_temp_.png\">"),
                             colour="turquoise", size=0.5,
                             shape="http://maps.google.com/mapfiles/kml/pal2/icon18.png")
        } else{
          temp <- transectObject$sampledPoints %>% sf::as_Spatial()
          plotKML::kml_layer(temp,subfolder.name="sampledPoints",
                             points_names = temp$pointID,
                             colour="turquoise", size=0.5,
                             shape="http://maps.google.com/mapfiles/kml/pal2/icon18.png")
        }
      }

      #--
      if(!is.null(transectObject$leftSide))
        transectObject$leftSide %>%
        sf::st_cast("MULTILINESTRING") %>%
        sf::st_line_merge() %>%
        sf::st_as_sf() %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="leftSide",
                           colour="red4",alpha=.5,width=5)
      #--
      if(!is.null(transectObject$rightSide))
        transectObject$rightSide %>%
        sf::st_cast("MULTILINESTRING") %>%
        sf::st_line_merge() %>%
        sf::st_as_sf() %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="rightSide",
                           colour="purple4",alpha=.5,width=5)
      #--
      if(!is.null(transectObject$ls0))
        transectObject$ls0 %>%
        sf::st_as_sf() %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="ls_transects",
                           colour="red",alpha=.3,width=3)
      #--
      if(!is.null(transectObject$rs0))
        transectObject$rs0 %>%
        sf::st_as_sf() %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="rs_transects",
                           colour="purple",alpha=.3,width=3)
      #--
      if(!is.null(transectObject$ProcessPolygons_1ft))
        transectObject$ProcessPolygons_1ft %>%
        sf::st_as_sf() %>%
        dplyr::mutate(Name = "ProcessPolygons_1ft",
                      Area_m2=Area_m2%>%round(2)) %>%
        dplyr::select(Name,Area_m2,pointID,Side) %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="ProcessPolygons_1ft",
                           colour="blue",fill="blue",alpha=.2,width=3,
                           balloon = TRUE)
      #---
      if(!is.null(transectObject$ProcessPolygons_2ft))
        transectObject$ProcessPolygons_2ft %>%
        sf::st_as_sf() %>%
        dplyr::mutate(Name = "ProcessPolygons_2ft",
                      Area_m2=Area_m2%>%round(2)) %>%
        dplyr::select(Name,Area_m2,pointID,Side) %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="ProcessPolygons_2ft",
                           colour="green",alpha=.2,width=3,
                           balloon = TRUE)
      ##------------MERGED POLYGONS:
      if(!is.null(transectObject$ProcessPolygons_1ft))
        transectObject$ProcessPolygons_1ft %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        dplyr::mutate(Name = "ProcessPolygons_1ft_Merged",
                      Area_m2=sf::st_area(.,)%>%round(2),
                      Area_acres=(Area_m2/4047)%>%round(2)) %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="ProcessPolygons_1ft_Merged",
                           colour="blue",fill="blue",alpha=.2,width=3,
                           balloon = TRUE)
      #--
      if(!is.null(transectObject$ProcessPolygons_2ft))
        transectObject$ProcessPolygons_2ft %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        dplyr::mutate(Name = "ProcessPolygons_2ft_Merged",
                      Area_m2=sf::st_area(.,)%>%round(2),
                      Area_acres=(Area_m2/4047)%>%round(2)) %>%
        sf::as_Spatial() %>%
        plotKML::kml_layer(subfolder.name="ProcessPolygons_2ft_Merged",
                           balloon = TRUE,
                           colour="green",alpha=.2,width=3)
      #--
      #outerBoundsRange <- max(abs(range(transectObject$XSectionPlotData$deltaEl)))

      if(!is.null(transectObject$detrendedRaster)){
        tempRaster <- raster::raster(transectObject$detrendedRaster)
        raster::writeRaster(tempRaster,paste0(sectionName,"_DetrendedRaster.tif"))
        tempRaster <- tempRaster %>% raster::mask(transectObject$streamBuffer)
        tempRaster[tempRaster < rasterPaletteLims[1]] <- rasterPaletteLims[1]
        tempRaster[tempRaster > rasterPaletteLims[2]] <- rasterPaletteLims[2]
        tempRaster[1:2,1] <- rasterPaletteLims
        plotKML::kml_layer(obj = tempRaster,
                           subfolder.name="Height Above Water",
                           raster_name = paste0(sectionName,"_temp_","El.png"),
                           colour=DetrendedElevation,
                           colour_scale=SAGA_pal[[21]])
      }

      #Wrap it up:
      plotKML::kml_close(file.name=paste0(sectionName,"_temp_.kml"))
      # files <- list.files(pattern=paste0(sectionName,"_temp_"),
      #                     path = getwd(),recursive=TRUE)
      files <- c(paste0(sectionName,"_temp_El.png"),
                 paste0(sectionName,"_temp_El_legend.png"),
                 paste0(sectionName,"-Images"),
                 paste0(sectionName,"_temp_.kml"),
                 paste0(sectionName,"_DetrendedRaster.tif"))
      kml_compress_fixed(file.name=paste0(sectionName,"_temp_.kml"),
                         files = files)
    })
  })

}

