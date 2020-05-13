#' longitudinalElevation
#'
#' @param transectObject
#' @param rasterDir
#' @param makePlot
#' @param returnData
#' @param plotFileName
#'
#' @return
#' @export
#'
#' @examples
longitudinalElevation <- function(transectObject,
                                  rasterDir = "GeoData/Raster/ChildsDEM_m.tif",
                                  makePlot=TRUE,
                                  returnData=TRUE,
                                  plotFileName="longPlot.png"){
  cat(crayon::green("Generating Longitudinal Elevation\n"))
  # Velox Method, Super fast but can't move along the line object:
  #   r <- velox(rasterDir)
  #   transectObject$mainLine_projected <- transectObject$mainLine %>%
  #     sf::st_transform(crs = r$crs)
  #   q <- r$copy()
  #   q$crop(transectObject$mainLine_projected)
  #   mainChannelSlopes <- q$extract(transectObject$mainLine_projected)
  # mainChannelSlopes %>% do.call(what = rbind) %>% data.frame(El = .) %>%
  #   arrange(desc(El)) %>% mutate(Index=row_number())
  #Slow raster::version:
  r <- raster::raster(rasterDir)
  transectObject$mainLine_projected <- transectObject$mainLine %>%
    sf::st_transform(raster::crs(r))
  ElPoints <- sf::st_line_sample(transectObject$mainLine_projected,
                                 density = units::as_units(1,"ft")) #1pts/foot

  pointsPerSeg = lapply(ElPoints,length) %>%
    unlist() %>%
    data.frame(Points = ./2) %>%
    dplyr::mutate(segLength_ft = as.numeric(sf::st_length(transectObject$mainLine_projected)),
                  segLength_m =  as.numeric(sf::st_length(transectObject$mainLine)),
                  Segment = as.factor(transectObject$mainLine_projected$arcid)) %>%
    dplyr::select(-.)
  ElPoints <- ElPoints %>% sf::as_Spatial() %>% sp::SpatialPoints()

  temp <- raster::crop(x = r,y = transectObject$mainLine_projected)
  # mainChannelSlopes <- raster::extract(temp,
  #                                      transectObject$mainLine_projected,
  #                                      method="simple",along=TRUE)

  ElPoints$El <- raster::extract(temp,ElPoints,method="simple")
  ElPoints <- ElPoints %>% data.frame() %>%
    dplyr::select(-optional) %>%
    dplyr::mutate(Segment = as.factor(rep(transectObject$mainLine_projected$arcid,
                                          pointsPerSeg$Points)),
                  SegmentName = rep(paste0("arcid_",
                                           transectObject$mainLine_projected$arcid),
                                    pointsPerSeg$Points)) %>%
    dplyr::left_join(pointsPerSeg,by="Segment") %>%
    dplyr::arrange(dplyr::desc(El)) %>%
    dplyr::mutate(Index=dplyr::row_number())


  if(makePlot==TRUE){
    longitudinalPlotData <- ElPoints %>%
      tidyr::drop_na() %>%
      dplyr::group_by(Segment) %>%
      dplyr::mutate(percentLength = seq(from=0,to=1,length.out = dplyr::n()),
                    metersLength = percentLength*segLength_m,
                    deltaLength = segLength_m/dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cumulativeLength=cumsum(deltaLength))
    # {temp1 <<- group_by(., Segment) %>%
    #   summarize(maxEL = max(El)) %>%
    #   arrange(desc(maxEL)) %>% dplyr::select(Segment)} %>%
    # mutate(Segment = factor(Segment,levels=temp1$Segment)) %>%
    # arrange(Segment) %T>%
    # {temp2 <<- group_by(.,Segment) %>% summarize(length=first(length)) %>%
    #   ungroup() %>% mutate(cumlength=cumsum(length),
    #                        prevLength = lag(cumlength,default = 0))} %>%
    # left_join(temp2) %>%
    # mutate(cumulativeLength = metersLength+prevLength)

    breaks = seq(0,max(longitudinalPlotData$cumulativeLength),length.out = 5) %>%
      round(-1)

    cumPlot1 <- ggplot2::ggplot(longitudinalPlotData,
                                ggplot2::aes(x=cumulativeLength,y=El,
                                             group=Segment,col=as.numeric(Segment))) +
      # stat_smooth(geom="line",method="lm",se=FALSE,
      #             fullrange=TRUE,show.legend = FALSE,alpha=.6,linetype=3) +
      ggplot2::geom_line(size=1,show.legend=FALSE)  +
      ggplot2::scale_color_gradient(low = "blue",high="green") +
      ggplot2::geom_text(data=longitudinalPlotData %>%
                           dplyr::group_by(Segment) %>%
                           dplyr::slice(1),
                         mapping = ggplot2::aes(label=Segment),
                         col="black",size=1.5) +
      ggplot2::ylab("Elevation (m)") +
      ggplot2::scale_x_continuous(breaks = breaks)  +
      ggplot2::ggtitle("Longitudinal Profile") +
      ggplot2::theme_bw()+
      ggplot2::theme(axis.title.x = ggplot2::element_blank())#;cumPlot1
    #ggsave("CumulativeSlope.png",height=3,width=4,dpi=600)

    segmentLength = round(max(longitudinalPlotData$cumulativeLength)/40, -1)
    cumPlot2 <- longitudinalPlotData %>%
      dplyr::mutate(cumulativeLengthSegment = cumulativeLength +
                      (segmentLength - cumulativeLength %% segmentLength),
                    pixelSlope = El-dplyr::lag(El)) %>%
      dplyr::group_by(cumulativeLengthSegment) %>%
      dplyr::summarize(segmentEl = mean(El),
                       segmentLength=  dplyr::last(cumulativeLength)-dplyr::first(cumulativeLength),
                       segmentSlopeperPixel = mean(pixelSlope),
                       segmentPercentSlope = (sum(pixelSlope)/segmentLength)*-100,
                       Segment=dplyr::first(Segment)) %>%
      dplyr::mutate(TenMeterSlope = segmentEl-dplyr::lag(segmentEl)) %>%
      ggplot2::ggplot(ggplot2::aes(x=cumulativeLengthSegment,y=segmentPercentSlope)) +
      ggplot2::geom_line(ggplot2::aes(col=as.numeric(cumulativeLengthSegment)),
                         show.legend=FALSE) +
      ggplot2::geom_smooth() +
      ggplot2::scale_color_gradient(low = "blue",high="green") +
      ggplot2::ylab("Percent Slope") +
      ggplot2::xlab("Cumulative Length (m)") +
      ggplot2::geom_text(label=sprintf("Segment Length: %s m",segmentLength),
                         x=0,y=0,hjust=-0,size=3) +
      ggplot2::scale_x_continuous(breaks=breaks) +
      #scale_y_continuous(breaks = c(0,-2,-4,-6),
      #minor_breaks = c(-1,-3,-5))+
      ggplot2::theme_bw()#;cumPlot2

    cumPlot1 + cumPlot2 + patchwork::plot_layout(ncol = 1,heights = c(2,1))
    ggplot2::ggsave(plotFileName,height=4,width=4,dpi=600)
  }

  if(returnData == TRUE){
    transectObject$longitudinalPlotData <- longitudinalPlotData
    return(transectObject)
  }
}

