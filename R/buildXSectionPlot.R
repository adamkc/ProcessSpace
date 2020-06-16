#' buildXSectionPlot
#'
#' @param transectObject
#' @param plotFileName
#' @param returnData
#' @param verticalCutoff
#'
#' @return
#' @export
#'
#' @examples

buildXSectionPlot <- function(transectObject,
                              plotFileName = "test.png",
                              returnData=TRUE,
                              verticalCutoff = 4,
                              streamChannelFile="GeoData/STREAM CHANNELS.shp",
                              ...){

  if(is.null(transectObject$XsectionElevations)){
    cat(crayon::red("Xsection plot data missing"))
    return()
  }

  tempPlotData <-transectObject$XsectionElevations %>% data.frame() %>%
    dplyr::group_by(Transect,Side) %>%
    dplyr::arrange(Index) %>%
    dplyr::mutate(exceedsLimit = (deltaEl > verticalCutoff | deltaEl < -verticalCutoff),
                  pastFirstLimit = cumsum(exceedsLimit)) %>%
    dplyr::filter(!pastFirstLimit) %>%
    dplyr::ungroup() %>%
    dplyr::select(-exceedsLimit,-pastFirstLimit)
  ##
  topLabel.df <- tempPlotData %>%
    dplyr::group_by(Side) %>%
    dplyr::summarize(x = mean(metersLength,na.rm=TRUE)) %>%
    dplyr::bind_rows(data.frame(Side="center",x=0,stringsAsFactors = FALSE)) %>%
    dplyr::mutate(label=factor(c("Right Side\n Looking Upstream",
                                 "Left Side\n Looking Upstream",
                                 "Stream Channel"),
                               levels=c("Right Side\n Looking Upstream",
                                        "Left Side\n Looking Upstream",
                                        "Stream Channel")),
                  y=c(max(tempPlotData$El,na.rm=TRUE)))
  ###


  transectPlot <- ggplot2::ggplot(tempPlotData,
                                  ggplot2::aes(x=-metersLength,y=El,
                                               col=deltaElCat,group=Transect))

  if(!is.null(transectObject$plotDataSideChannels)){
    transectPlot <- transectPlot +
      ggplot2::geom_point(data = transectObject$plotDataSideChannels,
                          mapping = ggplot2::aes(x=Dist,y=Elevation),size=.2,
                          col="lightblue",inherit.aes=FALSE)
  }

  transectPlot <- transectPlot +
    ggplot2::geom_line(show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(Water = "blue",
                                            Veg="darkgreen",
                                            Dry="tan")) +
    ggplot2::geom_vline(xintercept = 0,linetype=2,alpha=.4) +
    ggplot2::xlab("Distance from channel center (m)") +
    ggplot2::ylab("Elevation") +
    ggplot2::ggtitle("Stream Cross sections",
                     subtitle = "Color indicates height from channel bottom:
          Blue: <30cm; Green: <60cm; Tan: >60cm; heights > 5m removed.") +
    ggplot2::geom_hline(ggplot2::aes(yintercept=streamEl),alpha=.1,linetype=4) +
    ggplot2::geom_text(data=transectObject$XsectionElevations%>%
                         dplyr::filter(Side=="ls",Index==1)%>%
                         #group_by(Transect) %>% slice(1) %>% ungroup() %>%
                         data.frame(),
                       ggplot2::aes(y=El,label=paste0(Transect,"--")),x=-5,
                       size=3,inherit.aes = FALSE) +
    ggplot2::geom_label(data=topLabel.df,
                        mapping = ggplot2::aes(x=x,y=y,label=label,fill=Side),
                        col="white",show.legend=FALSE,
                        inherit.aes=FALSE,size=4) +
    ggplot2::scale_fill_manual(values = c("blue","red4","purple4"))+
    ggplot2::ylim(c(min(transectObject$sampledPoints$streamEl,na.rm=TRUE-5),
                    max(transectObject$sampledPoints$streamEl,na.rm=TRUE)+5)) +
    ggplot2::xlim(c(transectObject$XsectionElevations$length[1],
                    -transectObject$XsectionElevations$length[1])) +
    ggthemes::theme_clean()#;transectPlot


  transectObject$XSectionPlot <- transectPlot
  transectObject$XSectionPlotData <- tempPlotData %>%
    sf::st_as_sf()


  transectObject <- mapPlotter(transectObject,streamChannelFile=streamChannelFile)

  plotToSave <-  cowplot::plot_grid(transectObject$mapPlot, transectPlot)



  ggplot2::ggsave(plot = plotToSave,filename = plotFileName,
                  height=10,width=20,dpi=300)
  if(returnData) return(transectObject)
}


