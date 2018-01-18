library(dplyr)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(ggplot2)
library(PAMsbuoy)
stationSummaryReport <- function(stationList, title='Sonobuoy Deployment Summary', outdir='Report') {
  if(!dir.exists(outdir)) {
    dir.create(outdir)
  }
  detSummary <- detectionSummary(stationList)
  buoyPositions <- do.call(rbind, lapply(stationList, function(s) {
    do.call(rbind, lapply(s$buoys, function(b) {
      buoyDat <- b$position[1,]
      buoyDat$Station <- gsub('(.*)\\..*', '\\1', attr(s, 'station'))
      buoyDat
    }))
  }))
  myMap <- getMap(buoyPositions)
  stationPlot <- mapStations(stationList, map=myMap)
  detectionPlotCombined <- mapDetections(detSummary, map=myMap, value='NumDetections')
  detectionPlot <- mapDetections(detSummary, combine=FALSE, map=myMap, value='NumDetections')
  ggsave(filename='detectionPlot.jpeg', plot=detectionPlot, path=outdir,
         width=4, height=3, units='in')
  tempRows <- 20
  rmarkdown::render(input='reportTemplate.Rmd', output_file = 'reportTemplate.docx',
                    output_dir = outdir,quiet=TRUE, output_format='word_document')
}


