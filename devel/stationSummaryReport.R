library(dplyr)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(ggplot2)
library(PAMsbuoy)
library(webshot)
stationSummaryReport <- function(stationList, title='Sonobuoy Deployment Summary',
                                 fileName='reportTemplate', outDir='Report', format='word') {
  reportDirs <- paste0(outDir, c('', '/Tables', '/Figures'))
  for(dir in reportDirs[which(!dir.exists(reportDirs))]) {
    dir.create(dir)
  }
  detSummary <- detectionSummary(stationList)
  myMap <- getMap(detSummary)
  stationPlot <- mapStations(stationList, map=myMap, size=2)
  stationPlotTitle <- stationPlot + labs(title='Figure 1: Caption style 1')
  detectionPlotCombined <- mapDetections(detSummary, map=myMap, value='NumDetections', size=2)
  detectionPlot <- mapDetections(detSummary, combine=FALSE, map=myMap, value='NumDetections', size=2)

  unlink(list.files(paste0(outDir, '/Figures'), full.names=TRUE))

  ggsave(filename='stationPlotTitle.jpeg', plot=stationPlotTitle, path=paste0(outDir, '/Figures'),
         width=4, height=3, units='in', scale=2)
  ggsave(filename='stationPlot.jpeg', plot=stationPlot, path=paste0(outDir, '/Figures'),
         width=4, height=3, units='in', scale=2)
  ggsave(filename='detectionPlotCombined.jpeg', plot=detectionPlotCombined, path=paste0(outDir, '/Figures'),
         width=4*.8, height=3*.8, units='in', scale=2)
  ggsave(filename='detectionPlot.jpeg', plot=detectionPlot, path=paste0(outDir, '/Figures'),
         width=8*.8, height=4*.8, units='in', scale=1.5)

  tempRows <- 50
  detSummary <- head(detSummary, tempRows)

  unlink(list.files(paste0(outDir, '/Tables'), full.names=TRUE))

  perStation <- detSummary %>% filter(Station == .$Station[1]) %>% nrow
  maxRows <- 25
  # Round the max number down so a station never gets split
  tableRows <- maxRows - (maxRows %% perStation)
  htmlTableToImage(makeHtmlTable(detSummary), tableRows = nrow(detSummary),
                   outDir=paste0(outDir, '/Tables'), fileName = 'detectionSummaryTable', maxRows = tableRows)

  switch(format,
         html = {
           outFile <- paste0(fileName, '.html')
           outFormat <- 'html_document'
         },
         word = {
           outFile <- paste0(fileName, '.docx')
           outFormat <- 'word_document'
           })
  rmarkdown::render(input='reportTemplate.Rmd', output_file = outFile,
                    output_dir = outDir,quiet=TRUE, output_format=outFormat)
}

htmlTableToImage <- function(inTable, tableRows, headerHeight=59, rowHeight=37,
                             outDir='Report', fileName='table',  maxRows=30) {
  # Height 59 if no cap, 94 if cap
  # Off by a pixel
  if(is.null(webshot:::find_phantom())) {
    webshot::install_phantomjs()
  }
  outPath <- paste0(outDir, '/')
  tmp <- tempfile('tmpTable', fileext = '.html')
  myTable <- inTable
  rmarkdown::render('tableTemplate.Rmd', tmp, output_format='html_document', quiet=TRUE)
  for(tbls in 0:ceiling(tableRows/maxRows)) {
    imageName <- paste0(outPath, fileName, '_', tbls-1, '.png')
    if(tbls==0) {
      imageName <- paste0(outPath, fileName, 'Header.png')
      top <- 0
      length <- headerHeight
    } else if(tbls==ceiling(tableRows/maxRows)) {
      top <- headerHeight + rowHeight*maxRows*(tbls-1)
      length <- min(maxRows, tableRows-maxRows*(tbls-1))*rowHeight + 1
    } else {
      top <- headerHeight + rowHeight*maxRows*(tbls-1)
      length <- min(maxRows, tableRows-maxRows*(tbls-1))*rowHeight
    }
    webshot(tmp, file=imageName, cliprect=c(top, 0, 1000, length))
  }
  unlink(tmp)
}

makeHtmlTable <- function(summaryData) {
  detSummary <- summaryData %>%
    mutate(StationNum = as.numeric(as.factor(Station)),
           KSpecies=paste0(Species, ' <b>[', UniqueDetections,']</b>'),
           KBuoy=paste0(Buoy,' <b>[', NumDetections,']</b>'))
  odds <- which(detSummary$StationNum %% 2 == 1)
  detSummary <- select(detSummary, -StationNum) %>%
    select(Station, KSpecies, KBuoy, Latitude, Longitude, UTC)
  myColumns <- c('Station', 'Species<br/>[Unique Detections]', 'Buoy<br/>[Detections]',
                 'Latitude', 'Longitude', 'UTC')
  kable(detSummary,  align='c', digits=2,
        col.names=myColumns, escape=FALSE, format='html') %>%
    kable_styling('bordered') %>%
    row_spec(odds, background='#edf0f4') %>%
    collapse_rows(which(colnames(detSummary) %in% c('KSpecies','Station')))
}
