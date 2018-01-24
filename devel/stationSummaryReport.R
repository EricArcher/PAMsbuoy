library(dplyr)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(ggplot2)
library(PAMsbuoy)
library(webshot)
stationSummaryReport <- function(stationList, title='Sonobuoy Deployment Summary', outdir='Report', format='word') {
  reportDirs <- paste0(outdir, c('', '/Tables', '/Figures'))
  for(dir in reportDirs[which(!dir.exists(reportDirs))]) {
    dir.create(dir)
  }
  detSummary <- detectionSummary(stationList)
  myMap <- getMap(detSummary)
  stationPlot <- mapStations(stationList, map=myMap)
  stationPlotTitle <- stationPlot + labs(title='Figure 1: Caption style 1')
  detectionPlotCombined <- mapDetections(detSummary, map=myMap, value='NumDetections')
  detectionPlot <- mapDetections(detSummary, combine=FALSE, map=myMap, value='NumDetections')

  unlink(list.files(paste0(outdir, '/Figures'), full.names=TRUE))

  ggsave(filename='stationPlot.jpeg', plot=stationPlot, path=paste0(outdir, '/Figures'),
         width=4, height=3, units='in', scale=1)
  ggsave(filename='detectionPlotCombined.jpeg', plot=detectionPlotCombined, path=paste0(outdir, '/Figures'),
         width=4*.8, height=3*.8, units='in', scale=2)
  ggsave(filename='detectionPlot.jpeg', plot=detectionPlot, path=paste0(outdir, '/Figures'),
         width=8*.8, height=4*.8, units='in', scale=1.5)

  tempRows <- 10
  detSummary <- head(detSummary, tempRows)

  unlink(list.files(paste0(outdir, '/Tables'), full.names=TRUE))

  htmlTableToImage(makeHtmlTable(detSummary, caption='Table 1: This is where my name goes'), tableRows = nrow(detSummary),
                   outdir=paste0(outdir, '/Tables'), filename = 'detectionSummaryTable',maxRows = 3)

  switch(format,
         html = {
           outFile <- 'reportTemplate.html'
           outFormat <- 'html_document'
         },
         word = {
           outFile <- 'reportTemplate.docx'
           outFormat <- 'word_document'
           })
  rmarkdown::render(input='reportTemplate.Rmd', output_file = outFile,
                    output_dir = outdir,quiet=TRUE, output_format=outFormat)
}

htmlTableToImage <- function(inTable, tableRows, headerHeight=94, rowHeight=37,
                             outdir='Report', filename='table',  maxRows=30) {
  # Height 59 if no cap, 94 if cap
  if(is.null(webshot:::find_phantom())) {
    webshot::install_phantomjs()
  }
  outPath <- paste0(outdir, '/')
  tmp <- tempfile('tmpTable', fileext = '.html')
  myTable <- inTable
  rmarkdown::render('tableTemplate.Rmd', tmp, output_format='html_document', quiet=TRUE)
  for(tbls in 1:ceiling(tableRows/maxRows)) {
    imageName <- paste0(outPath, filename, '_', tbls, '.png')
    if(tbls==1) {
      top <- 0
      length <- min(maxRows, tableRows)*rowHeight + headerHeight
    } else {
      top <- headerHeight + rowHeight*maxRows*(tbls-1)
      length <- min(maxRows, tableRows-maxRows*(tbls-1))*rowHeight
    }
    webshot(tmp, file=imageName, cliprect=c(top, 0, 1000, length))
  }
  unlink(tmp)
}

makeHtmlTable <- function(summaryData, caption=NULL) {
  detSummary <- summaryData %>%
    mutate(StationNum = as.numeric(as.factor(Station)),
           KSpecies=paste0(Species, ' <b>[', UniqueDetections,']</b>'),
           KBuoy=paste0(Buoy,' <b>[', NumDetections,']</b>'))
  odds <- which(detSummary$StationNum %% 2 == 1)
  detSummary <- select(detSummary, -StationNum) %>%
    select(Station, KSpecies, KBuoy, Latitude, Longitude, UTC)
  myColumns <- c('Station', 'Species<br/>[Unique Detections]', 'Buoy<br/>[Detections]',
                 'Latitude', 'Longitude', 'UTC')
  kable(detSummary,  align='c', digits=2, caption=caption,
        col.names=myColumns, escape=FALSE, format='html') %>%
    kable_styling('bordered') %>%
    row_spec(odds, background='#edf0f4') %>%
    collapse_rows(which(colnames(detSummary) %in% c('KSpecies','Station')))
}
