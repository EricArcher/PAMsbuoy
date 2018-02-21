#' @name createSummaryReport
#' @title Create a Summary Report for a List of Stations
#' @description Creates a summary report to be used for cruise reports or as a starting
#'   point for publications. Saves the report as a word document, and saves all
#'   associated figures and tables as images in a Report folder. Automatically fills in
#'   summary information (number of sonobuoys deployed, detections, etc.), and prompts
#'   user for some information not contained in the databases. The created report should
#'   not be considered a final document, some information will need to be added / edited.
#'
#' @param stationList list of stations to create the summary report for
#' @param outDir directory to write the report to. Will create subfolders Report,
#'   Report/Tables, and Report/Figures within this directory
#' @param fileName name of the report file
#' @param format output format of report, either word or html
#'
#' @author Taiki Sakai \email{taiki.sakai@@noaa.gov}
#'
#' @import dplyr
#' @import webshot
#' @importFrom rmarkdown render
#' @importFrom knitr kable
#' @importFrom ggplot2 ggsave
#' @importFrom kableExtra collapse_rows row_spec kable_styling
#' @export
#'
createSummaryReport <- function(stationList, outDir='.', fileName='summaryReport', format='word') {
  # Need absolute paths for the reportTemplate.Rmd later
  outDir <- normalizePath(paste0(outDir, '/Report'))
  reportDirs <- paste0(outDir, c('', '/Tables', '/Figures'))

  for(dir in reportDirs[which(!dir.exists(reportDirs))]) {
    dir.create(dir, recursive=TRUE)
  }
  detSummary <- detectionSummary(stationList)

  makeReportMaps(stationList, detSummary, path=paste0(outDir, '/Figures'))

  # Just for testing
  tempRows <- 100
  detSummary <- head(detSummary, tempRows)


  # cruiseName <- readline(prompt = 'What is the name of this cruise? \n')
  # cruiseAbbr <- readline(prompt = 'What is the abbreviated name for this cruise? \n')
  cruiseName <- 'Not a Real Cruise'
  cruiseAbbr <- 'NaRC'
  # numVessels <- readline(prompt = 'How many vessels ')

  # Clear out tables directory
  unlink(list.files(paste0(outDir, '/Tables'), full.names=TRUE))

  tableToImage(detSummary, outDir=paste0(outDir, '/Tables'), fileName = 'detectionSummaryTable', maxRows = 25)

  switch(format,
         html = {
           outFile <- paste0(fileName, '.html')
           outFormat <- 'html_document'
         },
         word = {
           outFile <- paste0(fileName, '.docx')
           outFormat <- 'word_document'
           })
  # Once we're inside this render the wd is the system.file templates dir, so everything inside
  # the reportTemplate.Rmd needs to refer to absolute paths
  rmarkdown::render(input=system.file('templates/reportTemplate.Rmd', package='PAMsbuoy'), output_file = outFile,
                    output_dir = outDir,quiet=TRUE, output_format=outFormat)
}

tableToImage <- function(summaryData, headerHeight=59, rowHeight=37,
                             outDir='Report', fileName='table',  maxRows=30) {
  # Height 59 if no cap, 94 if cap
  # Off by a pixel

  outPath <- paste0(outDir, '/')
  tmp <- tempfile('tmpTable', fileext = '.html')
  # myTable is name used in tableTemplate.Rmd
  myTable <- makeHtmlTable(summaryData)
  rmarkdown::render('tableTemplate.Rmd', tmp, output_format='html_document', quiet=TRUE)
  # Saved as one big html in tmp file, now we slice it into bits
  stationRows <- summaryData %>% group_by(Station) %>% summarise(Rows=n(), mn=min(UTC)) %>% arrange(mn)
  stationRows <- stationRows$Rows
  imageNum <- 1
  thisRows <- 0
  for(st in 0:length(stationRows)) {
    if(st==0) {
      imageTop <- 0
      imageLength <- headerHeight
      imageName <- paste0(outPath, fileName, 'Header.png')
    } else {
      thisRows <- thisRows + stationRows[st]
      if((st == length(stationRows)) |
         (thisRows + stationRows[st+1]) > maxRows) {
        imageLength <- thisRows * rowHeight
        imageName <- paste0(outPath, fileName, '_', imageNum, '.png')
        imageNum <- imageNum + 1
        thisRows <- 0
      } else {
        next
      }
    }
    webshot(tmp, file=imageName, cliprect=c(imageTop, 0, 1000, imageLength))
    imageTop <- imageTop + imageLength
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

makeReportMaps <- function(stationList, detSummary, path) {
  myMap <- getMap(detSummary)
  stationPlot <- mapStations(stationList, map=myMap, size=2) + labs(title='Figure 1: Sonobuoy Stations')
  # detectionPlotCombined <- mapDetections(detSummary, map=myMap, value='NumDetections', size=2)
  detectionPlot <- mapDetections(detSummary, combine=FALSE, map=myMap, value='NumDetections', size=2, ncol=3) +
    labs(title='Figure 2: Species Detections')

  unlink(list.files(path, full.names=TRUE))

  ggsave(filename='stationPlot.jpeg', plot=stationPlot, path=path,
         width=6, height=4.5, units='in', dpi=200)
  # ggsave(filename='detectionPlotCombined.jpeg', plot=detectionPlotCombined, path=path,
  #        width=4*.8, height=3*.8, units='in', scale=2)
  ggsave(filename='detectionPlot.jpeg', plot=detectionPlot, path=path,
         width=8, height=4, units='in', dpi=300)
}
