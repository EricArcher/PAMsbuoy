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
#' @importFrom webshot webshot install_phantomjs
#' @importFrom stringr str_replace_all str_replace_na
#' @importFrom utils getFromNamespace
#' @importFrom rmarkdown render
#' @importFrom knitr kable include_graphics
#' @importFrom ggplot2 ggsave
#' @importFrom kableExtra collapse_rows row_spec kable_styling group_rows
#' @importFrom swfscMisc das.read
#' @export
#'
createSummaryReport <- function(stationList, outDir='.', fileName='summaryReport', format='word', test=FALSE,
                                wincruz = NULL, specFile = NULL) {
  # Need absolute paths for the reportTemplate.Rmd later
  outDir <- normalizePath(paste0(outDir, '/Report'))
  reportDirs <- paste0(outDir, c('', '/Tables', '/Figures'))

  for(dir in reportDirs[which(!dir.exists(reportDirs))]) {
    dir.create(dir, recursive=TRUE)
  }

  # Clear out tables and figures directory
  unlink(list.files(paste0(outDir, '/Figures'), full.names=TRUE))
  unlink(list.files(paste0(outDir, '/Tables'), full.names=TRUE))

  detSummary <- detectionSummary(stationList)
  reportText <- list()
  if(!test) {
  reportText$cruiseName <- readline(prompt = 'What is the name of this cruise? \n')
  reportText$cruiseAbbr <- readline(prompt = 'What is the abbreviated name for this cruise? \n')

  # Get names for vessels, then use to format for report
  myVessels <- levels(as.factor(detSummary$Cruise))
  if(length(myVessels)==0) {
    myVessels <- readline(prompt='No vessel IDs found in database. What is the name of the vessel? \n')
    detSummary$Cruise <- factor(str_replace_na(detSummary$Cruise, myVessels), levels = myVessels)
  } else {
    names(myVessels) <- myVessels
    for(v in seq_along(myVessels)) {
      myVessels[v] <- readline(prompt = paste0('What is the name of the vessel with ID ', myVessels[v], '? \n'))
    }
    detSummary$Cruise <- factor(str_replace_all(detSummary$Cruise, myVessels), levels = myVessels)
  }
  } else {
    reportText$cruiseName <- 'California Current Cetacean Ecosystem Assessment Survey'
    reportText$cruiseAbbr <- 'CalCurCEAS'
    myVessels <- c('1706' = 'Reuben Lasker')
    detSummary$Cruise <- 'Reuben Lasker'
  }

  # detSummary$Cruise <- factor(str_replace_all(detSummary$Cruise, myVessels), levels = myVessels)
  # Adding italics for rmd
  reportText$myVessels <- paste0('*', myVessels, '*')

  # Making some text for report. For now just using 1st station as example?
  stationInfo <- stationList[[1]]$stationInfo
  reportText$recordingSystem <- stationInfo$recordingSystem
  reportText$sampleRate <- round(stationInfo$sampleRate/1000, 0)
  reportText$sonobuoyType <- stationInfo$instrument_id
  reportText$nSonobuoys <- nrow(distinct(detSummary, Station, Buoy))
  recordLengths <- detSummary %>% group_by(StationType) %>% distinct(Station, RecordingLength) %>%
    summarise(Length=sum(RecordingLength))
  reportText$nStations <- nrow(distinct(filter(detSummary, StationType=='DensityEstimate'), Station))
  reportText$stationLength <- recordLengths %>% filter(StationType=='DensityEstimate') %>% .$Length
  reportText$nOpportunistic <- nrow(distinct(filter(detSummary, StationType=='Opportunistic'), Station, Buoy))
  reportText$opportunisticLength <- recordLengths %>% filter(StationType=='Opportunistic') %>% .$Length

  colorPalette <- c("#000000","#009E73", "#0072B2", "#D55E00", "#F0E442", "#CC79A7")
  colorNames <- c('Black', 'Green', 'Blue', 'Orange', 'Yellow', 'Pink')


  #### MAPPING. Do I need to recheck anything with filtered sets opp/de ?
  cat('Creating maps... \n')
  myMap <- getMap(detSummary, quiet=TRUE)
  figurePath <- paste0(outDir, '/Figures')
  # Plot of all the stations
  reportText$figureCount <- 1
  stationPlot <- makeStationPlot(detSummary, myMap, figurePath, reportText)

  # Opportunistic Only. Checking for WinCruz file to match species to vis id
  if(reportText$nOpportunistic > 0) {
    reportText$figureCount <- reportText$figureCount + 1
    opportunisticPlot <- makeOpportunisticPlot(detSummary, myMap, figurePath, reportText, wincruz, specFile)
  }

  # Plot of detections facetted by species
  reportText$figureCount <- reportText$figureCount + 1
  detectionPlot <- makeDetectionPlot(detSummary, myMap, figurePath, reportText)

  # Formatting vessel text in paragraph.
  reportText$vesselText <- if(length(myVessels)==1) {
    '.'
  } else {
    paste0(', including ',
           formatListGrammar(
             sapply(reportText$myVessels, function(v) {
               num <- detSummary %>% filter(Cruise==gsub('\\*', '', v)) %>% distinct(Buoy, Station) %>% nrow()
               paste0(num, ' from vessel ', v)
             })))
  }

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

  cat('\nCreating report...\n')
  # Once we're inside this render the wd is the system.file templates dir, so everything inside
  # the reportTemplate.Rmd needs to refer to absolute paths
  rmarkdown::render(input=system.file('templates/reportTemplate.Rmd', package='PAMsbuoy'), output_file = outFile,
                    output_dir = outDir,quiet=TRUE, output_format=outFormat)
}

tableToImage <- function(summaryData, headerHeight=59, rowHeight=37,
                             outDir='Report', fileName='table',  maxRows=30) {
  # Height 59 if no caption, 94 if caption. New head 58, 37 + 37 per row
  # find_phantom isnt exported, gotta do this all janky like
  checkPhantom <- utils::getFromNamespace('find_phantom', 'webshot')
  if(is.null(checkPhantom())) {
    webshot::install_phantomjs()
  }
  outPath <- paste0(outDir, '/')
  tmp <- tempfile('tmpTable', fileext = '.html')
  # myTable is name used in tableTemplate.Rmd
  myTable <- makeHtmlTable(summaryData)
  rmarkdown::render(system.file('templates/tableTemplate.Rmd', package='PAMsbuoy')
                    , tmp, output_format='html_document', quiet=TRUE)
  # Saved as one big html in tmp file, now we slice it into bits. Get rows per station here.
  stationRows <- summaryData %>% group_by(Station) %>% summarise(Rows=n(), mn=min(UTC)) %>% arrange(mn)
  stationRows <- stationRows$Rows
  imageNum <- 1
  thisRows <- 0
  cat('Creating tables... \n')
  pb <- txtProgressBar(min=0, max=length(stationRows), style=3)
  for(st in 0:length(stationRows)) {
    if(st==0) {
      # adding 1 later to pad end of table
      imageTop <- 0
      imageLength <- headerHeight - 1
      imageName <- paste0(outPath, fileName, 'Header.png')
    } else {
      thisRows <- thisRows + stationRows[st] + 1
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
    webshot::webshot(tmp, file=imageName, cliprect=c(imageTop, 0, 750, imageLength + 1))
    imageTop <- imageTop + imageLength
    setTxtProgressBar(pb, st)
  }
  unlink(tmp)
}

makeHtmlTable <- function(summaryData) {
  # This is to order factors by date, otherwise order is 1, 10 etc.
  stationOrder <- summaryData %>%
    arrange(UTC) %>%
    distinct(Station)
  stationOrder <- stationOrder$Station
  # HTMLS are just formatted for boldness
  detSummary <- summaryData %>%
    mutate(StationNum = as.numeric(factor(Station, levels=stationOrder)),
           HTMLSpecies=paste0(Species, ' <b>[', UniqueDetections,']</b>'),
           HTMLBuoy=paste0(Buoy,' <b>[', NumDetections,']</b>'))
  groupId <- summary(factor(detSummary$Station, levels=stationOrder))
  names(groupId) <- paste('Station:', names(groupId))
  odds <- which(detSummary$StationNum %% 2 == 1)
  detSummary <- select(detSummary, -StationNum) %>%
    select(HTMLSpecies, HTMLBuoy, Latitude, Longitude, UTC)
  myColumns <- c('Species<br/>[Unique Detections]', 'Buoy<br/>[Detections]',
                 'Latitude', 'Longitude', 'UTC')
  kable(detSummary,  align='c', digits=2,
        col.names=myColumns, escape=FALSE, format='html') %>%
    kable_styling('bordered', full_width=FALSE, position = 'left') %>%
    row_spec(odds) %>%
    group_rows(index=groupId, label_row_css = "background-color: #666; color: #fff;") %>%
    collapse_rows(which(colnames(detSummary) %in% c('HTMLSpecies')))

  # kable(detSummary,  align='c', digits=2,
  #       col.names=myColumns, escape=FALSE, format='html') %>%
  #   kable_styling('bordered') %>%
  #   row_spec(odds, background='#edf0f4') %>%
  #   collapse_rows(which(colnames(detSummary) %in% c('KSpecies','Station')))
}

# Proper formatting of list of words
formatListGrammar <- function(words) {
  if(length(words) == 2) {
    paste(words, collapse = ' and ')
  } else if(length(words) > 2) {
    gsub('(.*),([^,]*)$', '\\1, and\\2', paste(words, collapse=', '))
  } else {
    words
  }
}

makeStationPlot <- function(detSummary, myMap, figurePath, reportText) {
  plot <- mapStations(filter(detSummary, StationType == 'DensityEstimate'),
                             map=myMap, size=2, colorBy = 'Cruise') +
    labs(title = paste0('Figure ', reportText$figureCount, ': Sonobuoy Stations')) +
    guides(color = guide_legend(tile='Vessel(s)'))
  colorNames <- paste0('(', c('Black', 'Green', 'Blue', 'Orange', 'Yellow', 'Pink')[1:length(reportText$myVessels)], ')')
  vesselColors <- formatListGrammar(paste(reportText$myVessels, colorNames))
  caption <- paste0('Figure ', reportText$figureCount, ': Map of Sonobuoy Stations during the ',
                       reportText$cruiseAbbr, ' survey for vessel(s) ', vesselColors)
  ggsave(filename='stationPlot.jpeg', plot=plot, path=figurePath,
         width=6, height=4.5, units='in', dpi=200)
  list(plot=plot, caption=caption)
}

makeOpportunisticPlot <- function(detSummary, myMap, figurePath, reportText, wincruz=NULL, specFile=NULL) {
  # Check if we can grab a wincruz file
  if(is.null(wincruz)) {
    haveWin <- menu(title='Do you have a WinCruz file for this cruise? \n',
                    choices = c('Yes', 'No'))
    if(haveWin == 1) {
      wincruz <- file.choose()
    }
  }
  # Now check if we provided one or not.
  if(is.null(wincruz)) {
    cat('Species names cannot be provide without WinCruz data. \n')
    plotColor <- 'Cruise'
    plotLegend <- 'Vessel(s)'
  } else {
    # If we have wincruz, need to get species data too. Defaults to provided one, or read csv
    wincruz <- swfscMisc::das.read(wincruz) %>%
      filter(!is.na(Sight) & !is.na(Spp1)) %>%
      select(SightingId = Sight, Code = Spp1) %>%
      distinct()
    if(is.null(specFile)) {
      # Just save the DF in wincruz. as data object in package?
      cat('Using species codes from SpCodes_2013.dat. To use a different file, please',
          'provide a csv file with columns Code and SpeciesName in the "specFile" argument. \n')
      suppressWarnings(specCode <- read.fwf(system.file('wincruz/SpCodes_2013.dat', package = 'PAMsbuoy'),
                                            widths=c(4, 11, 39), stringsAsFactors=FALSE))
      colnames(specCode) <- c('Code', 'ShortName', 'SpeciesName')
    } else {
      # Get provided csv, check if columns are right. If not go back to default.
      specCode <- read.csv(specFile)
      if(!all(c('Code', 'SpeciesName') %in% colnames(specCode))) {
        cat('Provided csv file does not have columns Code and SpeciesName. Using codes',
            'from SpCodes_2013.dat instead.')
        suppressWarnings(specCode <- read.fwf(system.file('wincruz/SpCodes_2013.dat', package = 'PAMsbuoy'),
                                              widths=c(4, 11, 39), stringsAsFactors=FALSE))
        colnames(specCode) <- c('Code', 'ShortName', 'SpeciesName')
      }
    }
    specCode <- specCode %>% mutate(Code = str_trim(as.character(Code)), SpeciesName = str_trim(SpeciesName))
    plotColor <- 'SpeciesName'
    plotLegend <- 'Species'
    detSummary <- detSummary %>% left_join(wincruz, by='SightingId') %>%
      left_join(specCode, by='Code') %>% mutate(SpeciesName = str_replace_na(SpeciesName, 'No Sighting ID'))
  }
  caption <- paste0('Figure ', reportText$figureCount, ': Map of opportunistic sonobuoys deployed during the ',
                             reportText$cruiseAbbr, ' survey')
  text <- paste0('Opportunistic sonobuoys included a total of ',
                 round(reportText$opportunisticLength/60, 0), ' minutes of recordings')
  if(plotColor=='SpeciesName') {
    oppSpecies <- detSummary %>% filter_(.dots = list('!is.na(plotColor)')) %>% .[[plotColor]] %>% unique()
    oppSpecies <- oppSpecies[which(oppSpecies != 'No Sighting ID')]
    detSummary$SpeciesName <- factor(detSummary$SpeciesName, levels=c(oppSpecies, 'No Sighting ID'))
    text <- paste0(text, ' during confirmed visual sightings of ', formatListGrammar(oppSpecies), ' (Table 1).')
  } else {
    text <- paste0(text, '.')
  }
  plot <- mapStations(filter(detSummary, StationType == 'Opportunistic'),
                      map=myMap, size=2, colorBy = plotColor) +
    labs(title = paste0('Figure ', reportText$figureCount, ': Opportunistic Sonobuoys')) +
    guides(color = guide_legend(title=plotLegend))
  ggsave(filename='opportunisticPlot.jpeg', plot=plot, path=figurePath,
         width=6, height=4.5, units='in', dpi=200)
  list(plot=plot, caption=caption, text=text)
}

makeDetectionPlot <- function(detSummary, myMap, figurePath, reportText) {
  plot <- mapDetections(detSummary, combine=FALSE, map=myMap, value='NumDetections', size=2, ncol=3) +
    labs(title = paste0('Figure ', reportText$figureCount, ': Call Type Detections'))
  caption <- paste0('Figure ', reportText$figureCount, ': Acoustic detection of call types attributed to ',
                         'species [x]')
  ggsave(filename='detectionPlot.jpeg', plot=plot, path=figurePath,
         width=8, height=4, units='in', dpi=300)
  list(plot=plot, caption=caption)
}