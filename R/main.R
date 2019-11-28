#------------------------------------------------
#' @title Process main streaming data for R
#'
#' @description Process the main json file to produce a data frame where each
#' row states the song name, artist, length of the song played and the time it
#' was played
#'
#' @param fileName directory leading to the json file 
#'
#' @export

initProcess <- function(fileName)
{
  # read file
  raw <- rjson::fromJSON(file = fileName)
  datlength <- length(raw)

  # create objects to clean data
  endTime <- artistName <- trackName <- rep("NA", datlength)
  timePlayed <- rep(NA, datlength)

  # run through data collating each list into category
  for(i in 1:datlength)
    {
    endTime[i] <- raw[[i]]$endTime
    artistName[i] <- raw[[i]]$artistName
    trackName[i] <- raw[[i]]$trackName
    timePlayed[i] <- raw[[i]]$msPlayed  
    }
  
  return(data.frame(endTime = endTime, artistName = artistName, trackName = trackName, timePlayed = timePlayed))
    
}

#------------------------------------------------
#' @title Plot a time series of your data
#'
#' @description Produces a radar chart/histogram detailing hours of the day
#' the user listens to music
#'
#' @param data the data produced from the initProcess fct 
#' @param type specify on wat times scale, currently only hours
#'
#' @export

plotTimeSeries <- function(data, type = "hours")
{
  # create strings
  hourCount <- hourString <- rep(NA, 24)
  dayCount <- dayString <- rep(NA, 7)
  
  if(type == "hours")
  {
    # for each hour pull out how many times music is listened to  
    for(i in 0:23)
    { 
    hourString[i + 1] <- paste(sprintf("%02d", i), ":", sep = "")  
    hourCount[i + 1] <-  length(grep(hourString[i + 1], data$endTime))
    }
    
    # plot
    hourDF <- data.frame(time = sprintf("%02d",0:23), counts = hourCount)
    p = ggplot(hourDF, aes(x = time, y = counts, fill = counts)) + 
    geom_bar(stat= "identity") + 
    coord_polar() +  
    theme(plot.title = element_text(hjust = 0.5)) + labs(title = "Top listening times", ylab = NULL)
    show(p)
  } else if(type == "days")
  {
    # TODO # do the same for days instead
    # for(i in 1:7)
    # { 
    # dayString[i + 1] <- 
    # dayCount[i + 1] <-  
    # }
  }
}
  
#------------------------------------------------
#' @title Scrape artists country of origin
#'
#' @description scrape the country of origin of each artist using wiki and 
#' last.fm
#'
#' @param artistNames a vector of artist names 
#'
#' @export

scrapeLocations <- function(artistNames)
{
  # time process  
  begin <- Sys.time()
  nArtists <- length(artistNames)
  lastfmURLS <- rep(NA, nArtists)
  lastfmLoc <- matrix(NA, ncol = 2, nrow = nArtists)
  
  # build a set of urls to scrape from 
  for(i in 1:nArtists)
    {
    lastfmURLS[i] <- paste("https://www.last.fm/music/", artistNames[i], "/+wiki", sep = "")
    }
  
  # -----------------------------------
  # now scrape 
  print(paste(nArtists, " URLs generated - now scraping", sep = ""))
  Sys.sleep(1)
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = nArtists, style = 3)
  
  for(i in 1:nArtists)
    {
    tryCatch({ # stop any failed scrapings from interrupting the loop
    # update progress bar
    setTxtProgressBar(pb, i)
  
    readingSite <- read_html(lastfmURLS[i])               # read the html from site 
    origin <- html_nodes(readingSite, ".factbox-item p")  # extract specific info

    # remove any nasty characters from the string
    lastfmLoc[i, 1:length(origin)] <- str_replace_all(as.character(origin[1:length(origin)]), "[,.\"_<=/>]", "")
    }, error=function(e){})
    }
    close(pb)
    
    fail <- length(which(is.na(lastfmLoc[,1]) & is.na(lastfmLoc[,2]) == TRUE))
    print(paste(fail, " of ", nArtists, " yielded no information", sep = ""))
    Sys.sleep(1)

    end <- Sys.time()
    timeTaken <- end - begin
    print(timeTaken)
    df <- list(artists = artistNames, URL = lastfmURLS, messyLoc = lastfmLoc)
    return(df)
}
