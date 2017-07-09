#remember to set your working directory & install the "readxl" package

#install.packages("readxl")
require(readxl)

#initialize data frame to hold team information
teams <- read_xlsx("Analytics_Attachment.xlsx", sheet = 1)
teams$wins <- 0
teams$losses <- 0
teams$ptdiff <- 0

#pull in game information
games <- read_xlsx("Analytics_Attachment.xlsx", sheet = 2)
games$Date <- as.Date(games$Date)
gamedays <- unique(games$Date)
currentDay <- gamedays[1]

tallyScores <- function(currentDay) {
  resultsToday <- subset(games, games$Date == currentDay)
  for (game in 1:dim(resultsToday)[1]) {
      if (resultsToday$Winner[game] == "Home") {
      winner <- resultsToday$`Home Team`[game]
      loser <- resultsToday$`Away Team`[game]
      spread <- resultsToday$`Home Score`[game] - resultsToday$`Away Score`[game]
      } else {
        winner <- resultsToday$`Away Team`[game]
        loser <- resultsToday$`Home Team`[game]
        spread <- resultsToday$`Away Score`[game] - resultsToday$`Home Score`[game]
      }
    
    windex <- which(teams$Team_Name == winner)
    lindex <- which(teams$Team_Name == loser)
    
    #assign wins, losses, and ptdiff to the global teams df
    teams$wins[windex] <<- teams$wins[windex] + 1
    teams$ptdiff[windex] <<- teams$ptdiff[windex] + spread
    teams$losses[lindex] <<- teams$losses[lindex] + 1
    teams$ptdiff[lindex] <<- teams$ptdiff[lindex] - spread
  }
}

advanceDay <- function() {
  gamesToday <- games[which(games$Date == currentDay)]
  
  currentDay <- gamedays[which(gamedays == currentDay)+1]
}