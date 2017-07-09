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

#update teams matrix with information from game day
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

#advance the game date
advanceDay <- function() {
  gamesToday <- games[which(games$Date == currentDay)]
  
  currentDay <- gamedays[which(gamedays == currentDay)+1]
}

#conferences
East <- as.vector(subset(teams$Team_Name,teams$Conference_id=="East"))
West <- as.vector(subset(teams$Team_Name,teams$Conference_id=="West"))
#divisions
Atlantic <- as.vector(subset(teams$Team_Name,teams$Division_id=="Atlantic"))
Central <- as.vector(subset(teams$Team_Name,teams$Division_id=="Central"))
Southeast <- as.vector(subset(teams$Team_Name,teams$Division_id=="Southeast"))
Northwest <- as.vector(subset(teams$Team_Name,teams$Division_id=="NOrthwest"))
Pacific <- as.vector(subset(teams$Team_Name,teams$Division_id=="Pacific"))
Southwest <- as.vector(subset(teams$Team_Name,teams$Division_id=="Southwest"))

#test teamName
teamName <- teams$Team_Name[13]

#simulate season in a best case scenario for team in question
bestCase <- function(teamName) {
  simSeason <- subset(games,games$Date > currentDay)
  
}