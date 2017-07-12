#remember to set your working directory & install the "readxl" package

#install.packages("readxl")
require(readxl)

#initialize data frame to hold team information
teams <- read_xlsx("Analytics_Attachment.xlsx", sheet = 1)
teams$wins <- 0
teams$losses <- 0
teams$ptdiff <- 0
teams$cwins <- 0
teams$closses <- 0
teams$dwins <- 0
teams$dlosses <- 0

#pull in game information
games <- read_xlsx("Analytics_Attachment.xlsx", sheet = 2)
games$Date <- as.Date(games$Date)
gamedays <- unique(games$Date)
currentDay <- gamedays[1]

#update teams matrix with information from game day (need to update with cwins/closses/dwins/dlosses)
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
confs <- list("East" = as.vector(subset(teams$Team_Name,teams$Conference_id=="East")),
     "West" = as.vector(subset(teams$Team_Name,teams$Conference_id=="West")))

#divisions
divisions <- list("Atlantic" = as.vector(subset(teams$Team_Name,teams$Division_id=="Atlantic")),
                  "Central" = as.vector(subset(teams$Team_Name,teams$Division_id=="Central")), 
                  "Southeast" = as.vector(subset(teams$Team_Name,teams$Division_id=="Southeast")),
                  "Northwest" = as.vector(subset(teams$Team_Name,teams$Division_id=="NOrthwest")),
                  "Pacific" = as.vector(subset(teams$Team_Name,teams$Division_id=="Pacific")),
                  "Southwest" = as.vector(subset(teams$Team_Name,teams$Division_id=="Southwest")))

#test teamName
teamName <- teams$Team_Name[13]

#We probs want to initialize a best case scenario for each team and then update that with what changes
#   due to how actual results play out... but we always know that they want to
# win out
# have everyone in their conference lose to the other conference
# have everyone in their division lose to teams outside of it
# winner for games in same division is the team with the worse record of the pair... (this will be tricky)
# can't rule a team out on pt differential

#want to store each bestcase scenario in a giant list for the teams

#initial simulation of best case scenario for a team (IN PROGRESS)
generateBestCase <- function(teamName) {
  teamconf <- teams$Conference_id[which(teams$Team_Name == teamName)]
  
  #subset the games where a team in the same conference is playing
  if (teamconf == "East") {
    simSeason <- subset(games,(games$Date > currentDay) & (games$`Home Team` %in% confs$East | 
                                                             games$`Away Team` %in% confs$East))
  } else {
    simSeason <- subset(games,(games$Date > currentDay) & (games$`Home Team` %in% confs$West | 
                                                             games$`Away Team` %in% confs$West))
  }

  #current team wins out
  simSeason$Winner[which(simSeason$`Home Team` == teamName | simSeason$`Away Team` == teamName)] <- teamName
  
  #games b/w conferences
  outconfs <- which((simSeason$`Home Team` %in% confs$East & simSeason$`Away Team` %in% confs$West) | 
                       (simSeason$`Home Team` %in% confs$West & simSeason$`Away Team` %in% confs$East))
  
  #team conference loses out to the other conference
  if (teamconf == "East") {
    simSeason$Winner[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs$East)]] <- 
      simSeason$`Away Team`[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs$East)]]
    
    simSeason$Winner[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs$East)]] <- 
      simSeason$`Home Team`[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs$East)]]
  } else {
    simSeason$Winner[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs$West)]] <- 
      simSeason$`Away Team`[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs$West)]]
    
    simSeason$Winner[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs$West)]] <- 
      simSeason$`Home Team`[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs$West)]]
  }
  
  
  
}