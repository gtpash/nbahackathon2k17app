#remember to set your working directory & install the "readxl" package

#install.packages(c("readxl","magrittr","dplyr")
library(readxl)
library(magrittr)
library(dplyr)

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

#set winner to be the team name
hdex <- which(games$Winner == "Home")
adex <- which(games$Winner == "Away")
games$Winner[hdex] <- games$`Home Team`[hdex]
games$Winner[adex] <- games$`Away Team`[adex]
rm(adex,hdex)

eliminations <- read_xlsx("Analytics_Attachment.xlsx",sheet=3)
eliminations$`Date Eliminated` = "Playoffs"

#update teams matrix with information from game day (need to update with cwins/closses/dwins/dlosses)
tallyScores <- function(currentDay) {
  resultsToday <- subset(games, games$Date == currentDay)
  for (game in 1:dim(resultsToday)[1]) {
      if (resultsToday$Winner[game] == resultsToday$`Home Team`[game]) {
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
    
    if (teams$Division_id[windex] == teams$Division_id[lindex]) {
      teams$dwins[windex] <<- teams$dwins[windex] + 1
      teams$dlosses[lindex] <<- teams$dlosses[lindex] + 1
    }
    
    if (teams$Conference_id[windex] == teams$Conference_id[lindex]) {
      teams$cwins[windex] <<- teams$cwins[windex] + 1
      teams$closses[lindex] <<- teams$closses[lindex] + 1
    }
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
  teamdiv <- teams$Division_id[which(teams$Team_Name == teamName)]
  
  #subset the games where a team in the same conference is playing
  simSeason <- subset(games,(games$Date > currentDay) & (games$`Home Team` %in% confs[[teamconf]] | 
                                                           games$`Away Team` %in% confs[[teamconf]]))
  
  #games b/w your division and other divisions
  outdivs <- which((simSeason$`Home Team` %in% divisions[[teamdiv]]))
  
  #teams in your conference lose to other divisions
  simSeason$Winner[outdivs[which(simSeason$`Home Team`[outdivs] %in% divisions[[teamdiv]])]] <- 
    simSeason$`Away Team`[outdivs[which(simSeason$`Home Team`[outdivs] %in% divisions[[teamdiv]])]]
  simSeason$Winner[outdivs[which(simSeason$`Away Team`[outdivs] %in% divisions[[teamdiv]])]] <- 
    simSeason$`Home Team`[outdivs[which(simSeason$`Away Team`[outdivs] %in% divisions[[teamdiv]])]]
  
  #games b/w conferences
  outconfs <- which((simSeason$`Home Team` %in% confs$East & simSeason$`Away Team` %in% confs$West) | 
                       (simSeason$`Home Team` %in% confs$West & simSeason$`Away Team` %in% confs$East))
  
  #team conference loses out to the other conference
  simSeason$Winner[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs[[teamconf]])]] <- 
    simSeason$`Away Team`[outconfs[which(simSeason$`Home Team`[outconfs] %in% confs[[teamconf]])]]
  simSeason$Winner[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs[[teamconf]])]] <- 
    simSeason$`Home Team`[outconfs[which(simSeason$`Away Team`[outconfs] %in% confs[[teamconf]])]]
  
  #current team wins out
  simSeason$Winner[which(simSeason$`Home Team` == teamName | simSeason$`Away Team` == teamName)] <- teamName
  
  #games b/w teams in other divisions that are in you
  othergames <- which(simSeason$Winner == "Home" | simSeason$Winner == "Away")
  
  
}

checkPlayoffTeams <- function(teams, teamName, teamConf, teamDiv, currentDate) {
  
  teamsCopy <- teams %>% filter(Conference_id == teamConf) %>% arrange(desc(wins),desc(dwins),desc(cwins))
  cutoff <- teamsCopy[8,4]
  teamsCopy %>% filter(wins >= cutoff) -> teamsCopy
  if (nrow(teamsCopy) == 8){
    playoffTeams <- teamsCopy$Team_Name
    return(playoffTeams)
  }
  if (nrow(teamsCopy) == 9 & teamsCopy[7,4] > cutoff) {
    playoffTeams <- teamsCopy$Team_Name[1:7]
    c(playoffTeams,twoTeamLogic(teams,teamsCopy[8,]$Team_Name,teamsCopy[9,]$Team_Name, currentDate, playoffTeams))
    return(playoffTeams)
  }
  if (nrow(teamsCopy) > 8) {
    playoffTeams <- teamsCopy %>% filter(wins>cutoff) %>% .$Team_Name
    needed <- 8-length(playoffTeams)
    contentionTeams <- teamsCopy %>% filter(wins>cutoff) %>% .$Team_Name
    order <- threePlusTeamLogic(teams, contentionTeams, currentDate, playoffTeams, needed)
    c(playoffTeams, order[1:needed])
    return(playoffTeams)
  }
  
}

twoTeamLogic <- function(teams, team1, team2, currentDate, playoffTeams) {
  team1Wins <- 0
  team2Wins <- 0  
  team1Div <- teams$Division_id[which(teams$Team_Name == team1)] 
  team2Div <- teams$Division_id[which(teams$Team_Name == team2)] 
  team1Conf <- teams$Conference_id[which(teams$Team_Name == team1)]
  team2Conf <- teams$Conference_id[which(teams$Team_Name == team2)]
  #Criteria 1
  criteria1 <- games %>% filter(Date <= currentDate,(`Home Team`==team1 & `Away Team`==team2)|(`Home Team`==team2 & `Away Team`==team1))
  criteria1 <- rbind(criteria1, simSeason %>% filter(Date > currentDate,(`Home Team`==team1 & `Away Team`==team2)|(`Home Team`==team2 & `Away Team`==team1)))
  for (i in 1:nrow(criteria1)){
    if (criteria[i,]$Winner == team1) {
      team1Wins <- team1Wins + 1
    } else {
      team2Wins <- team2Wins + 1
    }
  }
  if (team1Wins > team2Wins){
    return(team1)
  }else if(team1Wins < team2Wins){
    return(team2)
  }
  #Criteria 2
  criteria2a <- teams %>% filter(Division_id == team1Div) %>% arrange(decr(dwins))
  criteria2b <- teams %>% filter(Division_id == team2Div) %>% arrange(decr(dwins))
  if (criteria2a[1,1] == team1 & criteria2b[1,1] != team2){
    return(team1)
  }
  if (criteria2a[1,1] != team1 & criteria2b[1,1] == team2){
    return(team2)
  }
  
  team1stats <- teams %>% filter(Team_Name == team1)
  team2stats <- teams %>% filter(Team_Name == team2)
  
  #Criteria 3
  if (team1Div == team2Div) {
     team1DivP <- team1stats$dwins/(team1stats$dwins + team1stats$dlosses)
     team2DivP <- team2stats$dwins/(team2stats$dwins + team2stats$dlosses)
     if (team1DivP > team2DivP) {
       return(team1)
     }
     if (team2DivP < team2DivP) {
       return(team2)
     }
  }
  
  #Criteria 4
  team1ConfP <- team1stats$cwins/(team1stats$cwins + team1stats$closses)
  team2ConfP <- team2stats$cwins/(team2stats$cwins + team2stats$closses)
  if (team1ConfP > team2ConfP) {
    return(team1)
  }
  if (team2ConfP < team2ConfP) {
    return(team2)
  }
  
  #Criteria 5
  possibleTeams <- c(playoffTeams,team1,team2)
  criteria5a <- criteria1 %>% filter((`Home Team` == team1 & `Away Team` %in% possibleTeams) |
                          (`Away Team` == team1 & `Home Team` %in% possibleTeams)) %>% 
                          filter((`Home Team` == team1 & team1Conf == teams$Conference_id[which(teams$Team_Name == `Away Team`)]) |
                          (`Away Team` == team1 & team1Conf == teams$Conference_id[which(teams$Team_Name == `Home Team`)]))
  criteria5b <- criteria1 %>% filter((`Home Team` == team2 & `Away Team` %in% possibleTeams) |
                          (`Away Team` == team2 & `Home Team` %in% possibleTeams)) %>% 
                          filter((`Home Team` == team2 & team2Conf == teams$Conference_id[which(teams$Team_Name == `Away Team`)]) |
                          (`Away Team` == team2 & team2Conf == teams$Conference_id[which(teams$Team_Name == `Home Team`)]))
  team1part5 <- nrow((criteria5a %>% filter(Winner == team1)))/nrow(criteria5a)
  team2part5 <- nrow((criteria5b %>% filter(Winner == team2)))/nrow(criteria5b)
  if (team1part5 > team2part5) {
    return(team1)
  }
  if (team1part5 < team2part5) {
    return(team2)
  }
  
  #Criteria 6
  criteria6a <- criteria1 %>% filter((`Home Team` == team1 & `Away Team` %in% possibleTeams) |
                                    (`Away Team` == team1 & `Home Team` %in% possibleTeams)) %>% 
                                    filter((`Home Team` == team1 & team1Conf != teams$Conference_id[which(teams$Team_Name == `Away Team`)]) |
                                    (`Away Team` == team1 & team1Conf != teams$Conference_id[which(teams$Team_Name == `Home Team`)]))
  criteria6b <- criteria1 %>% filter((`Home Team` == team2 & `Away Team` %in% possibleTeams) |
                                    (`Away Team` == team2 & `Home Team` %in% possibleTeams)) %>% 
                                    filter((`Home Team` == team2 & team2Conf != teams$Conference_id[which(teams$Team_Name == `Away Team`)]) |
                                    (`Away Team` == team2 & team2Conf != teams$Conference_id[which(teams$Team_Name == `Home Team`)]))
  team1part6 <- nrow((criteria6a %>% filter(Winner == team1)))/nrow(criteria6a)
  team2part6 <- nrow((criteria6b %>% filter(Winner == team2)))/nrow(criteria6b)
  if (team1part6 > team2part6) {
    return(team1)
  }
  if (team1part6 < team2part6) {
    return(team2)
  }
  
  return(c(team1, team2))
}

threePlusTeamLogic <- function(teams, contentionTeams, currentDate, playoffTeams, numNeeded){
  ans <- c()
  n <- numNeeded
  if (length(contentionTeams) == 2){
    return(twoTeamLogic(teams,contentionTeams[1],contentionTeams[2],currentDate,playoffTeams))
  }
  #Criteria1
  for (i in length(contentionTeams)){
    x <- c()
    divleader <- teams %>% filter(Division_id == teams$Division_id[which(teams$Team_Name == contentionTeams[i])] ) %>% arrange(decr(dwins)) %>% .$Team_Name[1]
    x <- c(x,divleader == contentionTeams[i])
  }
  clinch <- c()
  for (j in length(x)){
    if (x[j]){
      clinch <- c(clinch, contentionTeams[j])
    }
  }
  if (length(clinch) > 0){
    if (length(clinch) == numNeeded) {
      return(clinch)
    } else if(length(clinch) > numNeeded) {
      return(teamThreePlusLogic(teams, clinch, currentDate, playoffTeams, numNeeded - len(clinch)))
    } else {
      ans <- c(ans, clinch)
    }
  }
  contentionTeams[clinch]<- NULL
  n <- n - length(clinch)
  
  #Criteria 2
  criteria2 <- games %>%  filter(Date<= currentDate,`Home Team` %in% contentionTeams, `Away Team` %in% contentionTeams)
  criteria2 <- rbind(criteria2, simSeason %>% filter(Date > currentDate, `Home Team` %in% contentionTeams, `Away Team` %in% contentionTeams))
  criteria2P <- c()
  for (i in length(contentionTeams)) {
    involved <- criteria2 %>% filter(`Home Team` == contentionTeams[i]|`Away Team` == contentionTeams[i])
    w <- nrow(involved %>% filter(Winner == contentionTeams[i]))/nrow(involved)
    criteria2P <- c(criteria2P, w)
  }
  listOfLists2 <- list(contentionTeams,criteria2P)
  c2df <- data.frame(matrix(unlist(listOfLists2), nrow=nrow(contentionTeams),byrow=F),stringsAsFactors = FALSE)
  c2df %>% arrange(decr(X2)) -> c2df
  c2cutoff <- c2df[n,2]
  if (nrow(c2df %>% filter(X2 >= cutoff))==n){
    return(c(ans,c2df$X1[1:n]))
  } else {
    n2 <- nrow(c2df %>% filter(X2 > cutoff))
    remainder = c2df %>% filter(X2 = cutoff) %>% .$Team_Name
    playoffTeams <- c(playoffTeams,(c2df %>% filter(X2>cutoff) %>% .$Team_Name))
    return(threePlusTeamLogic(teams,remainder,currentDate,playoffTeams,numNeeded - n2))
  }
  
  #Criteria 3
  
  
}
