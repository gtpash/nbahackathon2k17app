#remember to set your working directory & install the "readxl" package

#install.packages(c("readxl","magrittr","dplyr","lubridate"))
library(readxl)
library(magrittr)
library(dplyr)
library(lubridate)

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

#set winner to be the team name
hdex <- which(games$Winner == "Home")
adex <- which(games$Winner == "Away")
games$Winner[hdex] <- games$`Home Team`[hdex]
games$Winner[adex] <- games$`Away Team`[adex]
rm(adex,hdex)

eliminations <- read_xlsx("Analytics_Attachment.xlsx",sheet=3)
eliminations$`Date Eliminated` <- "Playoffs"

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
                  "Northwest" = as.vector(subset(teams$Team_Name,teams$Division_id=="Northwest")),
                  "Pacific" = as.vector(subset(teams$Team_Name,teams$Division_id=="Pacific")),
                  "Southwest" = as.vector(subset(teams$Team_Name,teams$Division_id=="Southwest")))

## Rules for best case
# win out
# have everyone in their conference lose to the other conference
# have everyone in their division lose to teams outside of it
# can't rule a team out on pt differential
# other games are decided by a coinflip

#simulate best case scenario for a team
generateBestCase <- function(teamName, currentDay) {
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
  
  #get indices for games not yet handled
  simgames <- nrow(simSeason)
  temp <- 1:simgames
  othergames <- which(!temp%in%union(outconfs,outdivs))
  rm(temp,simgames)
  #coinflip outcomes
  outcomes <- runif(length(othergames))
  hwins <- othergames[outcomes > 0.5]
  lwins <- setdiff(othergames,hwins)
  
  simSeason$Winner[hwins] <- simSeason$`Home Team`[hwins]
  simSeason$Winner[lwins] <- simSeason$`Away Team`[lwins]
  
  return(simSeason)
  
}

checkPlayoffTeams <- function(checkTeams, teamName, teamConf, teamDiv, currentDate) {
  teamsCopy <- checkTeams %>% filter(Conference_id == teamConf) %>% arrange(desc(wins),desc(dwins),desc(cwins))
  cutoff <- teamsCopy$wins[8]
  teamsCopy %>% filter(wins >= cutoff) -> teamsCopy
  if (nrow(teamsCopy) == 8){
    playoffTeams <- teamsCopy$Team_Name
    return(playoffTeams)
  }
  if (nrow(teamsCopy) == 9 & teamsCopy$wins[7] > cutoff) {
    playoffTeams <- teamsCopy$Team_Name[1:7]
  #  print("2 teams tied")
    playoffTeams <- c(playoffTeams,twoTeamLogic(teams,teamsCopy$Team_Name[8],teamsCopy$Team_Name[9], currentDate, playoffTeams))
   # print("2 team tie broken")
    return(playoffTeams)
  }
  if (nrow(teamsCopy) > 8) {
    playoffTeams <- teamsCopy %>% filter(wins>cutoff) %>% .$Team_Name
    needed <- 8-length(playoffTeams)
    contentionTeams <- teamsCopy %>% filter(wins>cutoff) %>% .$Team_Name
  #  print("3+ teams tied here")
    order <- threePlusTeamLogic(checkTeams, contentionTeams, currentDate, playoffTeams, needed)
  #  print("3+ teams tie broken")
    c(playoffTeams, order[1:needed])
    return(playoffTeams)
  }
  
}

getConfs <- function(team){
  x <- vector("character",length(team))
  for (t in 1:length(team)){
    x[t] = teams$Conference_id[which(teams$Team_Name == team[t])]
  }
  return(x)
}

twoTeamLogic <- function(checkTeams, team1, team2, currentDate, playoffTeams) {
  team1Wins <- 0
  team2Wins <- 0  
  team1Div <- checkTeams$Division_id[which(checkTeams$Team_Name == team1)] 
  team2Div <- checkTeams$Division_id[which(checkTeams$Team_Name == team2)] 
  team1Conf <- checkTeams$Conference_id[which(checkTeams$Team_Name == team1)]
  team2Conf <- checkTeams$Conference_id[which(checkTeams$Team_Name == team2)]
  #Criteria 1, win/loss against each other
  criteria1 <- games %>% filter(Date <= currentDate,(`Home Team`==team1 & `Away Team`==team2)|(`Home Team`==team2 & `Away Team`==team1))
  criteria1 <- rbind(criteria1, simSeason %>% filter(Date > currentDate,(`Home Team`==team1 & `Away Team`==team2)|(`Home Team`==team2 & `Away Team`==team1)))
  for (i in 1:nrow(criteria1)){
    if (criteria1[i,]$Winner == team1) {
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
  #Criteria 2 division leader
  criteria2a <- checkTeams %>% filter(Division_id == team1Div) %>% arrange(desc(dwins))
  criteria2b <- checkTeams %>% filter(Division_id == team2Div) %>% arrange(desc(dwins))
  if (criteria2a[1,1] == team1 & criteria2b[1,1] != team2){
    return(team1)
  }
  if (criteria2a[1,1] != team1 & criteria2b[1,1] == team2){
    return(team2)
  }
  
  team1stats <- checkTeams %>% filter(Team_Name == team1)
  team2stats <- checkTeams %>% filter(Team_Name == team2)
  
  #Criteria 3 division win/loss
  if (team1Div == team2Div) {
     team1DivP <- team1stats$dwins/(team1stats$dwins + team1stats$dlosses)
     team2DivP <- team2stats$dwins/(team2stats$dwins + team2stats$dlosses)
     if(!(is.na(team1DivP)|is.na(team2DivP))){
      if (team1DivP > team2DivP) {
        return(team1)
      }
      if (team2DivP < team2DivP) {
        return(team2)
      }
     }
  }
  
  #Criteria 4 conf win/loss
  team1ConfP <- team1stats$cwins/(team1stats$cwins + team1stats$closses)
  team2ConfP <- team2stats$cwins/(team2stats$cwins + team2stats$closses)
  if (team1ConfP > team2ConfP) {
    return(team1)
  }
  if (team2ConfP < team2ConfP) {
    return(team2)
  }
  
  #Criteria 5 win % against eligible playoff teams, own conf
  possibleTeams <- c(playoffTeams,team1,team2)
  eligibleTeams <- games %>% filter(Date <= currentDate, `Home Team` %in% possibleTeams,`Away Team` %in% possibleTeams)
  eligibleTeams <- rbind(eligibleTeams,simSeason %>% filter(Date>currentDate,`Home Team` %in% possibleTeams,`Away Team` %in% possibleTeams))
  
  criteria5a <- eligibleTeams %>% filter((`Home Team` == team1)|(`Away Team` == team1)) 
  criteria5b <- eligibleTeams %>% filter((`Home Team` == team2)|(`Away Team` == team2)) 
                        
  team1part5 <- nrow((criteria5a %>% filter(Winner == team1)))/nrow(criteria5a)
  team2part5 <- nrow((criteria5b %>% filter(Winner == team2)))/nrow(criteria5b)
  if (team1part5 > team2part5) {
    return(team1)
  }
  if (team1part5 < team2part5) {
    return(team2)
  }
  
  #Criteria 6 win % against eligible playoff teams, other conf
  otherConf <- checkTeams %>% filter(Conference_id != team1Conf) %>% arrange(desc(wins))
  otherCutoff <- otherConf$wins[8]
  otherConf %>% filter(wins >= otherCutoff) %>% .$Team_Name -> otherConfTeams
  games6 <- games %>% mutate(hConf = getConfs(`Home Team`)) %>% mutate(aConf = getConfs(`Away Team`))
  simSeason6 <- simSeason %>% mutate(hConf = getConfs(`Home Team`)) %>% mutate(aConf = getConfs(`Away Team`))
  
  spliced1 <- games6 %>% filter(Date <= currentDate, (`Home Team` == team1|`Away Team` == team1), hConf != aConf)
  spliced1 <- rbind(spliced1, simSeason6 %>% filter(Date > currentDate, (`Home Team` == team1|`Away Team` == team1), hConf != aConf))
  spliced2 <- games6 %>% filter(Date <= currentDate, (`Home Team` == team2|`Away Team` == team2), hConf != aConf)
  spliced2 <- rbind(spliced2, simSeason6 %>% filter(Date > currentDate, (`Home Team` == team2|`Away Team` == team2), hConf != aConf))
 
   team1part6 <- nrow((spliced1 %>% filter(Winner == team1)))/nrow(spliced1)
  team2part6 <- nrow((spliced2 %>% filter(Winner == team2)))/nrow(spliced2)
  print(team1part6)
  if (team1part6 > team2part6) {
    return(team1)
  }
  if (team1part6 < team2part6) {
    return(team2)
  }
  #Criteria 7 point diff
  return(c(team1, team2))
}

threePlusTeamLogic <- function(checkTeams, contentionTeams, currentDate, playoffTeams, numNeeded){
  ans <- c()
  n <- numNeeded
  #print(numNeeded)
  if (length(contentionTeams) < 2){return()}
 # if (length(numNeeded) < 0){return()}
  if (length(contentionTeams) == 2){
    return(twoTeamLogic(checkTeams,contentionTeams[1],contentionTeams[2],currentDate,playoffTeams))
  }
  #Criteria1 division leader
  for (i in length(contentionTeams)){
    x <- vector("character",length(contentionTeams))
    divleader <- checkTeams %>% filter(Division_id == checkTeams$Division_id[which(checkTeams$Team_Name == contentionTeams[i])] ) %>% arrange(desc(dwins)) %>% .[1,1]
    x[i] = (divleader[1,1] == contentionTeams[i])[1]
  }
  clinch <- c()
  for (j in length(x)){
    if (x[j]){
      clinch <- c(clinch, contentionTeams[j])
    }
  }
  if (length(clinch) != length(contentionTeams)){
  if (length(clinch) > 0){
    if (length(clinch) == numNeeded) {
      return(c(ans,clinch))
    } else if(length(clinch) > numNeeded) {
      return(c(ans,teamThreePlusLogic(checkTeams, clinch, currentDate, playoffTeams, numNeeded - len(clinch))))
    } else {
      ans <- c(ans, clinch)
    }
  }
  for (k in clinch){
    contentionTeams[-(match(k,contentionTeams))]
  }
 # contentionTeams[clinch]<- NULL
  n <- n - length(clinch)
  }
 
  #Criteria 2 win % games among tied teams
  criteria2 <- games %>%  filter(Date<= currentDate,`Home Team` %in% contentionTeams, `Away Team` %in% contentionTeams)
  criteria2 <- rbind(criteria2, simSeason %>% filter(Date > currentDate, `Home Team` %in% contentionTeams, `Away Team` %in% contentionTeams))
  criteria2P <- vector(mode="double",length=length(contentionTeams))
  for (i in 1:length(contentionTeams)) {
    involved <- criteria2 %>% filter(`Home Team` == contentionTeams[i]|`Away Team` == contentionTeams[i])
    w <- nrow(involved %>% filter(Winner == contentionTeams[i]))/nrow(involved)
    criteria2P[i] = w
  }
  
  listOfLists2 <- list(contentionTeams,criteria2P)
#  c2df <- tibble(matrix(unlist(listOfLists2), nrow=nrow(contentionTeams),byrow=F),stringsAsFactors = FALSE)
  c2df <- data.frame(lapply(data.frame(t(listOfLists2)),unlist),stringsAsFactors = FALSE)
  c2df %>% arrange(desc(X2)) -> c2df
  c2cutoff <- c2df[n,2]
  
  if (nrow(c2df %>% filter(X2 >= c2cutoff)) != length(contentionTeams)){
    if (nrow(c2df %>% filter(X2 >= c2cutoff))==n){
      return(c(ans,c2df$X1[1:n]))
    } else {
      n2 <- nrow(c2df %>% filter(X2 > c2cutoff))
      remainder <- c2df %>% filter(X2 == c2cutoff) %>% .$Team_Name
      
      
      playoffTeams <- c(playoffTeams,(c2df %>% filter(X2 > c2cutoff) %>% .$Team_Name))
      return(c(ans,threePlusTeamLogic(checkTeams,remainder,currentDate,playoffTeams,n - n2)))
    }
  }
 

  #Criteria 3 div win % if all same div
  divs <- vector("character",length(contentionTeams))
  for(i in 1:length(contentionTeams)){
    divs[i] = checkTeams$Division_id[which(checkTeams$Team_Name == contentionTeams[i])]
  }
  if (length(unique(divs))==1){
    divArray <- vector("double",length(contentionTeams))
    for(i in 1: length(contentionTeams)){
      stats <- checkTeams %>% filter(Team_Name == contentionTeams[i])
      divArray[i] = stats$dwins/(stats$dwins+stats$dlosses)
    }
    listOfLists3 <- list(contentionTeams, divArray)
    c3df <- data.frame(lapply(data.frame(t(listOfLists3)),unlist),stringsAsFactors = FALSE)
    c3df %>% arrange(desc(X2)) -> c3df
    c3cutoff <- c3df[n,2]
    
    if (nrow(c3df %>% filter(X2 >= c3cutoff)) != length(contentionTeams)){
      if (nrow(c3df %>% filter(X2 >= c3cutoff))==n){
        return(c(ans,c3df$X1[1:n]))
      } else {
        n3 <- nrow(c3df %>% filter(X2 >= c3cutoff))
        remainder <- c3df %>% filter(X2 == c3cutoff) %>% .$Team_Name
        playoffTeams <- c(playoffTeams,(c3df %>% filter(X2 > c3cutoff)%>% .$Team_Name))
        return(c(ans,threePlusTeamLogic(checkTeams,remainder,currentDate, playoffTeams,n-n3)))
      }
    }
  }
  
  #Criteria 4 conf win %
  confArray <- vector("double",length(contentionTeams))
  for(i in 1: length(contentionTeams)){
    statsConfs <- checkTeams %>% filter(Team_Name == contentionTeams[i])
    confArray[i] = statsConfs$cwins/(statsConfs$cwins+statsConfs$closses)
  }
  listOfLists4 <- list(contentionTeams, confArray)
  c4df <- data.frame(lapply(data.frame(t(listOfLists4)),unlist),stringsAsFactors = FALSE)
  c4df %>% arrange(desc(X2)) -> c4df
  c4cutoff <- c4df[n,2]
  
  if (nrow(c4df %>% filter(X2 >= c4cutoff)) != length(contentionTeams)){
    if (nrow(c4df %>% filter(X2 >= c4cutoff))==n){
      return(c(ans,c4df$X1[1:n]))
    } else {
      n4 <- nrow(c4df %>% filter(X2 >= c4cutoff))
      remainder <- c4df %>% filter(X2 == c4cutoff) %>% .$Team_Name
      playoffTeams <- c(playoffTeams,(c4df %>% filter(X2 > c4cutoff)%>% .$Team_Name))
      return(c(ans,threePlusTeamLogic(checkTeams,remainder,currentDate, playoffTeams,n-n4)))
    }
  }
  
  #Criteria 5 win% against eligible playoff teams own conf

  c5array <- vector("double",length(contentionTeams))
  all <- c(playoffTeams, contentionTeams)
  c5Eligible <- games %>% filter(Date <= currentDate, `Home Team` %in% all,`Away Team` %in% all)
  c5Eligible <- rbind(c5Eligible, simSeason %>% filter(Date > currentDate, `Home Team` %in% all,`Away Team` %in% all))
  
  for (i in 1:length(contentionTeams)) {
    c6stats <- c5Eligible %>% filter((`Home Team` == contentionTeams[i])|(`Away Team` == contentionTeams[i]))
    c5array[i] = nrow((c6stats %>% filter(Winner == contentionTeams[i])))/nrow(c6stats)
  }
  listOfLists5 <- list(contentionTeams, c5array)
  c5df <- data.frame(lapply(data.frame(t(listOfLists5)),unlist),stringsAsFactors = FALSE)
  c5df %>% arrange(desc(X2)) -> c5df
  c5cutoff <- c5df[n,2]
  
  if (nrow(c5df %>% filter(X2 >= c5cutoff)) != length(contentionTeams)){
    if (nrow(c5df %>% filter(X2 >= c5cutoff))==n){
      return(c(ans,c5df$X1[1:n]))
    } else {
        return(c(ans,contentionTeams))
    }
  }
  
  #Criteria 6 point diff
  return(c(ans,contentionTeams))
}

#main loop
numElim <- 0
for (i in 1:length(gamedays)){
#for (i in 161:162){
  #no need to simulate seasons after the first day, nobody could be eliminated yet skip to game day 80
  if (i < 25) {
    tallyScores(gamedays[i])
    print(gamedays[i])
  } else {
    tallyScores(gamedays[i])
    print(gamedays[i])
    playoffTeams <- eliminations$Team[which(eliminations$`Date Eliminated` == "Playoffs") & (numElim<15)]
    
    for (team in playoffTeams){
      
      #print(team)
      simno <- 1
      tempTeams <- teams
      
      #print("sims starting")
      while ((eliminations$`Date Eliminated`[which(eliminations$Team == team)] == "Playoffs") & (simno < 6)) {
        
        if (i < 90) {
          simSeason <- generateBestCase(team, gamedays[i])
          
          #update simTeams here
          simTeams <- tempTeams
          for (game in 1:dim(simSeason)[1]) {
            if (simSeason$Winner[game] == simSeason$`Home Team`[game]) {
              winner <- simSeason$`Home Team`[game]
              loser <- simSeason$`Away Team`[game]
              spread <- simSeason$`Home Score`[game] - simSeason$`Away Score`[game]
            } else {
              winner <- simSeason$`Away Team`[game]
              loser <- simSeason$`Home Team`[game]
              spread <- simSeason$`Away Score`[game] - simSeason$`Home Score`[game]
            }
            
            windex <- which(simTeams$Team_Name == winner)
            lindex <- which(simTeams$Team_Name == loser)
            
            #assign wins, losses, and ptdiff to the global simTeams df
            simTeams$wins[windex] <- simTeams$wins[windex] + 1
            simTeams$ptdiff[windex] <- simTeams$ptdiff[windex] + spread
            simTeams$losses[lindex] <- simTeams$losses[lindex] + 1
            simTeams$ptdiff[lindex] <- simTeams$ptdiff[lindex] - spread
            
            if (simTeams$Division_id[windex] == simTeams$Division_id[lindex]) {
              simTeams$dwins[windex] <- simTeams$dwins[windex] + 1
              simTeams$dlosses[lindex] <- simTeams$dlosses[lindex] + 1
            }
            
            if (simTeams$Conference_id[windex] == simTeams$Conference_id[lindex]) {
              simTeams$cwins[windex] <- simTeams$cwins[windex] + 1
              simTeams$closses[lindex] <- simTeams$closses[lindex] + 1
            }
          }
        } else {
          simTeams <- teams
        }
        
        
        #check who makes playoffs here
        teamconf <- teams$Conference_id[which(teams$Team_Name == team)]
        teamdiv <- teams$Division_id[which(teams$Team_Name == team)]
        pcheck <- checkPlayoffTeams(simTeams,team,teamconf,teamdiv,gamedays[i])
        #print(pcheck)
        if (!team %in% pcheck) {
          print("update eliminations")
          print(gamedays[i])
          eliminations$`Date Eliminated`[which(eliminations$Team == team)] <- paste(month(gamedays[i]),day(gamedays[i]),year(gamedays[i]),sep="/")
          numElim <- numElim + 1
        }
        simno <- simno + 1
        rm(simTeams)
      }
     # print("sims ended")
      rm(tempTeams)
      
    }
    
  }
  
}

finalTeamsE <- checkPlayoffTeams(teams,"Miami Heat", "East","Southeast",gamedays[162])
finalTeamsW <- checkPlayoffTeams(teams,"Utah Jazz","West","Northwest",gamedays[162])
finalTeams <- c(finalTeamsE,finalTeamsW)
for (team in finalTeams){
  eliminations$`Date Eliminated`[which(eliminations$Team == team)] <- "Playoffs"
}