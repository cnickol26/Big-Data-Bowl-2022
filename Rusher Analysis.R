library(tidyverse)
library(stringr)

#Load in the data
games<-read.csv("games.csv")
pff_scout<-read.csv("PFFScoutingData.csv")
players<-read.csv("players.csv")
plays<-read.csv("plays.csv")

#Filter to be punts only
punts<-plays%>%filter(specialTeamsPlayType=="Punt")

#Change NAs to be listed as None
punts[is.na(punts)] = "None"

#Remove penalties
punts<-punts%>%filter(penaltyCodes=="None")

#Join with the games and scouting data
punts<-punts%>%full_join(games,by=c("gameId"))
punts<-punts%>%full_join(pff_scout, by=c("gameId","playId"))

#Filter to remove blocks,touchbacks, and non special teams results
filter_punts<-punts%>%filter(specialTeamsResult!="Non-Special Teams Result" & 
                                             specialTeamsResult != "Blocked Punt"&specialTeamsResult!="Touchback")%>%
  mutate(kickLength=as.numeric(kickLength))%>%
  mutate(kickReturnYardage=as.numeric(kickReturnYardage))

#Change New NAs to be 0
filter_punts[is.na(filter_punts)] = 0

#Remove plays where there were 0 rushers
punts_punters<-filter_punts%>%filter(puntRushers!=0)%>%select(gameId,playId,kickerId,kickLength,playResult,puntRushers)

#Count then number of rushers
punts_punters$Number_Rushers<-str_count(punts_punters$puntRushers,";")

#Add 1 to make count accurate
punts_punters<-punts_punters%>%
  mutate(Number_of_Rushers=Number_Rushers+1,nflId=kickerId)

#Filter data to just these columns
punts_punters<-punts_punters%>%select(nflId,kickLength,playResult,Number_of_Rushers)

#Get summary statistics
punts_by_rushers<-punts_punters%>%group_by(Number_of_Rushers)%>%summarise(Avg_Length=mean(kickLength),Avg_Net=mean(playResult),Attempts=n())

## Blocked Punts
block_punts<-punts%>%filter(specialTeamsResult=="Blocked Punt")
block_punts[is.na(block_punts)] = 0
block_punts$Number_Rushers<-str_count(block_punts$puntRushers,";")
block_punts<-block_punts%>%mutate(Number_of_Rushers=Number_Rushers+1)
block_punts_summary<-block_punts%>%group_by(Number_of_Rushers)%>%summarise(Num_Blocked=n())

#Join bocked data with regular data
punts_by_rushers<-punts_by_rushers%>%left_join(block_punts_summary)

#Change NAs to be 0
punts_by_rushers[is.na(punts_by_rushers)]=0

#Round the summary statistics
punts_by_rushers$Avg_Length<-round(punts_by_rushers$Avg_Length,digits = 2)
punts_by_rushers$Avg_Net<-round(punts_by_rushers$Avg_Net,digits = 2)



