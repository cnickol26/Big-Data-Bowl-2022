library(tidyverse)

#Load in the data
games<-read.csv("games.csv")
pff_scout<-read.csv("PFFScoutingData.csv")
plays<-read.csv("plays.csv")

# Filter plays to only be punts
punts<-plays%>%filter(specialTeamsPlayType=="Punt")

#Change NAs to say None
punts[is.na(punts)] = "None"

#Remove penalties
punts<-punts%>%filter(penaltyCodes=="None")

#Join with the scouting and games data
punts<-punts%>%full_join(games,by=c("gameId"))
punts<-punts%>%full_join(pff_scout, by=c("gameId","playId"))

#Remove blocks, touchbacks, non special teams results
punts_filter<-punts%>%filter(specialTeamsResult!="Non-Special Teams Result" & 
                               specialTeamsResult != "Blocked Punt" & specialTeamsResult != "Touchback")%>%
  mutate(kickLength=as.numeric(kickLength))%>%
  mutate(kickReturnYardage=as.numeric(kickReturnYardage))

#Change new NAs to be 0
punts_filter[is.na(punts_filter)] = 0

## Examining punts over 70 yards
seventy<-punts_filter%>%filter(kickLength>70)

#Filter to be long punts that were returned only 
test_long_punt<-punts_filter%>%filter(kickLength>=46 & kickLength <=70)%>%filter(specialTeamsResult=="Return")

#Create the kick length bins
test_long_punt$kickLengthbins<-cut(test_long_punt$kickLength,12)

#Count the number for each distance
test_long_yards<-test_long_punt%>%group_by(kickLengthbins)%>%summarise(n=n())

#Get the summary statistics
test_long_median<-test_long_punt%>%group_by(kickLengthbins)%>%summarise(Median_Net=median(playResult),Mean_Net=mean(playResult),Avg_return=mean(kickReturnYardage))%>%left_join(test_long_yards)

#Round the statistics
test_long_median$Mean_Net<-round(test_long_median$Mean_Net,digits = 2)
test_long_median$Avg_return<-round(test_long_median$Avg_return,digits = 2)




