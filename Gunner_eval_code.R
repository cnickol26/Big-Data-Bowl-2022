library(tidyverse)
library(tree)
library(ROCR)
library(randomForest)
library(gbm)

#Load in the data downloaded from Kaggle
games<-read.csv("games.csv")

pff_scout<-read.csv("PFFScoutingData.csv")

players<-read.csv("players.csv")

plays<-read.csv("plays.csv")

track_18<-read.csv("tracking2018.csv")

track_19<-read.csv("tracking2019.csv")

track_20<-read.csv("tracking2020.csv")

#Filter plays to be punts

punts<-plays%>%filter(specialTeamsPlayType=="Punt")

#Change NAs to be listed as "None"

punts[is.na(punts)] = "None"

#Filter out any penalties

punts<-punts%>%filter(penaltyCodes=="None")

#Join with the games and scouting data

punts<-punts%>%full_join(games,by=c("gameId"))
punts<-punts%>%full_join(pff_scout, by=c("gameId","playId"))

#Filter to be fair catches and returns only 

punts_clean<-punts%>%filter(kickContactType=="CC")%>%
  filter(specialTeamsResult=="Fair Catch"|specialTeamsResult=="Return")

#Join in the player tracking data

punts_clean_2018<-punts_clean%>%filter(season==2018)%>%left_join(track_18,by=c("gameId","playId"))

#Filter out the punters and make the event ball_snap so we can look player positioning at the snap 

punts_clean_2018<-punts_clean_2018%>%filter(position!="P")%>%filter(event=="ball_snap")

#Combine gameId and playId so we have one variable with the info from both

punts_clean_2018<-punts_clean_2018%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))

str(punts_clean_2018)

#create the return team and is not returner variables and select only the necessary columns

punts_clean_2018<-punts_clean_2018%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                          specialTeamsResult,returnerId,
                                                                                          nflId,kickLength,kickReturnYardage,
                                                                                          season,operationTime,hangTime,
                                                                                          kickType,gunners,vises,kickContactType,
                                                                                          x,y,o,event,displayName,jerseyNumber,
                                                                                          team,playDirection,return_team,is_not_rtr)
# 1 if the player is not the returner, 0 if he is

for (i in 1:nrow(punts_clean_2018)){
  if (punts_clean_2018$returnerId[i]==punts_clean_2018$nflId[i]){
    punts_clean_2018$is_not_rtr[i]<-0
  } else{
    punts_clean_2018$is_not_rtr[i]<-1
  }
}

#Arrange so the returner is the first player for each set of play data

punts_clean_2018<-punts_clean_2018%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

#List what team is returning the ball home or away 

returning_team<-"0"

for (i in 1:nrow(punts_clean_2018)) {
  ifelse(punts_clean_2018$is_not_rtr[i]==0,
         returning_team<-punts_clean_2018$team[i],
         punts_clean_2018$return_team[i]<-returning_team)
}

#Create new dummy variables

punts_clean_2018<-punts_clean_2018%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

# 1 if player is on the return team 0 otherwise

for(i in 1:nrow(punts_clean_2018)){
  ifelse(punts_clean_2018$return_team[i]==punts_clean_2018$team[i],punts_clean_2018$is_on_rtr_team[i]<-1,
         punts_clean_2018$is_on_rtr_team[i]<-0)
}

# 1 if the player is a gunner, 0 if not

for (i in 1:nrow(punts_clean_2018)){
  ifelse(grepl(punts_clean_2018$jerseyNumber[i],punts_clean_2018$gunners[i])&punts_clean_2018$is_on_rtr_team[i]==0,
         punts_clean_2018$is_gunner[i]<-1,punts_clean_2018$is_gunner[i]<-0)
}

# 1 if the player is a vise, 0 if not 

for (i in 1:nrow(punts_clean_2018)){
  ifelse(grepl(punts_clean_2018$jerseyNumber[i],punts_clean_2018$vises[i])&punts_clean_2018$is_on_rtr_team[i]==1,
         punts_clean_2018$is_vise[i]<-1,punts_clean_2018$is_vise[i]<-0)
}

# Filter to be only gunners and vises, list the vises first

punts_clean_2018_filter<-punts_clean_2018%>%
  filter(is_gunner==1|is_vise==1)%>%group_by(game_play)%>%arrange(is_gunner,.by_group = TRUE)%>%ungroup()

#Create a second game_play and lag it so we can compare. Also create the doubled variable

punts_clean_2018_filter<-punts_clean_2018_filter%>%mutate(game_play_2=game_play)
punts_clean_2018_filter<-punts_clean_2018_filter%>%mutate(game_play_2=lag(game_play_2,default = first(game_play)))
punts_clean_2018_filter<-punts_clean_2018_filter%>%mutate(doubled=0)

#Empty arrays for use later

vise_x<-array()
vise_y<-array()
vise_dist<-array()

# Remove any NAs

punts_clean_2018_filter_test<-punts_clean_2018_filter%>%na.omit()

# Count the of vises on each gunner and store it in doubled

for (i in 1:nrow(punts_clean_2018_filter_test)){
  if(punts_clean_2018_filter_test$game_play[i]==punts_clean_2018_filter_test$game_play_2[i]){
    if (punts_clean_2018_filter_test$is_vise[i]==1){
      vise_y[i]<-punts_clean_2018_filter_test$y[i]
    }
    else{
      x<-0
      vise_y<-na.omit(vise_y)
      for (j in 1:length(vise_y)){
        if (abs(punts_clean_2018_filter_test$y[i]-vise_y[j])<2){
          x<-x+1
        }
      }
      punts_clean_2018_filter_test$doubled[i]<-x
    }
  }
  else{
    vise_y<-array()
    vise_y[i]<-punts_clean_2018_filter_test$y[i]
  }
}

#Create a second variable to store gunner x and y at the snap and filter to be gunners only

punts_clean_2018_gunners<-punts_clean_2018_filter_test%>%filter(is_gunner==1)%>%mutate(snap_x=x,snap_y=y)

# Start over fresh, going to look at point of catch instead of snap now 

punts_clean_2018_new<-punts_clean%>%filter(season==2018)%>%left_join(track_18,by=c("gameId","playId"))

#Repeat similar steps to before but with event equal to punt_received

punts_clean_2018_new_filtered<-punts_clean_2018_new%>%filter(position!="P")%>%filter(event=="punt_received"|event=="fair_catch")
punts_clean_2018_new_filtered<-punts_clean_2018_new_filtered%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))


punts_clean_2018_new_filtered<-punts_clean_2018_new_filtered%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                                                    specialTeamsResult,returnerId,
                                                                                                                    nflId,kickLength,kickReturnYardage,
                                                                                                                    season,operationTime,hangTime,
                                                                                                                    kickType,gunners,vises,kickContactType,
                                                                                                                    x,y,o,event,displayName,jerseyNumber,
                                                                                                                    team,playDirection,return_team,is_not_rtr)


for (i in 1:nrow(punts_clean_2018_new_filtered)){
  if (punts_clean_2018_new_filtered$returnerId[i]==punts_clean_2018_new_filtered$nflId[i]){
    punts_clean_2018_new_filtered$is_not_rtr[i]<-0
  } else{
    punts_clean_2018_new_filtered$is_not_rtr[i]<-1
  }
}

punts_clean_2018_new_filtered<-punts_clean_2018_new_filtered%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

returning_team<-"0"

for (i in 1:nrow(punts_clean_2018_new_filtered)) {
  ifelse(punts_clean_2018_new_filtered$is_not_rtr[i]==0,
         returning_team<-punts_clean_2018_new_filtered$team[i],
         punts_clean_2018_new_filtered$return_team[i]<-returning_team)
}

punts_clean_2018_new_filtered<-punts_clean_2018_new_filtered%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

for(i in 1:nrow(punts_clean_2018_new_filtered)){
  ifelse(punts_clean_2018_new_filtered$return_team[i]==punts_clean_2018_new_filtered$team[i],punts_clean_2018_new_filtered$is_on_rtr_team[i]<-1,
         punts_clean_2018_new_filtered$is_on_rtr_team[i]<-0)
}

for (i in 1:nrow(punts_clean_2018_new_filtered)){
  ifelse(grepl(punts_clean_2018_new_filtered$jerseyNumber[i],punts_clean_2018_new_filtered$gunners[i])&punts_clean_2018_new_filtered$is_on_rtr_team[i]==0,
         punts_clean_2018_new_filtered$is_gunner[i]<-1,punts_clean_2018_new_filtered$is_gunner[i]<-0)
}

for (i in 1:nrow(punts_clean_2018_new_filtered)){
  ifelse(grepl(punts_clean_2018_new_filtered$jerseyNumber[i],punts_clean_2018_new_filtered$vises[i])&punts_clean_2018_new_filtered$is_on_rtr_team[i]==1,
         punts_clean_2018_new_filtered$is_vise[i]<-1,punts_clean_2018_new_filtered$is_vise[i]<-0)
}

# Filter to include returners and gunners

punts_clean_2018_new_filtered<-punts_clean_2018_new_filtered%>%filter(return_team=="Returner"|is_gunner==1)

#Filter out gunners who had 0 vises on them

punts_clean_2018_gunners_2<-punts_clean_2018_gunners%>%select(game_play,nflId,doubled,snap_x,snap_y)%>%filter(doubled>0)

#Join with the snap data set created earlier

punts_2018_gunners<-punts_clean_2018_new_filtered%>%left_join(punts_clean_2018_gunners_2,by=c("game_play","nflId"))

#Filter out a couple games with NAs and remove Taysom Hill

punts_2018_gunners<-punts_2018_gunners%>%filter(game_play!="2018091606 4125"|game_play!="2018100702 3909"|game_play!="2018110400 4196")%>%filter(nflId!="45244")

#Make any NAs 0

punts_2018_gunners$snap_x[is.na(punts_2018_gunners$snap_x)] = 0
punts_2018_gunners$snap_y[is.na(punts_2018_gunners$snap_y)] = 0

#Create a dummy variable for extra info

punts_2018_gunners<-punts_2018_gunners%>%mutate(is_extra=0)

# List as a 1 gunners with no snap info

for (i in 1:nrow(punts_2018_gunners)){
  if (punts_2018_gunners$is_gunner[i]==1){
    if (punts_2018_gunners$snap_x[i]==0){
      punts_2018_gunners$is_extra[i]<-1
    }
  }
}

#Filter those players out and create euclidean distance variables

punts_2018_gunners<-punts_2018_gunners%>%filter(is_extra==0)%>%select(-is_extra)
punts_2018_gunners<-punts_2018_gunners%>%mutate(euc_dis_snap=0,euc_dist_from_rtr=0)

#Calculate E.D. of gunner to returner at catch point

#Function to calculate euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

returner_x<-0
returner_y<-0

for (i in 1:nrow(punts_2018_gunners)){
  ifelse (punts_2018_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2018_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2018_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2018_gunners$x[i],punts_2018_gunners$y[i]); punts_2018_gunners$euc_dist_from_rtr[i] <- euclidean(returner,player)})
}

#Calculate E.D. of gunner at the snap to catch point 

for (i in 1:nrow(punts_2018_gunners)){
  ifelse (punts_2018_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2018_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2018_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2018_gunners$snap_x[i],punts_2018_gunners$snap_y[i]); punts_2018_gunners$euc_dis_snap[i] <- euclidean(returner,player)})
}

#Filter out returners and create under_8 dummy variable

punts_2018_gunners_only<-punts_2018_gunners%>%filter(return_team!="Returner")%>%mutate(under_8=0)

# 1 if gunner is under 8, 0 if not

punts_2018_gunners_only$under_8<-ifelse(punts_2018_gunners_only$euc_dist_from_rtr<8,punts_2018_gunners_only$under_8<-1,punts_2018_gunners_only$under_8<-0)

#Save important variables for the classification 

Data<-punts_2018_gunners_only%>%select(game_play,nflId,kickLength,season,operationTime,hangTime,displayName,jerseyNumber,doubled,euc_dis_snap,euc_dist_from_rtr,under_8)%>%
  mutate(under_8=as.factor(under_8))%>%mutate(kickLength=as.numeric(kickLength))


##### Repeating for the 2019 data


punts_clean_2019<-punts_clean%>%filter(season==2019)%>%left_join(track_19,by=c("gameId","playId"))

punts_clean_2019<-punts_clean_2019%>%filter(position!="P")%>%filter(event=="ball_snap")
punts_clean_2019<-punts_clean_2019%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))

str(punts_clean_2019)
punts_clean_2019<-punts_clean_2019%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                          specialTeamsResult,returnerId,
                                                                                          nflId,kickLength,kickReturnYardage,
                                                                                          season,operationTime,hangTime,
                                                                                          kickType,gunners,vises,kickContactType,
                                                                                          x,y,o,event,displayName,jerseyNumber,
                                                                                          team,playDirection,return_team,is_not_rtr)
for (i in 1:nrow(punts_clean_2019)){
  if (punts_clean_2019$returnerId[i]==punts_clean_2019$nflId[i]){
    punts_clean_2019$is_not_rtr[i]<-0
  } else{
    punts_clean_2019$is_not_rtr[i]<-1
  }
}

punts_clean_2019<-punts_clean_2019%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

returning_team<-"0"

for (i in 1:nrow(punts_clean_2019)) {
  ifelse(punts_clean_2019$is_not_rtr[i]==0,
         returning_team<-punts_clean_2019$team[i],
         punts_clean_2019$return_team[i]<-returning_team)
}

punts_clean_2019<-punts_clean_2019%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

for(i in 1:nrow(punts_clean_2019)){
  ifelse(punts_clean_2019$return_team[i]==punts_clean_2019$team[i],punts_clean_2019$is_on_rtr_team[i]<-1,
         punts_clean_2019$is_on_rtr_team[i]<-0)
}

for (i in 1:nrow(punts_clean_2019)){
  ifelse(grepl(punts_clean_2019$jerseyNumber[i],punts_clean_2019$gunners[i])&punts_clean_2019$is_on_rtr_team[i]==0,
         punts_clean_2019$is_gunner[i]<-1,punts_clean_2019$is_gunner[i]<-0)
}

for (i in 1:nrow(punts_clean_2019)){
  ifelse(grepl(punts_clean_2019$jerseyNumber[i],punts_clean_2019$vises[i])&punts_clean_2019$is_on_rtr_team[i]==1,
         punts_clean_2019$is_vise[i]<-1,punts_clean_2019$is_vise[i]<-0)
}

punts_clean_2019_filter<-punts_clean_2019%>%
  filter(is_gunner==1|is_vise==1)%>%group_by(game_play)%>%arrange(is_gunner,.by_group = TRUE)%>%ungroup()

punts_clean_2019_filter<-punts_clean_2019_filter%>%mutate(game_play_2=game_play)
punts_clean_2019_filter<-punts_clean_2019_filter%>%mutate(game_play_2=lag(game_play_2,default = first(game_play)))
punts_clean_2019_filter<-punts_clean_2019_filter%>%mutate(doubled=0)


vise_x<-array()
vise_y<-array()
vise_dist<-array()

punts_clean_2019_filter_test<-punts_clean_2019_filter%>%na.omit()

vise_y<-array()

for (i in 1:nrow(punts_clean_2019_filter_test)){
  if(punts_clean_2019_filter_test$game_play[i]==punts_clean_2019_filter_test$game_play_2[i]){
    if (punts_clean_2019_filter_test$is_vise[i]==1){
      vise_y[i]<-punts_clean_2019_filter_test$y[i]
    }
    else{
      x<-0
      vise_y<-na.omit(vise_y)
      for (j in 1:length(vise_y)){
        if (abs(punts_clean_2019_filter_test$y[i]-vise_y[j])<2){
          x<-x+1
        }
      }
      punts_clean_2019_filter_test$doubled[i]<-x
    }
  }
  else{
    vise_y<-array()
    vise_y[i]<-punts_clean_2019_filter_test$y[i]
  }
}

punts_clean_2019_gunners<-punts_clean_2019_filter_test%>%filter(is_gunner==1)%>%mutate(snap_x=x,snap_y=y)

punts_clean_2019_new<-punts_clean%>%filter(season==2019)%>%left_join(track_19,by=c("gameId","playId"))


punts_clean_2019_new_filtered<-punts_clean_2019_new%>%filter(position!="P")%>%filter(event=="punt_received"|event=="fair_catch")
punts_clean_2019_new_filtered<-punts_clean_2019_new_filtered%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))


punts_clean_2019_new_filtered<-punts_clean_2019_new_filtered%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                                                    specialTeamsResult,returnerId,
                                                                                                                    nflId,kickLength,kickReturnYardage,
                                                                                                                    season,operationTime,hangTime,
                                                                                                                    kickType,gunners,vises,kickContactType,
                                                                                                                    x,y,o,event,displayName,jerseyNumber,
                                                                                                                    team,playDirection,return_team,is_not_rtr)


for (i in 1:nrow(punts_clean_2019_new_filtered)){
  if (punts_clean_2019_new_filtered$returnerId[i]==punts_clean_2019_new_filtered$nflId[i]){
    punts_clean_2019_new_filtered$is_not_rtr[i]<-0
  } else{
    punts_clean_2019_new_filtered$is_not_rtr[i]<-1
  }
}

punts_clean_2019_new_filtered<-punts_clean_2019_new_filtered%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

returning_team<-"0"

for (i in 1:nrow(punts_clean_2019_new_filtered)) {
  ifelse(punts_clean_2019_new_filtered$is_not_rtr[i]==0,
         returning_team<-punts_clean_2019_new_filtered$team[i],
         punts_clean_2019_new_filtered$return_team[i]<-returning_team)
}

punts_clean_2019_new_filtered<-punts_clean_2019_new_filtered%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

for(i in 1:nrow(punts_clean_2019_new_filtered)){
  ifelse(punts_clean_2019_new_filtered$return_team[i]==punts_clean_2019_new_filtered$team[i],punts_clean_2019_new_filtered$is_on_rtr_team[i]<-1,
         punts_clean_2019_new_filtered$is_on_rtr_team[i]<-0)
}

for (i in 1:nrow(punts_clean_2019_new_filtered)){
  ifelse(grepl(punts_clean_2019_new_filtered$jerseyNumber[i],punts_clean_2019_new_filtered$gunners[i])&punts_clean_2019_new_filtered$is_on_rtr_team[i]==0,
         punts_clean_2019_new_filtered$is_gunner[i]<-1,punts_clean_2019_new_filtered$is_gunner[i]<-0)
}

for (i in 1:nrow(punts_clean_2019_new_filtered)){
  ifelse(grepl(punts_clean_2019_new_filtered$jerseyNumber[i],punts_clean_2019_new_filtered$vises[i])&punts_clean_2019_new_filtered$is_on_rtr_team[i]==1,
         punts_clean_2019_new_filtered$is_vise[i]<-1,punts_clean_2019_new_filtered$is_vise[i]<-0)
}

punts_clean_2019_new_filtered<-punts_clean_2019_new_filtered%>%filter(return_team=="Returner"|is_gunner==1)
punts_clean_2019_gunners_2<-punts_clean_2019_gunners%>%select(game_play,nflId,doubled,snap_x,snap_y)%>%filter(doubled>0)

punts_2019_gunners<-punts_clean_2019_new_filtered%>%left_join(punts_clean_2019_gunners_2,by=c("game_play","nflId"))

punts_2019_gunners<-punts_2019_gunners%>%filter(game_play!="2019111010 4195")%>%filter(nflId!="45244")

punts_2019_gunners$snap_x[is.na(punts_2019_gunners$snap_x)] = 0
punts_2019_gunners$snap_y[is.na(punts_2019_gunners$snap_y)] = 0

punts_2019_gunners<-punts_2019_gunners%>%mutate(is_extra=0)

for (i in 1:nrow(punts_2019_gunners)){
  if (punts_2019_gunners$is_gunner[i]==1){
    if (punts_2019_gunners$snap_x[i]==0){
      punts_2019_gunners$is_extra[i]<-1
    }
  }
}

punts_2019_gunners<-punts_2019_gunners%>%filter(is_extra==0)%>%select(-is_extra)
punts_2019_gunners<-punts_2019_gunners%>%mutate(euc_dis_snap=0,euc_dist_from_rtr=0)

returner_x<-0
returner_y<-0

for (i in 1:nrow(punts_2019_gunners)){
  ifelse (punts_2019_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2019_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2019_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2019_gunners$x[i],punts_2019_gunners$y[i]); punts_2019_gunners$euc_dist_from_rtr[i] <- euclidean(returner,player)})
}

for (i in 1:nrow(punts_2019_gunners)){
  ifelse (punts_2019_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2019_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2019_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2019_gunners$snap_x[i],punts_2019_gunners$snap_y[i]); punts_2019_gunners$euc_dis_snap[i] <- euclidean(returner,player)})
}

punts_2019_gunners_only<-punts_2019_gunners%>%filter(return_team!="Returner")%>%mutate(under_8=0)

punts_2019_gunners_only$under_8<-ifelse(punts_2019_gunners_only$euc_dist_from_rtr<8,punts_2019_gunners_only$under_8<-1,punts_2019_gunners_only$under_8<-0)

Data_19<-punts_2019_gunners_only%>%select(game_play,nflId,kickLength,season,operationTime,hangTime,displayName,jerseyNumber,doubled,euc_dis_snap,euc_dist_from_rtr,under_8)%>%
  mutate(under_8=as.factor(under_8))%>%mutate(kickLength=as.numeric(kickLength))




##### Repeat once again for 2020 data




punts<-plays%>%filter(specialTeamsPlayType=="Punt")
punts[is.na(punts)] = "None"
punts<-punts%>%filter(penaltyCodes=="None")

punts<-punts%>%full_join(games,by=c("gameId"))
punts<-punts%>%full_join(pff_scout, by=c("gameId","playId"))
punts_clean<-punts%>%filter(kickContactType=="CC")%>%
  filter(specialTeamsResult=="Fair Catch"|specialTeamsResult=="Return")

punts_clean_2020<-punts_clean%>%filter(season==2020)%>%left_join(track_20,by=c("gameId","playId"))

punts_clean_2020<-punts_clean_2020%>%filter(position!="P")%>%filter(event=="ball_snap")
punts_clean_2020<-punts_clean_2020%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))

str(punts_clean_2020)
punts_clean_2020<-punts_clean_2020%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                          specialTeamsResult,returnerId,
                                                                                          nflId,kickLength,kickReturnYardage,
                                                                                          season,operationTime,hangTime,
                                                                                          kickType,gunners,vises,kickContactType,
                                                                                          x,y,o,event,displayName,jerseyNumber,
                                                                                          team,playDirection,return_team,is_not_rtr)
for (i in 1:nrow(punts_clean_2020)){
  if (punts_clean_2020$returnerId[i]==punts_clean_2020$nflId[i]){
    punts_clean_2020$is_not_rtr[i]<-0
  } else{
    punts_clean_2020$is_not_rtr[i]<-1
  }
}

punts_clean_2020<-punts_clean_2020%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

returning_team<-"0"

for (i in 1:nrow(punts_clean_2020)) {
  ifelse(punts_clean_2020$is_not_rtr[i]==0,
         returning_team<-punts_clean_2020$team[i],
         punts_clean_2020$return_team[i]<-returning_team)
}

punts_clean_2020<-punts_clean_2020%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

for(i in 1:nrow(punts_clean_2020)){
  ifelse(punts_clean_2020$return_team[i]==punts_clean_2020$team[i],punts_clean_2020$is_on_rtr_team[i]<-1,
         punts_clean_2020$is_on_rtr_team[i]<-0)
}

for (i in 1:nrow(punts_clean_2020)){
  ifelse(grepl(punts_clean_2020$jerseyNumber[i],punts_clean_2020$gunners[i])&punts_clean_2020$is_on_rtr_team[i]==0,
         punts_clean_2020$is_gunner[i]<-1,punts_clean_2020$is_gunner[i]<-0)
}

for (i in 1:nrow(punts_clean_2020)){
  ifelse(grepl(punts_clean_2020$jerseyNumber[i],punts_clean_2020$vises[i])&punts_clean_2020$is_on_rtr_team[i]==1,
         punts_clean_2020$is_vise[i]<-1,punts_clean_2020$is_vise[i]<-0)
}

punts_clean_2020_filter<-punts_clean_2020%>%
  filter(is_gunner==1|is_vise==1)%>%group_by(game_play)%>%arrange(is_gunner,.by_group = TRUE)%>%ungroup()

punts_clean_2020_filter<-punts_clean_2020_filter%>%mutate(game_play_2=game_play)
punts_clean_2020_filter<-punts_clean_2020_filter%>%mutate(game_play_2=lag(game_play_2,default = first(game_play)))
punts_clean_2020_filter<-punts_clean_2020_filter%>%mutate(doubled=0)

vise_x<-array()
vise_y<-array()
vise_dist<-array()

punts_clean_2020_filter_test<-punts_clean_2020_filter%>%na.omit()

vise_y<-array()

for (i in 1:nrow(punts_clean_2020_filter_test)){
  if(punts_clean_2020_filter_test$game_play[i]==punts_clean_2020_filter_test$game_play_2[i]){
    if (punts_clean_2020_filter_test$is_vise[i]==1){
      vise_y[i]<-punts_clean_2020_filter_test$y[i]
    }
    else{
      x<-0
      vise_y<-na.omit(vise_y)
      for (j in 1:length(vise_y)){
        if (abs(punts_clean_2020_filter_test$y[i]-vise_y[j])<2){
          x<-x+1
        }
      }
      punts_clean_2020_filter_test$doubled[i]<-x
    }
  }
  else{
    vise_y<-array()
    vise_y[i]<-punts_clean_2020_filter_test$y[i]
  }
}

punts_clean_2020_gunners<-punts_clean_2020_filter_test%>%filter(is_gunner==1)%>%mutate(snap_x=x,snap_y=y)

punts_clean_2020_new<-punts_clean%>%filter(season==2020)%>%left_join(track_20,by=c("gameId","playId"))


punts_clean_2020_new_filtered<-punts_clean_2020_new%>%filter(position!="P")%>%filter(event=="punt_received"|event=="fair_catch")
punts_clean_2020_new_filtered<-punts_clean_2020_new_filtered%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))


punts_clean_2020_new_filtered<-punts_clean_2020_new_filtered%>%mutate(return_team="Returner",is_not_rtr=0)%>%select(game_play,specialTeamsPlayType,
                                                                                                                    specialTeamsResult,returnerId,
                                                                                                                    nflId,kickLength,kickReturnYardage,
                                                                                                                    season,operationTime,hangTime,
                                                                                                                    kickType,gunners,vises,kickContactType,
                                                                                                                    x,y,o,event,displayName,jerseyNumber,
                                                                                                                    team,playDirection,return_team,is_not_rtr)


for (i in 1:nrow(punts_clean_2020_new_filtered)){
  if (punts_clean_2020_new_filtered$returnerId[i]==punts_clean_2020_new_filtered$nflId[i]){
    punts_clean_2020_new_filtered$is_not_rtr[i]<-0
  } else{
    punts_clean_2020_new_filtered$is_not_rtr[i]<-1
  }
}

punts_clean_2020_new_filtered<-punts_clean_2020_new_filtered%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group = TRUE)%>%ungroup()

returning_team<-"0"

for (i in 1:nrow(punts_clean_2020_new_filtered)) {
  ifelse(punts_clean_2020_new_filtered$is_not_rtr[i]==0,
         returning_team<-punts_clean_2020_new_filtered$team[i],
         punts_clean_2020_new_filtered$return_team[i]<-returning_team)
}

punts_clean_2020_new_filtered<-punts_clean_2020_new_filtered%>%mutate(is_gunner=0,is_vise=0,is_on_rtr_team=0)

for(i in 1:nrow(punts_clean_2020_new_filtered)){
  ifelse(punts_clean_2020_new_filtered$return_team[i]==punts_clean_2020_new_filtered$team[i],punts_clean_2020_new_filtered$is_on_rtr_team[i]<-1,
         punts_clean_2020_new_filtered$is_on_rtr_team[i]<-0)
}

for (i in 1:nrow(punts_clean_2020_new_filtered)){
  ifelse(grepl(punts_clean_2020_new_filtered$jerseyNumber[i],punts_clean_2020_new_filtered$gunners[i])&punts_clean_2020_new_filtered$is_on_rtr_team[i]==0,
         punts_clean_2020_new_filtered$is_gunner[i]<-1,punts_clean_2020_new_filtered$is_gunner[i]<-0)
}

for (i in 1:nrow(punts_clean_2020_new_filtered)){
  ifelse(grepl(punts_clean_2020_new_filtered$jerseyNumber[i],punts_clean_2020_new_filtered$vises[i])&punts_clean_2020_new_filtered$is_on_rtr_team[i]==1,
         punts_clean_2020_new_filtered$is_vise[i]<-1,punts_clean_2020_new_filtered$is_vise[i]<-0)
}

punts_clean_2020_new_filtered<-punts_clean_2020_new_filtered%>%filter(return_team=="Returner"|is_gunner==1)
punts_clean_2020_gunners_2<-punts_clean_2020_gunners%>%select(game_play,nflId,doubled,snap_x,snap_y)%>%filter(doubled>0)

punts_2020_gunners<-punts_clean_2020_new_filtered%>%left_join(punts_clean_2020_gunners_2,by=c("game_play","nflId"))

punts_2020_gunners<-punts_2020_gunners%>%filter(game_play!="2020110100 2417"&game_play!="2020120611 1329"&game_play!="2020120611 870")%>%filter(nflId!="45244")

punts_2020_gunners$snap_x[is.na(punts_2020_gunners$snap_x)] = 0
punts_2020_gunners$snap_y[is.na(punts_2020_gunners$snap_y)] = 0

punts_2020_gunners<-punts_2020_gunners%>%mutate(is_extra=0)

for (i in 1:nrow(punts_2020_gunners)){
  if (punts_2020_gunners$is_gunner[i]==1){
    if (punts_2020_gunners$snap_x[i]==0){
      punts_2020_gunners$is_extra[i]<-1
    }
  }
}

punts_2020_gunners<-punts_2020_gunners%>%filter(is_extra==0)%>%select(-is_extra)
punts_2020_gunners<-punts_2020_gunners%>%mutate(euc_dis_snap=0,euc_dist_from_rtr=0)

returner_x<-0
returner_y<-0

for (i in 1:nrow(punts_2020_gunners)){
  ifelse (punts_2020_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2020_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2020_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2020_gunners$x[i],punts_2020_gunners$y[i]); punts_2020_gunners$euc_dist_from_rtr[i] <- euclidean(returner,player)})
}

for (i in 1:nrow(punts_2020_gunners)){
  ifelse (punts_2020_gunners$return_team[i]=="Returner",{assign("returner_x",punts_2020_gunners$x[i],envir = .GlobalEnv); assign("returner_y",punts_2020_gunners$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_2020_gunners$snap_x[i],punts_2020_gunners$snap_y[i]); punts_2020_gunners$euc_dis_snap[i] <- euclidean(returner,player)})
}

punts_2020_gunners_only<-punts_2020_gunners%>%filter(return_team!="Returner")%>%mutate(under_8=0)

punts_2020_gunners_only$under_8<-ifelse(punts_2020_gunners_only$euc_dist_from_rtr<8,punts_2020_gunners_only$under_8<-1,punts_2020_gunners_only$under_8<-0)

Data_20<-punts_2020_gunners_only%>%select(game_play,nflId,kickLength,season,operationTime,hangTime,displayName,jerseyNumber,doubled,euc_dis_snap,euc_dist_from_rtr,under_8)%>%
  mutate(under_8=as.factor(under_8))%>%mutate(kickLength=as.numeric(kickLength))




## 2018 and 2019 as train, 2020 as test
Data_18_19<-Data%>%full_join(Data_19)

#Count number of observations for each gunner and put ones with less than 5 for 2020 in the training data

Data_20_count<-Data_20%>%group_by(nflId)%>%summarise(n=n())
Data_20<-Data_20%>%left_join(Data_20_count)
mean(Data_20$n)
Data_20_small<-Data_20%>%filter(n<5)
Data_20<-Data_20%>%filter(n>5)
Data_18_19<-Data_18_19%>%full_join(Data_20_small)
str(Data_20)

## Logistic Regression

result_train<-glm(under_8~kickLength+operationTime+hangTime+doubled+euc_dis_snap,family = binomial,data = Data_18_19)

summary(result_train)

preds<-predict(result_train,newdata = Data_20,type = "response")

rates<-ROCR::prediction(preds, Data_20$under_8)

roc_result<-ROCR::performance(rates,measure = "tpr",x.measure = "fpr")

plot(roc_result,main="Roc Curve")
lines(x=c(0,1),y=c(0,1),col="red")

auc<-ROCR::performance(rates,measure="auc")
auc@y.values

confusion.mat<-table(Data_20$under_8,preds > 0.5)
confusion.mat

(confusion.mat[1,1]+confusion.mat[2,2])/sum(confusion.mat)

# 73.3% accuracy with log reg

## Bagging

pred.test<-Data_20$under_8

set.seed(1)
bag.class<-randomForest::randomForest(under_8~kickLength+operationTime+hangTime+doubled+euc_dis_snap,data=Data_18_19,mtry=5,importance=TRUE)
bag.class

randomForest::importance(bag.class)
randomForest::varImpPlot(bag.class)

pred.bag<-predict(bag.class,newdata = Data_20)

table(pred.test,pred.bag)
mean(pred.bag==pred.test)

# 76.2% accuracy with bagging

## Random Forest

set.seed(1)
rf.class<-randomForest::randomForest(under_8~kickLength+operationTime+hangTime+doubled+euc_dis_snap,data=Data_18_19,mtry=3,importance=TRUE)
rf.class

importance(rf.class)
varImpPlot(rf.class)

pred.rf<-predict(rf.class,newdata = Data_20)
mean(pred.rf==pred.test)

# Accuracy of 76.4% with random forest

## Boosting

Data_dummy<-Data_18_19%>%mutate(under_8=as.numeric(under_8))%>%select(under_8,kickLength,operationTime,hangTime,doubled,euc_dis_snap)
Data_dummy$under_8<-ifelse(Data_dummy$under_8==2,1,0)
Data_dummy_20<-Data_20%>%mutate(under_8=as.numeric(under_8))%>%select(under_8,kickLength,operationTime,hangTime,doubled,euc_dis_snap)
Data_dummy_20$under_8<-ifelse(Data_dummy_20$under_8==2,1,0)

pred.test.dummy<-Data_dummy_20$under_8

boost.class<-gbm::gbm(under_8~.,data = Data_dummy,distribution = "bernoulli",n.trees = 500)
summary(boost.class)

plot(boost.class,i="euc_dis_snap")
plot(boost.class,i="hangTime")

pred.boost<-predict(boost.class,newdata = Data_dummy_20, n.trees = 500, type = "response")

boost.tab<-table(pred.test.dummy, pred.boost > 0.45)
boost.tab

(boost.tab[1,1]+boost.tab[2,2])/sum(boost.tab)

# 77.2% accuracy with Boosting

# Using Random Forest for the predictions and filtering data to be used in the table for presentaion

Data_20<-Data_20%>%mutate(prediction_prob=0,prediction_class=0)
Data_20$prediction_prob<-pred.boost
Data_20$prediction_class<-ifelse(Data_20$prediction_prob>0.45,Data_20$prediction_class<-1,Data_20$prediction_class<-0)

Data_20<-Data_20%>%left_join(Data_20_count)%>%mutate(under_8=as.numeric(under_8))
Data_20$under_8<-ifelse(Data_20$under_8==2,Data_20$under_8<-1,Data_20$under_8<-0)

Data_20_actual<-Data_20%>%group_by(nflId)%>%tally(wt=under_8)%>%mutate(actual=n)%>%select(-n)

Data_20<-Data_20%>%left_join(Data_20_actual)
Data_20_predicted<-Data_20%>%group_by(nflId)%>%tally(wt=prediction_class)%>%mutate(predicted=n)%>%select(-n)

Data_20<-Data_20%>%left_join(Data_20_predicted)

Data_20_actual_rate<-Data_20%>%group_by(nflId)%>%summarise(actual/n)%>%slice(1)
Data_20_predicted_rate<-Data_20%>%group_by(nflId)%>%summarise(predicted/n)%>%slice(1)

colnames(Data_20_actual_rate)<-c("nflId","actual_rate")
colnames(Data_20_predicted_rate)<-c("nflId","predicted_rate")

Data_20_eval<-Data_20_actual_rate%>%left_join(Data_20_predicted_rate)%>%left_join(players)%>%mutate(rate_dif=actual_rate-predicted_rate)%>%left_join(Data_20_count)

dean_2<-punts%>%filter(gameId==	2020100800)

data_20_eval_save<-Data_20_eval%>%select(displayName,n,actual_rate,predicted_rate,rate_dif)

data_20_eval_save$actual_rate<-round(data_20_eval_save$actual_rate,digits = 2)
data_20_eval_save$predicted_rate<-round(data_20_eval_save$predicted_rate,digits = 2)
data_20_eval_save$rate_dif<-round(data_20_eval_save$rate_dif,digits = 2)