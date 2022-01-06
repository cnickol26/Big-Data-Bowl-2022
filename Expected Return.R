library(tidyverse)
library(tree)
library(randomForest)
library(glmnet)

#Load in the data downloaded from Kaggle
games<-read.csv("games.csv")

pff_scout<-read.csv("PFFScoutingData.csv")

players<-read.csv("players.csv")

plays<-read.csv("plays.csv")

track_18<-read.csv("tracking2018.csv")

track_19<-read.csv("tracking2019.csv")

track_20<-read.csv("tracking2020.csv")

#Filter the plays data to only include punts and then punts that were returned
punts<-plays%>%filter(specialTeamsPlayType=="Punt")%>%
  filter(specialTeamsResult=="Return")

#Change any NAs to instead be lised as "None"
punts[is.na(punts)] = "None"

#Filter out any penalties 
punts<-punts%>%filter(penaltyCodes=="None")

#Add in the games and pff_scout data to the filtered punts data 
punts<-punts%>%full_join(games,by=c("gameId"))
punts<-punts%>%full_join(pff_scout, by=c("gameId","playId"))

# Going to break punts down by year to add in the tracking data
#2018 Punts 
punts_18<-punts%>%filter(season==2018)%>%
  left_join(track_18, by=c("gameId","playId"))

#Change any NAs to be listed as "None"
punts_18[is.na(punts_18)]= "None"

#Filter out a few plays were there was no playId and then filter to be only a normal style punt and a clean catch by the returner
punts_18<-punts_18%>%filter(playId!="None"&nflId!="None")%>%
  filter(kickType=="N")%>%
  filter(kickContactType=="CC")%>%
  filter(event=="punt_received")

#Repeat for 2019 and 2020
#2019
punts_19<-punts%>%filter(season==2019)%>%
  left_join(track_19, by=c("gameId","playId"))

punts_19[is.na(punts_19)]= "None"

punts_19<-punts_19%>%filter(playId!="None"&nflId!="None")%>%
  filter(kickType=="N")%>%
  filter(kickContactType=="CC")%>%
  filter(event=="punt_received")

#2020
punts_20<-punts%>%filter(season==2020)%>%
  left_join(track_20, by=c("gameId","playId"))

punts_20[is.na(punts_20)]= "None"

punts_20<-punts_20%>%filter(playId!="None"&nflId!="None")%>%
  filter(kickType=="N")%>%
  filter(kickContactType=="CC")%>%
  filter(event=="punt_received")

##Joining 2018 and 2019 together since they will be the training data
punts_18_19<-punts_18%>%full_join(punts_19)


##Going to clean 2020 separately from 18 and 19

#Adding in a variable that will say if home or away is the return team and if a player is not the returner and the euclidean distance from the returner
punts_18_19<-punts_18_19%>%mutate(return_team="Returner",is_not_rtr=0,euc_dis_from_rtr=0)
punts_20<-punts_20%>%mutate(return_team="Returner",is_not_rtr=0,euc_dis_from_rtr=0)

# Creating a variable that is a combination of gameId and playId
punts_18_19<-punts_18_19%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))
punts_20<-punts_20%>%mutate(gameId=as.character(gameId),playId=as.character(playId))%>%mutate(game_play=paste(gameId,playId))

#Creating a dummy variable that is a 0 if the player is the returner, a 1 otherwise
for (i in 1:nrow(punts_18_19)){
  ifelse(punts_18_19$nflId[i]==punts_18_19$returnerId[i],
         punts_18_19$is_not_rtr[i]<-0,
         punts_18_19$is_not_rtr[i]<-1)
}

#Repeat for 2020
for (i in 1:nrow(punts_20)){
  ifelse(punts_20$nflId[i]==punts_20$returnerId[i],
         punts_20$is_not_rtr[i]<-0,
         punts_20$is_not_rtr[i]<-1)
}

#Arrange the rows so the returner is first for a given play
punts_18_19<-punts_18_19%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group=TRUE)%>%ungroup()
punts_20<-punts_20%>%group_by(game_play)%>%arrange(is_not_rtr,.by_group=TRUE)%>%ungroup()

#Create a variable that will say if the return team is home or away
return_team<-""

for (i in 1:nrow(punts_18_19)) {
  ifelse(punts_18_19$is_not_rtr[i]==0,
         returning_team<-punts_18_19$team[i],
         punts_18_19$return_team[i]<-returning_team)
}

for (i in 1:nrow(punts_20)) {
  ifelse(punts_20$is_not_rtr[i]==0,
         returning_team<-punts_20$team[i],
         punts_20$return_team[i]<-returning_team)
}

#Function to calculate euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))

#Setting x,y,o to be numeric instead of characters
punts_18_19<-punts_18_19%>%mutate(x=as.numeric(x),y=as.numeric(y),o=as.numeric(o))
punts_20<-punts_20%>%mutate(x=as.numeric(x),y=as.numeric(y),o=as.numeric(o))

#Variables to store returner info
returner_x<-0
returner_y<-0

#Calculate the euclidean distance for each player to the returner at the time of the punt being caught
for (i in 1:nrow(punts_18_19)){
  ifelse (punts_18_19$is_not_rtr[i]==0,{assign("returner_x",punts_18_19$x[i],envir = .GlobalEnv); assign("returner_y",punts_18_19$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_18_19$x[i],punts_18_19$y[i]); punts_18_19$euc_dis_from_rtr[i] <- euclidean(returner,player)})
}

#Repeat for 2020
for (i in 1:nrow(punts_20)){
  ifelse (punts_20$is_not_rtr[i]==0,{assign("returner_x",punts_20$x[i],envir = .GlobalEnv); assign("returner_y",punts_20$y[i],envir = .GlobalEnv)}, 
          {returner<-c(returner_x,returner_y);player<-c(punts_20$x[i],punts_20$y[i]); punts_20$euc_dis_from_rtr[i] <- euclidean(returner,player)})
}

# Create a second game_play variable and lag it for comparison
punts_18_19<-punts_18_19%>%mutate(game_play_2=game_play)
punts_18_19<-punts_18_19%>%mutate(game_play_2=lag(game_play_2,default = first(game_play)))
punts_20<-punts_20%>%mutate(game_play_2=game_play)
punts_20<-punts_20%>%mutate(game_play_2=lag(game_play_2,default = first(game_play)))

#Create variables for the 3 closest return and coverage players (Offensive is the returning team here)
punts_18_19<-punts_18_19%>%mutate(def_1=0,def_2=0,def_3=0,off_1=0,off_2=0,off_3=0)
punts_20<-punts_20%>%mutate(def_1=0,def_2=0,def_3=0,off_1=0,off_2=0,off_3=0)

#Empty arrays to store distances
away_players<-array(0,11)
home_players<-array(0,11)

#Assign the created variables distances for those players 
for (i in 1:nrow(punts_18_19)){
  if (punts_18_19$game_play[i]==punts_18_19$game_play_2[i]){
    ifelse(punts_18_19$team[i] == "away",
           home_players[i]<-punts_18_19$euc_dis_from_rtr[i],
           away_players[i]<-punts_18_19$euc_dis_from_rtr[i])
  } else{
    home_players<-sort(home_players)
    away_players<-sort(away_players)
    home_players<-home_players[!home_players %in% c(0.0)]
    away_players<-away_players[!away_players %in% c(0.0)]
    if (punts_18_19$return_team[i] == "home"){
      punts_18_19$off_1[i-1]<-home_players[1]
      punts_18_19$off_2[i-1]<-home_players[2]
      punts_18_19$off_3[i-1]<-home_players[3]
      punts_18_19$def_1[i-1]<-away_players[1]
      punts_18_19$def_2[i-1]<-away_players[2]
      punts_18_19$def_3[i-1]<-away_players[3]
    }else{
      punts_18_19$def_1[i-1]<-home_players[1]
      punts_18_19$def_2[i-1]<-home_players[2]
      punts_18_19$def_3[i-1]<-home_players[3]
      punts_18_19$off_1[i-1]<-away_players[1]
      punts_18_19$off_2[i-1]<-away_players[2]
      punts_18_19$off_3[i-1]<-away_players[3]
    }
    home_players<-array(0,11)
    away_players<-array(0,11)
  }
}

##Repeat for 2020

#Empty arrays to store distances
away_players<-array(0,11)
home_players<-array(0,11)

#Assign the created variables distances for those players 
for (i in 1:nrow(punts_20)){
  if (punts_20$game_play[i]==punts_20$game_play_2[i]){
    ifelse(punts_20$team[i] == "home",
           home_players[i]<-punts_20$euc_dis_from_rtr[i],
           away_players[i]<-punts_20$euc_dis_from_rtr[i])
  } else{
    home_players<-sort(home_players)
    away_players<-sort(away_players)
    home_players<-home_players[!home_players %in% c(0.0)]
    away_players<-away_players[!away_players %in% c(0.0)]
    if (punts_20$return_team[i] == "home"){
      punts_20$off_1[i-1]<-home_players[1]
      punts_20$off_2[i-1]<-home_players[2]
      punts_20$off_3[i-1]<-home_players[3]
      punts_20$def_1[i-1]<-away_players[1]
      punts_20$def_2[i-1]<-away_players[2]
      punts_20$def_3[i-1]<-away_players[3]
    }else{
      punts_20$def_1[i-1]<-home_players[1]
      punts_20$def_2[i-1]<-home_players[2]
      punts_20$def_3[i-1]<-home_players[3]
      punts_20$off_1[i-1]<-away_players[1]
      punts_20$off_2[i-1]<-away_players[2]
      punts_20$off_3[i-1]<-away_players[3]
    }
    home_players<-array(0,11)
    away_players<-array(0,11)
  }
}

#Create the distance to sideline and goaline variables
dist_sdline<-0
punts_18_19<-punts_18_19%>%mutate(dist_sdl=0,dist_goall=0)

for (i in 1:nrow(punts_18_19)){
  if (punts_18_19$return_team[i]=="Returner"){
    ifelse(punts_18_19$y[i]<=26.7,dist_sdline<-punts_18_19$y[i],dist_sdline<-(53.3-punts_18_19$y[i]))
  }else{
    punts_18_19$dist_sdl[i]<-dist_sdline
  }
}

dist_goal_line<-0

for (i in 1:nrow(punts_18_19)){
  if (punts_18_19$return_team[i]=="Returner"){
    ifelse(punts_18_19$x[i]<=60,dist_goal_line<-(punts_18_19$x[i]-10),dist_goal_line<-(110-punts_18_19$x[i]))
  }else{
    punts_18_19$dist_goall[i]<-dist_goal_line
  }
}

# 2020
dist_sdline<-0
punts_20<-punts_20%>%mutate(dist_sdl=0,dist_goall=0)

for (i in 1:nrow(punts_20)){
  if (punts_20$return_team[i]=="Returner"){
    ifelse(punts_20$y[i]<=26.7,dist_sdline<-punts_20$y[i],dist_sdline<-(53.3-punts_20$y[i]))
  }else{
    punts_20$dist_sdl[i]<-dist_sdline
  }
}

dist_goal_line<-0

for (i in 1:nrow(punts_20)){
  if (punts_20$return_team[i]=="Returner"){
    ifelse(punts_20$x[i]<=60,dist_goal_line<-(punts_20$x[i]-10),dist_goal_line<-(110-punts_20$x[i]))
  }else{
    punts_20$dist_goall[i]<-dist_goal_line
  }
}

# Creating the def blocked variables
punts_18_19<-punts_18_19%>%mutate(def_1_blkd=0,def_2_blkd=0,def_3_blkd=0)
punts_20<-punts_20%>%mutate(def_1_blkd=0,def_2_blkd=0,def_3_blkd=0)

def_1_array<-array(100,4)
def_2_array<-array(100,4)
def_3_array<-array(100,4)
off_1_array<-array(100,4)
off_2_array<-array(100,4)
off_3_array<-array(100,4)
returner_position<-0
play_direction<-""
x<-0

#Function that calculates the euclidean distance between 2 players
dist_btw_players<-function(array1,array2){
  blocker<-c(array1[2],array1[3])
  tackler<-c(array2[2],array2[3])
  euclidean(blocker,tackler)
}

#Function that calculates the defensive player's angle in comparison to the returner
calc_def_angle<-function(array1,array2){
  x_dist<-array2[2]-array1[2]
  y_dist<-array2[3]-array1[3]
  angle_cos<-acos(x_dist/dist_btw_players(array1,array2))
  angle_sin<-asin(x_dist/dist_btw_players(array1,array2))
  if (x_dist >= 0 & y_dist >= 0){
    angle_cos
  }
  else if (x_dist >= 0 & y_dist <= 0){
    angle_sin
  }
  else if (x_dist <= 0 & y_dist >= 0){
    angle_sin
  }
  else if (x_dist <= 0 & y_dist <= 0){
    angle_cos
  }
}

#Function to figure out whether the defensive player's angle is within the field of view of the offensive blocker
check_angle<-function(angle,array1){
  if (array1[4]<60){
    top_range<-array1[4]+60
    x<-60-array1[4]
    bottom_range<-360-x
    if(angle <= top_range & angle >= 0 | angle >= bottom_range){
      1
    }
    else{
      0
    }
  }
  else if(array1[4]>300){
    x<-360-array1[4]
    top_range<-x
    bottom_range<-array1[4]-60
    if (angle >= bottom_range | angle <= top_range){
      1
    }
    else{
      0
    }
  }
  else{
    top_range<-array1[4]+60
    bottom_range<-array1[4]-60
    if (angle <= top_range & angle >= bottom_range){
      1
    }
    else{
      0
    }
  }
}


#Calculates if any of the 3 closest defenders are actively being blocked
for (i in 1:nrow(punts_18_19)){
  if (punts_18_19$return_team[i]=="home"){
    if (punts_18_19$team[i]=="home"){
      if (punts_18_19$euc_dis_from_rtr[i]<off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array<-off_1_array
        off_1_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_1_array[2]<-punts_18_19$x[i]
        off_1_array[3]<-punts_18_19$y[i]
        off_1_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<off_2_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_2_array[2]<-punts_18_19$x[i]
        off_2_array[3]<-punts_18_19$y[i]
        off_2_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<off_3_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_1_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_2_array[1]){
        off_3_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_3_array[2]<-punts_18_19$x[i]
        off_3_array[3]<-punts_18_19$y[i]
        off_3_array[4]<-punts_18_19$o[i]
      }
    }
    else{
      if (punts_18_19$euc_dis_from_rtr[i]<def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array<-def_3_array
        def_1_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_1_array[2]<-punts_18_19$x[i]
        def_1_array[3]<-punts_18_19$y[i]
        def_1_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<def_2_array[1] & punts_18_19$euc_dis_from_rtr[i] > def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_2_array[2]<-punts_18_19$x[i]
        def_2_array[3]<-punts_18_19$y[i]
        def_2_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<def_3_array[1] & punts_18_19$euc_dis_from_rtr[i] > def_1_array[1] & punts_18_19$euc_dis_from_rtr[i] > def_2_array[1]){
        def_3_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_3_array[2]<-punts_18_19$x[i]
        def_3_array[3]<-punts_18_19$y[i]
        def_3_array[4]<-punts_18_19$o[i]
      }
    }
  }
  else if (punts_18_19$return_team[i]=="away"){
    if (punts_18_19$team[i]=="away"){
      if (punts_18_19$euc_dis_from_rtr[i]<off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array<-off_1_array
        off_1_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_1_array[2]<-punts_18_19$x[i]
        off_1_array[3]<-punts_18_19$y[i]
        off_1_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<off_2_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_2_array[2]<-punts_18_19$x[i]
        off_2_array[3]<-punts_18_19$y[i]
        off_2_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<off_3_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_1_array[1] & punts_18_19$euc_dis_from_rtr[i] > off_2_array[1]){
        off_3_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        off_3_array[2]<-punts_18_19$x[i]
        off_3_array[3]<-punts_18_19$y[i]
        off_3_array[4]<-punts_18_19$o[i]
      }
    }
    else{
      if (punts_18_19$euc_dis_from_rtr[i]<def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array<-def_1_array
        def_1_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_1_array[2]<-punts_18_19$x[i]
        def_1_array[3]<-punts_18_19$y[i]
        def_1_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<def_2_array[1] & punts_18_19$euc_dis_from_rtr[i]>def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_2_array[2]<-punts_18_19$x[i]
        def_2_array[3]<-punts_18_19$y[i]
        def_2_array[4]<-punts_18_19$o[i]
      }
      else if (punts_18_19$euc_dis_from_rtr[i]<def_3_array[1] & punts_18_19$euc_dis_from_rtr[i] > def_1_array[1] & punts_18_19$euc_dis_from_rtr[i] > def_2_array[1]){
        def_3_array[1]<-punts_18_19$euc_dis_from_rtr[i]
        def_3_array[2]<-punts_18_19$x[i]
        def_3_array[3]<-punts_18_19$y[i]
        def_3_array[4]<-punts_18_19$o[i]
      }
    }
  }
  else{
    if (play_direction=="left"){
      if (off_1_array[2]>returner_position){
        if (dist_btw_players(off_1_array,def_1_array)<2){
          angle<-calc_def_angle(off_1_array,def_1_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_2_array)<2){
          angle<-calc_def_angle(off_1_array,def_2_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_2_blkd[i-1]<-1,punts_18_19$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_3_array)<2){
          angle<-calc_def_angle(off_1_array,def_3_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_3_blkd[i-1]<-1,punts_18_19$def_3_blkd[i-1]<-0)
        }
      }
      if (off_2_array[2]>returner_position){
        if (dist_btw_players(off_2_array,def_1_array)<2){
          angle<-calc_def_angle(off_2_array,def_1_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_2_array)<2){
          angle<-calc_def_angle(off_2_array,def_2_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_2_blkd[i-1]<-1,punts_18_19$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_3_array)<2){
          angle<-calc_def_angle(off_2_array,def_3_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_3_blkd[i-1]<-1,punts_18_19$def_3_blkd[i-1]<-0)
        }
      }
      if (off_3_array[3]>returner_position){
        if (dist_btw_players(off_3_array,def_1_array)<2){
          angle<-calc_def_angle(off_3_array,def_1_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_3_array,def_2_array)<2){
          angle<-calc_def_angle(off_3_array,def_2_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_2_blkd[i-1]<-1,punts_18_19$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_3_array,def_3_array)<2){
          angle<-calc_def_angle(off_3_array,def_3_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_3_blkd[i-1]<-1,punts_18_19$def_3_blkd[i-1]<-0)
        }
      }
    } else{
      if (off_1_array[2]<returner_position){
        if (dist_btw_players(off_1_array,def_1_array)<2){
          angle<-calc_def_angle(off_1_array,def_1_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_2_array)<2){
          angle<-calc_def_angle(off_1_array,def_2_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_3_array)<2){
          angle<-calc_def_angle(off_1_array,def_3_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
      }
      if (off_2_array[2]<returner_position){
        if (dist_btw_players(off_2_array,def_1_array)<2){
          angle<-calc_def_angle(off_2_array,def_1_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_2_array)<2){
          angle<-calc_def_angle(off_2_array,def_2_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_2_blkd[i-1]<-1,punts_18_19$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_3_array)<2){
          angle<-calc_def_angle(off_2_array,def_3_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_18_19$def_3_blkd[i-1]<-1,punts_18_19$def_3_blkd[i-1]<-0)
        }
      }
      if (off_3_array[2]<returner_position){
        if (off_3_array[3]>returner_position){
          if (dist_btw_players(off_3_array,def_1_array)<2){
            angle<-calc_def_angle(off_3_array,def_1_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_1_blkd[i-1]<-1,punts_18_19$def_1_blkd[i-1]<-0)
          }
          else if (dist_btw_players(off_3_array,def_2_array)<2){
            angle<-calc_def_angle(off_3_array,def_2_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_2_blkd[i-1]<-1,punts_18_19$def_2_blkd[i-1]<-0)
          }
          else if (dist_btw_players(off_3_array,def_3_array)<2){
            angle<-calc_def_angle(off_3_array,def_3_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_18_19$def_3_blkd[i-1]<-1,punts_18_19$def_3_blkd[i-1]<-0)
          }
        }
      }
    }
    returner_position<-punts_18_19$x[i]
    play_direction<-punts_18_19$playDirection[i]
    def_1_array<-array(100,4)
    def_2_array<-array(100,4)
    def_3_array<-array(100,4)
    off_1_array<-array(100,4)
    off_2_array<-array(100,4)
    off_3_array<-array(100,4)
  } 
}

##Repeat for 2020

def_1_array<-array(100,4)
def_2_array<-array(100,4)
def_3_array<-array(100,4)
off_1_array<-array(100,4)
off_2_array<-array(100,4)
off_3_array<-array(100,4)
returner_position<-0
play_direction<-""
x<-0

for (i in 1:nrow(punts_20)){
  if (punts_20$return_team[i]=="home"){
    if (punts_20$team[i]=="home"){
      if (punts_20$euc_dis_from_rtr[i]<off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array<-off_1_array
        off_1_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_1_array[2]<-punts_20$x[i]
        off_1_array[3]<-punts_20$y[i]
        off_1_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<off_2_array[1] & punts_20$euc_dis_from_rtr[i] > off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_2_array[2]<-punts_20$x[i]
        off_2_array[3]<-punts_20$y[i]
        off_2_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<off_3_array[1] & punts_20$euc_dis_from_rtr[i] > off_1_array[1] & punts_20$euc_dis_from_rtr[i] > off_2_array[1]){
        off_3_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_3_array[2]<-punts_20$x[i]
        off_3_array[3]<-punts_20$y[i]
        off_3_array[4]<-punts_20$o[i]
      }
    }
    else{
      if (punts_20$euc_dis_from_rtr[i]<def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array<-def_3_array
        def_1_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_1_array[2]<-punts_20$x[i]
        def_1_array[3]<-punts_20$y[i]
        def_1_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<def_2_array[1] & punts_20$euc_dis_from_rtr[i] > def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_2_array[2]<-punts_20$x[i]
        def_2_array[3]<-punts_20$y[i]
        def_2_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<def_3_array[1] & punts_20$euc_dis_from_rtr[i] > def_1_array[1] & punts_20$euc_dis_from_rtr[i] > def_2_array[1]){
        def_3_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_3_array[2]<-punts_20$x[i]
        def_3_array[3]<-punts_20$y[i]
        def_3_array[4]<-punts_20$o[i]
      }
    }
  }
  else if (punts_20$return_team[i]=="away"){
    if (punts_20$team[i]=="away"){
      if (punts_20$euc_dis_from_rtr[i]<off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array<-off_1_array
        off_1_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_1_array[2]<-punts_20$x[i]
        off_1_array[3]<-punts_20$y[i]
        off_1_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<off_2_array[1] & punts_20$euc_dis_from_rtr[i] > off_1_array[1]){
        off_3_array<-off_2_array
        off_2_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_2_array[2]<-punts_20$x[i]
        off_2_array[3]<-punts_20$y[i]
        off_2_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<off_3_array[1] & punts_20$euc_dis_from_rtr[i] > off_1_array[1] & punts_20$euc_dis_from_rtr[i] > off_2_array[1]){
        off_3_array[1]<-punts_20$euc_dis_from_rtr[i]
        off_3_array[2]<-punts_20$x[i]
        off_3_array[3]<-punts_20$y[i]
        off_3_array[4]<-punts_20$o[i]
      }
    }
    else{
      if (punts_20$euc_dis_from_rtr[i]<def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array<-def_1_array
        def_1_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_1_array[2]<-punts_20$x[i]
        def_1_array[3]<-punts_20$y[i]
        def_1_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<def_2_array[1] & punts_20$euc_dis_from_rtr[i]>def_1_array[1]){
        def_3_array<-def_2_array
        def_2_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_2_array[2]<-punts_20$x[i]
        def_2_array[3]<-punts_20$y[i]
        def_2_array[4]<-punts_20$o[i]
      }
      else if (punts_20$euc_dis_from_rtr[i]<def_3_array[1] & punts_20$euc_dis_from_rtr[i] > def_1_array[1] & punts_20$euc_dis_from_rtr[i] > def_2_array[1]){
        def_3_array[1]<-punts_20$euc_dis_from_rtr[i]
        def_3_array[2]<-punts_20$x[i]
        def_3_array[3]<-punts_20$y[i]
        def_3_array[4]<-punts_20$o[i]
      }
    }
  }
  else{
    if (play_direction=="left"){
      if (off_1_array[2]>returner_position){
        if (dist_btw_players(off_1_array,def_1_array)<2){
          angle<-calc_def_angle(off_1_array,def_1_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_2_array)<2){
          angle<-calc_def_angle(off_1_array,def_2_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_2_blkd[i-1]<-1,punts_20$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_3_array)<2){
          angle<-calc_def_angle(off_1_array,def_3_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_3_blkd[i-1]<-1,punts_20$def_3_blkd[i-1]<-0)
        }
      }
      if (off_2_array[2]>returner_position){
        if (dist_btw_players(off_2_array,def_1_array)<2){
          angle<-calc_def_angle(off_2_array,def_1_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_2_array)<2){
          angle<-calc_def_angle(off_2_array,def_2_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_2_blkd[i-1]<-1,punts_20$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_3_array)<2){
          angle<-calc_def_angle(off_2_array,def_3_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_3_blkd[i-1]<-1,punts_20$def_3_blkd[i-1]<-0)
        }
      }
      if (off_3_array[3]>returner_position){
        if (dist_btw_players(off_3_array,def_1_array)<2){
          angle<-calc_def_angle(off_3_array,def_1_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_3_array,def_2_array)<2){
          angle<-calc_def_angle(off_3_array,def_2_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_20$def_2_blkd[i-1]<-1,punts_20$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_3_array,def_3_array)<2){
          angle<-calc_def_angle(off_3_array,def_3_array)
          ifelse(check_angle(angle,off_3_array)==1,punts_20$def_3_blkd[i-1]<-1,punts_20$def_3_blkd[i-1]<-0)
        }
      }
    } else{
      if (off_1_array[2]<returner_position){
        if (dist_btw_players(off_1_array,def_1_array)<2){
          angle<-calc_def_angle(off_1_array,def_1_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_2_array)<2){
          angle<-calc_def_angle(off_1_array,def_2_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_1_array,def_3_array)<2){
          angle<-calc_def_angle(off_1_array,def_3_array)
          ifelse(check_angle(angle,off_1_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
      }
      if (off_2_array[2]<returner_position){
        if (dist_btw_players(off_2_array,def_1_array)<2){
          angle<-calc_def_angle(off_2_array,def_1_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_2_array)<2){
          angle<-calc_def_angle(off_2_array,def_2_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_2_blkd[i-1]<-1,punts_20$def_2_blkd[i-1]<-0)
        }
        else if (dist_btw_players(off_2_array,def_3_array)<2){
          angle<-calc_def_angle(off_2_array,def_3_array)
          ifelse(check_angle(angle,off_2_array)==1,punts_20$def_3_blkd[i-1]<-1,punts_20$def_3_blkd[i-1]<-0)
        }
      }
      if (off_3_array[2]<returner_position){
        if (off_3_array[3]>returner_position){
          if (dist_btw_players(off_3_array,def_1_array)<2){
            angle<-calc_def_angle(off_3_array,def_1_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_20$def_1_blkd[i-1]<-1,punts_20$def_1_blkd[i-1]<-0)
          }
          else if (dist_btw_players(off_3_array,def_2_array)<2){
            angle<-calc_def_angle(off_3_array,def_2_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_20$def_2_blkd[i-1]<-1,punts_20$def_2_blkd[i-1]<-0)
          }
          else if (dist_btw_players(off_3_array,def_3_array)<2){
            angle<-calc_def_angle(off_3_array,def_3_array)
            ifelse(check_angle(angle,off_3_array)==1,punts_20$def_3_blkd[i-1]<-1,punts_20$def_3_blkd[i-1]<-0)
          }
        }
      }
    }
    returner_position<-punts_20$x[i]
    play_direction<-punts_20$playDirection[i]
    def_1_array<-array(100,4)
    def_2_array<-array(100,4)
    def_3_array<-array(100,4)
    off_1_array<-array(100,4)
    off_2_array<-array(100,4)
    off_3_array<-array(100,4)
  } 
}

str(punts_18_19)

punts_18_19<-punts_18_19%>%mutate(kickReturnYardage=as.numeric(kickReturnYardage),kickLength=as.numeric(kickLength),hangTime=as.numeric(hangTime),operationTime=as.numeric(operationTime))
punts_20<-punts_20%>%mutate(kickReturnYardage=as.numeric(kickReturnYardage),kickLength=as.numeric(kickLength),hangTime=as.numeric(hangTime),operationTime=as.numeric(operationTime))


train<-punts_18_19%>%select(season,game_play,kickReturnYardage,hangTime,operationTime,returnerId,kickLength,
                            def_1,def_2,def_3,off_1,off_2,off_3,dist_sdl,dist_goall,def_1_blkd,def_2_blkd,def_3_blkd)%>%filter(def_1>0)%>%na.omit()
test<-punts_20%>%select(season,game_play,kickReturnYardage,hangTime,operationTime,returnerId,kickLength,
                            def_1,def_2,def_3,off_1,off_2,off_3,dist_sdl,dist_goall,def_1_blkd,def_2_blkd,def_3_blkd)%>%filter(def_1>0)%>%na.omit()

test_count<-test%>%group_by(returnerId)%>%summarise(n=n())
test<-test%>%left_join(test_count)
test_low_attempts<-test%>%filter(n<5)%>%select(-n)
test<-test%>%filter(n>=5)%>%filter(kickReturnYardage<33)
train<-train%>%full_join(test_low_attempts)%>%filter(kickReturnYardage<33)

#OLS Regression
result<-lm(kickReturnYardage~kickLength+operationTime+hangTime+def_1+def_2+def_3+off_1+off_2+off_3+dist_sdl+dist_goall+def_1_blkd+def_2_blkd+def_3_blkd,data = train)
result<-lm(kickReturnYardage~kickLength+hangTime+def_1+def_3+dist_goall,data = train)
summary(result)
ols.predict<-predict(result,newdata = test)

test.predict<-test$kickReturnYardage

test_mse_ols<-mean((test.predict-ols.predict)^2)
test_mse_ols

# Recursive Binary Splitting
tree.reg.train<-tree::tree(kickReturnYardage~kickLength+operationTime+hangTime+def_1+def_2+def_3+off_1+off_2+off_3+dist_sdl+dist_goall+def_1_blkd+def_2_blkd+def_3_blkd,data = train)
summary(tree.reg.train)

plot(tree.reg.train)
text(tree.reg.train)

tree.pred<-predict(tree.reg.train, newdata=test)

test_mse_RBS<-mean((test$kickReturnYardage-tree.pred)^2)
test_mse_RBS

#Random Forest
rf.class<-randomForest::randomForest(kickReturnYardage~kickLength+operationTime+hangTime+def_1+def_2+def_3+off_1+off_2+off_3+dist_sdl+dist_goall+def_1_blkd+def_2_blkd+def_3_blkd, data=train, mtry=3,importance=TRUE)
rf.class

pred.rf<-predict(rf.class, newdata=test)

test_mse_RF<-mean((test$kickReturnYardage-pred.rf)^2)
test_mse_RF

importance(rf.class)
varImpPlot(rf.class)

#Bagging
bag.class<-randomForest::randomForest(kickReturnYardage~kickLength+operationTime+hangTime+def_1+def_2+def_3+off_1+off_2+off_3+dist_sdl+dist_goall+def_1_blkd+def_2_blkd+def_3_blkd, data=train, mtry=14,importance=TRUE)
bag.class

pred.bag<-predict(bag.class,newdata = test)
test_mse_bag<-mean((test$kickReturnYardage-pred.bag)^2)
test_mse_bag

#Boosting
boost.reg<-gbm::gbm(kickReturnYardage~kickLength+operationTime+hangTime+def_1+def_2+def_3+off_1+off_2+off_3+dist_sdl+dist_goall+def_1_blkd+def_2_blkd+def_3_blkd, data=train, n.trees = 500)
summary(boost.reg)

pred.boost<-predict(boost.reg,newdata = test)

test_mse_boost<-mean((test$kickReturnYardage-pred.boost)^2)
test_mse_boost

## Analysis using Random Forest Predictions
test$prediction<-pred.rf

test_prediction<-test%>%group_by(returnerId)%>%summarise(avg=mean(kickReturnYardage-prediction))%>%left_join(test_count)%>%mutate(nflId=as.numeric(returnerId))%>%left_join(players)

renfroe<-punts%>%filter(gameId==2020091303 & playId == 2143)

test_prediction_filter<-test_prediction%>%select(displayName,n,avg)

colnames(test_prediction_filter)<-c("Name","Attempts","Average of Actual vs. Predicted")


