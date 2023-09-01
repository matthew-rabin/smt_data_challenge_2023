library(tidyverse)
library(dplyr)
library(readr)

#---------------------------game event data prep---------------------------------

# limit game events data for only plays acquired by first baseman, second baseman, third baseman, shortstop or OF

#combine csv files
setwd("~/Portfolio/SMT Data Challenge/SMT-Data-Challenge/smt_data_challenge_2023/game_events")
file_names = dir()
game_events_data = do.call(rbind,lapply(file_names,read.csv))

# add identifier for game and play
game_play_id = paste(game_events_data$game_str,game_events_data$play_id, sep="_")
game_events_data = cbind(game_events_data, game_play_id)

#timestamp mapping for first fielder to acquire, first bounce, last pitch per play

  #first fielder to acquire
      ball_acquired_events = subset(game_events_data, event_code == 2)
          #event_code == 2 definition: ball acquired
      first_ball_acquired_event = ball_acquired_events %>% group_by(game_play_id) %>% top_n(-1,timestamp)
          #group by MIN timestamp of acquired ball per play
      first_ball_acquired_event = data.frame(
        game_play_id = first_ball_acquired_event$game_play_id,
        first_acquired_timestamp = first_ball_acquired_event$timestamp,
        acquired_by = first_ball_acquired_event$player_position
                                            )
      #limit to acquired by first baseman, second baseman, third baseman, shortstop or OF
      first_ball_acquired_event = subset(first_ball_acquired_event, acquired_by > 2 & acquired_by < 10)

  #first bounce
    ball_bounce_events = subset(game_events_data, event_code == 16)
        #event_code == 16 definition: ball bounce
    first_ball_bounce_event = ball_bounce_events %>% group_by(game_play_id) %>% top_n(-1,timestamp)
        #group by MIN timestamp of bounce per play
    first_ball_bounce_event = data.frame(
        game_play_id = first_ball_bounce_event$game_play_id,
        first_bounce_timestamp = first_ball_bounce_event$timestamp                           
                                         )
    first_bounce_id = paste(first_ball_bounce_event$game_play_id,first_ball_bounce_event$first_bounce_timestamp, sep="_")
        #this ID will be used later for ball location coordinates
    
  #last pitch
    pitch_events = subset(game_events_data, event_code == 1)
    #event_code == 1 definition: pitch
    last_pitch_event = pitch_events %>% group_by(game_play_id) %>% top_n(1,timestamp)
    #group by MAX timestamp of pitch per play
    last_pitch_id = paste(last_pitch_event$game_play_id,last_pitch_event$timestamp, sep="_")
        #this ID will be used later for player location coordinates

#Compare first_ball_bounce and first_ball_acquired timestamps to eliminate fly ball outs
bounced_before_acquired = subset((merge(x=first_ball_acquired_event,y=first_ball_bounce_event,by='game_play_id')),
                                 first_acquired_timestamp > first_bounce_timestamp)
acquiring_fielder = data.frame(
  game_play_id = bounced_before_acquired$game_play_id,
  acquired_by = ifelse (bounced_before_acquired$acquired_by < 7, "IF", "OF")
                                )

#remove dataframes that are no longer needed
rm("ball_acquired_events", "ball_bounce_events", "bounced_before_acquired", "first_ball_acquired_event",
   "first_ball_bounce_event","last_pitch_event","game_events_data", "pitch_events")


#---------------------------player position data prep---------------------------------

#combine csv files
setwd("~/Portfolio/SMT Data Challenge/SMT-Data-Challenge/smt_data_challenge_2023/player_pos_consolidated")
file_names = dir()
player_pos_data = do.call(rbind,lapply(file_names,read.csv))

# add identifier for game and play
game_play_id = paste(player_pos_data$game_str,player_pos_data$play_id, sep="_")
player_pos_data = cbind(player_pos_data, game_play_id)

# limit player position results based on game event data / add acquiring_fielder column from game event data
# limit player position data to first baseman, second baseman, third baseman, shortstop and batter
player_pos_data = subset(merge(x=player_pos_data,y=acquiring_fielder,by='game_play_id'),
                         (player_position < 7 & player_position > 2) | player_position == 10
                        )

# limit player position data to timestamp of last pitch per play
player_pos_data = subset(player_pos_data, paste(player_pos_data$game_play_id,player_pos_data$timestamp, sep="_") %in% last_pitch_id)

#identify left/right of second base
player_pos_data = data.frame(
  game_str = player_pos_data$game_str,
  play_id = player_pos_data$play_id,
  game_play_id = player_pos_data$game_play_id,
  timestamp = player_pos_data$timestamp,
  player_position = player_pos_data$player_position,
  field_x = player_pos_data$field_x,
  field_y = player_pos_data$field_y,
  RO2B = ifelse (player_pos_data$field_x>0, 1, 0),
  LO2B = ifelse (player_pos_data$field_x<0, 1, 0)
)

#limit to fielders only to identify shift
fielder_pos_data = subset(player_pos_data, player_position != 10)

#aggregate with sum of infielders left/right of second base per play
fielder_pos_data = fielder_pos_data %>% group_by(game_str,play_id,game_play_id, timestamp) %>%
  summarize(LO2B = sum(LO2B), RO2B = sum(RO2B),
            .groups = 'drop')

#identify plays with shifts
shift = subset(fielder_pos_data, LO2B > 2 | RO2B > 2)
shift_id = paste(shift$game_play_id)
  #this ID will be used later to flag shift in aggregated data

#identify plays with left handed batters
left_handed_batters = subset(player_pos_data,
                             player_position == 10 & field_x > 0)
left_handed_batter_id =  paste(left_handed_batters$game_play_id)                            
  #this ID will be used later to identify handedness in aggregated data

#remove batters from player_pos_data - this will be a filter in visualization but not a plot in the graph.
player_pos_data = subset(player_pos_data, player_position !=10)

#format in new data frame - to be combined with ball position data.
player_pos_data = data.frame(
  game_str = player_pos_data$game_str,
  play_id = player_pos_data$play_id,
  game_play_id = player_pos_data$game_play_id,
  location_type = "Position Player",
  location_name = player_pos_data$player_position,
  x = player_pos_data$field_x,
  y = player_pos_data$field_y,
  shift = ifelse(player_pos_data$game_play_id %in% shift_id, 1, 0),
  batter_handedness = ifelse(player_pos_data$game_play_id %in% left_handed_batter_id, "Left Handed", "Right Handed")
)

#test average distances of middle infielders
#write.csv(player_pos_data,file='C:/Users/rabinm/Documents/Portfolio/SMT Data Challenge/SMT-Data-Challenge/smt_data_challenge_2023/output/player_pos_data_all.csv')


#remove dataframes that are no longer needed
rm("fielder_pos_data", "shift", "left_handed_batters")


#---------------------------ball position data prep---------------------------------

#combine csv files
setwd("~/Portfolio/SMT Data Challenge/SMT-Data-Challenge/smt_data_challenge_2023/ball_pos")
file_names = dir()
ball_pos_data = do.call(rbind,lapply(file_names,read.csv))

#limit to first_bounce identified from game event data prep
ball_pos_data = subset(ball_pos_data, paste(ball_pos_data$game_str,ball_pos_data$play_id,ball_pos_data$timestamp, sep="_") %in% first_bounce_id)

#limit to the angle identified as "up the middle" by average positioning of second baseman and shortstop on non-shifted plays.
ball_pos_data = subset(ball_pos_data,
                         (
                           (ball_pos_data$ball_position_y/ball_pos_data$ball_position_x) >= tan(1.33289)
                           | (ball_pos_data$ball_position_y/ball_pos_data$ball_position_x) <= -tan(1.33289)
                         )
                         & ball_pos_data$ball_position_y > 0
                      )

#format in new data frame - to be combined with player position data.
ball_pos_data = data.frame(
  game_str = ball_pos_data$game_str,
  play_id = ball_pos_data$play_id,
  game_play_id = paste(ball_pos_data$game_str,ball_pos_data$play_id, sep="_"),
  location_type = "Baseball",
  location_name = 0,
  x = ball_pos_data$ball_position_x,
  y = ball_pos_data$ball_position_y,
  shift = ifelse(paste(ball_pos_data$game_str,ball_pos_data$play_id, sep="_") %in% shift_id, 1, 0),
  batter_handedness = ifelse(paste(ball_pos_data$game_str,ball_pos_data$play_id, sep="_") %in% left_handed_batter_id, "Left Handed", "Right Handed")
)

#remove player position data where the ball was not up the middle
player_pos_data = subset(player_pos_data, game_play_id %in% ball_pos_data$game_play_id)

#combine player position and ball position data
output_data = rbind(ball_pos_data, player_pos_data)

#add Acquiring fielder column
output_data = merge(x=output_data,y=acquiring_fielder,by='game_play_id')

#remove dataframes that are no longer needed
rm("acquiring_fielder", "ball_pos_data", "player_pos_data")

#write to CSV for visualization in tableau
write.csv(output_data,file='C:/Users/rabinm/Documents/Portfolio/SMT Data Challenge/SMT-Data-Challenge/smt_data_challenge_2023/output/output.csv')
