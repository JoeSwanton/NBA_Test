library(tidyverse)
library(zoo)
library(randomForest)
library(BasketballAnalyzeR)
library(hoopR)
library(nbastatR)
library(caret)
library(shiny)
library(bs4Dash)
library(ggplot2)
library(dplyr)
library(grid)
library(png)
library(httr)
library(jsonlite)
library(RSelenium)

##### Gathering the data ####
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)


game_data <- game_logs(seasons = 2025, result_types = "team")

player <- game_logs(seasons = 2025, result_types = "player")

player <- player[-c(1:7, 9:21)]


gmae <- nbastatR::bref_teams_stats(seasons = 2025)

game_data <- game_data %>%
  mutate(nameTeam = ifelse(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam))

##### Totals for PIE ####
team_data <- game_data %>%
  group_by(idGame) %>%
  summarize(
    team_pts = sum(ptsTeam, na.rm = TRUE),
    team_fgm = sum(fgmTeam, na.rm = TRUE),
    team_ftm = sum(ftmTeam, na.rm = TRUE),
    team_fga = sum(fgaTeam, na.rm = TRUE),
    team_fta = sum(ftaTeam, na.rm = TRUE),
    team_dreb = sum(drebTeam, na.rm = TRUE),
    team_oreb = sum(orebTeam, na.rm = TRUE),
    team_ast = sum(astTeam, na.rm = TRUE),
    team_stl = sum(stlTeam, na.rm = TRUE),
    team_blk = sum(blkTeam, na.rm = TRUE),
    team_pf = sum(pfTeam, na.rm = TRUE),
    team_tov = sum(tovTeam, na.rm = TRUE)
  )


game_stats <- team_data %>%
  group_by(idGame) %>%
  summarize(
    game_pts = sum(team_pts, na.rm = TRUE),
    game_fgm = sum(team_fgm, na.rm = TRUE),
    game_ftm = sum(team_ftm, na.rm = TRUE),
    game_fga = sum(team_fga, na.rm = TRUE),
    game_fta = sum(team_fta, na.rm = TRUE),
    game_dreb = sum(team_dreb, na.rm = TRUE),
    game_oreb = sum(team_oreb, na.rm = TRUE),
    game_ast = sum(team_ast, na.rm = TRUE),
    game_stl = sum(team_stl, na.rm = TRUE),
    game_blk = sum(team_blk, na.rm = TRUE),
    game_pf = sum(team_pf, na.rm = TRUE),
    game_tov = sum(team_tov, na.rm = TRUE)
  )

game_data <- game_data %>% 
  left_join(game_stats, by = "idGame")


#### Possessions per game ####
game_data <- game_data %>% 
  group_by(nameTeam) %>% 
  mutate(Total_Points = sum(ptsTeam),
         Total_FGA = sum(fgaTeam),
         Total_FTA = sum(ftaTeam),
         True_Shoot_Pct = Total_Points / (2 * (Total_FGA + 0.44 * Total_FTA)))



game_data <- game_data %>% 
  left_join(dataBREFPerGameTeams %>% select(nameTeam, drbPerGameOpponent, orbPerGameOpponent, 
                                            fgaPerGameOpponent, fgmPerGameOpponent, tovPerGameOpponent,
                                            fgaPerGameTeam, ftaPerGameTeam, fgmPerGameTeam, 
                                            orbPerGameTeam, tovPerGameTeam, drbPerGameTeam, 
                                            ftaPerGameOpponent, ptsPerGameTeam, ptsPerGameOpponent), by = "nameTeam")

game_data <- game_data %>%
  mutate(
    Possessions_Per_Game = 0.5 * (
      (fgaPerGameTeam + 0.4 * ftaPerGameTeam 
       - 1.07 * (orbPerGameTeam / (orbPerGameTeam + drbPerGameOpponent)) * (fgaPerGameTeam - fgmPerGameTeam) 
       + tovPerGameTeam) +
        (fgaPerGameOpponent + 0.4 * ftaPerGameOpponent 
         - 1.07 * (orbPerGameOpponent / (orbPerGameOpponent + drbPerGameTeam)) * (fgaPerGameOpponent - fgmPerGameOpponent) 
         + tovPerGameOpponent)
    )
  )

game_data <- game_data %>% 
  group_by(nameTeam) %>% 
  mutate(Off_Eff = (ptsPerGameTeam / Possessions_Per_Game),
         Def_Eff = (ptsPerGameOpponent / Possessions_Per_Game))

game_data <- game_data %>% 
  group_by(nameTeam, )


game_data <- game_data %>% 
  group_by(nameTeam) %>% 
  mutate(Total_Wins = sum(isWin))




#### True Shooting ####
game_data <- game_data %>% 
  group_by(nameTeam) %>% 
  mutate(
    True_Shooting_Pctg = mean(True_Shoot_Pct, na.rm = TRUE)
  ) %>% 
  ungroup()

game_data_summary <- game_data %>%
  group_by(nameTeam) %>%
  summarize(
    OREB_pctg = mean((orebTeam / (orebTeam + drbPerGameOpponent)) * 100, na.rm = TRUE),
    DREB_pctg = mean((drebTeam / (drebTeam + orbPerGameOpponent)) * 100, na.rm = TRUE),
    TOV_pctg = mean(tovTeam / (fgaTeam + 0.44 * ftaTeam + tovTeam) * 100, na.rm = TRUE),
    Turnovers_Per_OP = mean(tovTeam / Possessions_Per_Game * 100, na.rm = TRUE)
  )


game_data_summary <- game_data %>%
  group_by(nameTeam) %>%
  summarize(
    OREB_pctg = mean((orebTeam / (orebTeam + drbPerGameOpponent)) * 100, na.rm = TRUE),
    DREB_pctg = mean((drebTeam / (drebTeam + orbPerGameOpponent)) * 100, na.rm = TRUE),
    TOV_pctg = mean((tovTeam / (fgaTeam + 0.44 * ftaTeam + tovTeam)) * 100, na.rm = TRUE),
    Turnovers_Per_OP = mean((tovTeam / Possessions_Per_Game) * 100, na.rm = TRUE)
  )

game_data <- game_data %>% 
  left_join(game_data_summary, by = "nameTeam")

##### Calculating Rolling Averages ####
game_data <- game_data %>% 
  arrange(nameTeam, dateGame) %>% 
  group_by(nameTeam) %>% 
  mutate(
    L5_OREB_pctg = rollapply(OREB_pctg, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_DREB_pctg = rollapply(DREB_pctg, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_True_Shooting_Pctg = rollapply(True_Shooting_Pctg, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_Off_Eff = rollapply(Off_Eff, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_Def_Eff = rollapply(Def_Eff, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE)) %>% 
  fill(L5_OREB_pctg, .direction = "down") %>% 
  ungroup()

#### Adding in player impact rating ####
game_data <- game_data %>% 
  left_join(player, by = "nameTeam")

game_data <- game_data %>%
  mutate(
    PIE = (pts + fgm + ftm - fga - fta + dreb + oreb / 2 + ast + stl + blk / 2 - pf - tov) / 
      (game_pts + game_fgm + game_ftm - game_fga - game_fta + game_dreb + game_oreb / 2 + game_ast + game_stl +
         game_blk / 2 - game_pf - game_tov) * 100 )


game_data <- game_data %>% 
  group_by(namePlayer) %>% 
  mutate(M_PIE = mean(PIE))

##### Gathering the data ####
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)


game_data2 <- game_logs(seasons = 2025, result_types = "team")

player2 <- game_logs(seasons = 2025, result_types = "player")

player2 <- player[-c(1:7, 9:21)]


gmae2 <- nbastatR::bref_teams_stats(seasons = 2025)

game_data2 <- game_data2 %>%
  mutate(nameTeam = ifelse(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam))


#### seperating into home and away ####
home <- game_data2 %>% 
  filter(locationGame == "H")

away <- game_data2 %>% 
  filter(locationGame == "A")
##### Totals  ####
home_totals <- home %>%
  group_by(idGame) %>%
  summarize(
    team_pts = sum(ptsTeam, na.rm = TRUE),
    team_fgm = sum(fgmTeam, na.rm = TRUE),
    team_ftm = sum(ftmTeam, na.rm = TRUE),
    team_fga = sum(fgaTeam, na.rm = TRUE),
    team_fta = sum(ftaTeam, na.rm = TRUE),
    team_dreb = sum(drebTeam, na.rm = TRUE),
    team_oreb = sum(orebTeam, na.rm = TRUE),
    team_ast = sum(astTeam, na.rm = TRUE),
    team_stl = sum(stlTeam, na.rm = TRUE),
    team_blk = sum(blkTeam, na.rm = TRUE),
    team_pf = sum(pfTeam, na.rm = TRUE),
    team_tov = sum(tovTeam, na.rm = TRUE)
  )

away_totals <- away %>%
  group_by(idGame) %>%
  summarize(
    team_pts = sum(ptsTeam, na.rm = TRUE),
    team_fgm = sum(fgmTeam, na.rm = TRUE),
    team_ftm = sum(ftmTeam, na.rm = TRUE),
    team_fga = sum(fgaTeam, na.rm = TRUE),
    team_fta = sum(ftaTeam, na.rm = TRUE),
    team_dreb = sum(drebTeam, na.rm = TRUE),
    team_oreb = sum(orebTeam, na.rm = TRUE),
    team_ast = sum(astTeam, na.rm = TRUE),
    team_stl = sum(stlTeam, na.rm = TRUE),
    team_blk = sum(blkTeam, na.rm = TRUE),
    team_pf = sum(pfTeam, na.rm = TRUE),
    team_tov = sum(tovTeam, na.rm = TRUE)
  )

#### home and away averages ####
home_stats <- home %>%
  group_by(nameTeam) %>%
  summarize(
    team_pts = mean(ptsTeam, na.rm = TRUE),
    team_fgm = mean(fgmTeam, na.rm = TRUE),
    team_ftm = mean(ftmTeam, na.rm = TRUE),
    team_fga = mean(fgaTeam, na.rm = TRUE),
    team_fta = mean(ftaTeam, na.rm = TRUE),
    team_dreb = mean(drebTeam, na.rm = TRUE),
    team_oreb = mean(orebTeam, na.rm = TRUE),
    team_ast = mean(astTeam, na.rm = TRUE),
    team_stl = mean(stlTeam, na.rm = TRUE),
    team_blk = mean(blkTeam, na.rm = TRUE),
    team_pf = mean(pfTeam, na.rm = TRUE),
    team_tov = mean(tovTeam, na.rm = TRUE)
  )

away_stats <- away %>%
  group_by(nameTeam) %>%
  summarize(
    team_pts = mean(ptsTeam, na.rm = TRUE),
    team_fgm = mean(fgmTeam, na.rm = TRUE),
    team_ftm = mean(ftmTeam, na.rm = TRUE),
    team_fga = mean(fgaTeam, na.rm = TRUE),
    team_fta = mean(ftaTeam, na.rm = TRUE),
    team_dreb = mean(drebTeam, na.rm = TRUE),
    team_oreb = mean(orebTeam, na.rm = TRUE),
    team_ast = mean(astTeam, na.rm = TRUE),
    team_stl = mean(stlTeam, na.rm = TRUE),
    team_blk = mean(blkTeam, na.rm = TRUE),
    team_pf = mean(pfTeam, na.rm = TRUE),
    team_tov = mean(tovTeam, na.rm = TRUE)
  )
##### Joining rolling averages ####

columns_to_join <- c("L5_OREB_pctg", "L5_DREB_pctg", "L5_True_Shooting_Pctg",
                     "L5_Off_Eff", "L5_Def_Eff")

# Select these columns from another dataframe (e.g., `source_df`)
columns_to_join <- game_data %>%
  group_by(nameTeam) %>%  # Group by team to ensure one row per team
  summarize(
    L5_OREB_pctg = mean(L5_OREB_pctg, na.rm = TRUE),
    L5_DREB_pctg = mean(L5_DREB_pctg, na.rm = TRUE),
    L5_True_Shooting_Pctg = mean(L5_True_Shooting_Pctg, na.rm = TRUE),
    L5_Off_Eff = mean(L5_Off_Eff, na.rm = TRUE),
    L5_Def_Eff = mean(L5_Def_Eff, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarization
  )

away_stats <- away_stats %>% 
  left_join(columns_to_join, by = "nameTeam")

home_stats <- home_stats %>% 
  left_join(columns_to_join, by = "nameTeam")
#### team mapping ####
# Define the mapping from abbreviations to full names
team_mapping <- c(
  "BOS" = "Boston Celtics",
  "MIN" = "Minnesota Timberwolves",
  "LAL" = "Los Angeles Lakers",
  "NYK" = "New York Knicks",
  "MIA" = "Miami Heat",
  "MIL" = "Milwaukee Bucks",
  "TOR" = "Toronto Raptors",
  "CHA" = "Charlotte Hornets",
  "MEM" = "Memphis Grizzlies",
  "ORL" = "Orlando Magic",
  "CHI" = "Chicago Bulls",
  "BKN" = "Brooklyn Nets",
  "IND" = "Indiana Pacers",
  "PHX" = "Phoenix Suns",
  "POR" = "Portland Trail Blazers",
  "PHI" = "Philadelphia 76ers",
  "ATL" = "Atlanta Hawks",
  "HOU" = "Houston Rockets",
  "DET" = "Detroit Pistons",
  "UTA" = "Utah Jazz",
  "LAC" = "Los Angeles Clippers",
  "GSW" = "Golden State Warriors",
  "NOP" = "New Orleans Pelicans",
  "CLE" = "Cleveland Cavaliers",
  "SAS" = "San Antonio Spurs",
  "DAL" = "Dallas Mavericks",
  "WAS" = "Washington Wizards",
  "DEN" = "Denver Nuggets",
  "SAC" = "Sacramento Kings",
  "OKC" = "Oklahoma City Thunder"
)

# Apply the mapping to slugOpponent in game_data2
game_data2 <- game_data2 %>%
  mutate(slugOpponent = dplyr::recode(slugOpponent, !!!team_mapping))
# Verify the results
unique(game_data2$slugOpponent)

#### joining to game_data 2 ####
home_stats <- home_stats %>% 
  rename_with(~ paste0("H_", .), -nameTeam)

away_stats <- away_stats %>% 
  rename_with(~ paste0("A_", .), -nameTeam)

game_data2 <- game_data2 %>%
  left_join(home_stats, by = "nameTeam") %>%
  left_join(away_stats, by = c("slugOpponent" = "nameTeam"))

unique(away$nameTeam)


#### Dropping unwanted columns ####
game_data2 <- game_data2[-c(1:4,6,9:11,14:17,19,20,22:24,27,30:47)]
#### Writing the RF Model ####
game_data2 <- game_data2 %>%
  mutate(isB2BSecond = as.factor(isB2BSecond))

game_data2 <- game_data2 %>% 
  group_by(nameTeam) %>% 
  mutate(pctg3pt = mean(pctFG3Team),
         FTpctg = mean(pctFTTeam),
         FGpctg = mean(pctFGTeam)) %>% 
  ungroup()

game_data2 <- game_data2 %>%
  drop_na()

#inverting turnovers
game_data2 <- game_data2 %>%
  mutate(
    H_team_tov = 1 / (H_team_tov + 1e-6),  # Add small constant to avoid division by zero
    A_team_tov = 1 / (A_team_tov + 1e-6)
  )


set.seed(123)
train_index <- createDataPartition(game_data2$outcomeGame, p = 0.8, list = FALSE)
train_data <- game_data2[train_index, ]
test_data <- game_data2[-train_index, ]

train_data <- train_data %>% 
  mutate(outcomeGame = as.factor(outcomeGame))




# Train the Random Forest Model
rf_model <- randomForest(
  formula = outcomeGame ~ FGpctg + pctg3pt + FTpctg + H_team_pts + H_team_fgm + H_team_ftm +
    H_team_fga + H_team_fta + H_team_dreb + H_team_oreb + H_team_ast + H_team_stl + H_team_blk + H_team_pf + H_team_tov + 
    H_L5_OREB_pctg + H_L5_DREB_pctg + H_L5_True_Shooting_Pctg + H_L5_Off_Eff + H_L5_Def_Eff + A_team_pts + 
    A_team_fgm + A_team_ftm + A_team_fga + A_team_fta + A_team_dreb + A_team_oreb + A_team_ast + A_team_stl + A_team_blk + 
    A_team_pf + A_team_tov + A_L5_OREB_pctg + A_L5_DREB_pctg + A_L5_True_Shooting_Pctg + A_L5_Off_Eff + A_L5_Def_Eff,
  data = train_data,
  ntree = 500,
  mtry = sqrt(ncol(train_data) - 3),  # Subtract 3 for excluded columns
  importance = TRUE
)

varImpPlot(rf_model)



#### Predicting upcoming games ####

home_stats2 <- game_data2 %>%
  select(nameTeam, 12:28, 46:48) %>% 
  distinct(nameTeam, .keep_all = TRUE)

away_stats2 <- game_data2 %>% 
  select(nameTeam, 12:28, 46:48) %>% 
  distinct(nameTeam, .keep_all = TRUE)



home_team <- "Toronto Raptors"

home_stats2 <- game_data2 %>%
  filter(nameTeam == home_team) %>%
  distinct(nameTeam, .keep_all = TRUE) %>% 
  select(nameTeam, H_team_pts:H_L5_Def_Eff, pctg3pt, FTpctg, FGpctg) 

away_team <- "Orlando Magic"

away_stats2 <- game_data2 %>%
  filter(nameTeam == away_team) %>%
  distinct(nameTeam, .keep_all = TRUE) %>% 
  select(nameTeam, A_team_pts:A_L5_Def_Eff, pctg3pt, FTpctg, FGpctg)

if (nrow(home_stats2) == 0) {
  stop(paste("No data available for home team:", home_team))
}
if (nrow(away_stats2) == 0) {
  stop(paste("No data available for away team:", away_team))
}

matchup_data <- cbind(home_stats2, away_stats2)

required_columns <- setdiff(all.vars(rf_model$call$formula), "outcomeGame")
missing_columns <- setdiff(required_columns, colnames(matchup_data))

if (length(missing_columns) > 0) {
  stop(paste("Missing columns in matchup_data:", paste(missing_columns, collapse = ", ")))
}

# Check the combined matchup data
predicted_outcome <- predict(rf_model, newdata = matchup_data, type = "class")
predicted_prob <- predict(rf_model, newdata = matchup_data, type = "prob")
print(predicted_outcome)


result <- data.frame(
  Home_Team = home_team,
  Away_Team = away_team,
  Predicted_Winner = ifelse(predicted_prob[1, "W"] > predicted_prob[1, "L"], home_team, away_team),
  Home_Win_Probability = predicted_prob[1, "W"],
  Away_Win_Probability = predicted_prob[1, "L"]
)

print(result)


#### Gathering Data ####
player <- game_logs(seasons = 2025, result_types = "player")

player <- player[-c(1:5,7,9:21)]

player <- player %>%
  mutate(nameTeam = ifelse(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam))

props <- player %>% 
  left_join(game_data2, by = "nameTeam")

props <- props[-c(4:7, 15:17, 27:29, 33:35, 37:39)]

opp3 <- game_logs(seasons = 2025, result_types = "team")

opp3 <- opp3 %>%
  select(any_of(c("nameTeam", "slugOpponent", "fg3mTeam", "fg3aTeam", "fgmTeam", "fgaTeam", "astTeam",
                  "drebTeam", "orebTeam", "trebTeam")))

opponent <- opp3 %>% 
  group_by(slugOpponent) %>% 
  summarise(opp_fg3a = mean(fg3aTeam, na.rm = TRUE),
            opp_fg2a = mean(fgaTeam, na.rm = TRUE),
            opp_Dreb = mean(drebTeam, na.rm = TRUE),
            opp_Oreb = mean(orebTeam, na.rm = TRUE),
            opp_Treb = mean(trebTeam),
            opp_ast = mean(astTeam)) %>% 
  rename(nameTeam = slugOpponent)

#### team mapping ####


team_mapping_df <- data.frame(
  abbrev = c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", 
             "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", 
             "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"),
  full_name = c("Atlanta Hawks", "Brooklyn Nets", "Boston Celtics", "Charlotte Hornets", 
                "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", 
                "Detroit Pistons", "Golden State Warriors", "Houston Rockets", 
                "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers", 
                "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", 
                "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", 
                "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", 
                "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", 
                "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards")
)

opponent <- opponent %>%
  mutate(nameTeam = ifelse(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam))



# Join to map team names
opponent <- opponent %>%
  left_join(team_mapping_df, by = c("nameTeam" = "abbrev")) %>%
  mutate(nameTeam = full_name) %>%
  select(-full_name)  # Drop the temporary mapping column


props <- props %>%
  mutate(nameTeam = recode(nameTeam, "LA Clippers" = "Los Angeles Clippers"))


props <- props %>% 
  left_join(opponent, by = "nameTeam")




#### Player stats ####
props <- props %>% 
  group_by(namePlayer) %>%
  mutate(
    player_pts = mean(pts, na.rm = TRUE),
    player_fgm = mean(fgm, na.rm = TRUE),
    player_ftm = mean(ftm, na.rm = TRUE),
    player_fga = mean(fga, na.rm = TRUE),
    player_fg_pct = mean(fgm / fga, na.rm = TRUE),
    player_fta = mean(fta, na.rm = TRUE),
    player_ft_pct = mean(ftm / fga, na.rm = TRUE),
    player_dreb = mean(dreb, na.rm = TRUE),
    player_oreb = mean(oreb, na.rm = TRUE),
    player_ast = mean(ast, na.rm = TRUE),
    player_3p = mean(fg3m, na.rm = TRUE),
    player_3pct = mean(pctFG3, na.rm = TRUE),
  ) 

props <- props %>% 
  group_by(namePlayer, idGame) %>% 
  mutate(
    PRA = pts + treb + ast,
    RA = treb + ast,
    PR = treb + pts,
    PA = pts + ast,
  ) %>% 
  ungroup() %>% 
  distinct(namePlayer, idGame, .keep_all = TRUE)

props <- props %>% 
  mutate(
    player_True_Shooting_Pct = ifelse(
      (fga + 0.44 * fta) > 0,
      (pts / (2 * (fga + 0.44 * fta))) * 100,
      NA  # Assign NA or another default value for invalid cases
    )
  )


#Rolling averages
props <- props %>%
  arrange(namePlayer) %>%  # Sort by player and game
  group_by(namePlayer) %>%         # Group by player only
  mutate(
    L5_FG_Pct = rollapply(player_fg_pct, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_True_Shooting_Pctg = rollapply(player_True_Shooting_Pct, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_3pt_pct = rollapply(pctg3pt, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_FT_Pct = rollapply(FTpctg, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_ast = rollapply(ast, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE),
    L5_reb = rollapply(treb, width = 5, FUN = mean, align = "right", fill = NA, partial = TRUE)
  ) %>%
  ungroup()


props <- props %>%
  mutate(
    # Handle categorical variables
    nameTeam = ifelse(is.na(nameTeam), "Unknown", nameTeam),
    namePlayer = ifelse(is.na(namePlayer), "Unknown", namePlayer),
    outcomeGame = ifelse(is.na(outcomeGame), "Unknown", outcomeGame),
    slugOpponent = ifelse(is.na(slugOpponent), "Unknown", slugOpponent),
    
    # Handle numerical variables
    pctFG = ifelse(is.na(pctFG), mean(pctFG, na.rm = TRUE), pctFG),
    pctFG3 = ifelse(is.na(pctFG3), mean(pctFG3, na.rm = TRUE), pctFG3),
    player_fg_pct = ifelse(is.na(player_fg_pct), mean(player_fg_pct, na.rm = TRUE), player_fg_pct),
    player_ft_pct = ifelse(is.na(player_ft_pct), mean(player_ft_pct, na.rm = TRUE), player_ft_pct),
    player_True_Shooting_Pct = ifelse(is.na(player_True_Shooting_Pct), mean(player_True_Shooting_Pct, na.rm = TRUE), player_True_Shooting_Pct),
    L5_True_Shooting_Pctg = ifelse(is.na(L5_True_Shooting_Pctg), mean(L5_True_Shooting_Pctg, na.rm = TRUE), L5_True_Shooting_Pctg),
    L5_FG_Pct = ifelse(is.na(L5_FG_Pct), mean(L5_FG_Pct, na.rm = TRUE), L5_FG_Pct),
    L5_3pt_pct = ifelse(is.na(L5_3pt_pct), mean(L5_3pt_pct, na.rm = TRUE), L5_3pt_pct),
    L5_FT_Pct = ifelse(is.na(L5_FT_Pct), mean(L5_FT_Pct, na.rm = TRUE), L5_FT_Pct),
    L5_ast = ifelse(is.na(L5_ast), mean(L5_ast, na.rm = TRUE), L5_ast),
    L5_reb = ifelse(is.na(L5_reb), mean(L5_reb, na.rm = TRUE), L5_reb),
    
    
    # Rolling averages filled with 0 where no data is available
  )


#### Points model ####
set.seed(123)
train_index <- createDataPartition(props$pts, p = 0.8, list = FALSE)
train_pts <- props[train_index, ]
test_pts <- props[-train_index, ]

colnames(props)

rf_model_player <- randomForest(
  formula = pts ~ H_team_pts + H_team_fgm + H_team_fga +  H_L5_True_Shooting_Pctg + H_L5_Off_Eff + 
    H_L5_Def_Eff + A_team_pts + A_team_fgm  + A_team_fga + A_L5_True_Shooting_Pctg + A_L5_Off_Eff + A_L5_Def_Eff +
    player_fga +  player_fg_pct + player_3pct + opp_fg3a + opp_fg2a + L5_FG_Pct + L5_True_Shooting_Pctg + L5_3pt_pct +
    L5_FT_Pct,
  data = train_pts,
  ntree = 500,
  mtry = sqrt(ncol(train_data) - 3),  # Subtract 3 for excluded columns
  importance = TRUE
)

colSums(is.na(train_pts))

####Scraping Potential Assists ####
# NBA Stats API URL
url <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Passing&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

# Headers from the cURL command
headers <- c(
  "Accept" = "*/*",
  "Accept-Language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "Connection" = "keep-alive",
  "Origin" = "https://www.nba.com",
  "Referer" = "https://www.nba.com/",
  "Sec-Fetch-Dest" = "empty",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Site" = "same-site",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36",
  "dnt" = "1",
  "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Google Chrome\";v=\"132\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"Windows\"",
  "sec-gpc" = "1"
)

# Make the GET request
response <- GET(url, add_headers(.headers = headers))

# Parse the JSON response
data <- content(response, as = "text", encoding = "UTF-8")
parsed_data <- fromJSON(data, flatten = TRUE)

# Extract rows and assign column names manually
rows <- parsed_data$resultSets$rowSet

# Convert rows to a data frame
potential_assists <- as.data.frame(do.call(rbind, rows))

# Manually assign column names
colnames(potential_assists) <- c(
  "PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "GP", 
  "W", "L", "MIN", "PASSES_MADE", "PASSES_RECEIVED", "AST", 
  "FT_AST", "SECONDARY_AST", "POTENTIAL_AST", "AST_POINTS_CREATED", 
  "AST_ADJ", "AST_TO_PASS_PCT", "AST_TO_PASS_PCT_ADJ"
)

# View the cleaned data
head(potential_assists)



#### Scraping Potential Rebounds ####
library(httr)
library(jsonlite)

# Correct API URL
url2 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=Rebounding&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

# Headers from the cURL command
headers2 <- c(
  "Accept" = "*/*",
  "Accept-Language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "Connection" = "keep-alive",
  "Origin" = "https://www.nba.com",
  "Referer" = "https://www.nba.com/",
  "Sec-Fetch-Dest" = "empty",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Site" = "same-site",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36",
  "dnt" = "1",
  "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Google Chrome\";v=\"132\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"Windows\"",
  "sec-gpc" = "1"
)

# Make the GET request
response2 <- GET(url2, add_headers(.headers = headers2))

# Parse the JSON response
data2 <- content(response2, as = "text", encoding = "UTF-8")
parsed_data2 <- fromJSON(data2, flatten = TRUE)

# Extract rows from the JSON response
rows2 <- parsed_data2$resultSets$rowSet

# Convert rows to a data frame
potential_rebounds <- as.data.frame(do.call(rbind, rows2))

# Manually assign column names
colnames(potential_rebounds) <- c(
  "PLAYER_ID", "PLAYER_NAME", "TEAM_ID", "TEAM_ABBREVIATION", "GP", 
  "W", "L", "MIN", "REB", "CONTESTED_REB", "CONTESTED_REB_PCT", 
  "REB_CHANCES", "REB_CHANCE_PCT", "DEFERRED_REB_CHANCES", 
  "ADJUSTED_REB_CHANCE_PCT", "AVG_REB_DISTANCE"
)

# View the cleaned data
head(potential_rebounds)

col_names <- parsed_data2$resultSets$headers[[1]]

# Convert rows to a data frame and assign headers dynamically
potential_rebounds <- as.data.frame(do.call(rbind, rows2))
colnames(potential_rebounds) <- col_names

# View the data
head(potential_rebounds)
#### Scraping Advanced Team Stats ####
# Correct API URL
url_team <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2024-25&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="

# Headers from the cURL command
headers_team <- c(
  "Accept" = "*/*",
  "Accept-Language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "Connection" = "keep-alive",
  "Origin" = "https://www.nba.com",
  "Referer" = "https://www.nba.com/",
  "Sec-Fetch-Dest" = "empty",
  "Sec-Fetch-Mode" = "cors",
  "Sec-Fetch-Site" = "same-site",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/132.0.0.0 Safari/537.36",
  "dnt" = "1",
  "sec-ch-ua" = "\"Not A(Brand\";v=\"8\", \"Chromium\";v=\"132\", \"Google Chrome\";v=\"132\"",
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = "\"Windows\"",
  "sec-gpc" = "1"
)

# Make the GET request
response_team <- GET(url_team, add_headers(.headers = headers_team))

# Parse the JSON response
data_team <- content(response_team, as = "text", encoding = "UTF-8")
parsed_data_team <- fromJSON(data_team, flatten = TRUE)

# Extract rows and headers
rows_team <- parsed_data_team$resultSets$rowSet
col_names_team <- parsed_data_team$resultSets$headers[[1]]

# Convert rows to a data frame and assign headers dynamically
team_stats <- as.data.frame(do.call(rbind, rows_team))
colnames(team_stats) <- col_names_team

# Remove columns with all NA values
team_stats <- team_stats[, colSums(is.na(team_stats)) < nrow(team_stats)]

# View the cleaned data
head(team_stats)

#### Filtering to only include wanted stats ####
pot_reb <- potential_rebounds %>% 
  select(PLAYER_NAME, REB, REB_CONTEST, REB_UNCONTEST, REB_CHANCES,
         REB_CHANCE_PCT_ADJ)

pot_ast <- potential_assists %>% 
  select(PLAYER_NAME, AST, PASSES_MADE, POTENTIAL_AST, 
         AST_POINTS_CREATED, AST_TO_PASS_PCT)

ad_team_stats <- team_stats %>% 
  select(TEAM_NAME, NET_RATING, PACE, PACE_PER40)

props_test <- props

pot_reb <- pot_reb %>% 
  rename(namePlayer = PLAYER_NAME)

pot_ast <- pot_ast %>% 
  rename(namePlayer = PLAYER_NAME)

ad_team_stats <- ad_team_stats %>% 
  rename(nameTeam = TEAM_NAME)

ad_team_stats <- ad_team_stats %>%
  mutate(nameTeam = ifelse(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam))

props <- left_join(props, pot_reb, by = c("namePlayer"))

props <- left_join(props, pot_ast, by = c("namePlayer"))

props <- left_join(props, ad_team_stats, by = c("nameTeam"))


#### Assists model ####
props <- props %>%
  mutate(
    AST = as.numeric(AST),
    PASSES_MADE = as.numeric(PASSES_MADE),
    POTENTIAL_AST = as.numeric(POTENTIAL_AST),
    AST_TO_PASS_PCT = as.numeric(AST_TO_PASS_PCT)
  )

props <- props %>%
  mutate(across(
    c(AST, PASSES_MADE, POTENTIAL_AST, AST_TO_PASS_PCT, opp_ast),
    ~ {
      scaled_value <- (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
      ifelse(is.nan(scaled_value) | is.infinite(scaled_value), 0, scaled_value)
    },
    .names = "scaled_{col}"
  ))


set.seed(123)
train_index <- createDataPartition(props$ast, p = 0.8, list = FALSE)
train_ast <- props[train_index, ]
test_ast <- props[-train_index, ]

mtry_value_ast <- sqrt(ncol(train_ast) - 3)

train_ast <- na.omit(train_ast)


rf_model_ast <- randomForest(
  formula = ast ~ scaled_AST + scaled_PASSES_MADE + scaled_POTENTIAL_AST + 
    scaled_AST_TO_PASS_PCT + scaled_opp_ast + 
    H_team_fgm + H_L5_True_Shooting_Pctg + H_L5_Off_Eff + 
    H_L5_Def_Eff + A_team_fgm + A_L5_True_Shooting_Pctg + 
    A_L5_Off_Eff + A_L5_Def_Eff,
  data = train_ast,
  ntree = 500,
  mtry = mtry_value_ast,
  importance = TRUE
)


colSums(is.na(train_ast))
#### Rebounds Model ####
set.seed(123)
train_index <- createDataPartition(props$treb, p = 0.8, list = FALSE)
train_reb <- props[train_index, ]
train_reb <- props[-train_index, ]

colSums(is.na(train_reb))

train_reb <- na.omit(train_reb)

print(ncol(train_reb))  # Check total columns
print(colnames(train_reb))  # See column names


num_predictors <- ncol(train_reb) - 1  # Only exclude the response column
print(paste("Number of Predictors:", num_predictors))
mtry_value <- max(1, min(floor(sqrt(num_predictors)), num_predictors))  # Ensure mtry is within range
print(paste("mtry_value:", mtry_value))


rf_model_reb <- randomForest(
  formula = treb ~  REB + REB_CHANCES + REB_CHANCE_PCT_ADJ + H_L5_OREB_pctg + L5_reb +
    H_L5_DREB_pctg + A_L5_OREB_pctg + A_L5_DREB_pctg + opp_Treb + PACE,
  data = train_reb,
  ntree = 500,
  mtry = mtry_value,  # Subtract 3 for excluded columns
  importance = TRUE
)

#### Now adjust to choose player and team ####

predict_player_points <- function(player_name, home_team, away_team, model, data) {
  # Filter player-specific stats
  player_data <- data %>%
    filter(namePlayer == player_name) %>%
    summarise(
      player_fga = mean(player_fga, na.rm = TRUE),
      player_fg_pct = mean(player_fg_pct, na.rm = TRUE),
      player_3pct = mean(player_3pct, na.rm = TRUE),
      opp_fg3a = mean(opp_fg3a, na.rm = TRUE),
      opp_fg2a = mean(opp_fg2a, na.rm = TRUE),
      L5_FG_Pct = mean(L5_FG_Pct, na.rm = TRUE),
      L5_True_Shooting_Pctg = mean(L5_True_Shooting_Pctg, na.rm = TRUE),
      L5_3pt_pct = mean(L5_3pt_pct, na.rm = TRUE),
      L5_FT_Pct = mean(L5_FT_Pct, na.rm = TRUE)
    )
  
  # Filter home team stats
  home_stats <- data %>%
    filter(nameTeam == home_team) %>%
    summarise(
      H_team_pts = mean(H_team_pts, na.rm = TRUE),
      H_team_fgm = mean(H_team_fgm, na.rm = TRUE),
      H_team_fga = mean(H_team_fga, na.rm = TRUE),
      H_L5_True_Shooting_Pctg = mean(H_L5_True_Shooting_Pctg, na.rm = TRUE),
      H_L5_Off_Eff = mean(H_L5_Off_Eff, na.rm = TRUE),
      H_L5_Def_Eff = mean(H_L5_Def_Eff, na.rm = TRUE)
    )
  
  # Filter away team stats
  away_stats <- data %>%
    filter(nameTeam == away_team) %>%
    summarise(
      A_team_pts = mean(A_team_pts, na.rm = TRUE),
      A_team_fgm = mean(A_team_fgm, na.rm = TRUE),
      A_team_fga = mean(A_team_fga, na.rm = TRUE),
      A_L5_True_Shooting_Pctg = mean(A_L5_True_Shooting_Pctg, na.rm = TRUE),
      A_L5_Off_Eff = mean(A_L5_Off_Eff, na.rm = TRUE),
      A_L5_Def_Eff = mean(A_L5_Def_Eff, na.rm = TRUE)
    )
  
  # Combine all features into a single-row dataset
  input_data <- cbind(player_data, home_stats, away_stats)
  
  # Predict the player's points
  predicted_points <- predict(model, newdata = input_data)
  
  # Prepare the output
  result <- data.frame(
    Player = player_name,
    Home_Team = home_team,
    Away_Team = away_team,
    Predicted_Points = predicted_points
  )
  
  return(result)
}

#Example Usage
result <- predict_player_points(
  player_name = "Malik Beasley",
  home_team = "Detroit Pistons",
  away_team = "Indiana Pacers",
  model = rf_model_player,
  data = props
)

print(result)




#### Rebound Prediction ####
predict_player_rebounds <- function(player_name, home_team, away_team, model, data) {
  # Filter player-specific stats (use the existing values directly)
  player_data <- data %>%
    filter(namePlayer == player_name) %>%
    select(REB, REB_CHANCES, REB_CHANCE_PCT_ADJ, opp_Treb, PACE, opp_fg3a, opp_fg2a, L5_reb) %>%
    slice(1)  # Ensure only one row is selected
  
  # Filter home team stats (summarize for team-specific stats)
  home_stats <- data %>%
    filter(nameTeam == home_team) %>%
    summarise(
      H_L5_OREB_pctg = mean(H_L5_OREB_pctg, na.rm = TRUE),
      H_L5_DREB_pctg = mean(H_L5_DREB_pctg, na.rm = TRUE)
    )
  
  # Filter away team stats (summarize for team-specific stats)
  away_stats <- data %>%
    filter(nameTeam == away_team) %>%
    summarise(
      A_L5_OREB_pctg = mean(A_L5_OREB_pctg, na.rm = TRUE),
      A_L5_DREB_pctg = mean(A_L5_DREB_pctg, na.rm = TRUE)
    )
  
  # Combine all features into a single-row dataset
  input_data <- cbind(player_data, home_stats, away_stats)
  
  # Check if input_data has any missing or NA values
  if (any(is.na(input_data))) {
    warning("Input data contains NA values. Prediction might not be accurate.")
  }
  
  # Predict the player's rebounds
  predicted_rebounds <- predict(model, newdata = input_data)
  
  # Prepare the output
  result <- data.frame(
    Player = player_name,
    Home_Team = home_team,
    Away_Team = away_team,
    Predicted_Rebounds = predicted_rebounds
  )
  
  return(result)
}


# Example Usage
player_name <- "Miles McBride"
home_team <- "Brooklyn Nets"
away_team <- "New York Knicks"

result <- predict_player_rebounds(
  player_name = player_name,
  home_team = home_team,
  away_team = away_team,
  model = rf_model_reb,  # Replace with your trained rebounds model
  data = props  # Replace with your actual dataset
)

print(result)



#### Assists Prediction ####
predict_player_assists <- function(player_name, home_team, away_team, model, data) {
  # Apply scaling or preprocessing to the data
  data <- data %>%
    mutate(
      scaled_AST = scale(as.numeric(AST)),
      scaled_PASSES_MADE = scale(as.numeric(PASSES_MADE)),
      scaled_POTENTIAL_AST = scale(as.numeric(POTENTIAL_AST)),
      scaled_AST_TO_PASS_PCT = scale(as.numeric(AST_TO_PASS_PCT)),
      scaled_opp_ast = scale(as.numeric(opp_ast))
    )
  
  # Filter and deduplicate player-specific stats
  player_data <- data %>%
    filter(namePlayer == player_name) %>%
    select(scaled_AST, scaled_PASSES_MADE, scaled_POTENTIAL_AST, scaled_AST_TO_PASS_PCT, scaled_opp_ast) %>%
    slice(1)  # Ensure only one row is selected
  
  if (nrow(player_data) == 0) {
    stop("No player-specific data found for player: ", player_name)
  }
  
  # Debug: Check player-specific data
  print("Player Data (Scaled):")
  print(player_data)
  
  # Filter and summarize home team stats
  home_stats <- data %>%
    filter(nameTeam == home_team) %>%
    summarise(
      H_team_fgm = mean(H_team_fgm, na.rm = TRUE),
      H_L5_True_Shooting_Pctg = mean(H_L5_True_Shooting_Pctg, na.rm = TRUE),
      H_L5_Off_Eff = mean(H_L5_Off_Eff, na.rm = TRUE),
      H_L5_Def_Eff = mean(H_L5_Def_Eff, na.rm = TRUE)
    )
  
  if (nrow(home_stats) == 0) {
    stop("No home team stats found for team: ", home_team)
  }
  
  # Debug: Check home team stats
  print("Home Team Stats:")
  print(home_stats)
  
  # Filter and summarize away team stats
  away_stats <- data %>%
    filter(nameTeam == away_team) %>%
    summarise(
      A_team_fgm = mean(A_team_fgm, na.rm = TRUE),
      A_L5_True_Shooting_Pctg = mean(A_L5_True_Shooting_Pctg, na.rm = TRUE),
      A_L5_Off_Eff = mean(A_L5_Off_Eff, na.rm = TRUE),
      A_L5_Def_Eff = mean(A_L5_Def_Eff, na.rm = TRUE)
    )
  
  if (nrow(away_stats) == 0) {
    stop("No away team stats found for team: ", away_team)
  }
  
  # Debug: Check away team stats
  print("Away Team Stats:")
  print(away_stats)
  
  # Combine all features into a single-row dataset
  input_data <- cbind(player_data, home_stats, away_stats)
  
  # Debug: Check combined input data
  print("Combined Input Data:")
  print(input_data)
  
  # Check for missing values in the input data
  if (any(is.na(input_data))) {
    stop("Input data contains NA values. Please verify data integrity.")
  }
  
  # Predict the player's assists
  predicted_assists <- predict(model, newdata = input_data)
  
  # Prepare the output
  result <- data.frame(
    Player = player_name,
    Home_Team = home_team,
    Away_Team = away_team,
    Predicted_Assists = predicted_assists
  )
  
  return(result)
}

# Define player, home team, and away team
player_name <- "Jalen Duren"
home_team <- "Atlanta Hawks"
away_team <- "Detroit Pistons"

# Call the function
result <- predict_player_assists(
  player_name = player_name,
  home_team = home_team,
  away_team = away_team,
  model = rf_model_ast,  # Your trained assist prediction model
  data = props           # Your dataset
)

# Print the result
print(result)



#### WORKING #### 
# Define UI
ui <- bs4DashPage(
  title = "NBA Prediction App",
  dark = TRUE,  # Enable dark mode
  header = bs4DashNavbar(
    title = "NBA Prediction App"
  ),
  sidebar = bs4DashSidebar(
    title = "NBA Prediction",
    skin = "dark",
    status = "primary",
    sidebarMenu(
      menuItem("Player Prediction", tabName = "player_prediction", icon = icon("user")),
      selectInput("player", "Select Player:", choices = NULL),
      selectInput("home_team", "Select Home Team:", choices = NULL),
      selectInput("away_team", "Select Away Team:", choices = NULL),
      numericInput("over_under_points", "Over/Under (Points):", value = 20, min = 0),
      numericInput("over_under_rebounds", "Over/Under (Rebounds):", value = 10, min = 0),
      numericInput("over_under_assists", "Over/Under (Assists):", value = 5, min = 0),
      actionButton("predict", "Predict", class = "btn-primary")
    )
  ),
  body = bs4DashBody(
    tabItems(
      tabItem(
        tabName = "player_prediction",
        fluidRow(
          column(
            width = 6,
            bs4Card(
              title = "Player Photo",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              uiOutput("player_photo")
            )
          ),
          column(
            width = 6,
            bs4Card(
              title = "Prediction Results",
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              fluidRow(
                column(
                  width = 6,  # Original metrics
                  tableOutput("player_prediction")
                ),
                column(
                  width = 6,  # Combined metrics
                  tableOutput("combined_metrics")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            bs4Card(
              title = "Points in Last 5 Games",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              plotOutput("last_5_points_plot", height = "300px")
            )
          ),
          column(
            width = 4,
            bs4Card(
              title = "Rebounds in Last 5 Games",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              plotOutput("last_5_rebounds_plot", height = "300px")
            )
          ),
          column(
            width = 4,
            bs4Card(
              title = "Assists in Last 5 Games",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              plotOutput("last_5_assists_plot", height = "300px")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Card(
              title = "Points Line Coverage Statistics",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              fluidRow(
                column(width = 2, h5("Last 5 Games"), textOutput("coverage_last_5")),
                column(width = 2, h5("Last 10 Games"), textOutput("coverage_last_10")),
                column(width = 2, h5("Last 15 Games"), textOutput("coverage_last_15")),
                column(width = 2, h5("Last 20 Games"), textOutput("coverage_last_20")),
                column(width = 2, h5("All Games"), textOutput("coverage_all_games"))
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Card(
              title = "Rebounds Line Coverage Statistics",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              fluidRow(
                column(width = 2, h5("Last 5 Games"), textOutput("rebounds_coverage_last_5")),
                column(width = 2, h5("Last 10 Games"), textOutput("rebounds_coverage_last_10")),
                column(width = 2, h5("Last 15 Games"), textOutput("rebounds_coverage_last_15")),
                column(width = 2, h5("Last 20 Games"), textOutput("rebounds_coverage_last_20")),
                column(width = 2, h5("All Games"), textOutput("rebounds_coverage_all_games"))
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Card(
              title = "Assists Line Coverage Statistics",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              fluidRow(
                column(width = 2, h5("Last 5 Games"), textOutput("assists_coverage_last_5")),
                column(width = 2, h5("Last 10 Games"), textOutput("assists_coverage_last_10")),
                column(width = 2, h5("Last 15 Games"), textOutput("assists_coverage_last_15")),
                column(width = 2, h5("Last 20 Games"), textOutput("assists_coverage_last_20")),
                column(width = 2, h5("All Games"), textOutput("assists_coverage_all_games"))
              )
            )
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Populate dropdown choices dynamically based on the dataset
  observe({
    updateSelectInput(session, "player", choices = unique(props$namePlayer))
    updateSelectInput(session, "home_team", choices = unique(props$nameTeam))
    updateSelectInput(session, "away_team", choices = unique(props$nameTeam))
  })
  
  # Function to calculate coverage percentage for a given number of games
  calculate_coverage_percentage <- function(player_name, data, metric, line, num_games = NULL) {
    filtered_data <- data %>%
      filter(namePlayer == player_name) %>%
      arrange(desc(idGame))
    
    if (!is.null(num_games)) {
      filtered_data <- filtered_data %>% slice_head(n = num_games)
    }
    
    coverage_percentage <- filtered_data %>%
      mutate(Covered = ifelse(.data[[metric]] >= line, 1, 0)) %>%
      summarise(Coverage = round(mean(Covered) * 100, 2)) %>%
      pull(Coverage)
    
    return(coverage_percentage)
  }
  
  # Run prediction and render outputs when the button is clicked
  observeEvent(input$predict, {
    # Player predictions
    points_result <- predict_player_points(input$player, input$home_team, input$away_team, rf_model_player, props)
    rebounds_result <- predict_player_rebounds(input$player, input$home_team, input$away_team, rf_model_reb, props)
    assists_result <- predict_player_assists(input$player, input$home_team, input$away_team, rf_model_ast, props)
    
    # Extract predictions
    predicted_points <- points_result$Predicted_Points
    predicted_rebounds <- rebounds_result$Predicted_Rebounds
    predicted_assists <- assists_result$Predicted_Assists
    
    # Standard metrics
    standard_results <- data.frame(
      Metric = c("Points", "Rebounds", "Assists"),
      Predicted = c(predicted_points, predicted_rebounds, predicted_assists)
    )
    
    # Combined metrics
    combined_metrics <- data.frame(
      Metric = c("PRA (Pts+Reb+Ast)", "PR (Pts+Reb)", "PA (Pts+Ast)", "AR (Ast+Reb)"),
      Predicted = c(
        predicted_points + predicted_rebounds + predicted_assists,
        predicted_points + predicted_rebounds,
        predicted_points + predicted_assists,
        predicted_rebounds + predicted_assists
      )
    )
    
    # Render standard metrics
    output$player_prediction <- renderTable({
      standard_results
    })
    
    # Render combined metrics
    output$combined_metrics <- renderTable({
      combined_metrics
    })
    
    # Player photo
    player_photo_url <- props %>% filter(namePlayer == input$player) %>% pull(urlPlayerThumbnail) %>% unique()
    output$player_photo <- renderUI({
      if (length(player_photo_url) > 0) {
        tags$img(src = player_photo_url, height = "200px", style = "border-radius: 10px;")
      } else {
        "No photo available"
      }
    })
    
    # Last 5 games visualizations
    output$last_5_points_plot <- renderPlot({ last_5_viz(input$player, props, input$over_under_points) })
    output$last_5_rebounds_plot <- renderPlot({ last_5_viz_reb(input$player, props, input$over_under_rebounds) })
    output$last_5_assists_plot <- renderPlot({ last_5_viz_ast(input$player, props, input$over_under_assists) })
    
    # Line coverage for Points
    output$coverage_last_5 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "pts", input$over_under_points, 5), "%") })
    output$coverage_last_10 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "pts", input$over_under_points, 10), "%") })
    output$coverage_last_15 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "pts", input$over_under_points, 15), "%") })
    output$coverage_last_20 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "pts", input$over_under_points, 20), "%") })
    output$coverage_all_games <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "pts", input$over_under_points, NULL), "%") })
    
    # Line coverage for Rebounds
    output$rebounds_coverage_last_5 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "treb", input$over_under_rebounds, 5), "%") })
    output$rebounds_coverage_last_10 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "treb", input$over_under_rebounds, 10), "%") })
    output$rebounds_coverage_last_15 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "treb", input$over_under_rebounds, 15), "%") })
    output$rebounds_coverage_last_20 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "treb", input$over_under_rebounds, 20), "%") })
    output$rebounds_coverage_all_games <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "treb", input$over_under_rebounds, NULL), "%") })
    
    # Line coverage for Assists
    output$assists_coverage_last_5 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "ast", input$over_under_assists, 5), "%") })
    output$assists_coverage_last_10 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "ast", input$over_under_assists, 10), "%") })
    output$assists_coverage_last_15 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "ast", input$over_under_assists, 15), "%") })
    output$assists_coverage_last_20 <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "ast", input$over_under_assists, 20), "%") })
    output$assists_coverage_all_games <- renderText({ paste0(calculate_coverage_percentage(input$player, props, "ast", input$over_under_assists, NULL), "%") })
  })
}

# Run the app
shinyApp(ui, server)
