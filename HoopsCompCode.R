# Import Libraries
library(tidyverse)
library(magrittr)
# setwd()

pbp <- read_csv("pbp.csv")

# Select Important Columns
df <- pbp %>%
  select(game_id, home, away, half, secs_remaining, secs_remaining_absolute,
         home_score, away_score, score_diff, play_length,
         scoring_play, foul, naive_win_prob, action_team,
         shot_team, shot_outcome, shooter, three_pt, free_throw,
         possession_before, possession_after)

# Clean and fix missing or incorrect info
df$naive_win_prob[which(is.na(df$naive_win_prob))] <- 0.5

away_teams <- df$away[is.na(df$shot_team) & !is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)]
df$shot_team[is.na(df$shot_team) & !is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)] <- away_teams
home_teams <- df$home[is.na(df$shot_team) & !is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)]
df$shot_team[is.na(df$shot_team) & !is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)] <- home_teams

away_teams <- df$away[!is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)]
home_teams <- df$home[!is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)]

df$shot_team[!is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)] <- away_teams
df$shot_team[!is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)] <- home_teams

# Add opponent column
df %<>%
  mutate(opponent = NA)
home_teams <- df$home[!is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)]
df$opponent[!is.na(df$shooter) & df$action_team == "away" & !is.na(df$action_team)] <- home_teams
away_teams <- df$away[!is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)]
df$opponent[!is.na(df$shooter) & df$action_team == "home" & !is.na(df$action_team)] <- away_teams

# Determine if first play of game to fix potential errors in code
df %<>%
  group_by(game_id) %>%
  mutate(first_play = ifelse(row_number() == 1, 1, 0))

# Recreate Naive Win Prob to determine counterfactual win probabilities (Make --> Miss, Miss --> Make)
naive_win_prob_function <- function(score_diff, secs_remaining, k, a) {
  return(1 / (1 + exp(-k * (score_diff / (1 + a * secs_remaining)))))
}

# Define the objective function to minimize MSE
objective_function <- function(params, score_diff, secs_remaining, naive_win_prob) {
  k <- params[1]
  a <- params[2]
  
  # Calculate fitted win probabilities
  fitted_wp <- naive_win_prob_function(score_diff, secs_remaining, k, a)
  
  # Calculate MSE between fitted and actual naive win probabilities
  mse <- mean((fitted_wp - naive_win_prob)^2)
  return(mse)
}

# Run the optimization using initial guesses for k and a
initial_params <- c(1.25, 0.006)  # Starting points for k and a

# Perform optimization
result <- optim(
  par = initial_params,
  fn = objective_function,
  score_diff = df$score_diff,
  secs_remaining = df$secs_remaining,
  naive_win_prob = df$naive_win_prob,
  method = "L-BFGS-B",  # Bounded optimization method
  lower = c(0, 0)       # Ensure non-negative values for k and a
)
# Extract optimized k and a values
optimized_k <- result$par[1]
optimized_a <- result$par[2]

k <- optimized_k
a <- optimized_a

# Restructure Naive Win Prob
df$naive_win_prob <- 1 / (1 + exp(-k*(df$score_diff / (1 + a * df$secs_remaining))))

# Convert Naive Win Prob to 1 or 0 if the game is over, 0.5 if the score is tied
df %<>%
  mutate(naive_win_prob = case_when(secs_remaining == 0 & score_diff != 0 ~ round(naive_win_prob),
                                    TRUE ~ naive_win_prob))

df %<>%
  # Add Win Prob Added w.r.t shooting/action team
  # Add points on play, shot type, and eFG
  group_by(game_id) %>%
  mutate(points = case_when(action_team == "away" & first_play == 0 ~ away_score - lag(away_score),
                            action_team == "away" & first_play == 1 ~ away_score - 0,
                            action_team == "home" & first_play == 1 ~ home_score - 0,
                            TRUE ~ home_score - lag(home_score)),
         shot_type = case_when(free_throw == TRUE ~ 1,
                               three_pt == TRUE ~ 3,
                               free_throw == FALSE & three_pt == FALSE ~ 2,
                               TRUE ~ 0),
         eFG = case_when(shot_outcome == "made" & three_pt == TRUE ~ 1.5,
                         shot_outcome == "made" & three_pt == FALSE & free_throw == FALSE ~ 1,
                         TRUE ~ 0))

# Home Score and Away Score given opposite shot outcomes
df %<>%
  mutate(opp_homescore = case_when(shot_outcome == "made" & action_team == "home" ~ home_score - points,
                                   shot_outcome == "missed" & action_team == "home" & three_pt == TRUE ~ home_score + 3,
                                   shot_outcome == "missed" & action_team == "home" & free_throw == TRUE ~ home_score + 1,
                                   action_team == "away" | is.na(shot_outcome) ~ home_score,
                                   TRUE ~ home_score + 2),
         opp_awayscore = case_when(shot_outcome == "made" & action_team == "away" ~ away_score - points,
                                   shot_outcome == "missed" & action_team == "away" & three_pt == TRUE ~ away_score + 3,
                                   shot_outcome == "missed" & action_team == "away" & free_throw == TRUE ~ away_score + 1,
                                   action_team == "home" | is.na(shot_outcome) ~ away_score,
                                   TRUE ~ away_score + 2))

# Score Differential given opposite shot outcome
df %<>%
  mutate(opp_scorediff = opp_homescore - opp_awayscore)

# Naive Win Probability given opposite shot outcome
df$alt_naive_wp <- 1 / (1 + exp(-k*(df$opp_scorediff / (1 + a * df$secs_remaining))))
df %<>%
  mutate(alt_naive_wp = case_when(secs_remaining == 0 & opp_scorediff != 0 ~ round(alt_naive_wp),
                                  TRUE ~ alt_naive_wp))

# Add Expected Pts by Shot Type
shot_pct <- df %>%
  group_by(shot_type) %>%
  summarize(fg_pct = sum(ifelse(shot_outcome == "made", 1, 0)) / n()) %>%
  mutate(exp_pts = fg_pct * shot_type) %>%
  select(shot_type, exp_pts)
df %<>%
  left_join(shot_pct, by = "shot_type")
df %<>%
  mutate(exp_home = case_when(action_team == "home" & shot_outcome == "missed" ~ home_score + exp_pts, 
                              action_team == "home" & shot_outcome == "made" ~ home_score - (points - exp_pts),
                              TRUE ~ home_score),
         exp_away = case_when(action_team == "away" & shot_outcome == "missed" ~ away_score + exp_pts, 
                              action_team == "away" & shot_outcome == "made" ~ away_score - (points - exp_pts), 
                              TRUE ~ away_score),
         exp_diff = exp_home - exp_away)

# Use XPts to calculate Naive WP given right before the shot to determine Win Probability Added 
df$prev_naive_wp <- 1 / (1 + exp(-k*(df$exp_diff / (1 + a * df$secs_remaining))))

df %<>%
  group_by(game_id) %>%
  mutate(wpa = ifelse(first_play == 0, naive_win_prob - prev_naive_wp, naive_win_prob - 0.5),
         adj_wpa = case_when(action_team == "away" ~ wpa * (-1),
                             TRUE ~ wpa))

# Potential WPA = WPA assuming all shots are made, useful for measuring the stakes of each shot
df %<>%
  group_by(game_id) %>%
  mutate(alt_wpa = ifelse(first_play == 0, alt_naive_wp - prev_naive_wp, alt_naive_wp - 0.5),
         adj_alt_wpa = case_when(action_team == "away" ~ alt_wpa * (-1),
                                 TRUE ~ alt_wpa),
         potential_wpa = case_when(shot_outcome == "made" ~ adj_wpa,
                                   TRUE ~ adj_alt_wpa))

# Flat definition of "The clutch"
df %<>%
  mutate(clutch = case_when(
    secs_remaining <= 300 & half >= 2 & abs(score_diff) <= 5 ~ 1,
    TRUE ~ 0
  )) %>%
  # Dummy variable for game that goes into clutch
  group_by(game_id) %>%
  mutate(clutch_game = max(clutch))

# Determine time spent in clutch for each game
clutch_df <- df %>%
  filter(clutch == 1) %>%
  group_by(game_id) %>%
  summarize(clutch_length = max(secs_remaining_absolute) - min(secs_remaining_absolute)) %>%
  arrange(desc(clutch_length))
df %<>%
  left_join(clutch_df, by = c("game_id"))
# write_csv(df, "pbp_updated.csv") # Used in Power BI

# Shooter season stats from 2023-24
# Clutch Rate defined as WPA / Potential WPA
shooter_df <- df %>%
  group_by(shooter, shot_team) %>%
  summarize(season_wpa = sum(adj_wpa),
            season_potential_wpa = sum(potential_wpa),
            season_clutch_rate = sum(adj_wpa) / sum(potential_wpa),
            season_points = sum(points),
            season_3PA = sum(ifelse(shot_type == 3, 1, 0)),
            season_3P_pct = sum(ifelse(shot_outcome == "made" & shot_type == 3, 1, 0)) / season_3PA,
            season_2PA = sum(ifelse(shot_type == 2, 1, 0)),
            season_2P_pct = sum(ifelse(shot_outcome == "made" & shot_type == 2, 1, 0)) / season_2PA,
            season_FTA = sum(ifelse(shot_type == 1, 1, 0)),
            season_FTr = sum(ifelse(shot_type == 1 & shot_outcome == "made", 1, 0))/ (season_3PA + season_2PA),
            season_eFG = mean(ifelse(season_3PA == 0, (season_2PA * season_2P_pct) / (season_2PA), ifelse(season_2PA == 0, (season_3P_pct * season_3PA * 1.5) / season_3PA,(season_3P_pct * season_3PA * 1.5 + season_2PA * season_2P_pct) / (season_2PA + season_3PA)))),
            season_shots = season_3PA + season_2PA,
            season_wpa3 = sum(ifelse(shot_type == 3, adj_wpa, 0)),
            season_potential_wpa3 = sum(ifelse(shot_type == 3, potential_wpa, 0)),
            season_clutch_rate3 = season_wpa3 / season_potential_wpa3,
            season_wpa2 =  sum(ifelse(shot_type == 2, adj_wpa, 0)),
            season_potential_wpa2 = sum(ifelse(shot_type == 2, potential_wpa, 0)),
            season_clutch_rate2 = season_wpa2 / season_potential_wpa2,
            season_wpa1 =  sum(ifelse(shot_type == 1, adj_wpa, 0)),
            season_potential_wpa1 = sum(ifelse(shot_type == 1, potential_wpa, 0)),
            season_clutch_rate1 = season_wpa1 / season_potential_wpa1) %>%
  filter(season_shots >= 50,
         season_3PA >= 15,
         season_2PA >= 25)

# Opponent Season Stats
opp_df <- df %>%
  group_by(opponent) %>%
  summarize(opp_season_wpa = sum(adj_wpa),
            opp_season_pot_wpa = sum(potential_wpa),
            opp_season_clutch_rate = sum(adj_wpa) / sum(potential_wpa),
            opp_season_points = sum(points),
            opp_season_3PA = sum(ifelse(shot_type == 3, 1, 0)),
            opp_season_3P_pct = sum(ifelse(shot_outcome == "made" & shot_type == 3, 1, 0)) / opp_season_3PA,
            opp_season_2PA = sum(ifelse(shot_type == 2, 1, 0)),
            opp_season_2P_pct = sum(ifelse(shot_outcome == "made" & shot_type == 2, 1, 0)) / opp_season_2PA,
            opp_season_FTA = sum(ifelse(shot_type == 1, 1, 0)),
            opp_season_FTr = sum(ifelse(shot_type == 1 & shot_outcome == "made", 1, 0))/ (opp_season_3PA + opp_season_2PA),
            opp_season_eFG = (opp_season_3P_pct * opp_season_3PA * 1.5 + opp_season_2PA * opp_season_2P_pct) / (opp_season_2PA + opp_season_3PA),
            opp_season_shots = opp_season_3PA + opp_season_2PA,
            opp_season_wpa3 = sum(ifelse(shot_type == 3, adj_wpa, 0)),
            opp_season_pot_wpa3 = sum(ifelse(shot_type == 3, potential_wpa, 0)),
            opp_season_clutch_rate3 = opp_season_wpa3 / opp_season_pot_wpa3,
            opp_season_wpa2 =  sum(ifelse(shot_type == 2, adj_wpa, 0)),
            opp_season_pot_wpa2 = sum(ifelse(shot_type == 2, potential_wpa, 0)),
            opp_season_clutch_rate2 = opp_season_wpa2 / opp_season_pot_wpa2,
            opp_season_wpa1 =  sum(ifelse(shot_type == 1, adj_wpa, 0)),
            opp_season_pot_wpa1 = sum(ifelse(shot_type == 1, potential_wpa, 0)),
            opp_season_clutch_rate1 = opp_season_wpa1 / opp_season_pot_wpa1) %>%
  filter(opp_season_shots >= 800) %>%
  select(-c(opp_season_points, opp_season_3PA, opp_season_2PA, opp_season_FTA, opp_season_shots))

# Shooter stats by game
shooter_game <- df %>%
  mutate(shooter = case_when(shooter == "Ja'derryus Eatmon" ~ "Ja'derryus Eatmon",
                             TRUE ~ shooter)) %>%
  filter(clutch == 0) %>%
  group_by(shooter, shot_team, game_id) %>%
  summarize(`2P%` = sum(ifelse(shot_outcome == "made" & shot_type == 2, 1, 0)) / sum(ifelse(shot_type == 2, 1, 0)),
            `3P%` = sum(ifelse(shot_outcome == "made" & shot_type == 3, 1, 0)) / sum(ifelse(shot_type == 3, 1, 0)),
            `2PM` = sum(ifelse(shot_outcome == "made" & shot_type == 2, 1, 0)),
            `3PM` = sum(ifelse(shot_outcome == "made" & shot_type == 3, 1, 0)),
            eFG = sum(ifelse(shot_outcome == "made" & shot_type == 3, 1.5, ifelse(shot_outcome == "made" & shot_type == 2, 1, 0))) / sum(ifelse(shot_type %in% c(2, 3), 1, 0)),
            shots = sum(ifelse(shot_type %in% c(2, 3), 1, 0)),
            `3PA` = sum(ifelse(shot_type == 3, 1, 0)),
            `2PA` = sum(ifelse(shot_type == 2, 1, 0)))

# Create DF of Shooting Binomial Distributions
# Define clutch shooting as shots with Potential WPA of 0.075 or more
shooting <- df %>%
  mutate(shooter = ifelse(shooter == "Shawn Jones jr.", "Shawn Jones Jr.", shooter),
         shooter = ifelse(shooter == "Ubongabasi Etim", "UbongAbasi Etim", shooter)) %>%
  group_by(shooter, shot_team) %>%
  summarize(nonclutch_shots = sum(ifelse(shot_type > 1 & potential_wpa < 0.075 & potential_wpa > 0, 1, 0)),
            nonclutch_eFG = sum(ifelse(shot_type == 2 & shot_outcome == "made" & potential_wpa < 0.075 & potential_wpa > 0, 1, ifelse(shot_type == 3 & shot_outcome == "made" & potential_wpa < 0.075 & potential_wpa > 0, 1.5, 0))) / nonclutch_shots,
            clutch_shots = sum(ifelse(shot_type > 1 & potential_wpa >= 0.075, 1, 0)),
            clutch_eFG = sum(ifelse(shot_type == 2 & shot_outcome == "made" & potential_wpa >= 0.075, 1, ifelse(shot_type == 3 & shot_outcome == "made" & potential_wpa >= 0.075, 1.5, 0))) / clutch_shots) %>%
  mutate(nonclutch_eFG = nonclutch_eFG / 1.5,
         clutch_eFG = clutch_eFG / 1.5) %>%
  filter(nonclutch_shots > 0,
         clutch_shots > 0) %>%
  mutate(clutch_exp = clutch_eFG * clutch_shots,
         clutch_var = clutch_exp * (1-clutch_eFG),
         nonclutch_exp = nonclutch_eFG * nonclutch_shots,
         nonclutch_var = nonclutch_exp * (1-nonclutch_eFG))

# Determine CI for Clutch and Non-Clutch Shooting
shooting %<>%
  mutate(clutch_ci_low = 0,
         clutch_ci_high = 0,
         nonclutch_ci_low = 0,
         nonclutch_ci_high = 0,
         p_val = 0)

for(i in 1:nrow(shooting)){
  makes_clutch <- 0:shooting$clutch_shots[i]
  makes_non_clutch <- 0:shooting$nonclutch_shots[i]
  
  probs_clutch <- dbinom(makes_clutch, shooting$clutch_shots[i], shooting$clutch_eFG[i])
  probs_non_clutch <- dbinom(makes_non_clutch, shooting$nonclutch_shots[i], shooting$nonclutch_eFG[i])
  
  # Code for plots used in Power BI
  # par(mfrow = c(1, 2)) # Side-by-side plots
  # 
  # plot(makes_clutch, probs_clutch, type = "h", lwd = 2,
  #      main = "Clutch Shooting Distribution",
  #      xlab = "Makes", ylab = "Probability")
  # 
  # plot(makes_non_clutch, probs_non_clutch, type = "h", lwd = 2,
  #      main = "Non-Clutch Shooting Distribution",
  #      xlab = "Makes", ylab = "Probability")

  ci_clutch <- prop.test(shooting$clutch_shots[i] * shooting$clutch_eFG[i], shooting$clutch_shots[i])$conf.int
  
  ci_nonclutch <- prop.test(shooting$nonclutch_shots[i] * shooting$nonclutch_eFG[i], shooting$nonclutch_shots[i])$conf.int
  shooting$clutch_ci_low[i] <- ci_clutch[1]
  shooting$clutch_ci_high[i] <- ci_clutch[2]
  shooting$nonclutch_ci_low[i] <- ci_nonclutch[1]
  shooting$nonclutch_ci_high[i] <- ci_nonclutch[2]
  
  test_res <- prop.test(
    x = c(shooting$clutch_shots[i] * shooting$clutch_eFG[i], shooting$nonclutch_shots[i] * shooting$nonclutch_eFG[i]),
    n = c(shooting$clutch_shots[i], shooting$nonclutch_shots[i]),
    correct = FALSE  # Setting to FALSE to avoid Yates' continuity correction
  )
  shooting$p_val[i] <- test_res$p.value
}

shooting %<>%
  mutate(p_val = ifelse(clutch_eFG < nonclutch_eFG, 2 - p_val, p_val),
         nonclutch_eFG = nonclutch_eFG * 1.5,
         clutch_eFG = clutch_eFG * 1.5,
         clutch_ci_low = clutch_ci_low * 1.5,
         clutch_ci_high = clutch_ci_high * 1.5,
         nonclutch_ci_low = nonclutch_ci_low * 1.5,
         nonclutch_ci_high = nonclutch_ci_high * 1.5) %>%
  select(-c(clutch_exp, clutch_var, nonclutch_exp, nonclutch_var))

# Who are the clutchest players and biggest chokers?
clutch_players <- shooting %>%
  arrange((p_val)) %>%
  filter(p_val <= 0.1)
choke_players <- shooting %>%
  arrange(desc(p_val)) %>%
  filter(p_val >= 1.9)

# Same thing as above, but for opponents
opp <- df %>%
  group_by(opponent) %>%
  summarize(nonclutch_shots = sum(ifelse(shot_type > 1 & potential_wpa < 0.075 & potential_wpa > 0, 1, 0)),
            nonclutch_eFG = sum(ifelse(shot_type == 2 & shot_outcome == "made" & potential_wpa < 0.075 & potential_wpa > 0, 1, ifelse(shot_type == 3 & shot_outcome == "made" & potential_wpa < 0.075 & potential_wpa > 0, 1.5, 0))) / nonclutch_shots,
            clutch_shots = sum(ifelse(shot_type > 1 & potential_wpa >= 0.075, 1, 0)),
            clutch_eFG = sum(ifelse(shot_type == 2 & shot_outcome == "made" & potential_wpa >= 0.075, 1, ifelse(shot_type == 3 & shot_outcome == "made" & potential_wpa >= 0.075, 1.5, 0))) / clutch_shots) %>%
  mutate(nonclutch_eFG = nonclutch_eFG / 1.5,
         clutch_eFG = clutch_eFG / 1.5) %>%
  filter(nonclutch_shots > 0,
         clutch_shots > 0) %>%
  mutate(clutch_exp = clutch_eFG * clutch_shots,
         clutch_var = clutch_exp * (1-clutch_eFG),
         nonclutch_exp = nonclutch_eFG * nonclutch_shots,
         nonclutch_var = nonclutch_exp * (1-nonclutch_eFG)) %>%
  filter(nonclutch_shots >= 800)

opp %<>%
  mutate(clutch_ci_low = 0,
         clutch_ci_high = 0,
         nonclutch_ci_low = 0,
         nonclutch_ci_high = 0,
         p_val = 0)

for(i in 1:nrow(opp)){
  makes_clutch <- 0:opp$clutch_shots[i]
  makes_non_clutch <- 0:opp$nonclutch_shots[i]
  
  probs_clutch <- dbinom(makes_clutch, opp$clutch_shots[i], opp$clutch_eFG[i])
  probs_non_clutch <- dbinom(makes_non_clutch, opp$nonclutch_shots[i], opp$nonclutch_eFG[i])
  # par(mfrow = c(1, 2)) # Side-by-side plots
  # 
  # plot(makes_clutch, probs_clutch, type = "h", lwd = 2,
  #      main = "Clutch opp Distribution",
  #      xlab = "Makes", ylab = "Probability")
  # 
  # plot(makes_non_clutch, probs_non_clutch, type = "h", lwd = 2,
  #      main = "Non-Clutch opp Distribution",
  #      xlab = "Makes", ylab = "Probability")

  ci_clutch <- prop.test(opp$clutch_shots[i] * opp$clutch_eFG[i], opp$clutch_shots[i])$conf.int
  
  ci_nonclutch <- prop.test(opp$nonclutch_shots[i] * opp$nonclutch_eFG[i], opp$nonclutch_shots[i])$conf.int
  opp$clutch_ci_low[i] <- ci_clutch[1]
  opp$clutch_ci_high[i] <- ci_clutch[2]
  opp$nonclutch_ci_low[i] <- ci_nonclutch[1]
  opp$nonclutch_ci_high[i] <- ci_nonclutch[2]
  
  test_res <- prop.test(
    x = c(opp$clutch_shots[i] * opp$clutch_eFG[i], opp$nonclutch_shots[i] * opp$nonclutch_eFG[i]),
    n = c(opp$clutch_shots[i], opp$nonclutch_shots[i]),
    correct = FALSE  # Setting to FALSE to avoid Yates' continuity correction
  )
  opp$p_val[i] <- test_res$p.value
}

opp %<>%
  mutate(p_val = ifelse(clutch_eFG < nonclutch_eFG, 2 - p_val, p_val),
         nonclutch_eFG = nonclutch_eFG * 1.5,
         clutch_eFG = clutch_eFG * 1.5,
         clutch_ci_low = clutch_ci_low * 1.5,
         clutch_ci_high = clutch_ci_high * 1.5,
         nonclutch_ci_low = nonclutch_ci_low * 1.5,
         nonclutch_ci_high = nonclutch_ci_high * 1.5) %>%
  select(-c(clutch_exp, clutch_var, nonclutch_exp, nonclutch_var))

# Clutchest Teams and Biggest Chokers
clutch_opp <- opp %>%
  arrange(desc(p_val)) %>%
  filter(p_val >= 1.9)
choke_opp <- opp %>%
  arrange((p_val)) %>%
  filter(p_val <= 0.1)