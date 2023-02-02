library(tidyverse)

load(here::here("data", "season_df.rda"))
load(here::here("data", "tournament_data.rda"))

#for testing only
# season_df <- filter(
#   season_df,
#   str_detect(season, "04|05|06")
# )
# tournament_data <- filter(
#   tournament_data,
#   year %in% as.character(2004:2006)
# )

main_stats <- c(
  "FGM", "FGA",
  "3PTM", "3PTA",
  "FTM", "FTA",
  "OREB", "DREB",
  "AST", "STL", "BLK",
  "TO", "PF", "PTS"
)
pct_stats <- paste0(c("FG", "3PT", "FT"), "_pct")

#Game stats
game_stats <- season_df %>%
  group_by(season, game_id, team, opponent) %>%
  summarize(
    across(
      all_of(main_stats),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    transmute(
      season_df,
      season, game_id, team, PTS_opp = PTS
    ),
    by = c("season", "opponent" = "team", "game_id")
  ) %>%
  mutate(
    FG_pct = FGM / FGA,
    `3PT_pct` = `3PTM` / `3PTA`,
    FT_pct = FTM / FTA,
    win = case_when(
      PTS > PTS_opp ~ 1,
      PTS < PTS_opp ~ 0,
      PTS == PTS_opp ~ 0.5,
      TRUE ~ NA_real_
    )
  )



#stats from opposing teams
opposing_stats <- game_stats %>%
  rename_with(
      ~ paste0("opp_", .),
      c(FGM, FGA, FG_pct, PTS)
  ) %>%
  group_by(season, opponent) %>% #collect all games for a given opponent
  summarize( #calculate mean for FG stats
    across(
      starts_with("opp_"),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename(team = opponent) #rename to merge later

#Unique season starters
unique_starters <- season_df %>%
  filter(starter) %>%
  distinct(season, team, player) %>%
  count(season, team, name = "num_starters")

# sd across players on the team in games that they played
  #games not played are not in the denominator
player_stats <- season_df %>%
  mutate(
    FG_pct = FGM / FGA,
    `3PT_pct` = `3PTM` / `3PTA`,
    FT_pct = FTM / FTA
  ) %>%
  group_by(season, team, player_id) %>%
  summarize( #avg game-stats for each player
    across(
      all_of(c(main_stats, pct_stats)),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(season, team) %>%
  summarize( #sd of the avg game_stats across players on a team
    across(
      all_of(c(main_stats, pct_stats)),
      sd,
      na.rm = TRUE,
      .names = "{.col}_p.sd"
    ),
    .groups = "drop"
  )


# team stats
team_stats <- game_stats %>%
  group_by(season, team) %>%
  summarize( #calculate season stats for each team
    across(
      all_of(c(main_stats, pct_stats, "win", "PTS_opp")),
      mean,
      na.rm = TRUE,
      .names = "{.col}_t.mean"
    ),
    .groups = "drop"
  )

team_df <- list(
  opposing_stats,
  player_stats,
  unique_starters
) %>%
  reduce(
    left_join,
    by = c("season", "team"),
    .init = team_stats
  )

df <- tournament_data %>%
  mutate(
    season = sprintf(
      "%s-%s",
      as.numeric(year) - 1,
      str_sub(year, -2)
  )) %>%
  left_join(
    team_df,
    by = c("season", "team1_name" = "team")
  ) %>%
  left_join(
    team_df,
    by = c("season", "team2_name" = "team"),
    suffix = c("_team1", "_team2")
  )

usethis::use_data(df, overwrite = TRUE)
