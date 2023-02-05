library(tidyverse)

load(here::here("data", "season_df.rda"))
load(here::here("data", "tournament_df.rda"))

#for testing only
load(here::here("data", "all_schedules.rda"))
season_df <- season_df %>%
  left_join(
    all_schedules %>%
      filter(year %in% 2018),
    by = c("team_id", "game_id" = "game_ids")
  )
tournament_df <- filter(
  tournament_df,
  year %in% as.character(2018)
)

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
  group_by(year, game_id, team_id) %>%
  summarize(
    across(
      all_of(main_stats),
      sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(game_id) %>%
  mutate(
    team = 1:n()
  ) %>%
  ungroup() %>%
  {
    left_join(
      .,
      transmute(., year, game_id, PTS_opp = PTS) %>%
        group_by(game_id) %>%
        mutate(team = n():1) %>%
        ungroup(),
      by = c("year", "game_id", "team")
    )
  } %>%
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
#no longer have all games, so don't have data on how well opposing team did against other teams
# opposing_stats <- game_stats %>%
#   rename_with(
#       ~ paste0("opp_", .),
#       c(FGM, FGA, FG_pct, PTS)
#   ) %>%
#   group_by(season, opponent) %>% #collect all games for a given opponent
#   summarize( #calculate mean for FG stats
#     across(
#       starts_with("opp_"),
#       mean,
#       na.rm = TRUE
#     ),
#     .groups = "drop"
#   ) %>%
#   rename(team = opponent) #rename to merge later

#Unique season starters
unique_starters <- season_df %>%
  filter(starter) %>%
  distinct(year, team_id, player_id) %>%
  count(year, team_id, name = "num_starters")

# sd across players on the team in games that they played
  #games not played are not in the denominator
player_stats <- season_df %>%
  mutate(
    FG_pct = FGM / FGA,
    `3PT_pct` = `3PTM` / `3PTA`,
    FT_pct = FTM / FTA
  ) %>%
  group_by(year, team_id, player_id) %>%
  summarize( #avg game-stats for each player
    across(
      all_of(c(main_stats, pct_stats)),
      mean,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(year, team_id) %>%
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
  group_by(year, team_id) %>%
  summarize( #calculate season stats for each team
    across(
      all_of(c(main_stats, pct_stats, "win", "PTS_opp")),
      mean,
      na.rm = TRUE,
      .names = "{.col}_t.mean"
    ),
    .groups = "drop"
  )

#team ids
team_df <- tournament_df %>%
  select(-score) %>%
  mutate(
    year = as.integer(year),
    team_id = as.character(team_id)
  ) %>%
  reduce(
    .x = list(
      team_stats,
      # opposing_stats,
      player_stats,
      unique_starters
    ),
    .f = left_join,
    by = c("year", "team_id"),
    .init = .
  ) %>%
  pivot_wider(
    id_cols = c(year, match),
    names_from = team,
    values_from = c(-year, -match, -team),
    names_glue = "{team}_{.value}"
  )


usethis::use_data(df, overwrite = TRUE)
