library(httr)
library(jsonlite)

#from the base ESPN API, retrieve a given query and convert to JSON and then to a list/tibble
read_api <- function(ext = NULL){
  "http://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/" %>%
    paste0(ext) %>%
    GET() %>%
    pluck("content") %>%
    rawToChar() %>%
    fromJSON()
}




#obtain all team names and ids
all_teams <- read_api("teams?groups=400&limit=400") %>%
  pluck(
    "sports",
    "leagues", 1,
    "teams", 1,
    "team"
  ) %>%
  select(
    id, slug, abbreviation,
    displayName, shortDisplayName, name, nickname, location
  )

#obtain all game ids for each team-season
all_schedules <- expand_grid(
  id = all_teams$id,
  year = 2003:2022
)
pb <- progress::progress_bar$new(total = nrow(all_schedules))
all_schedules <- all_schedules %>%
  pmap_dfr(
   function(id, year) {
     pb$tick()
     sprintf("teams/%s/schedule?season=%s", id, year) %>%
       read_api() %>%
       pluck("events", "id") %>%
       as_tibble_col("game_ids") %>%
       mutate(year = year, team_id = id)
   },
   .progress = TRUE
  )

#filter game ids only to teams that made it to the tournament in each season
load(here::here("data", "tournament_df.rda"))
tourn_teams <- tournament_df %>%
  distinct(team_id, year) %>%
  mutate(
    year = as.integer(year),
    team_id = as.character(team_id)) %>%
  left_join(
    all_schedules,
    by = c("year", "team_id")
  )

#Rename list of vectors nested in a dataframe
nested_stats <- function(x, nm){
  if(length(x) == 0){
    return(x)
  } else {
    set_names(x, nm) %>%
      enframe() %>%
      pivot_wider(everything()) %>%
      mutate(
        across(
          any_of(c("FG", "3PT", "FT")),
          list(
            M = ~ str_extract(., "^.*(?=-)"),
            A = ~ str_extract(., "(?<=-).*$")
          ),
          .names = "{.col}{.fn}"
        )
      ) %>%
      select(-any_of(c("FG", "3PT", "FT")))
  }
}
#take JSON df data (nested) at the game-id level and break it down to the player level
game2player_json <- function(data){
  player_stats <- c(
    "MIN", "TO", "PF",
    "OREB", "DREB", "REB",
    "AST", "STL", "BLK",
    "PTS", "FGM", "FGA", "3PTM", "3PTA", "FTM", "FTA")
  data %>%
    unnest(c(team, statistics)) %>%
    transmute(
      team_id = id, team = location,
      names, athletes
    ) %>%
    unnest(athletes) %>%
    filter(!didNotPlay) %>%
    mutate(
      stats = map2(
        stats,
        names,
        ~nested_stats(.x, .y)
      )
    ) %>%
    unnest_wider(athlete) %>%
    transmute(
      team_id, team,
      player_id = id, shortName,
      position = pluck(position, "abbreviation"),
      starter, stats
    ) %>%
    unnest_wider(
      col = stats
    ) %>%
    mutate(
      across(
        any_of(player_stats),
        as.numeric
      )
    )

}

#obtain all the player-level stats for given game ids
pb <- progress::progress_bar$new(total = nrow(tourn_teams))
season_df <- tourn_teams[["game_ids"]] %>%
  map_dfr(
    function(game_id){
      pb$tick()
      players <- sprintf("summary?event=%s", game_id) %>%
        read_api() %>%
        pluck("boxscore", "players") %>%
        {tryCatch(
          game2player_json(.),
          error = function(x) tibble()
        )} %>%
        mutate(game_id = game_id)
    }
  )

usethis::use_data(season_df, overwrite = TRUE)
