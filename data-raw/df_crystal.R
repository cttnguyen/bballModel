library(tidyverse)
library(ncaahoopR)

teams <- ids %>%
  mutate(
    divvy = case_when(
      team == "Northwestern St" ~ 1,
      team == "Youngstown St" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  fill(divvy, .direction = "up") %>%
  filter(divvy == 1) %>%
  pull(team)
seasons <- as.character(2002:2021) %>%
  paste0("-") %>%
  paste0(
    str_pad(
      string = 3:22,
      width = 2,
      side = "left",
      pad = "0"
    )
  )

# df_crystal <- tibble()
# usethis::use_data(df_crystal)

pb <- progress::progress_bar$new(total = length(teams) * length(seasons))
expand_grid(
  teams,
  seasons
) %>%
  as.list() %>%
  pwalk(
    function(teams, seasons){
      load(here::here("data", "df_crystal.rda"))
      print(paste(teams, seasons))
      pb$tick()
      df_temp <- tibble()
      suppressMessages(try(
        {
          df_temp <- season_boxscore(
            teams,
            seasons,
            "raw"
          ) %>%
            mutate(season = seasons)
        },
        silent = TRUE
      ))
      df_crystal <- bind_rows(df_crystal, df_temp)
      suppressMessages(usethis::use_data(df_crystal, overwrite = TRUE))
    },
    .progress = TRUE
  )
