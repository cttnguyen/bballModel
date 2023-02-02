library(tidyverse)
library(ncaahoopR)

teams <- pluck(ids, "team")
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

season_df <- tibble()
usethis::use_data(season_df)

pb <- progress::progress_bar$new(total = length(teams) * length(seasons))
expand_grid(
  teams,
  seasons
) %>%
  as.list() %>%
  pwalk(
    function(teams, seasons){
      load(here::here("data", "season_df.rda"))
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
      season_df <- bind_rows(season_df, df_temp)
      suppressMessages(usethis::use_data(season_df, overwrite = TRUE))
    },
    .progress = TRUE
  )

