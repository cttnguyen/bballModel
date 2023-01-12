library(tidyverse)
library(rvest)

get_bracket <- function(year){
  Sys.sleep(3)

  if(year == "2020"){
    warning("Tournament cancelled in 2020")
    return(tibble())
  }

  #webscrape bracket from espn website
  site_path <- "http://www.espn.com/mens-college-basketball/tournament/bracket/_/id/%s22/%s-ncaa-tournament"
  site <- sprintf(site_path, year, year) %>%
    read_html() %>%
    html_elements("dl")

  #read scores from the dd class (definition)
  scores <- site %>%
    html_elements("dd") %>%
    html_text2() %>% #scores on each game
    map(str_split, pattern = "\\n", simplify = TRUE, n = 2) %>% #one element per team per game
    unlist() %>%
    as.numeric()
  #read team names and seed from dt class (term)
  team_seed <- site %>%
    html_elements("dt") %>%
    html_text2() %>% #seed and team for each game
    {if(year == "2021"){#this is the year VCU couldn't play due to COVID so Oregon proceeded
      #remove the VCU vs Oregon element
      discard(
        .,
        str_detect,
        pattern = "[0-9]* (Oregon|VCU)\\n[0-9]* (Oregon|VCU)"
      )
    } else .} %>%
    map(str_split, pattern = "\\n", simplify = TRUE, n = 2) %>% #one element per team per game
    unlist() %>%
    map_dfr(
      ~ tibble(
        seed = str_extract(., "^[0-9]*") %>% as.numeric(), #extract the numbers (seed)
        name = str_remove_all(., "^[0-9]* ") #remove the numbers, you're left with the name
      )
    )

  #get total number of games in the tournament
  n_games <- length(scores) / 2 #dynamic per season

  #compile and clean up the data in wide format
  df <- team_seed %>%
    mutate(
      score = scores,
      #keys for pivoting data for each game
      team = rep(paste0("team", 1:2), n_games),
      match = rep(1:n_games, each = 2)
    ) %>%
    pivot_wider(
      id_cols = match,
      names_from = team,
      values_from = c(name, score, seed),
      names_glue = "{team}_{.value}"
    ) %>%
    mutate(
      team1_win = (team1_score > team2_score), #define the game winner
      year = year #add year for mashing together later
    ) %>%
    select(-match)

  return(df)

}

tournament_data <- 2003:2022 %>%
  as.character() %>%
  map_dfr(get_bracket)



usethis::use_data(tournament_data, overwrite = TRUE)
