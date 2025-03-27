library(pacman)
p_load(tidyverse, readxl, BradleyTerry2, ggtext, showtext, ggpattern, 
       systemfonts, grid, magick, writexl)

# Read in the data
isl_raw <- read_excel("./data/Copy of isl_raw_data(1.9.9).xlsx")
isl_raw2 <- isl_raw |> 
  select(-time)

isl_raw2 |> head()

isl_raw3 <- isl_raw2 |> 
  mutate_all(as.character) |>
  pivot_longer(cols = starts_with('home_team_scorer') | starts_with('away_team_scorer'),
               names_to = c('.value', 'score_number'),
               names_sep = "_0") |> 
  dplyr::select(date, venue, home_team, away_team, home_team_score, away_team_score,
                home_team_scorer, home_team_scorer_time,
                away_team_scorer, away_team_scorer_time)

writexl::write_xlsx(isl_raw3, './data/isl_raw3.xlsx')


