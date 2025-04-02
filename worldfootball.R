library(pacman)
p_load(tidyverse, readxl, worldfootballR)

mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)


team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
# if it's not a league in the stored leagues data in worldfootballR_data repo:
league_one_teams <- tm_league_team_urls(start_year = 2020, 
                                        league_url = "https://www.transfermarkt.com/league-one/startseite/wettbewerb/GB3")

team_urls_ind <- tm_league_team_urls(country_name = "India", 
                                     start_year = 2024)
isl_teams <- tm_league_team_urls(league_url = "https://www.transfermarkt.com/indian-super-league/startseite/wettbewerb/IND1", start_year = 2024)

#----- for one team: -----#
bayern <- tm_team_transfers(team_url = "https://www.transfermarkt.com/fc-bayern-munchen/startseite/verein/27/saison_id/2020", transfer_window = "all")
dplyr::glimpse(bayern)

#----- or for multiple teams: -----#
# team_urls <- tm_league_team_urls(country_name = "England", start_year = 2020)
# epl_xfers_2020 <- tm_team_transfers(team_url = team_urls, transfer_window = "all")

mbsg <- tm_team_transfers(team_url = isl_teams[1], transfer_window = "all")
mbsg

mbsg_sq_stat <- tm_squad_stats(team_url = isl_teams[1])
mbsg_sq_stat

ebfc_sq_stat <- tm_squad_stats(team_url = isl_teams[4])
ebfc_sq_stat


