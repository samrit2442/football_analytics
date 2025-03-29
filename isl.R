library(pacman)
p_load(tidyverse, readxl, BradleyTerry2, ggtext, showtext, ggpattern, 
       systemfonts, grid, magick)

# Read in the data
isl_raw <- read_excel("./data/Copy of isl_raw_data(1.9.9).xlsx")
isl_raw2 <- isl_raw |> 
  select(-time)

# is.na(isl_raw2) |> colSums() |> as.data.frame() |> View()

# Clean the data
# atk_home_goals <- isl_raw2 |> 
#   filter(home_team == "Atletico de Kolkata") |>
#   summarise(total_home_goals = sum(home_team_score))
  

names(isl_raw2)
dat <- isl_raw2[1:829, c(3,4,6,7)]

standings <- dat |> 
  pivot_longer(cols = c(home_team, away_team), 
               names_to = "venue", values_to = "team") |> 
  mutate(goals_scored = if_else(venue == "home_team", home_team_score, away_team_score),
         goals_conceded = if_else(venue == "home_team", away_team_score, home_team_score),
         points = case_when(goals_scored > goals_conceded ~ 3,  # Win
                            goals_scored == goals_conceded ~ 1, # Draw
                            TRUE ~ 0)) |> 
  group_by(team) |> 
  summarize(
    matches_played = n(),
    wins = sum(points == 3),
    draws = sum(points == 1),
    losses = sum(points == 0),
    goals_scored = sum(goals_scored),
    goals_conceded = sum(goals_conceded),
    goal_difference = goals_scored - goals_conceded,
    points = sum(points)
  )


results <- dat |> 
  filter(away_team != 'M') |> 
  mutate(
    winner = ifelse(home_team_score > away_team_score, home_team,
                    ifelse(away_team_score > home_team_score, away_team, "Draw")),
    opponent = ifelse(winner == home_team, away_team,
                      ifelse(winner == away_team, home_team, NA))) |> 
  filter(winner != "Draw") |> 
  select(winner, opponent) |> 
  mutate(winner_fac = factor(winner, levels = unique(c(winner, opponent))),
         opponent_fac = factor(opponent, levels = unique(c(winner, opponent))))


# results$winner_fac <- factor(results$winner, levels = unique(c(results$winner, results$opponent)))
# results$opponent_fac <- factor(results$opponent, levels = unique(c(results$winner, results$opponent)))



# Fit the Bradley-Terry model
model <- BTm(outcome = rep(1, nrow(results)), winner_fac, opponent_fac, data = results)

# Summarize the model
summary(model)

# Extract coefficients
coef_ATK <- 0  # Reference category ATK
coef_NEFC <- coef(model)["..NorthEast United FC"]
coef_HFC <- coef(model)["..Hyderabad FC"]
coef_JFC <- coef(model)["..Jamshedpur FC"]
coef_OFC <- coef(model)["..Odisha FC"]
coef_BFC <- coef(model)["..Bengaluru FC"]
coef_CFC <- coef(model)["..Chennaiyin FC"]
coef_FCG <- coef(model)["..FC Goa"]
coef_KBFC <- coef(model)["..Kerala Blasters FC"]
coef_MCFC <- coef(model)["..Mumbai City FC"]
coef_EBFC <- coef(model)["..East Bengal FC"]
coef_MBSG <- coef(model)["..Mohun Bagan Super Giant"]
coef_PFC <- coef(model)["..Punjab FC"]


# Calculate strength parameters
strengths <- model$coefficients |> 
  as.data.frame() |>
  rownames_to_column(var = "Team") |>
  add_row(Team = "..Atletico de Kolkata", `model$coefficients` = 0) |>
  mutate(Strength = round(exp(`model$coefficients`), 3),
         Team = str_remove_all(Team, "\\..")) |> 
  select(Team, Strength) |>
  arrange(desc(Strength))

# Display strength parameters
print(strengths)

# Probability that MBSG is preferred over JFC
# lambda_MBSG <- strengths["Mohun Bagan Super Giant"]
# lambda_JFC <- strengths["Jamshedpur FC"]
# prob_MBSG_over_JFC <- lambda_MBSG / (lambda_MBSG + lambda_JFC)
# print(prob_MBSG_over_JFC)

font_add_google("Poppins", family = "poppins")
font_add_google("Inter", family = "inter")
showtext_auto()


stack_patterns <- function(...) {
  patterns <- list(...)
  
  # helper function to check for solid colours
  is_valid_colour <- function(x) {
    is(x, "character") &&
      (x %in% colours() ||
         grepl("^\\#[0-9a-fA-F]{6}$", x) ||
         grepl("^\\#[0-9a-fA-F]{8}$", x))
  }
  
  # check if any are not a pattern or colour
  stopifnot(
    "All supplied arguments must be patterns" =
      patterns |>
      sapply(\(x) is(x, "GridPattern") ||
               is_valid_colour(x)) |>
      all()
  )
  
  # wrap each gradient in a grob
  patterns |>
    lapply(\(x) grid::rectGrob(gp = grid::gpar(fill = x))) ->
    pattern_grobs
  
  # return as a compound pattern
  grid::pattern(
    do.call(grid::grobTree, pattern_grobs),
    extend = "none")
}




# gradient_start <- c("#008000", "#0000FF", "#62daf3", "#008000",
#                     "#df0e0e", "#ff8700", "#ef99eb", "#fdb006",
#                     "#670c4c", "#df0e0e", "#008000", "#62daf3",
#                     "#c66363", "#81cf0f")

# gradient_end <- c("#a51532", "#fa1717", "#ffffff", "#008000",
#                   "#df0e0e", "#ff8700", "#ef99eb", "#fdb006",
#                   "#670c4c", "#fff702", "#a51532", "#0000FF",
#                   "#c66363", "#81cf0f")

# strengths <- strengths |> 
#   mutate(gradient_start = gradient_start,
#          gradient_end = gradient_end)

# Plot the ranks
isl_ranks <- ggplot(strengths, aes(x = reorder(Team, Strength), y = Strength, fill = Team)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#ff0000", "#0000FF", "#063bfd", "#008000",
                               "#df0e0e", "#ff8700", "#ef99eb", "#fdb006",
                               "#670c4c", "#fff702", "#a51532", "#62daf3",
                               "#c66363", "#81cf0f")) +
  # geom_col_pattern(
  #   aes(pattern_fill = gradient_start, pattern_fill2 = gradient_end),
  #   pattern = "gradient",
  #   pattern_angle = 0,
  #   color = "black") +
  coord_flip() +
  labs(title = "ISL Teams' Strength Rankings Based on Bradley-Terry Model",
       subtitle = "Analyzing Team Performance through Statistical Ranking",
       x = "ISL Teams", 
       y = "Strength",
       caption = "Data collected till March 2023") +
  theme_minimal() +
  theme(text = element_text(family = "poppins"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 15),
        legend.position = "none",
        plot.background = element_rect(fill = "#fdd3d3", color = NA),
        panel.background = element_rect(fill = "#c7faaf", color = NA),
        panel.grid.minor = element_line(colour = "gray40")) +
  geom_text(aes(label = round(Strength, 2)), hjust = -0.1, size = 6.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1)
isl_ranks

# Save the plot
ggsave("ISL_Teams_Strength_Rankings.svg", isl_ranks,
       dpi = 900, limitsize = T, width = 15, height = 10, units = "in")
# ggsave("ISL_Teams_Strength_Rankings.png", isl_ranks,
#        dpi = 900, limitsize = T, width = 15, height = 10, units = "in")



# Read Data in Longer Format

isl_long <- read_excel("./data/isl_raw3.xlsx")

glimpse(isl_long)

isl_long_cleaned <- isl_long |> 
  dplyr::mutate(home_team_score = as.numeric(home_team_score),
                away_team_score = as.numeric(away_team_score))

to_remove <- isl_long |> 
  dplyr::mutate(home_team_score = as.numeric(home_team_score),
                away_team_score = as.numeric(away_team_score)) |> 
  dplyr::filter(home_team_score > 0 | away_team_score > 0) |> 
  dplyr::filter(is.na(home_team_scorer) & is.na(away_team_scorer))

isl_long_cleaned_final <- anti_join(isl_long_cleaned, to_remove)

# writexl::write_xlsx(isl_long_cleaned_final, './data/isl_long.xlsx')






  

