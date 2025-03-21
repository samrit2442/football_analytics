strengths

col1 <- c("#005830", "#29479f", "#6babdc", "#fddb00",
          "#f47b2c", "#c24533", "#a6234f", "#1c4792",
          "#2b2a71", "#f8ec4a", "#623d94", "#8769cc",
          "#e12529", "#f32735")

col2 <- c("#a82d32", "#ed212a", "#ffffff", "#231f20",
          "#1c2453", "#394a7f", "#1e2a5c", "#fcb813",
          "#e30613", "#013ca6", "#f6991f", "#313846",
          "#221f1f", "#ff9e18")



strengths <- strengths |>
  dplyr::mutate(gradient_start = col1,
                gradient_end = col2,
                orient = 'horizontal')


ggplot(strengths, aes(x = reorder(Team, Strength), y = Strength, fill = Team)) +
  geom_bar_pattern(
    aes(pattern_fill = I(gradient_start), 
        pattern_fill2 = I(gradient_end),
        pattern_orientation = I(orient)),
    stat = 'identity',
    pattern = "gradient",
    color = 'black') +
  coord_flip() +
  theme(legend.position = "none",
        text = element_text(family = "poppins"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 15),
        # plot.background = element_rect(fill = "#fdd3d3", color = NA),
        # panel.background = element_rect(fill = "#c7faaf", color = NA),
        panel.grid.minor = element_line(colour = "gray40")) +
  geom_text(aes(label = round(Strength, 2)), hjust = -0.1, size = 6.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  labs(title = "ISL Teams' Strength Rankings Based on Bradley-Terry Model",
       subtitle = "Analyzing Team Performance through Statistical Ranking",
       x = "ISL Teams", 
       y = "Strength",
       caption = "Data collected till March 2023")



plot_df <- data.frame(
  xmin    = c(0, 10, 3),
  xmax    = c(8, 18, 4),
  ymin    = c(0, 10, 8),
  ymax    = c(5, 19, 15),
  type    = c('a', 'b', 'c'),
  colour1 = c('red', 'black', 'blue'),
  colour2 = c('black', NA, 'yellow'),
  orient  = c('horizontal', 'radial', 'vertical'),
  stringsAsFactors = FALSE)





