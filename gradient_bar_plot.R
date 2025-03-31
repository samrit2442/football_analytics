strengths

col1 <- c("#005830", "#6babdc", "#29479f", "#f47b2c", "#bf3428",
          "#a6234f", "#f8ec4a", "#2b2a71", "#8769cc", "#1c4792",
          "#fddb00", "#623d94", "#db4a27", "#e12529", "#f32735")

col2 <- c("#a82d32", "#ffffff", "#ed212a", "#1c2453", "#2b3558",
          "#394a7f", "#013ca6", "#e30613", "#1e2a5c", "#fcb813",
          "#231f20", "#f6991f", "#ae2f24", "#313846", "#ff9e18")



# imgs <- paste0("<img src='", image_files, "'/>")

logos <- c("mbsg", "mcfc", "bfc", "fcg", "atk", "jfc",
            "kbfc", "ddfc", "ofc", "cfc", "hfc", "pcfc",
            "pfc", "neufc", "ebfc")

# team_logos <- paste0("<img src='file://", getwd(), "/logos/", logos, ".png' width='25' height='25'/>")
team_logos <- paste0("<img src='./logos/", logos, ".png' width='30' height='30'/>")

strengths <- strengths |>
  dplyr::mutate(gradient_start = col1,
                gradient_end = col2,
                orient = 'horizontal',
                logos = team_logos)


isl_plot <- ggplot(strengths, aes(x = reorder(Team, Strength), y = Strength, fill = Team)) +
  geom_bar_pattern(
    aes(pattern_fill = I(gradient_start), 
        pattern_fill2 = I(gradient_end),
        pattern_orientation = I(orient)),
    stat = 'identity',
    pattern = "gradient",
    color = 'black') +
  coord_flip() +
  scale_x_discrete(name = NULL, labels = rev(strengths$logos)) +
  theme(legend.position = "none",
        text = element_text(family = "poppins"),
        # axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 15),
        # axis.text.x = element_text(size = 20),
        axis.text.y = element_markdown(),
        # plot.background = element_rect(fill = "#fdd3d3", color = NA),
        # panel.background = element_rect(fill = "#c7faaf", color = NA),
        panel.grid.minor = element_line(colour = "gray40")) +
  geom_text(aes(label = round(Strength, 2)), hjust = -0.1, size = 6.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  labs(title = "ISL Teams' Strength Rankings Based on Bradley-Terry Model",
       subtitle = "Analyzing Team Performance through Statistical Ranking",
       x = "ISL Teams", 
       y = "Strength",
       caption = "Data collected till ISL Season 2023-24")
isl_plot
ggsave("./plots/ISL_Teams_Strength_Rankings2.svg", isl_plot,
       dpi = 900, limitsize = T, width = 15, height = 11, units = "in")

# plot_df <- data.frame(
#   xmin    = c(0, 10, 3),
#   xmax    = c(8, 18, 4),
#   ymin    = c(0, 10, 8),
#   ymax    = c(5, 19, 15),
#   type    = c('a', 'b', 'c'),
#   colour1 = c('red', 'black', 'blue'),
#   colour2 = c('black', NA, 'yellow'),
#   orient  = c('horizontal', 'radial', 'vertical'),
#   stringsAsFactors = FALSE)





