library('tidyverse')



# {baseballr} data

hitting <- baseballr::bref_daily_batter(
  Sys.Date()-60, Sys.Date()
)

pitching <- baseballr::bref_daily_pitcher(
  Sys.Date()-60, Sys.Date()
)

headshots <- mlbplotR::load_headshots()

teamviz <- mlbplotR::load_mlb_teams() %>%
  filter(!team_abbr %in% c("AL", "NL", "MLB"))

DodgersViz <- teamviz %>%
  filter(team_abbr == "LAD") %>%
  purrr::transpose()


# Data transformation

Dodgers_hitting <- 
  hitting %>%
  left_join(headshots, by = "bbref_id") %>%
  mutate(
    Dodgers = case_when(
      Team == "Los Angeles" & Level == "Maj-NL" ~ TRUE,
      TRUE ~ FALSE
    ), 
    SS = SB - CS
  ) %>%
  select(
    savant_id, Name, espn_headshot, Dodgers, 
    PA, HR, RBI, SS, 
    OBP, SLG, OPS
  ) %>%
  mutate(across(
    .cols =  -c(savant_id, Name, espn_headshot, Dodgers), 
    .fns = percent_rank, 
    .names = "{.col}_tile"
  )) %>%
  filter(Dodgers)



# {camcorder} setup

camcorder::gg_record(
  dir = "C:/Users/adamb/OneDrive/Pictures/Camcorder", 
  device = "PNG", 
  height = 12, 
  width = 10, 
  units = "cm", 
  dpi = 300
)

# Data visualization

ggplot() +
  geom_hline(
    yintercept = 0.5, linewidth = 0.6, 
    color = pluck(DodgersViz, 1, "team_color3")
  ) + 
  geom_vline(
    xintercept = 0.5, linewidth = 0.6, 
    color = pluck(DodgersViz, 1, "team_color3")
  ) + 
  geom_rect(
    aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5), 
    fill = pluck(DodgersViz, 1, "team_color2"), 
    alpha = 0.25
  ) +
  geom_rect(
    aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1), 
    fill = pluck(DodgersViz, 1, "team_color"), 
    alpha = 0.25
  ) +
  geom_point(
    aes(OPS_tile, PA_tile), 
    Dodgers_hitting
  ) +
  mlbplotR::geom_mlb_logos(
    aes(0.12, 0.88, team_abbr = team_abbr), 
    teamviz %>% filter(team_abbr == "LAD"), 
    height = 0.2
  ) +
  ggrepel::geom_text_repel(
    aes(OPS_tile, PA_tile, label = Name), 
    Dodgers_hitting, 
    size = 2.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(), 
    expand = c(0,0)
  ) +
  scale_x_continuous(
    labels = scales::label_percent(), 
    expand = c(0,0)
  ) +
  labs(
    title = "Past 60 Day Hitting Results",
    subtitle = "League-wide percentiles over time period",
    caption = "Accessed via {baseballr} | Designed by @adam_bushman", 
    x = "OPS %tile", 
    y = "PA %tile"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA), 
    plot.margin = margin(0.5, 0.5, 0.4, 0.5, "cm"), 
    plot.title.position = "plot", 
    panel.grid = element_line(color = pluck(DodgersViz, 1, "team_color3")), 
    panel.grid.major = element_line(linewidth = 0.1), 
    panel.grid.minor = element_line(linewidth = 0.1), 
    plot.title = element_text(
      color = pluck(DodgersViz, 1, "team_color"), 
      face = "bold", 
      margin = margin(0, 0, 7, 0)
    ), 
    plot.subtitle = element_text(
      color = pluck(DodgersViz, 1, "team_color3"), 
      face = "italic", 
      margin = margin(0, 0, 15, 0)
    ), 
    plot.caption = element_text(
      color = pluck(DodgersViz, 1, "team_color3"), 
      size = 6, 
      margin = margin(5, 0, 0, 0)
    ), 
    axis.title = element_text(face = "bold", size = 7), 
    axis.title.x = element_text(margin = margin(7, 0, 0, 0)), 
    axis.text = element_text(size = 6)
  )


