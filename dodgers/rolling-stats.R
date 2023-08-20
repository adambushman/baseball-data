library('tidyverse')


# Directory
here::i_am("dodgers/rolling-stats.R")


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
  mutate(
    Dodgers = case_when(
      stringr::str_detect(Team, "Los Angeles") & stringr::str_detect(Level, "Maj-NL") ~ TRUE,
      TRUE ~ FALSE
    ), 
    SS = SB - CS
  ) %>%
  select(
    Name, Dodgers, 
    PA, HR, RBI, SS, 
    OBP, SLG, OPS
  ) %>%
  mutate(across(
    .cols =  -c(Name, Dodgers), 
    .fns = percent_rank, 
    .names = "{.col}_tile"
  )) %>%
  filter(Dodgers)


Dodgers_pitching <- 
  pitching %>%
  mutate(
    Dodgers = case_when(
      stringr::str_detect(Team, "Los Angeles") & stringr::str_detect(Level, "Maj-NL") ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  select(
    Name, Dodgers, 
    IP, ERA, WHIP, BAbip
  ) %>%
  mutate(across(
    -c(Name,Dodgers), 
    ~ (1 - percent_rank(.)), 
    .names = "{.col}_tile"
  )) %>%
  filter(Dodgers)


# {camcorder} setup

camcorder::gg_record(
  dir = here::here("camcorder"), 
  device = "PNG", 
  height = 12, 
  width = 10, 
  units = "cm", 
  dpi = 300
)

# Data visualization

Dodgers_hitting_viz <- 
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
    aes(xmin = -0.07, xmax = 0.5, ymin = -0.07, ymax = 0.5), 
    fill = pluck(DodgersViz, 1, "team_color2"), 
    alpha = 0.25
  ) +
  geom_rect(
    aes(xmin = 0.5, xmax = 1.07, ymin = 0.5, ymax = 1.07), 
    fill = pluck(DodgersViz, 1, "team_color"), 
    alpha = 0.25
  ) +
  mlbplotR::geom_mlb_logos(
    aes(0.12, 0.88, team_abbr = team_abbr), 
    teamviz %>% filter(team_abbr == "LAD"), 
    height = 0.2, 
    alpha = 0.5
  ) +
  geom_point(
    aes(OPS_tile, PA_tile), 
    Dodgers_hitting
  ) +
  ggrepel::geom_text_repel(
    aes(OPS_tile, PA_tile, label = Name), 
    Dodgers_hitting, 
    size = 2.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(), 
    limits = c(-0.07, 1.07), 
    expand = c(0,0)
  ) +
  scale_x_continuous(
    labels = scales::label_percent(), 
    limits = c(-0.07, 1.07), 
    expand = c(0,0)
  ) +
  annotate(
    "text", x = -0.03, y = -0.03, hjust = 0, size = 2.25, 
    color = pluck(DodgersViz, 1, "team_color2"), 
    label = "Low Volume, Bad Results"
  ) +
  annotate(
    "text", x = 1.03, y = 1.03, hjust = 1, size = 2.25, 
    color = pluck(DodgersViz, 1, "team_color"), 
    label = "High Volume, Good Results"
  ) +
  labs(
    title = "Past 60 Day Hitting Results",
    subtitle = "League-wide percentiles over time period",
    caption = "Accessed via {baseballr} | Designed by @adam_bushman", 
    x = "OPS %tile", 
    y = "Plate Appearance %tile"
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



Dodgers_pitching_viz <- 
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
    aes(xmin = -0.07, xmax = 0.5, ymin = -0.07, ymax = 0.5), 
    fill = pluck(DodgersViz, 1, "team_color2"), 
    alpha = 0.25
  ) +
  geom_rect(
    aes(xmin = 0.5, xmax = 1.07, ymin = 0.5, ymax = 1.07), 
    fill = pluck(DodgersViz, 1, "team_color"), 
    alpha = 0.25
  ) +
  mlbplotR::geom_mlb_logos(
    aes(0.12, 0.88, team_abbr = team_abbr), 
    teamviz %>% filter(team_abbr == "LAD"), 
    height = 0.2, 
    alpha = 0.5
  ) +
  geom_point(
    aes(ERA_tile, IP_tile), 
    Dodgers_pitching
  ) +
  ggrepel::geom_text_repel(
    aes(ERA_tile, IP_tile, label = Name), 
    Dodgers_pitching, 
    size = 2.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(), 
    limits = c(-0.07, 1.07), 
    expand = c(0,0)
  ) +
  scale_x_continuous(
    labels = scales::label_percent(), 
    limits = c(-0.07, 1.07), 
    expand = c(0,0)
  ) +
  annotate(
    "text", x = -0.03, y = -0.03, hjust = 0, size = 2.25, 
    color = pluck(DodgersViz, 1, "team_color2"), 
    label = "Low Volume, Bad Results"
  ) +
  annotate(
    "text", x = 1.03, y = 1.03, hjust = 1, size = 2.25, 
    color = pluck(DodgersViz, 1, "team_color"), 
    label = "High Volume, Good Results"
  ) +
  labs(
    title = "Past 60 Day Pitching Results",
    subtitle = "League-wide percentiles over time period",
    caption = "Accessed via {baseballr} | Designed by @adam_bushman", 
    x = "ERA %tile", 
    y = "Innings Pitched %tile"
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

Dodgers_hitting_viz

Dodgers_pitching_viz


