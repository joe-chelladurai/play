


library(rvest)
library(tidyverse)
library(ggbump)
library(ggh4x)
library(worldfootballR)


get_current_matchweek <- function() {
  url <- ("https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1")

  html <- read_html(url)


  text <- html_nodes(html, ".fl") |>
    html_children() |>
    html_attr("href")

  text2 <- stringr::str_remove(text, "/premier-league/spieltag/wettbewerb/GB1/spieltag/")

  text2 |>
    str_extract("^.{2}") |>
    as.numeric() |>
    mean()
}




df_total <- data.frame()
for (i in 1:get_current_matchweek()) {
  print(paste("Getting Matchweek:", i))
  data <- tm_matchday_table(country_name = "England", start_year = "2022", matchday = i)
  df_total <- rbind(df_total, data)
}

df_total <- df_total |>
  group_by(squad) |>
  distinct(matchday, .keep_all = TRUE) |>
  ungroup()





current_standings <- tm_matchday_table("England", "2022", get_current_matchweek())

current_standings <- current_standings |>
  mutate(team = fct_inorder(squad)) |>
  relocate(team, .after = "squad") |>
  group_by(squad) |>
  distinct(matchday, .keep_all = TRUE) |>
  ungroup()

current_standings_team <- current_standings |> select(squad, team) |> mutate(squad = team)



team_colours <- tribble(
  ~team, ~colour,
  "Arsenal", "#DB0007",
  "Bournemouth", "#B50E12",
  "Leeds", "#FFCD00",
  "Crystal Palace", "#1B458F",
  "Leicester", "#003090",
  "Man City", "#6CABDD",
  "Newcastle", "#241F20",
  "Spurs", "#FFFFFF",
  "West Ham", "#7A263A",
  "Aston Villa", "#95bfe5",
  "Brighton", "#0057B8",
  "Chelsea", "#034694",
  "Everton", "#003399",
  "Liverpool", "#c8102E",
  "Man Utd", "#DA291C",
  "Fulham", "#000000",
  "Southampton", "#d71920",
  "Nottm Forest", "#e53233",
  "Wolves", "#FDB913",
  "Brentford", "#e30613"
)


current_standings_team

current_standings_team <- current_standings_team |> left_join(team_colours, by = "team")

current_standings_team <- current_standings_team |> mutate(team = squad)

#strip <- strip_themed(background_x = elem_list_rect(fill = current_standings_team$colour.y))


current_standings_team <- current_standings_team |>
  left_join(team_colours, by = "team") |>
  group_by(squad) |>
  distinct(team, .keep_all = TRUE) |>
  ungroup() |>
  mutate(team = squad)

strip <- strip_themed(background_x = elem_list_rect(fill = current_standings_team$colour.y))




df_total2 <- df_total |> left_join(current_standings_team, by = "squad")






df_total2 |>
  ggplot(aes(x = matchday, y = rk, group = team)) +
  geom_bump() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_reverse() +
  facet_wrap2(~team, strip = strip)

