

library(tidyverse)
library(ggbump)
library(ggh4x)
library(worldfootballR)
library(showtext)
library(rvest)
font_add_google("Josefin Sans", "josefin")
font_add_google("Source Sans Pro", "sourcesans")
font_add_google("Signika Negative", "signika")


showtext_auto()


get_current_matchweek <- function() {
  url <- ("https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1")

  html <- read_html(url)


  text <- html_nodes(html, ".fl") |>
    html_children() |>
    html_attr("href")

  text2 <- stringr::str_remove(text, "/premier-league/spieltag/wettbewerb/GB1/spieltag/")

  text2 |> str_extract("^.{2}") |> as.numeric() |> mean()

}

current_matchweek <- get_current_matchweek()

all_data <- data.frame()
for (i in 1:current_matchweek) {
  print(paste("Getting Matchweek:", i))
  data <- tm_matchday_table(country_name = "England", start_year = "2022", matchday = i)
  all_data <- rbind(all_data, data)
}

all_data <- all_data |>
  group_by(squad) |>
  distinct(matchday, .keep_all = TRUE) |>
  ungroup()



team_colours <- tribble(
  ~team, ~colour, ~text_colour,
  "Arsenal", "#DB0007", "white",
  "Bournemouth", "#B50E12", "white",
  "Leeds", "#FFCD00", "#000000",
  "Crystal Palace", "#1B458F", "white",
  "Leicester", "#003090", "white",
  "Man City", "#6CABDD", "white",
  "Newcastle", "#241F20", "white",
  "Spurs", "#F0F0F0", "#241F20",
  "West Ham", "#7A263A", "white",
  "Aston Villa", "#95bfe5", "#000000",
  "Brighton", "#0057B8", "white",
  "Chelsea", "#034694", "white",
  "Everton", "#003399", "white",
  "Liverpool", "#c8102E", "white",
  "Man Utd", "#DA291C", "white",
  "Fulham", "#000000", "white",
  "Southampton", "#d71920", "white",
  "Nottm Forest", "#e53233", "white",
  "Wolves", "#FDB913", "#000000",
  "Brentford", "#e30613", "white"
) |> rowid_to_column()




current_standings <- tm_matchday_table("England", "2022", 20) |>
  mutate(team = fct_inorder(squad)) |>
  relocate(team, .after = "squad") |>
  group_by(squad) |>
  distinct(matchday, .keep_all = TRUE) |>
  ungroup() |>
  select(squad, team) |>
  mutate(squad = team) |>
  left_join(team_colours, by = "team") |>
  mutate(team = squad)


strip <- strip_themed(
  background_x = elem_list_rect(fill = current_standings$colour),
  text_x = elem_list_text(colour = current_standings$text_colour)
)




all_data <- all_data |> left_join(current_standings, by = "squad")


plot <- all_data |>
  ggplot(aes(x = matchday, y = rk, group = team)) +
  geom_bump() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_reverse() +
  facet_wrap2(~team, ncol = 5, strip = strip) +
  labs(title = "English Premier League (2022/23)", subtitle = paste("League rank over time ending matchweek ", current_matchweek)) +
  theme(text = element_text(family = "josefin", size = 52)) +
  labs(caption = "Source: Transfermarkt via worldfootballR                                    Joe Chelladurai")


ggsave(paste0("plot", current_matchweek, ".png"))


plot

getwd()
# TO DO

# library(rvest)
# get_current_matchweek <- function() {
#   url <- ("https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1")
#
#   html <- read_html(url)
#
#   text <- html_nodes(html, ".fl") |>
#     html_children() |>
#     html_attr("href")
#
#   text2 <- stringr::str_remove(text, "/premier-league/spieltag/wettbewerb/GB1/spieltag/")
#
#   text2 |>
#     str_extract("^.{2}") |>
#     as.numeric() |>
#     mean()
# }
