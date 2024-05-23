# Matthew Lindeman Sec 001


# How I Retrieved the Data ------------------------------------------------

# Here is the URL to the website I found my data from:
# https://www.kaggle.com/justinas/nba-players-data/version/3
# I downloaded the csv file from there and imported that into R
# This data set has information about all the NBA player's stats
# From the 1996 season to the 2020 season

library(readr)
all_seasons <- read_csv("Proj250/all_seasons.csv")

# filter(), arrange(), mutate(), select(), summarize()
# Cleaning/Transforming the Data Set --------------------------------------

library(tidyverse)

cleaned_all_seasons <- all_seasons %>%
  mutate(player_height = player_height / 2.54,
         player_weight = round(player_weight * 2.20462))
# This changes the player's heights from centimeters to inches
# and changes their weights from kilograms to pounds

cleaned_all_seasons <- cleaned_all_seasons %>%
  select(-(college:draft_number))
# This removes the columns that are less important to the analysis of
# the player's statistics
# Columns removed: college, country, draft_year, draft_round, draft_year

cleaned_all_seasons <- cleaned_all_seasons %>%
  filter(...1 > 9074) %>% # All the players are numbered, 9075 is the first player of the 2011-12 season
  filter(gp > 9)
# This makes it so only data from the last five years are being used
# Seasons 2016-17 to 2020-21
# It also accounts for players statistics that may be misleading
# because they were injured (or another reason) and weren't able
# to play in at least 10 games

cleaned_all_seasons <- cleaned_all_seasons %>%
  arrange(desc(pts))
# This arranges the data set so that the players are in order based
# on how many points per game they averaged over the season


# Plotting Graphics -------------------------------------------------------

library(ggthemes)

points_by_age <- ggplot(data = cleaned_all_seasons, mapping = aes(x = age, y = pts, color = gp)) +
  geom_point(alpha = 1/3) +
  geom_smooth(se = FALSE, color = "black") +
  labs(
    title = "NBA Players Points Per Game by Age",
    subtitle = "For 2016-2021 NBA Seasons",
    caption = "Black line represents a regression line describing
    the relationship of PPG and Age",
    x = "Age in Years",
    y = "Points Per Game",
    color = "Games Played"
  ) +
  theme_clean()

cleaned_all_seasons$player_height <- as.character(cleaned_all_seasons$player_height)
# So that player_height can work as a fill option in geom_bar

decline_in_height <- ggplot(data = cleaned_all_seasons, mapping = aes(x = season, fill = player_height)) +
  geom_bar(position = "fill") +
  labs(
    title = "Decline in NBA Player Height",
    subtitle = "For 2016-2021 NBA Seasons",
    x = "NBA Season",
    y = "Proportion per Height",
    fill = "Height in Inches"
  ) +
  theme_clean()


# Saving Objects ----------------------------------------------------------

write.csv(cleaned_all_seasons, "~/STAT 250/Proj250/cleaned_all_seasons.csv", row.names = FALSE)

library(ggpubr)

ggarrange(points_by_age, decline_in_height) # Combining plots

ggsave(filename = "Proj250.pdf")


