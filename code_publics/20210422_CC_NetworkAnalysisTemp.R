## =================
# Rémi Toupin
# Projet : Network thesis
# created: 29 Mar 2020
# updated: 29 Mar 2021
# Encoding : UTF-8

library(igraph)
library(pracma)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(graphlayouts)
library(lubridate)

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("/Volumes/LACIE SHARE/1 - Thèse/Métho/1-Publics of Climate Change Research/Analyses R/Données_Articles/Network")
rtmentions <- read_excel("Data.xlsx", sheet="RTMentions", col_names = TRUE)
tweets <- read_excel("Data.xlsx", sheet="Tweets", col_names = TRUE)
tweets$Posted_on <- as_datetime(tweets$Posted_on)

tweets <- tweets %>%
  left_join(paperscp %>% select(id_art, datepubli), by = "id_art")
tweets$datepubli <- as_date(tweets$datepubli)

tweets_temp <- tweets %>%
  group_by(id_art) %>%
  arrange(Posted_on, by_group = T) %>%
  filter(row_number() <= floor(n()/2)) %>%
  summarise(halflife = round(time_length(interval(start = min(Posted_on), end = max(Posted_on)), unit = "day"), digits = 0))

tweets_delay <- tweets %>%
  group_by(id_art) %>%
  arrange(Posted_on, by_group = T) %>%
  filter(Posted_on <= datepubli) %>%
  summarise(daydelay = round(time_length(interval(start = min(Posted_on), end = max(Posted_on)), unit = "day"), digits = 0),
            tweetsdelay = n())

#select papers for analysis
userspapers <- rename(userspapers, "id_art" = "ID_Art")
papersfil <- papers %>%
  filter(id_art %in% papers10) %>%
  mutate(mean_tweet = round(tweets/users, digits = 1)) %>%
  left_join(userspapers %>% select(id_art, Tweets), by = "id_art") %>%
  group_by(id_art) %>%
  arrange(desc(Tweets), by_group=T) %>%
  filter(Tweets == max(Tweets)) %>%
  distinct() %>%
  ungroup() %>%
  left_join(paperscp, by = "id_art") %>%
  left_join(tweets_temp, by = "id_art") %>%
  left_join(tweets_delay %>% select(id_art, daydelay, tweetsdelay), by = "id_art")

papersfil$firsttweet <- as_datetime(papersfil$firsttweet)
papersfil$lasttweet <- as_datetime(papersfil$lasttweet)

#create data table - general structure of networks
generaltab <- papersfil %>%
  select(titre, Revue, users, tweetspan, halflife, daydelay, tweetsdelay) %>%
  arrange(desc(users)) %>%
  ungroup()
generaltab[is.na(generaltab)] <- 0

gt_table <- generaltab %>%
  gt(rowname_col = "titre") %>%
  tab_stubhead(label = "Titre") %>%
  cols_label(
    Revue = "Journal",
    users = "Nb comptes total",
    tweetspan = "Twitter timespan",
    halflife = "Twitter halflife",
    daydelay = "Nb days between 1st t and publi",
    tweetsdelay = "Nb tweets before publi"
  )

gt_table <- gt_table %>% 
  tab_style(style = list(
    cell_text(font = "Times", size = px(14))
  ),
  locations = list(cells_body(), cells_stub(), cells_stubhead(), cells_column_labels(1:6))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center")
  ),
  locations = list(cells_body(1:7))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center", font = "Times", size = px(16), weight = "bolder")
  ),
  locations = list(cells_column_labels(1:6), cells_stubhead())
  ) %>%
  tab_style(style = list(
    cell_text(style = "italic")
  ),
  locations = list(cells_body(2))
  ) %>%
  tab_style(style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_stub())
  )

gt_table