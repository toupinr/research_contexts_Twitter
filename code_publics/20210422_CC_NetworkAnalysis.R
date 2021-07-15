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

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network")
rtmentions <- read_excel("Data.xlsx", sheet="RTMentions", col_names = TRUE)
tweets <- read_excel("Data.xlsx", sheet="Tweets", col_names = TRUE)

tweets_sum <- tweets %>%
  filter(!RT %in% "NULL") %>%
  group_by(id_art) %>%
  summarise(nRTs = n())

RTMT <- rtmentions %>%
  group_by(id_art) %>%
  summarise(nRT = length(which(Tweet == "RT")),
            nMT = length(which(Tweet == "MT")),
            RtoM = round(nRT/nMT, digits = 0))

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
  left_join(tweets_sum, by = "id_art") %>%
  mutate(RT_share = percent(nRTs/tweets, accuracy = .1)) %>%
  left_join(RTMT %>% select(id_art, RtoM), by = "id_art")



#create data table - general structure of networks
generaltab <- papersfil %>%
  select(titre, Revue, users, mean_tweet, Tweets, compnodes, compedges, RT_share, RtoM) %>%
  arrange(desc(users)) %>%
  ungroup()

gt_table <- generaltab %>%
  gt(rowname_col = "titre") %>%
  tab_stubhead(label = "Titre") %>%
  cols_label(
    Revue = "Journal",
    users = "Nb comptes total",
    mean_tweet = "Nb moyen de tweets par compte",
    Tweets = "Plus grand nb de tweets par un compte",
    compnodes = "% de comptes dans plus grande composante",
    compedges = "% de liens dans plus grande composante",
    RT_share = "% de RTs",
    RtoM = "Ratio RTs:MTs"
  )

gt_table <- gt_table %>% 
  tab_style(style = list(
    cell_text(font = "Times", size = px(14))
  ),
  locations = list(cells_body(), cells_stub(), cells_stubhead(), cells_column_labels(1:8))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center")
  ),
  locations = list(cells_body(1:9))
  ) %>%
  tab_style(style = list(
    cell_text(align = "center", font = "Times", size = px(16), weight = "bolder")
  ),
  locations = list(cells_column_labels(1:8), cells_stubhead())
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