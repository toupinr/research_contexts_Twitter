## =================
# Rémi code for paper temporal diffusion
# created: 03 Feb 2022

library(ggplot2)
library(ggpubr)
library(showtext)
library(lubridate)
library(dplyr)

showtext_auto()

tweets <- read.csv("AllTweets.csv", header = T, encoding = "UTF-8", sep=";")
tweets$Posted_on <- as_datetime(tweets$Posted_on)
tweets$Day <- as_date(tweets$Posted_on)
tweets$id_art <- as.character(as.numeric(tweets$id_art))
tweets$tweet_id <- as.character(as.numeric(tweets$tweet_id))

tweets_temp <- tweets %>%
  group_by(id_art) %>%
  arrange(Posted_on, by_group = T) %>%
  filter(row_number() <= floor(n()/2)) %>%
  summarise(halflife = round(time_length(interval(start = min(Posted_on), end = max(Posted_on)), unit = "day"), digits = 0))

tweets <- left_join(tweets, tweets_temp, by = "id_art")

tweets_day <- tweets %>%
  group_by(id_art, Day) %>%
  summarise(tweetsday = n_distinct(tweet_id),
            halflife = halflife) %>%
  distinct()
  

tweets_sorted <- tweets %>%
  mutate(id_art = fct_reorder(id_art, halflife))

tweets_sorted$id_art <- gsub("53869748", "Fossil fuels", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("54363041", "Syrian drought", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("54641347", "Extinction risk", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("55016289", "Bumblebees", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("55701738", "Public health", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("56833212", "Dietary change", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("56998671", "Food production", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("57098278", "Harappa", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("57313486", "EU Nature legislation", tweets_sorted$id_art)
tweets_sorted$id_art <- gsub("59541576", "Ecological networks", tweets_sorted$id_art)

tweets_sorted2 <- tweets_day %>%
  mutate(id_art = fct_reorder(id_art, halflife))

tweets_sorted2$id_art <- gsub("53869748", "Fossil fuels", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("54363041", "Syrian drought", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("54641347", "Extinction risk", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("55016289", "Bumblebees", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("55701738", "Public health", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("56833212", "Dietary change", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("56998671", "Food production", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("57098278", "Harappa", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("57313486", "EU Nature legislation", tweets_sorted2$id_art)
tweets_sorted2$id_art <- gsub("59541576", "Ecological networks", tweets_sorted2$id_art)


#plot theme
theme_set(theme_light(base_size = 12, base_family = "Times"))

set.seed(2022)

date_graph <- ggplot(tweets_sorted, aes(x = id_art, y = Day, color = id_art)) + 
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_jitter(size = 1.5, alpha = 0.2, width = 0.2) +
  coord_flip() +
  ggsci::scale_color_aaas() +
  scale_y_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
  labs(title = "", x = "", y = "Timeline of tweets") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 12, family = "Times", face = "bold"),
    axis.title = element_text(size = 10, family = "Times"),
    axis.text.x = element_text(size = 10, family = "Times"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic"),
    strip.text.x = element_text(colour = "black", size = 10, face = "bold"),
    legend.text = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 30,  # Top margin
                         r = 30,  # Right margin
                         b = 30,  # Bottom margin
                         l = 30) # Left margin
  )

date_graph