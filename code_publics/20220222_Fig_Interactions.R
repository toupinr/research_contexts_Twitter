## =================
# Rémi code for Users ID
# created: 25 Nov 2019

library(ggplot2)
library(ggpubr)
library(showtext)
library(dplyr)
library(patchwork)

showtext_auto()

#plot theme
theme_set(theme_light(base_size = 12, base_family = "Times"))


#create plot for proportions
proportions_graph <- proportions_sorted %>%
  filter(component == "total") %>%
  ggplot(aes(x = groupcat, y = proportions)) +
  scale_shape_manual(values=1:10) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, aes(group = factor(article), shape = factor(article), color = factor(article))) +
  #facet_wrap(~ component, scales = "free") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = seq(0,100,10)) +
  labs(title = "", x = "", y = "% of users per article", colour = "Article", shape = "Article") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 12, family = "Times", face = "bold"),
    axis.text.x = element_text(size = 12, family = "Times"),
    axis.text.y = element_text(size = 12, family = "Times", face = "bold"),
    strip.text.x = element_text(colour = "black", size = 12, face = "bold"),
    legend.text = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 30,  # Top margin
                         r = 30,  # Right margin
                         b = 30,  # Bottom margin
                         l = 30) # Left margin
  )
proportions_graph  

proportions_sorted2 <- proportions_sorted %>%
  filter(component != "total")

propmean <- proportions_sorted2 %>%
  group_by(groupcat, component) %>%
  summarise(mean = mean(proportions))

proportions_graph2 <- ggplot(proportions_sorted2, aes(x = groupcat, y = proportions)) + 
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  stat_summary(fun=mean, geom = "errorbar", linetype = "dashed") +
  geom_text(data = propmean, aes(x = groupcat, y = mean, label = mean), family = "Times", size = 4, vjust = "bottom", hjust = "left") +
#  geom_point(size = 2, alpha = 1) +
  facet_grid(~ component, scales = "free") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 70), expand = c(0, 0), breaks = seq(0,100,10)) +
  labs(title = "", x = "", y = "% of users per component") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, family = "Times", face = "bold"),
    axis.title = element_text(size = 12, family = "Times", face = "bold"),
    axis.text.x = element_text(size = 12, family = "Times"),
    axis.text.y = element_text(size = 12, family = "Times", face = "bold"),
    strip.text.x = element_text(colour = "black", size = 12, face = "bold"),
    legend.text = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 30,  # Top margin
                         r = 30,  # Right margin
                         b = 30,  # Bottom margin
                         l = 30) # Left margin
  )
proportions_graph2  

prop_anova_two <- aov(proportions ~ groupcat + component, data = proportions_sorted2)
summary(prop_anova_two)
prop_anova_inter <- aov(proportions ~ groupcat * component, data = proportions_sorted2)
summary(prop_anova_inter)

proportions_spread <- proportions_sorted2 %>%
  select(article, groupcat, component, proportions) %>%
  spread(key=component, value=proportions) %>%
  select(-article)
proportions_chi <- chisq.test(as.matrix(proportions_spread[,-1]))
proportions_chi

#create plot for journals
interactions_graph <- interactions_sorted %>%
  dplyr::filter(Tweet == "RT") %>%
  ggplot(aes(x = grouptarget, y = interactions)) +
  scale_shape_manual(values=1:10) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, aes(group = factor(article), shape = factor(article), colour = factor(article))) +
  facet_wrap(~ groupsource, scales = "free") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = "Groups being retweeted", y = "% of retweets", colour = "Article", shape = "Article") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12, family = "Times", face = "bold"),
    axis.title = element_text(size = 10, family = "Times", face = "bold"),
    axis.text.x = element_text(size = 10, family = "Times"),
    axis.text.y = element_text(size = 10, family = "Times", face = "bold"),
    strip.text.x = element_text(colour = "black", size = 10, face = "bold"),
    legend.text = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 30,  # Top margin
                         r = 30,  # Right margin
                         b = 30,  # Bottom margin
                         l = 30) # Left margin
  )
interactions_graph  

interactions_graph2 <- interactions_sorted %>%
  dplyr::filter(Tweet == "MT") %>%
  ggplot(aes(x = grouptarget, y = interactions)) +
  scale_shape_manual(values=1:10) +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, aes(group = factor(article), shape = factor(article), colour = factor(article))) +
  facet_wrap(~ groupsource, scales = "free") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(x = "Groups being mentioned", y = "% of mentions", colour = "Article", shape = "Article") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 12, family = "Times", face = "bold"),
    axis.title = element_text(size = 10, family = "Times", face = "bold"),
    axis.text.x = element_text(size = 10, family = "Times"),
    axis.text.y = element_text(size = 10, family = "Times", face = "bold"),
    strip.text.x = element_text(colour = "black", size = 10, face = "bold"),
    legend.text = element_text(margin = margin(r = 10)),
    plot.margin = margin(t = 30,  # Top margin
                         r = 30,  # Right margin
                         b = 30,  # Bottom margin
                         l = 30) # Left margin
  )
interactions_graph2 

interactions_final <- ggarrange(interactions_graph, interactions_graph2, nrow = 2, common.legend = TRUE, legend = "bottom") 
interactions_final