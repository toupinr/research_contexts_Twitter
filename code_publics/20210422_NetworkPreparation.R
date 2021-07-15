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
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/53869748")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions53869748.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID53869748.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("53869748Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("53869748Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
users <- users %>% rename(Label = Author_id_on_Source)
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
)

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
)

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network53869748 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network53869748)$membership
V(network53869748)$comp <- components(network53869748)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp53869748 <- induced_subgraph(network53869748, V(network53869748)$comp==1)
nodescp53869748 <- percent(gorder(maincomp53869748)/gorder(network53869748),accuracy = .1)
edgescp53869748 <- percent(gsize(maincomp53869748)/gsize(network53869748),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/54363041")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions54363041.csv", header = TRUE, sep = ",")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID54363041.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("54363041Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("54363041Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network54363041 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network54363041)$membership
V(network54363041)$comp <- components(network54363041)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp54363041 <- induced_subgraph(network54363041, V(network54363041)$comp==203)
nodescp54363041 <- percent(gorder(maincomp54363041)/gorder(network54363041),accuracy = .1)
edgescp54363041 <- percent(gsize(maincomp54363041)/gsize(network54363041),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/54641347")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions54641347.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID54641347.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("54641347Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("54641347Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network54641347 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network54641347)$membership
V(network54641347)$comp <- components(network54641347)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp54641347 <- induced_subgraph(network54641347, V(network54641347)$comp==8)
nodescp54641347 <- percent(gorder(maincomp54641347)/gorder(network54641347),accuracy = .1)
edgescp54641347 <- percent(gsize(maincomp54641347)/gsize(network54641347),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("/Volumes/LACIE SHARE/1 - Thèse/Métho/1-Publics of Climate Change Research/Analyses R/Données_Articles/Network/55016289")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions55016289.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID55016289.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("55016289Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("55016289Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network55016289 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network55016289)$membership
V(network55016289)$comp <- components(network55016289)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp55016289 <- induced_subgraph(network55016289, V(network55016289)$comp==1)
nodescp55016289 <- percent(gorder(maincomp55016289)/gorder(network55016289),accuracy = .1)
edgescp55016289 <- percent(gsize(maincomp55016289)/gsize(network55016289),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/55701738")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions55701738.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID55701738.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("55701738Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("55701738Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users <- users %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network55701738 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network55701738)$membership
V(network55701738)$comp <- components(network55701738)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp55701738 <- induced_subgraph(network55701738, V(network55701738)$comp==1)
nodescp55701738 <- percent(gorder(maincomp55701738)/gorder(network55701738),accuracy = .1)
edgescp55701738 <- percent(gsize(maincomp55701738)/gsize(network55701738),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/56833212")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions56833212.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID56833212.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("56833212Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("56833212Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network56833212 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network56833212)$membership
V(network56833212)$comp <- components(network56833212)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp56833212 <- induced_subgraph(network56833212, V(network56833212)$comp==38)
nodescp56833212 <- percent(gorder(maincomp56833212)/gorder(network56833212),accuracy = .1)
edgescp56833212 <- percent(gsize(maincomp56833212)/gsize(network56833212),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/56998671")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions56998671.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID56998671.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("56998671Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("56998671Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network56998671 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network56998671)$membership
V(network56998671)$comp <- components(network56998671)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp56998671 <- induced_subgraph(network56998671, V(network56998671)$comp==5)
nodescp56998671 <- percent(gorder(maincomp56998671)/gorder(network56998671),accuracy = .1)
edgescp56998671 <- percent(gsize(maincomp56998671)/gsize(network56998671),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/57098278")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions57098278.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID57098278.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("57098278Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("57098278Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network57098278 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network57098278)$membership
V(network57098278)$comp <- components(network57098278)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp57098278 <- induced_subgraph(network57098278, V(network57098278)$comp==3)
nodescp57098278 <- percent(gorder(maincomp57098278)/gorder(network57098278),accuracy = .1)
edgescp57098278 <- percent(gsize(maincomp57098278)/gsize(network57098278),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/57313486")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions57313486.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID57313486.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("57313486Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("57313486Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label

##=================
# create igraph object with edges and vertices
##==================
network57313486 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network57313486)$membership
V(network57313486)$comp <- components(network57313486)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp57313486 <- induced_subgraph(network57313486, V(network57313486)$comp==2)
nodescp57313486 <- percent(gorder(maincomp57313486)/gorder(network57313486),accuracy = .1)
edgescp57313486 <- percent(gsize(maincomp57313486)/gsize(network57313486),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

## =================
# load network info (bibliographic coupling - nodes, edges, weight, type)
## =================

#set working directory
setwd("~/Documents/Thèse/Thèse Perso/Analyses/1-Publics of Climate Change Research/Analyses R/Articles/Network/59541576")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions59541576.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID59541576.txt", header = TRUE, sep = ",")
nodes$ID <- as.character(nodes$ID)
nodes$Label <- gsub(" ", "", nodes$Label)
nodes$Label <- tolower(nodes$Label)

#join edge nodes
nodes2 <- nodes %>%
  rename(Source = Label) %>%
  full_join(data %>% select(Source), by = "Source") %>%
  rename(Target = Source) %>%
  full_join(data %>% select(Target), by = "Target") %>%
  distinct() %>%
  rename(Label = Target)

#import tweets user list
tweets <- read_excel("59541576Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("59541576Data.xlsx", sheet="Tweets", col_names = TRUE)

#rename Label from users (code Publics) and tweets objets
tweets <- tweets %>% rename(Label = Author_id_on_Source)
users$Label <- tolower(users$Label)
tweets$Label <- tolower(tweets$Label)

#join users categories to tweets users list
tweets <- tweets %>% 
  left_join(users %>% select(Label, Academic, Communication, Personal, Bots, Organization, Professional, Political, Publishers), by = "Label")
tweets <- tweets[!duplicated(tweets$Label), ]

#join tweets users info to nodes list
nodes3 <- nodes2 %>%
  left_join(tweets, by ="Label")

#create tweet ordering by time
node_edges <- nodes3[order(nodes3$`first tweet`),]
node_edges <- mutate(node_edges, order = row_number())

#create main group for nodes
node_edges$Academic <- as.character(node_edges$Academic)
node_edges$Academic[is.na(node_edges$Academic)] <- 0
node_edges$Communication <- as.character(node_edges$Communication)
node_edges$Communication[is.na(node_edges$Communication)] <- 0
node_edges$Personal <- as.character(node_edges$Personal)
node_edges$Personal[is.na(node_edges$Personal)] <- 0
node_edges$Political <- as.character(node_edges$Political)
node_edges$Political[is.na(node_edges$Political)] <- 0
node_edges$Professional <- as.character(node_edges$Professional)
node_edges$Professional[is.na(node_edges$Professional)] <- 0
node_edges$Publishers <- as.character(node_edges$Publishers)
node_edges$Publishers[is.na(node_edges$Publishers)] <- 0
node_edges$Organization <- as.character(node_edges$Organization)
node_edges$Organization[is.na(node_edges$Organization)] <- 0
node_edges$Bots <- as.character(node_edges$Bots)
node_edges$Bots[is.na(node_edges$Bots)] <- 0

nodes_edges_2 <- node_edges %>%
  mutate(groupcat = if_else(
    Academic == 1, "Academia", (
      if_else(
        Communication == 1, "Communication", if_else(
          Political == 1, "Political", if_else(
            Professional == 1, "Pro", if_else(
              Personal == 1, "Personal", "Unassigned")
          )
        )
      )
    )
  )
  )

nodes_edges_2 <- nodes_edges_2 %>%
  mutate(grouporg = if_else(
    Publishers == 1, "Publishers", (
      if_else(
        Bots == 1, "Bots", if_else(
          Organization == 1, "Organization", "Unassigned")
      )
    )
  )
  )

nodes_edges_2$ID <- nodes_edges_2$Label


##=================
# create igraph object with edges and vertices
##==================
network59541576 <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network59541576)$membership
V(network59541576)$comp <- components(network59541576)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp59541576 <- induced_subgraph(network59541576, V(network59541576)$comp==1)
nodescp59541576 <- percent(gorder(maincomp59541576)/gorder(network59541576),accuracy = .1)
edgescp59541576 <- percent(gsize(maincomp59541576)/gsize(network59541576),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")

#create main component vectors for nodes and edges
papers10 <- c(53869748, 54363041, 54641347, 55016289, 55701738, 56833212, 56998671, 57098278, 57313486, 59541576)
compnodes <- c(nodescp53869748, nodescp54363041, nodescp54641347, nodescp55016289, nodescp55701738, nodescp56833212, nodescp56998671, nodescp57098278, nodescp57313486, nodescp59541576)
compedges <- c(edgescp53869748, edgescp54363041, edgescp54641347, edgescp55016289, edgescp55701738, edgescp56833212, edgescp56998671, edgescp57098278, edgescp57313486, edgescp59541576)
datepubli <- c("2015-01-07", "2015-03-17", "2015-05-01", "2015-07-10", "2015-06-22", "2016-04-12", "2016-03-02", "2016-05-26", "2015-07-10", "2016-12-23")

paperscp <- tibble(id_art = papers10, compnodes, compedges, datepubli)
