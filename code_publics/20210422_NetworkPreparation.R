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
setwd("...")

#import edges list (RTs and mentions)
data <- read.csv("RTMentions.csv", header = TRUE, sep = ";")
data$Source <- as.character(data$Source)
data$Target <- as.character(data$Target)
data$Source <- tolower(data$Source)
data$Target <- tolower(data$Target)

#import nodes list
nodes <- read.table("UsersID.txt", header = TRUE, sep = ",")
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
tweets <- read_excel("Data.xlsx", sheet="Users", col_names = TRUE)
tweets2 <- read_excel("Data.xlsx", sheet="Tweets", col_names = TRUE)

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
network <- graph_from_data_frame(data, vertices = nodes_edges_2) %>% as_tbl_graph()

nodes_edges_2$comp <- components(network)$membership
V(network)$comp <- components(network)$membership
nodes_edges_2 %>%
  count(comp) %>%
  arrange(desc(n))
maincomp <- induced_subgraph(network, V(network)$comp==1)
nodescp <- percent(gorder(maincomp)/gorder(network),accuracy = .1)
edgescp <- percent(gsize(maincomp)/gsize(network),accuracy = .1)

write_csv(nodes_edges_2, "Gephi_Nodes.csv")
write_csv(data, "Gephi_RTMentions.csv")
